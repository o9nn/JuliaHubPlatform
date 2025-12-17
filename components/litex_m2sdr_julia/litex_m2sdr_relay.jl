using VRT, Accessors, Sockets, Unitful
using SoapySDR, SoapyRemote_jll
using ArgParse

dev = nothing

function main(args...)
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--sample_rate", "-r"
            help = "RX sample rate in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 1.8u"MHz").val
        "--frequency", "-f"
            help = "RX frequency in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 95.7u"MHz").val
        "--gain", "-g"
            help = "RX gain in dB"
            arg_type = Float64
            default = 30
        "--bandwidth", "-b"
            help = "RX filter bandwidth in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 2u"MHz").val
        "--channel", "-c"
            help = "RX channel index (0 or 1)"
            arg_type = Int
            range_tester = x -> x in [0, 1]
            default = 0
        "--port", "-p"
            help = "UDP port to use for VRT communication"
            arg_type = Int
            default = 4991
        "--data_ip", "-d"
            help = "Multicast IP address to send replies to"
            arg_type = String
            default = "239.168.1.100"
        "--control_ip", "-x"
            help = "Unicast IP address to listen for control packets on"
            arg_type = String
            default = "127.0.0.1"
        "--stream", "-s"
            help = "Stream ID to use for VRT communication"
            arg_type = UInt32
            default = 0xdeadbeef
    end
    parsed_args = parse_args([args...], s)
    parsed_args === nothing && return

    # initialize the device
    global dev
    if dev === nothing
        devs = Devices(driver="remote", var"driver:remote" = "litexm2sdr")
        dev_args = devs[1]
        dev_args["eth_mode"] = "vrt"
        dev = Device(dev_args)
    end

    # configure a reception channel
    rx_channel = dev.rx[parsed_args["channel"] + 1]
    rx_channel.sample_rate = parsed_args["sample_rate"] * u"Hz"
    rx_channel.frequency = parsed_args["frequency"] * u"Hz"
    rx_channel.bandwidth = parsed_args["bandwidth"] * u"Hz"
    rx_channel.gain = parsed_args["gain"] * u"dB"
    rx_channel.gain_mode = false
    @info "SDR has been configured" rx_channel

    # configure a reception stream
    rx_stream = SoapySDR.Stream(Complex{Int16}, [rx_channel])
    SoapySDR.activate!(rx_stream)

    # where we'll send packets to
    data_socket = UDPSocket()
    data_ip = IPv4(parsed_args["data_ip"])

    # where to receive control packets on
    control_socket = UDPSocket()
    bind(control_socket, IPv4(parsed_args["control_ip"]), parsed_args["port"]; reuseaddr=true)

    # task management
    running = true
    function stop()
        running = false
        close(data_socket)
        close(control_socket)
        finalize(rx_stream)
    end

    # the current state of the SDR represented as a VRT field set
    fields = VRT.VRTFields()
    fields.rf_reference_frequency = parsed_args["frequency"] * u"Hz"
    fields.sample_rate = parsed_args["sample_rate"] * u"Hz"
    fields.bandwidth = parsed_args["bandwidth"] * u"Hz"
    fields.payload_format = VRT.VRTPayloadFormat(
        packing_method = VRT.VRT_DPM_PROCESSING_EFFICIENT,
        repeat_mode = VRT.VRT_DRM_CHANNEL_REPEATING,
        repeat_count = 1,
        vector_size = 1,

        # Complex{Int16}
        real_or_complex = VRT.VRT_DST_COMPLEX_CARTESIAN,
        # ... represented as 16 bytes of only data (no event or channel tags)
        item_packing_field_size = 16,
        data_item_size = 16,
        event_tag_size = 0,
        channel_tag_size = 0,
        # ... encoded as non-normalized signed floating point without a fraction
        data_item_format = VRT.VRT_DIF_SIGNED_FP_NN,
        data_item_fraction_size = 0
    )

    # send periodic context packets
    periodic_context_task = @async begin
        count = 0
        while running
            # create a context packet
            packet = VRTContextPacket()
            packet = @set packet.header.packet_count = count % 16
            packet = VRT.set_stream_id(packet, parsed_args["stream"])
            packet = VRT.set_timestamp(packet, time())
            packet = @set packet.context_fields = fields

            # send the packet
            try
                bytes = sprint(write, packet)
                send(data_socket, data_ip, 4991, bytes)
                count += 1
            catch
                isopen(data_socket) || break
                rethrow()
            end

            sleep(1)
        end
    end

    # respond to control packets
    control_task = @async begin
        count = 0
        while running
            addr, bytes = try
                recvfrom(control_socket)
            catch
                isopen(control_socket) || break
                rethrow()
            end

            # decode the packet
            packet = try
                read(IOBuffer(bytes), AbstractVRTPacket)
            catch e
                @warn "Could not decode data as VRT packet" exception=(e, catch_backtrace())
                continue
            end
            isa(packet, VRTControlPacket) || continue
            @info "Received control packet from $addr" packet

            # make sure this control packet is for us
            if packet.stream_id != parsed_args["stream"]
                @warn "Received control packet with unknown stream ID" packet
                continue
            end
            if packet.controllee !== nothing
                @warn "Received control packet with controllee ID; probably not meant for us" packet
                continue
            end

            if packet.cam.query_state_ack
                # create a query state acknowledge packet
                ack = VRTAcknowledgePacket(:query_state)
                ack = @set ack.header.packet_count = count % 16
                ack = VRT.set_stream_id(ack, packet.stream_id)
                ack = VRT.set_timestamp(ack, time())
                ack = @set ack.cam.query_state_ack = true
                ack = @set ack.message_id = packet.message_id
                ack = VRT.set_controller(ack, packet.controller)
                for (field, value) in packet.control_fields
                    value === missing && continue
                    value = getfield(fields, field)
                    if value === missing
                        @warn "Received query for unset field" field
                        continue
                    end
                    setfield!(ack.context_fields, field, value)
                end

                # send the packet
                @info "Acknowledging query state request" packet=ack
                try
                    bytes = sprint(write, ack)
                    send(data_socket, data_ip, 4991, bytes)
                    count += 1
                catch
                    isopen(data_socket) || break
                    rethrow()
                end

                # TODO: handle reception in stream decoder
                #       `query_state!(processor, stream_id)`
            else
                @warn "Unhandled control packet" packet
            end
        end
    end

    @info "Relay has started"
    @sync begin
        @async begin
            try
                wait(periodic_context_task)
            finally
                stop()
            end
        end
        @async begin
            try
                wait(control_task)
            finally
                stop()
            end
        end
    end
end

isinteractive() || main(ARGS...)
