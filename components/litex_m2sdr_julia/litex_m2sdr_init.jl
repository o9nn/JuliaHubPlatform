#!/usr/bin/env -S julia --project

using ArgParse
using SoapySDR
using Unitful

# Make sure SoapySDR.jl can load system-wide plugins
ENV["SOAPY_SDR_PLUGIN_PATH"] = "/usr/lib/SoapySDR/modules0.8/"

function parse_commandline()
    s = ArgParseSettings(description="LiteXM2SDR init script.")

    @add_arg_table s begin
        "--samplerate"
            help = "RX Sample rate in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 4u"MHz").val
        "--freq"
            help = "RX frequency in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 2.4u"GHz").val
        "--gain"
            help = "RX gain in dB"
            arg_type = Float64
            default = -20
        "--bandwidth"
            help = "RX filter bandwidth in Hz"
            arg_type = Float64
            default = uconvert(u"Hz", 56u"MHz").val
        "--channel"
            help = "RX channel index (0 or 1)"
            arg_type = Int
            range_tester = x -> x in [0, 1]
            default = 0
    end

    return parse_args(s)
end

function to_frequency(val)
    freq = val*u"Hz"
    if freq >= 1e9u"Hz"
        (freq.val / 1e9) * u"GHz"
    elseif freq >= 1e6u"Hz"
        (freq.val / 1e6) * u"MHz"
    elseif freq >= 1e3u"Hz"
        (freq.val / 1e3) * u"kHz"
    else
        freq
    end
end

function main()
    args = parse_commandline()

    # Initialize the LiteXM2SDR device
    devs = Devices(driver="LiteXM2SDR")
    dev_args = devs[1]
    dev_args["eth_mode"] = "vrt"
    dev = Device(dev_args)

    # Get the RX channel
    rx_channel = dev.rx[args["channel"] + 1]

    # Apply basic configuration for the selected RX channel
    rx_channel.sample_rate = args["samplerate"] * u"Hz"
    rx_channel.frequency = args["freq"] * u"Hz"
    rx_channel.gain = args["gain"] * u"dB"
    rx_channel.bandwidth = args["bandwidth"] * u"Hz"

    println("LiteXM2SDR device initialized with:")
    println("  Sample Rate: $(to_frequency(args["samplerate"]))")
    println("  Frequency  : $(to_frequency(args["freq"]))")
    println("  Gain       : $(args["gain"]) dB")
    println("  Bandwidth  : $(to_frequency(args["bandwidth"]))")
    println("  Channel    : $(args["channel"])\n")

    # open and activate the rx stream
    rx_stream = SoapySDR.Stream(Complex{Int16}, [rx_channel])
    SoapySDR.activate!(rx_stream)

    # Keep the device open
    println("Device has been activated. Press RETURN to exit.")
    readline(stdin)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
