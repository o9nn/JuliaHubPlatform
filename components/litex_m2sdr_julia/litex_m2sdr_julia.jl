#!/usr/bin/env -S julia --project

using CondaPkg, PythonCall

using ArgParse, Dates

@py import sys
sys.path.insert(0, @__DIR__)

@py import migen: Signal, ClockDomain, ClockSignal

@py import litex.gen: LiteXModule

@py import litex.soc.interconnect: stream

@py import litex.soc.integration.soc_core: SoCCore
@py import litex.soc.integration.builder: Builder

@py import litex_m2sdr_platform: Platform

@py import litex_m2sdr.gateware.qpll: SharedQPLL

@py import litex.soc.cores.clock: S7PLL, S7IDELAYCTRL
@py import litex.soc.cores.led: LedChaser
@py import litex.soc.cores.icap: ICAP
@py import litex.soc.cores.xadc: XADC
@py import litex.soc.cores.dna: DNA
@py import litex.soc.cores.gpio: GPIOOut
@py import litex.soc.cores.spi_flash: S7SPIFlash

@py import litepcie.phy.s7pciephy: S7PCIEPHY

@py import liteeth.phy.a7_1000basex: A7_1000BASEX, A7_2500BASEX
@py import liteeth.frontend.stream: LiteEthStream2UDPTX

@py import litex_m2sdr.gateware.si5351: SI5351
@py import litex_m2sdr.gateware.ad9361.core: AD9361RFIC
@py import litex_m2sdr.gateware.time: TimeGenerator
@py import litex_m2sdr.gateware.pps: PPSGenerator
@py import litex_m2sdr.gateware.measurement: MultiClkMeasurement

@py import litex_m2sdr.software: generate_litepcie_software

@py import gateware.time: TimeNsToPS
@py import gateware.rfic: RFICDataPacketizer
@py import gateware.vrt: VRTSignalPacketStreamer

# nuke the broken stream_sim deprecation
@pyexec `
import sys

class DummyModule:
    def __getattr__(self, name):
        return lambda *args, **kwargs: None

sys.modules['litex.soc.interconnect.stream_sim'] = DummyModule()
`

CRG = pytype("CRG", (LiteXModule,), [
    "__module__" => "__main__",

    "__init__" => pyfunc(
        function (self, platform, sys_clk_freq; with_eth)
            self.rst            = Signal()
            self.cd_sys         = ClockDomain("sys")
            self.cd_clk10       = ClockDomain("clk10")
            self.cd_idelay      = ClockDomain("idelay")
            self.cd_refclk_pcie = ClockDomain("refclk_pcie")
            self.cd_refclk_eth  = ClockDomain("refclk_eth")

            # # #

            # # Clk / Rst
            clk100 = platform.request("clk100")

            # PLL.
            self.pll = pll = S7PLL(speedgrade=-3)
            pyiadd(self.comb, self.pll.reset.eq(self.rst))  # TODO: @py x += y
            pll.register_clkin(clk100, 100e6)
            pll.create_clkout(self.cd_sys,    sys_clk_freq)
            pll.create_clkout(self.cd_clk10,  10e6)
            pll.create_clkout(self.cd_idelay, 200e6)
            platform.add_platform_command("set_property CLOCK_DEDICATED_ROUTE FALSE [get_nets {{*s7pll0_clkout_buf1}}]") # TODO: Improve.

            # IDelayCtrl.
            self.idelayctrl = S7IDELAYCTRL(self.cd_idelay)

            # Ethernet PLL.
            if Bool(with_eth)
                self.eth_pll = eth_pll = S7PLL()
                eth_pll.register_clkin(self.cd_sys.clk, sys_clk_freq)
                eth_pll.create_clkout(self.cd_refclk_eth, 156.25e6, margin=0)
            end

            return
        end,
    ),
])

BaseSoC = pytype("BaseSoC", (SoCCore,), [
    "__module__" => "__main__",

    "csr_map" => pydict([
        # SoC.
        "ctrl"            => 0,
        "uart"            => 1,
        "icap"            => 2,
        "flash_cs_n"      => 3,
        "xadc"            => 4,
        "dna"             => 5,
        "flash"           => 6,
        "leds"            => 7,
        "identifier_mem"  => 8,
        "timer0"          => 9,

        # PCIe.
        "pcie_phy"        => 10,
        "pcie_msi"        => 11,
        "pcie_dma0"       => 12,

        # Eth.
        "ethphy"          => 14,

        # VRT.
        "vrt_streamer"    => 16,

        # SDR.
        "si5351"          => 20,
        "time"            => 21,
        "ad9361"          => 24,
        "crossbar"        => 25,

        # Measurements/Analyzer.
        "clk_measurement" => 30,
        "analyzer"        => 31,
    ]),

    "__init__" => pyfunc(
        function (self; variant, with_pcie, pcie_lanes,
                        with_eth, eth_sfp, eth_phy, board_ip, vrt_dst_ip,
                        vrt_dst_port, sys_clk_freq=125_000_000,
                        with_jtagbone=true)
            ## Platform

            platform = Platform(build_multiboot=false)

            ## SoCCore

            SoCCore.__init__(
                self,
                platform,
                sys_clk_freq,
                cpu_type=nothing,

                uart_name = "crossover",
                ident = "LiteX SoC on M2SDR / $variant variant / built on $(now())",
                ident_version = true
            )

            ## Clocking

            # General
            self.crg = CRG(platform, sys_clk_freq; with_eth)

            # Shared QPLL.
            self.qpll = SharedQPLL(platform,
                with_pcie = with_pcie,
                with_eth  = with_eth,
                eth_phy   = eth_phy,
            )

            ## SI5351 Clock Generator

            self.si5351 = SI5351(platform; sys_clk_freq, clk_in=platform.request("sync_clk_in"))
            si5351_clk0 = platform.request("si5351_clk0")
            si5351_clk1 = platform.request("si5351_clk1")

            ## Time Generator

            time_s  = Signal(32) # Integer    Time in seconds.
            time_ps = Signal(64) # Fractional Time in picoseconds.

            self.time_gen = TimeGenerator(
                clk        = ClockSignal("sys"),
                clk_freq   = sys_clk_freq,
                with_csr   = true,
            )

            self.time_ns_to_ps = TimeNsToPS(
                time_ns = self.time_gen.time,
                time_s  = time_s,
                time_ps = time_ps,
            )

            # PPS Generator

            self.pps_gen = PPSGenerator(
                clk_freq = 100e6,
                time     = self.time_gen.time,
                reset    = self.time_gen.time_change,
                offset   = 0,
            )
            pyiadd(self.comb, time_s.eq(self.pps_gen.count - 1))

            # JTAGBone

            if Bool(with_jtagbone)
                self.add_jtagbone()
                platform.add_period_constraint(self.jtagbone_phy.cd_jtag.clk, 1e9/20e6)
            end

            # Leds

            self.leds = LedChaser(
                pads         = platform.request_all("user_led"),
                sys_clk_freq = sys_clk_freq
            )

            # ICAP

            self.icap = ICAP()
            self.icap.add_reload()
            self.icap.add_timing_constraints(platform, sys_clk_freq, self.crg.cd_sys.clk)

            # XADC

            self.xadc = XADC()

            # DNA

            self.dna = DNA()
            self.dna.add_timing_constraints(platform, sys_clk_freq, self.crg.cd_sys.clk)

            # SPI Flash

            self.flash_cs_n = GPIOOut(platform.request("flash_cs_n"))
            self.flash      = S7SPIFlash(platform.request("flash"), sys_clk_freq, 25e6)
            self.add_config("FLASH_IMAGE_SIZE", platform.image_size)

            # PCIe

            if Bool(with_pcie)
                if variant == "baseboard"
                    @assert pcie_lanes == 1
                end
                pcie_dmas = 1
                self.pcie_phy = S7PCIEPHY(platform, platform.request("pcie_x$(pcie_lanes)_$(variant)"),
                    data_width  = Dict(1 => 64, 2 => 64, 4 => 128)[pyconvert(Int,pcie_lanes)],
                    bar0_size   = 0x20000,
                    cd          = "sys",
                )
                pyiadd(self.comb, ClockSignal("refclk_pcie").eq(self.pcie_phy.pcie_refclk))
                if variant == "baseboard"
                    platform.toolchain.pre_placement_commands.append("reset_property LOC [get_cells -hierarchical -filter {{NAME=~pcie_s7/*gtp_channel.gtpe2_channel_i}}]")
                    platform.toolchain.pre_placement_commands.append("set_property LOC GTPE2_CHANNEL_X0Y4 [get_cells -hierarchical -filter {{NAME=~pcie_s7/*gtp_channel.gtpe2_channel_i}}]")
                end
                self.pcie_phy.update_config(Dict(
                    "Base_Class_Menu"          => "Wireless_controller",
                    "Sub_Class_Interface_Menu" => "RF_controller",
                    "Class_Code_Base"          => "0D",
                    "Class_Code_Sub"           => "10",
                ))
                self.add_pcie(phy=self.pcie_phy, address_width=64, ndmas=pcie_dmas, data_width=64,
                    with_dma_buffering    = true, dma_buffering_depth=8192,
                    with_dma_loopback     = true,
                    with_dma_synchronizer = true,
                    with_msi              = true
                )
                self.pcie_phy.use_external_qpll(qpll_channel=self.qpll.get_channel("pcie"))
                pyiadd(self.comb, self.pcie_dma0.synchronizer.pps.eq(1))
            end

            # Ethernet

            if Bool(with_eth)
                # PHY.
                eth_phy_cls = Dict(
                    "1000basex" => A7_1000BASEX,
                    "2500basex" => A7_2500BASEX,
                )[pyconvert(String, eth_phy)]
                self.ethphy = eth_phy_cls(
                    qpll_channel = self.qpll.get_channel("eth"),
                    data_pads    = self.platform.request("sfp", eth_sfp),
                    sys_clk_freq = sys_clk_freq,
                    rx_polarity  = 1, # Inverted on M2SDR.
                    tx_polarity  = 0, # Inverted on M2SDR and Acorn Baseboard Mini.
                )
                platform.add_period_constraint(self.ethphy.txoutclk, 1e9/(self.ethphy.tx_clk_freq/2))
                platform.add_period_constraint(self.ethphy.rxoutclk, 1e9/(self.ethphy.tx_clk_freq/2))

                # Core + MMAP (Etherbone).
                self.add_etherbone(phy=self.ethphy,
                    # Core Parameters.
                    mac_address = 0x10e2d5000000,
                    ip_address  = board_ip,
                    data_width  = 32,
                    arp_entries = 4,
                )
            end

            # AD9361 RFIC

            self.ad9361 = AD9361RFIC(
                rfic_pads    = platform.request("ad9361_rfic"),
                spi_pads     = platform.request("ad9361_spi"),
                sys_clk_freq = sys_clk_freq,
            )
            self.ad9361.add_prbs()
            self.ad9361.add_agc()
            self.platform.add_period_constraint(self.ad9361.cd_rfic.clk, 1e9/245.76e6)

            # RX Converter / Packetizer

            self.rx_conv       = stream.Converter(64, 32)
            self.rx_packetizer = RFICDataPacketizer(data_width=32, data_words=256)
            pyiadd(self.comb, self.rx_conv.source.connect(self.rx_packetizer.sink))


            # RX VRT (VITA Radio Transport)

            self.vrt_streamer = vrt_streamer = VRTSignalPacketStreamer(
                udp_crossbar = self.ethcore_etherbone.udp.crossbar,
                ip_address   = vrt_dst_ip,
                udp_port     = vrt_dst_port,
                data_width   = 32,
            )
            pyiadd(self.comb, [
                self.rx_packetizer.source.connect(self.vrt_streamer.sink),
                self.vrt_streamer.sink.stream_id.eq(0xdeadbeef),
                self.vrt_streamer.sink.timestamp_int.eq(time_s),
                self.vrt_streamer.sink.timestamp_fra.eq(time_ps),
            ])

            # RX UDP (Raw)

            udp_streamer_port = self.ethcore_etherbone.udp.crossbar.get_port(2345, dw=64, cd="sys")
            self.udp_streamer = LiteEthStream2UDPTX(
                ip_address = vrt_dst_ip, # FIXME.
                udp_port   = 2345,
                fifo_depth = 1024÷8,
                data_width = 64,
                with_csr   = false,
            )
            pyiadd(self.comb, self.udp_streamer.source.connect(udp_streamer_port.sink))

            # Crossbar

            self.crossbar = stream.Crossbar(layout=pylist([("data", 64)]), n=3, with_csr=true)

            # RFIC -> Crossbar -> VRT / UDP.
            pyiadd(self.comb, [
                self.ad9361.source.connect(self.crossbar.demux.sink),
                # Source 0: Flushing, always ready.
                self.crossbar.demux.source0.ready.eq(1),
                # Source 1: VRT.
                self.crossbar.demux.source1.connect(self.rx_conv.sink),
                # Source 2: UDP.
                self.crossbar.demux.source2.connect(self.udp_streamer.sink),
            ])

            # Timing Constraints/False Paths

            platform.add_false_path_constraints(
                # PCIe.
                #"main_s7pciephy_clkout0", # FIXME.
                #"main_s7pciephy_clkout1", # FIXME.
                #"main_s7pciephy_clkout2", # FIXME.
                #"main_s7pciephy_clkout3", # FIXME.

                # CRG.
                "{{*clk100}}",
                "{{*callbackvalue_s7pll0_clkout0}}",
                "{{*callbackvalue_s7pll0_clkout1}}",

                # Ethernet.
                "{{*eth_rx_clk}}",
                "{{*eth_tx_clk}}",
                "{{*a7_1000basex_rxoutclk}}",
                "{{*a7_1000basex_txoutclk}}",
                "{{*a7_1000basex_s7pll0_clkout0}}",
                "{{*a7_1000basex_s7pll1_clkout0}}",

                # RFIC.
                "{{*rfic_clk}}",

                # Internal Primitives.
                "{{*dna_clk}}",
                "{{*jtag_clk}}",
                "{{*icap_clk}}",

                # Sync.
                "{{*si5351_clk0}}",
                "{{*si5351_clk1}}",
                "{{*sync_clk_in}}",
            )

            # Clk Measurements

            self.clk_measurement = MultiClkMeasurement(clks=pydict([
                "clk0" => ClockSignal("sys"),
                "clk1" => Bool(with_pcie) ? ClockSignal("pcie") : 0,
                "clk2" => si5351_clk0,
                "clk3" => ClockSignal("rfic"),
                "clk4" => si5351_clk1,
            ]))

            return
        end,
    ),
])

using ArgParse

s = ArgParseSettings(description = "LiteX SoC on LiteX-M2SDR.")

@add_arg_table! s begin
    # Build/Load/Utilities
    "--build"
        action = :store_true
        help = "Build bitstream."
    "--load"
        action = :store_true
        help = "Load bitstream."
    "--reset"
        action = :store_true
        help = "Reset the device."
    "--flash"
        action = :store_true
        help = "Flash bitstream."
    "--rescan"
        action = :store_true
        help = "Execute PCIe Rescan while Loading/Flashing."
    "--driver"
        action = :store_true
        help = "Generate PCIe driver from LitePCIe (override local version)."

    # PCIe parameters
    "--with-pcie"
        action = :store_true
        help = "Enable PCIe Communication."
    "--pcie-lanes"
        arg_type = Int
        default = 1
        range_tester = (x -> x in [1, 2, 4])
        help = "PCIe Lanes."

    # Ethernet/VRT parameters
    "--eth-sfp"
        arg_type = Int
        default = 0
        range_tester = (x -> x in [0, 1])
        help = "Ethernet SFP."
    "--eth-phy"
        default = "1000basex"
        range_tester = (x -> x in ["1000basex", "2500basex"])
        help = "Ethernet PHY."
    "--board-ip"
        default = "192.168.1.50"
        help = "Board hardware IP address."
    "--vrt-dst-ip"
        default = "239.168.1.100"
        help = "VRT destination IP address."
    "--vrt-dst-port"
        default = 4991
        help = "VRT destination UDP port."
end

args = parse_args(s)

# Build SoC
soc = BaseSoC(
    # Generic
    variant = "baseboard",

    # PCIe
    with_pcie = args["with-pcie"],
    pcie_lanes = args["pcie-lanes"],

    # Ethernet
    with_eth = true,
    eth_sfp = args["eth-sfp"],
    eth_phy = args["eth-phy"],
    board_ip = args["board-ip"],
    vrt_dst_ip = args["vrt-dst-ip"],
    vrt_dst_port = args["vrt-dst-port"],
)

# Builder
build_name = "litex_m2sdr"
if args["with-pcie"]
    build_name *= "_pcie_x$(args["pcie-lanes"])"
end
builder = Builder(soc; output_dir=joinpath(@__DIR__, "build", build_name), csr_csv="csr.csv")
builder.build(; build_name, run=args["build"])

# Generate LitePCIe Driver
generate_litepcie_software(soc, "software", use_litepcie_software=args["driver"])

# Load Bistream.
if args["load"]
    prog = soc.platform.create_programmer()
    prog.load_bitstream(joinpath(string(builder.gateware_dir), build_name + ".bit"))
end

# Reset Device.
if args["reset"]
    prog = soc.platform.create_programmer()
    prog.reset()
end

# Flash Bitstream.
if args["flash"]
    prog = soc.platform.create_programmer()
    prog.flash(0, joinpath(string(builder.gateware_dir), build_name * ".bin"))
end

# Rescan PCIe Bus.
if args["rescan"]
    run(`sudo sh -c 'cd software && ./rescan.py'`)
end
