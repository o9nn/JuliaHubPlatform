#!/usr/bin/env python3

#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2024-2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

import os
import time
import argparse

from migen import *

from litex.gen import *
from litex.gen.genlib.cdc  import BusSynchronizer
from litex.gen.genlib.misc import WaitTimer

from litex.build.generic_platform import Subsignal, Pins

from litex.soc.interconnect.csr import *
from litex.soc.interconnect     import stream
from litex.soc.interconnect     import packet

from litex.soc.integration.soc_core import *
from litex.soc.integration.builder  import *

from litex.soc.cores.clock     import *
from litex.soc.cores.led       import LedChaser
from litex.soc.cores.icap      import ICAP
from litex.soc.cores.xadc      import XADC
from litex.soc.cores.dna       import DNA
from litex.soc.cores.gpio      import GPIOOut
from litex.soc.cores.spi_flash import S7SPIFlash

from litex.build.generic_platform import IOStandard, Subsignal, Pins

from litepcie.common        import *
from litepcie.phy.s7pciephy import S7PCIEPHY

from liteeth.common           import convert_ip
from liteeth.phy.a7_1000basex import A7_1000BASEX, A7_2500BASEX
from liteeth.frontend.stream  import LiteEthStream2UDPTX, LiteEthUDP2StreamRX

from litescope import LiteScopeAnalyzer

from litex_m2sdr.gateware.si5351      import SI5351
from litex_m2sdr.gateware.si5351_i2c  import SI5351I2C, i2c_program_si5351
from litex_m2sdr.gateware.ad9361.core import AD9361RFIC
from litex_m2sdr.gateware.qpll        import SharedQPLL
from litex_m2sdr.gateware.time        import TimeGenerator
from litex_m2sdr.gateware.pps         import PPSGenerator
from litex_m2sdr.gateware.header      import TXRXHeader
from litex_m2sdr.gateware.measurement import MultiClkMeasurement

from litex_m2sdr import Platform

from litex_m2sdr.software import generate_litepcie_software

from gateware.time import TimeNsToPS
from gateware.vrt  import vrt_signal_packet_user_description
from gateware.vrt  import VRTSignalPacketStreamer
from gateware.rfic import RFICDataGenerator, RFICDataPacketizer

# CRG ----------------------------------------------------------------------------------------------

class CRG(LiteXModule):
    def __init__(self, platform, sys_clk_freq, *, with_eth):
        self.rst            = Signal()
        self.cd_sys         = ClockDomain()
        self.cd_clk10       = ClockDomain()
        self.cd_idelay      = ClockDomain()
        self.cd_refclk_pcie = ClockDomain()
        self.cd_refclk_eth  = ClockDomain()

        # # #

        # Clk / Rst.
        clk100 = platform.request("clk100")

        # PLL.
        self.pll = pll = S7PLL(speedgrade=-3)
        self.comb += self.pll.reset.eq(self.rst)
        pll.register_clkin(clk100, 100e6)
        pll.create_clkout(self.cd_sys,    sys_clk_freq)
        pll.create_clkout(self.cd_clk10,  10e6)
        pll.create_clkout(self.cd_idelay, 200e6)

        # IDelayCtrl.
        self.idelayctrl = S7IDELAYCTRL(self.cd_idelay)

        # Ethernet PLL.
        if with_eth:
            self.eth_pll = eth_pll = S7PLL()
            eth_pll.register_clkin(self.cd_sys.clk, sys_clk_freq)
            eth_pll.create_clkout(self.cd_refclk_eth, 156.25e6, margin=0)

# BaseSoC ------------------------------------------------------------------------------------------

class BaseSoC(SoCCore):
    SoCCore.csr_map = {
        # SoC.
        "ctrl"            : 0,
        "uart"            : 1,
        "icap"            : 2,
        "flash_cs_n"      : 3,
        "xadc"            : 4,
        "dna"             : 5,
        "flash"           : 6,
        "leds"            : 7,
        "identifier_mem"  : 8,
        "timer0"          : 9,

        # PCIe.
        "pcie_phy"        : 10,
        "pcie_msi"        : 11,
        "pcie_dma0"       : 12,

        # Eth.
        "ethphy"          : 14,

        # VRT.
        "vrt_streamer"    : 16,

        # SDR.
        "si5351"          : 20,
        "time"            : 21,
        "ad9361"          : 24,
        "crossbar"        : 25,

        # Measurements/Analyzer.
        "clk_measurement" : 30,
        "analyzer"        : 31,
    }

    def __init__(self, *, variant,
        with_pcie, pcie_lanes,
        with_eth, eth_sfp, eth_phy,
        board_ip, vrt_dst_ip, vrt_dst_port,
        sys_clk_freq  = int(125e6),
        with_jtagbone = True,
    ):
        # Platform ---------------------------------------------------------------------------------

        platform = Platform(build_multiboot=False)

        # SoCCore ----------------------------------------------------------------------------------

        SoCCore.__init__(self, platform, sys_clk_freq,
            cpu_type      = None,
            uart_name     = "crossover",
            ident         = f"LiteX-M2SDR SoC / {variant} variant / built on",
            ident_version = True,
        )

        # Clocking ---------------------------------------------------------------------------------

        # General.
        self.crg = CRG(platform, sys_clk_freq, with_eth=with_eth)

        # Shared QPLL.
        self.qpll = SharedQPLL(platform,
            with_pcie = with_pcie,
            with_eth  = with_eth,
            eth_phy   = eth_phy,
        )

        # SI5351 Clock Generator -------------------------------------------------------------------

        self.si5351 = SI5351(platform, sys_clk_freq=sys_clk_freq, clk_in=platform.request("sync_clk_in"))
        si5351_clk0 = platform.request("si5351_clk0")
        si5351_clk1 = platform.request("si5351_clk1")

        # Time Generator ---------------------------------------------------------------------------

        time_s  = Signal(32) # Integer    Time in seconds.
        time_ps = Signal(64) # Fractional Time in picoseconds.

        self.time_gen = TimeGenerator(
            clk        = ClockSignal("sys"),
            clk_freq   = sys_clk_freq,
            with_csr   = True,
        )

        self.time_ns_to_ps = TimeNsToPS(
            time_ns = self.time_gen.time,
            time_s  = time_s,
            time_ps = time_ps,
        )

        # PPS Generator ----------------------------------------------------------------------------

        self.pps_gen = PPSGenerator(
            clk_freq = 100e6,
            time     = self.time_gen.time,
            reset    = self.time_gen.time_change,
            offset   = 0,
        )
        self.comb += time_s.eq(self.pps_gen.count - 1)

        # JTAGBone ---------------------------------------------------------------------------------

        if with_jtagbone:
            self.add_jtagbone()
            platform.add_period_constraint(self.jtagbone_phy.cd_jtag.clk, 1e9/20e6)

        # Leds -------------------------------------------------------------------------------------

        self.leds = LedChaser(
            pads         = platform.request_all("user_led"),
            sys_clk_freq = sys_clk_freq
        )

        # ICAP -------------------------------------------------------------------------------------

        self.icap = ICAP()
        self.icap.add_reload()
        self.icap.add_timing_constraints(platform, sys_clk_freq, self.crg.cd_sys.clk)

        # XADC -------------------------------------------------------------------------------------

        self.xadc = XADC()

        # DNA --------------------------------------------------------------------------------------

        self.dna = DNA()
        self.dna.add_timing_constraints(platform, sys_clk_freq, self.crg.cd_sys.clk)

        # SPI Flash --------------------------------------------------------------------------------

        self.flash_cs_n = GPIOOut(platform.request("flash_cs_n"))
        self.flash      = S7SPIFlash(platform.request("flash"), sys_clk_freq, 25e6)
        self.add_config("FLASH_IMAGE_SIZE", platform.image_size)

        # PCIe -------------------------------------------------------------------------------------

        if with_pcie:
            if variant == "baseboard":
                assert pcie_lanes == 1
            pcie_dmas = 1
            self.pcie_phy = S7PCIEPHY(platform, platform.request(f"pcie_x{pcie_lanes}_{variant}"),
                data_width  = {1: 64, 2: 64, 4: 128}[pcie_lanes],
                bar0_size   = 0x20000,
                cd          = "sys",
            )
            self.comb += ClockSignal("refclk_pcie").eq(self.pcie_phy.pcie_refclk)
            if variant == "baseboard":
                platform.toolchain.pre_placement_commands.append("reset_property LOC [get_cells -hierarchical -filter {{NAME=~pcie_s7/*gtp_channel.gtpe2_channel_i}}]")
                platform.toolchain.pre_placement_commands.append("set_property LOC GTPE2_CHANNEL_X0Y4 [get_cells -hierarchical -filter {{NAME=~pcie_s7/*gtp_channel.gtpe2_channel_i}}]")
            self.pcie_phy.update_config({
                "Base_Class_Menu"          : "Wireless_controller",
                "Sub_Class_Interface_Menu" : "RF_controller",
                "Class_Code_Base"          : "0D",
                "Class_Code_Sub"           : "10",
                }
            )
            self.add_pcie(phy=self.pcie_phy, address_width=64, ndmas=pcie_dmas, data_width=64,
                with_dma_buffering    = True, dma_buffering_depth=8192,
                with_dma_loopback     = True,
                with_dma_synchronizer = True,
                with_msi              = True
            )
            self.pcie_phy.use_external_qpll(qpll_channel=self.qpll.get_channel("pcie"))
            self.comb += self.pcie_dma0.synchronizer.pps.eq(1)

        # Ethernet ---------------------------------------------------------------------------------

        if with_eth:
            # PHY.
            eth_phy_cls = {
                "1000basex" : A7_1000BASEX,
                "2500basex" : A7_2500BASEX,
            }[eth_phy]
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

        # AD9361 RFIC ------------------------------------------------------------------------------

        self.ad9361 = AD9361RFIC(
            rfic_pads    = platform.request("ad9361_rfic"),
            spi_pads     = platform.request("ad9361_spi"),
            sys_clk_freq = sys_clk_freq,
        )
        self.ad9361.add_prbs()
        self.ad9361.add_agc()
        self.platform.add_period_constraint(self.ad9361.cd_rfic.clk, 1e9/245.76e6)

        # RX Converter / Packetizer ----------------------------------------------------------------

        self.rx_conv       = stream.Converter(64, 32)
        self.rx_packetizer = RFICDataPacketizer(data_width=32, data_words=256)
        self.comb += self.rx_conv.source.connect(self.rx_packetizer.sink)

        # RX VRT (VITA Radio Transport) ------------------------------------------------------------

        self.vrt_streamer = vrt_streamer = VRTSignalPacketStreamer(
            udp_crossbar = self.ethcore_etherbone.udp.crossbar,
            ip_address   = vrt_dst_ip,
            udp_port     = vrt_dst_port,
            data_width   = 32,
        )
        self.comb += [
            self.rx_packetizer.source.connect(self.vrt_streamer.sink),
            self.vrt_streamer.sink.stream_id.eq(0xdeadbeef),
            self.vrt_streamer.sink.timestamp_int.eq(time_s),
            self.vrt_streamer.sink.timestamp_fra.eq(time_ps),
        ]

        # RX UDP (Raw) -----------------------------------------------------------------------------

        udp_streamer_port = self.ethcore_etherbone.udp.crossbar.get_port(2345, dw=64, cd="sys")
        self.udp_streamer = LiteEthStream2UDPTX(
            ip_address = vrt_dst_ip, # FIXME.
            udp_port   = 2345,
            fifo_depth = 1024//8,
            data_width = 64,
            with_csr   = False,
        )
        self.comb += self.udp_streamer.source.connect(udp_streamer_port.sink)

        # RX Datapath ------------------------------------------------------------------------------

        # Crossbar.
        # ---------
        self.crossbar = stream.Crossbar(layout=[("data", 64)], n=3, with_csr=True)

        # RFIC -> Crossbar -> VRT / UDP.
        # -----------------------------------
        self.comb += [
            self.ad9361.source.connect(self.crossbar.demux.sink),
            # Source 0: Flushing, always ready.
            self.crossbar.demux.source0.ready.eq(1),
            # Source 1: VRT.
            self.crossbar.demux.source1.connect(self.rx_conv.sink),
            # Source 2: UDP.
            self.crossbar.demux.source2.connect(self.udp_streamer.sink),
        ]

        # Timing Constraints/False Paths -----------------------------------------------------------

        platform.add_false_path_constraints(
            # PCIe.
            #"main_s7pciephy_clkout0", # FIXME.
            #"main_s7pciephy_clkout1", # FIXME.
            #"main_s7pciephy_clkout2", # FIXME.
            #"main_s7pciephy_clkout3", # FIXME.

            # CRG.
            "{{*clk100}}",
            "{{*crg_s7pll0_clkout0}}",
            "{{*crg_s7pll0_clkout1}}",

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

        # Clk Measurements -------------------------------------------------------------------------

        self.clk_measurement = MultiClkMeasurement(clks={
            "clk0" : ClockSignal("sys"),
            "clk1" : 0 if not with_pcie else ClockSignal("pcie"),
            "clk2" : si5351_clk0,
            "clk3" : ClockSignal("rfic"),
            "clk4" : si5351_clk1,
        })

    # LiteScope Probes (Debug) ---------------------------------------------------------------------

    def add_ad9361_spi_probe(self):
        analyzer_signals = [self.platform.lookup_request("ad9361_spi")]
        self.analyzer = LiteScopeAnalyzer(analyzer_signals,
            depth        = 4096,
            clock_domain = "sys",
            register     = True,
            csr_csv      = "analyzer.csv"
        )

    def add_ad96361_data_probe(self):
        analyzer_signals = [
            self.ad9361.phy.sink,   # TX.
            self.ad9361.phy.source, # RX.
            self.ad9361.prbs_rx.fields.synced,
        ]
        self.analyzer = LiteScopeAnalyzer(analyzer_signals,
            depth        = 4096,
            clock_domain = "rfic",
            register     = True,
            csr_csv      = "analyzer.csv"
        )

    def add_eth_tx_probe(self):
        assert hasattr(self, "eth_streamer")
        analyzer_signals = [
            self.eth_streamer.sink,
        ]
        self.analyzer = LiteScopeAnalyzer(analyzer_signals,
            depth        = 1024,
            clock_domain = "sys",
            register     = True,
            csr_csv      = "analyzer.csv"
        )

# Build --------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="LiteX SoC on LiteX-M2SDR.", formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    # Build/Load/Utilities.
    parser.add_argument("--build",           action="store_true", help="Build bitstream.")
    parser.add_argument("--load",            action="store_true", help="Load bitstream.")
    parser.add_argument("--reset",           action="store_true", help="Reset the device.")
    parser.add_argument("--flash",           action="store_true", help="Flash bitstream.")
    parser.add_argument("--flash-multiboot", action="store_true", help="Flash multiboot bitstreams.")
    parser.add_argument("--rescan",          action="store_true", help="Execute PCIe Rescan while Loading/Flashing.")
    parser.add_argument("--driver",          action="store_true", help="Generate PCIe driver from LitePCIe (override local version).")

    # PCIe parameters.
    parser.add_argument("--with-pcie",       action="store_true", help="Enable PCIe Communication.")
    parser.add_argument("--pcie-lanes",      default=1, type=int, help="PCIe Lanes.", choices=[1, 2, 4])

    # Ethernet/VRT parameters.
    parser.add_argument("--eth-sfp",         default=0, type=int,     help="Ethernet SFP.", choices=[0, 1])
    parser.add_argument("--eth-phy",         default="1000basex",     help="Ethernet PHY.", choices=["1000basex", "2500basex"])
    parser.add_argument("--board-ip",        default="192.168.1.50",  help="Board hardware IP address.")
    parser.add_argument("--vrt-dst-ip",      default="239.168.1.100", help="VRT destination IP address.")
    parser.add_argument("--vrt-dst-port",    default=4991, type=int,  help="VRT destination UDP port.")

    # Litescope Analyzer Probes.
    probeopts = parser.add_mutually_exclusive_group()
    probeopts.add_argument("--with-ad9361-spi-probe",      action="store_true", help="Enable AD9361 SPI Probe.")
    probeopts.add_argument("--with-ad9361-data-probe",     action="store_true", help="Enable AD9361 Data Probe.")
    probeopts.add_argument("--with-eth-tx-probe",          action="store_true", help="Enable Ethernet Tx Probe.")

    args = parser.parse_args()

    # Build SoC.
    soc = BaseSoC(
        # Generic.
        variant       = "baseboard", # Specific to this project

        # PCIe.
        with_pcie     = args.with_pcie,
        pcie_lanes    = args.pcie_lanes,

        # Ethernet.
        with_eth      = True, # specific to this project
        eth_sfp       = args.eth_sfp,
        eth_phy       = args.eth_phy,
        board_ip      = args.board_ip,
        vrt_dst_ip    = args.vrt_dst_ip,
        vrt_dst_port  = args.vrt_dst_port,
    )

    # LiteScope Analyzer Probes.
    if args.with_ad9361_spi_probe:
        soc.add_ad9361_spi_probe()
    if args.with_ad9361_data_probe:
        soc.add_ad96361_data_probe()
    if args.with_eth_tx_probe:
        soc.add_eth_tx_probe()

    # Builder.
    def get_build_name():
        r = f"litex_m2sdr"
        if args.with_pcie:
            r += f"_pcie_x{args.pcie_lanes}"
        return r

    builder = Builder(soc, output_dir=os.path.join("build", get_build_name()), csr_csv="csr.csv")
    builder.build(build_name=get_build_name(), run=args.build)

    # Generate LitePCIe Driver.
    generate_litepcie_software(soc, "software", use_litepcie_software=args.driver)

    # Load Bistream.
    if args.load:
        prog = soc.platform.create_programmer()
        prog.load_bitstream(os.path.join(builder.gateware_dir, soc.build_name + ".bit"))

    # Reset Device.
    if args.reset:
        prog = soc.platform.create_programmer()
        prog.reset()

    # Flash Bitstream.
    if args.flash:
        prog = soc.platform.create_programmer()
        prog.flash(0, os.path.join(builder.gateware_dir, soc.build_name + ".bin"))

    # Flash Multiboot Bitstreams.
    if args.flash_multiboot:
        prog = soc.platform.create_programmer()
        prog.flash(            0x0000_0000,  builder.get_bitstream_filename(mode="flash").replace(".bin", "_fallback.bin"),    verify=True)
        prog.flash(soc.platform.image_size,  builder.get_bitstream_filename(mode="flash").replace(".bin", "_operational.bin"), verify=True)

    # Rescan PCIe Bus.
    if args.rescan:
        subprocess.run("sudo sh -c 'cd software && ./rescan.py'", shell=True)

if __name__ == "__main__":
    main()
