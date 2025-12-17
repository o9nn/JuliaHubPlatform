#!/usr/bin/env python3

#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2024-2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

import os
import sys
import socket
import argparse

from migen import *

from litex.gen import *
from litex.gen.genlib.misc import WaitTimer

from litex.build.generic_platform import *
from litex.build.sim              import SimPlatform
from litex.build.sim.config       import SimConfig
from litex.build.sim.verilator    import verilator_build_args, verilator_build_argdict

from litex.soc.integration.common   import *
from litex.soc.integration.soc_core import *
from litex.soc.integration.builder  import *
from litex.soc.interconnect.csr     import *
from litex.soc.interconnect         import stream
from litex.soc.interconnect         import packet

from liteeth.phy.model        import LiteEthPHYModel
from liteeth.frontend.stream  import LiteEthStream2UDPTX

from litex_m2sdr.gateware.time import TimeGenerator
from litex_m2sdr.gateware.pps  import PPSGenerator

from gateware.time import TimeNsToPS
from gateware.vrt  import VRTSignalPacketStreamer
from gateware.rfic import RFICDataGenerator, RFICDataPacketizer

# IOs ----------------------------------------------------------------------------------------------

_io = [
    # Clk / Rst.
    ("sys_clk", 0, Pins(1)),
    ("sys_rst", 0, Pins(1)),

    # Serial.
    ("serial", 0,
        Subsignal("source_valid", Pins(1)),
        Subsignal("source_ready", Pins(1)),
        Subsignal("source_data",  Pins(8)),

        Subsignal("sink_valid",   Pins(1)),
        Subsignal("sink_ready",   Pins(1)),
        Subsignal("sink_data",    Pins(8)),
    ),

    # Ethernet (Stream Endpoint).
    ("eth_clocks", 0,
        Subsignal("tx", Pins(1)),
        Subsignal("rx", Pins(1)),
    ),
    ("eth", 0,
        Subsignal("source_valid", Pins(1)),
        Subsignal("source_ready", Pins(1)),
        Subsignal("source_data",  Pins(8)),

        Subsignal("sink_valid",   Pins(1)),
        Subsignal("sink_ready",   Pins(1)),
        Subsignal("sink_data",    Pins(8)),
    ),
]

# Platform -----------------------------------------------------------------------------------------

class Platform(SimPlatform):
    def __init__(self):
        SimPlatform.__init__(self, "SIM", _io)

# Simulation SoC -----------------------------------------------------------------------------------

class SimSoC(SoCCore):
    def __init__(self, *, variant, test,
        board_ip, vrt_dst_ip, vrt_dst_port):
        # Platform ---------------------------------------------------------------------------------

        platform     = Platform()
        self.comb += platform.trace.eq(1) # Always enable tracing.
        sys_clk_freq = int(1e6)

        # SoCCore ----------------------------------------------------------------------------------

        SoCCore.__init__(self, platform, sys_clk_freq,
            cpu_type      = None,
            uart_name     = "sim",
            ident         = f"LiteX-M2SDR SoC / {variant} variant / built on",
            ident_version = True,
        )

        # CRG --------------------------------------------------------------------------------------

        self.crg = CRG(platform.request("sys_clk"))

        # Ethernet / Etherbone ---------------------------------------------------------------------

        self.ethphy  = LiteEthPHYModel(platform.request("eth"))
        self.add_etherbone(phy=self.ethphy,
            # Core Parameters.
            mac_address = 0x10e2d5000000,
            ip_address  = board_ip,
            data_width  = 32,
            arp_entries = 4,
        )

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

        # RFIC Generator/Packetizer ----------------------------------------------------------------

        self.rfic_generator  = RFICDataGenerator(data_width=32)
        self.rfic_packetizer = RFICDataPacketizer(data_width=32, data_words=256)
        self.comb += self.rfic_generator.source.connect(self.rfic_packetizer.sink)

        # VITA Radio Transport ---------------------------------------------------------------------

        self.vrt_streamer = vrt_streamer = VRTSignalPacketStreamer(
            udp_crossbar = self.ethcore_etherbone.udp.crossbar,
            ip_address   = vrt_dst_ip,
            udp_port     = vrt_dst_port,
            data_width   = 32,
        )
        self.comb += [
            self.rfic_packetizer.source.connect(self.vrt_streamer.sink),
            self.vrt_streamer.sink.stream_id.eq(0xdeadbeef),
            self.vrt_streamer.sink.timestamp_int.eq(time_s),
            self.vrt_streamer.sink.timestamp_fra.eq(time_ps),
        ]

# Build --------------------------------------------------------------------------------------------

def sim_args(parser):
    verilator_build_args(parser)
    parser.add_argument("--host-ip",       default="192.168.2.100", help="Host interface IP address.")
    parser.add_argument("--board-ip",      default="192.168.2.50",  help="Board hardware IP address.")
    parser.add_argument("--vrt-dst-ip",    default="239.168.2.100", help="VRT destination IP address.")
    parser.add_argument("--vrt-dst-port",  default=4991, type=int,  help="VRT destination UDP port.")


def main():
    parser = argparse.ArgumentParser(description="LiteX M2SDR Simulation.")
    parser.add_argument("--test", default="", help="Test to Run.", choices=[""])
    sim_args(parser)
    args = parser.parse_args()

    verilator_build_kwargs = verilator_build_argdict(args)

    sys_clk_freq = int(1e6)
    sim_config   = SimConfig()
    sim_config.add_clocker("sys_clk", freq_hz=sys_clk_freq)
    sim_config.add_module("ethernet", "eth", args={"interface": "tap0", "ip": args.host_ip})
    sim_config.add_module("serial2console", "serial")

    # Build SoC.
    soc = SimSoC(
        # Generic.
        variant       = "baseboard",
        test          = args.test,

        # Ethernet.
        board_ip      = args.board_ip,
        vrt_dst_ip    = args.vrt_dst_ip,
        vrt_dst_port  = args.vrt_dst_port,
    )
    builder = Builder(soc, csr_csv="csr.csv")
    builder.build(sim_config=sim_config, **verilator_build_kwargs)

if __name__ == "__main__":
    main()
