#!/usr/bin/env python3

#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

import sys

from migen import *

from litex.gen import *
from litex.gen.sim import run_simulation


sys.path.append("..")
from gateware.vrt import VRTSignalPacketInserter

# Test Bench ---------------------------------------------------------------------------------------

def vrt_signal_packet_generator(dut):
    # Constants.
    n_packets        = 4
    words_per_packet = 16
    timestamp_int    = 0x12345678
    timestamp_fra    = 0x5aa55aa55aa55aa5

    # Configure DUT.
    yield dut.timestamp_int.eq(timestamp_int)
    yield dut.timestamp_fra.eq(timestamp_fra)
    yield

    # Send multiple packets.
    for packet in range(n_packets):
        # Send counter data for one packet.
        for i in range(words_per_packet):
            yield dut.sink.valid.eq(1)
            yield dut.sink.first.eq(i == 0)
            yield dut.sink.last.eq(i == words_per_packet - 1)
            yield dut.sink.data.eq(i + (packet * words_per_packet))
            yield
            while not (yield dut.sink.ready):
                yield
        yield dut.sink.valid.eq(0)

        # Update timestamps for the next packet.
        timestamp_fra += 0x1000000000000000  # Increment fractional part.
        if timestamp_fra == 0:  # Check for overflow.
            timestamp_int += 1
        yield dut.timestamp_int.eq(timestamp_int)
        yield dut.timestamp_fra.eq(timestamp_fra)
        yield

@passive
def vrt_signal_packet_checker(dut):
    # Receive and display data indefinitely.
    cycle = 0
    while True:
        yield dut.source.ready.eq(1)
        if (yield dut.source.valid):
            data = (yield dut.source.data)
            last = (yield dut.source.last)
            print(f"Cycle {cycle:2d}: Data: 0x{data:016x}, Last: {last}")
        yield
        cycle += 1

# Test ---------------------------------------------------------------------------------------------

def test_vrt_signal_packet_inserter():
    # Instantiate DUT.
    dut = VRTSignalPacketInserter(data_width=64)

    # Run simulation.
    run_simulation(
        dut,
        [
            vrt_signal_packet_generator(dut),
            vrt_signal_packet_checker(dut)
        ],
        vcd_name="sim.vcd"
    )

# Main ---------------------------------------------------------------------------------------------

if __name__ == "__main__":
    test_vrt_signal_packet_inserter()