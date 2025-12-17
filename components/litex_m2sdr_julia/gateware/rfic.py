#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

from migen import *

from litex.gen import *

from litex.soc.interconnect import stream
from litex.soc.interconnect import packet

# RFIC Data Generator ------------------------------------------------------------------------------

class RFICDataGenerator(LiteXModule):
    def __init__(self, data_width=32):
        self.source = source = stream.Endpoint([("data", data_width)])

        # # #

        count = Signal(12)
        self.sync += count.eq(count + 1)

        self.comb += source.valid.eq(count == 0)
        self.sync += If(source.valid & source.ready,
            source.data.eq(source.data + 1)
        )

# RFIC Data Framer ---------------------------------------------------------------------------------

class RFICDataFramer(LiteXModule):
    def __init__(self, data_width=32, data_words=32):
        self.sink   = sink    = stream.Endpoint([("data", data_width)])
        self.source = source  = stream.Endpoint([("data", data_width)])

        # # #

        # Signals.
        last  = Signal()
        count = Signal(16)

        # Sink -> Source.
        self.comb += sink.connect(source)

        # Count/Last Generation.
        self.comb += last.eq(count == (data_words - 1))
        self.sync += [
            If(self.sink.valid & self.sink.ready,
                If(last,
                    count.eq(0)
                ).Else(
                    count.eq(count + 1)
                )
            )
        ]
        self.comb += self.source.last.eq(last)

# RFIC Data Packetizer -----------------------------------------------------------------------------

class RFICDataPacketizer(LiteXModule):
    def __init__(self, data_width=32, data_words=256):
        self.sink   = sink    = stream.Endpoint([("data", data_width)])
        self.source = source  = stream.Endpoint([("data", data_width), ("data_words", 16)])
        self.data_words = data_words

        # # #

        # Data Framer.
        self.data_framer = RFICDataFramer(data_width=data_width, data_words=data_words)

        # Packet FIFO.
        self.data_fifo = packet.PacketFIFO(
            layout        = [("data", 32)],
            payload_depth = data_words*2,
            param_depth   = None,
            buffered      = True,
        )

        # Pipeline.
        self.submodules += stream.Pipeline(
            self.sink,
            self.data_framer,
            self.data_fifo,
            self.source,
        )
        self.comb += self.source.data_words.eq(data_words)
