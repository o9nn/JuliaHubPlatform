#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

from migen import *

from litex.gen import *

# Time Nanoseconds to Picoseconds (Fraction) --------------------------------------------------------

class TimeNsToPS(LiteXModule):
    def __init__(self, time_ns, time_s, time_ps):
        assert len(time_ns) == 64
        assert len(time_s)  == 32
        assert len(time_ps) == 64

        # Signals.
        self.time_ns_reg  = time_ns_reg  = Signal(64)
        self.product_s1e9 = product_s1e9 = Signal(64)
        self.remainder_ns = remainder_ns = Signal(32)
        self.fraction_ps  = fraction_ps  = Signal(64)

        # 1) Align time_ns with time_s by storing it in time_ns_reg.
        # 2) Multiply time_s by 1e9.
        # 3) Subtract from the matching time_ns_reg to get remainder in ns.
        # 4) Convert remainder_ns from ns to ps (multiply by 1000).
        self.sync += [
            time_ns_reg.eq(time_ns),
            product_s1e9.eq(time_s * 1_000_000_000),
            remainder_ns.eq(time_ns_reg - product_s1e9),
            fraction_ps.eq(remainder_ns * 1000)
        ]

        # Drive time_ps combinationally from fraction_ps.
        self.comb += time_ps.eq(fraction_ps)