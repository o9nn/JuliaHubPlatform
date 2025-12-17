#
# This file is part of LiteX-M2SDR-Julia project.
#
# Copyright (c) 2025 Julia Computing <https://julialang.org>
# Copyright (c) 2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

from enum import IntEnum

from migen import *

from litex.gen import *

from litex.soc.interconnect import stream

from litex.soc.interconnect.packet import HeaderField, Header, Packetizer

from liteeth.frontend.stream  import LiteEthStream2UDPTX

# VRT Constants ------------------------------------------------------------------------------------

class VRT_Bool(IntEnum):
    """VRT Boolean Values for Fields like C and T"""
    DISABLED = 0x0  # Field not present (e.g., Class ID, Trailer).
    ENABLED  = 0x1  # Field present.

class VRT_PacketType(IntEnum):
    """VRT Packet Types"""
    SIG_DATA_NO_STREAM_ID   = 0x0  # Signal Data Packet without Stream Identifier.
    SIG_DATA_WITH_STREAM_ID = 0x1  # Signal Data Packet with Stream Identifier.
    EXT_DATA_NO_STREAM_ID   = 0x2  # Extension Data Packet without Stream Identifier.
    EXT_DATA_WITH_STREAM_ID = 0x3  # Extension Data Packet with Stream Identifier.
    CONTEXT                 = 0x4  # Context Packet.
    COMMAND                 = 0x5  # Command Packet.
    EXT_COMMAND             = 0x6  # Extension Command Packet.

class VRT_TSI(IntEnum):
    """VRT Timestamp Integer Types (TSI)"""
    NONE    = 0x0  # No Integer Timestamp.
    UTC     = 0x1  # Coordinated Universal Time.
    GPS     = 0x2  # GPS Time.
    OTHER   = 0x3  # Other (user-defined).

class VRT_TSF(IntEnum):
    """VRT Timestamp Fractional Types (TSF)"""
    NONE            = 0x0  # No Fractional Timestamp.
    SAMPLE_COUNT    = 0x1  # Sample Count.
    REAL_TIME       = 0x2  # Real-Time (picoseconds).
    FREE_RUNNING    = 0x3  # Free-Running Count.

class VRT_CIF0(IntEnum):
    """VRT Context Indicator Field 0 (CIF0) Enable Bits"""
    CHANGE_INDICATOR    = 1 << 31  # Context Field Change Indicator (bit 31, 1 bit).
    REF_POINT_ID        = 1 << 30  # Reference Point Identifier (bit 30, 2 words).
    BANDWIDTH           = 1 << 29  # Bandwidth (bit 29, 2 words).
    IF_REF_FREQ         = 1 << 28  # IF Reference Frequency (bit 28, 2 words).
    RF_REF_FREQ         = 1 << 27  # RF Reference Frequency (bit 27, 2 words).
    RF_REF_FREQ_OFFSET  = 1 << 26  # RF Reference Frequency Offset (bit 26, 2 words).
    IF_BAND_OFFSET      = 1 << 25  # IF Band Frequency Offset (bit 25, 2 words).
    REF_LEVEL           = 1 << 24  # Reference Level (bit 24, 1 word).
    GAIN                = 1 << 23  # Gain (bit 23, 1 word).
    OVER_RANGE_COUNT    = 1 << 22  # Over Range Count (bit 22, 1 word).
    SAMPLE_RATE         = 1 << 21  # Sample Rate (bit 21, 2 words).
    TIMESTAMP_ADJUST    = 1 << 20  # Timestamp Adjustment (bit 20, 2 words).
    TIMESTAMP_CALIB     = 1 << 19  # Timestamp Calibration Time (bit 19, 1 word).
    TEMPERATURE         = 1 << 18  # Temperature (bit 18, 1 word).
    DEVICE_ID           = 1 << 17  # Device Identifier (bit 17, 2 words).
    DATA_PAYLOAD_FORMAT = 1 << 16  # Signal Data Packet Payload Format (bit 16, 2 words).

class VRT_TrailerEnables(IntEnum):
    """VRT Trailer Enables Field (Bits 30-20)"""
    CALIBRATED_TIME    = 1 << 30  # Bit 30: Enable Calibrated Time Indicator.
    VALID_DATA         = 1 << 29  # Bit 29: Enable Valid Data Indicator.
    REF_LOCK           = 1 << 28  # Bit 28: Enable Reference Lock Indicator.
    AGC_MGC            = 1 << 27  # Bit 27: Enable AGC/MGC Indicator.
    DETECTED_SIGNAL    = 1 << 26  # Bit 26: Enable Detected Signal Indicator.
    SPECTRAL_INVERSION = 1 << 25  # Bit 25: Enable Spectral Inversion Indicator.
    OVER_RANGE         = 1 << 24  # Bit 24: Enable Over-range Indicator.
    SAMPLE_LOSS        = 1 << 23  # Bit 23: Enable Sample Loss Indicator.
    USER_DEFINED_3     = 1 << 22  # Bit 22: Enable User Defined Indicator 3.
    USER_DEFINED_2     = 1 << 21  # Bit 21: Enable User Defined Indicator 2.
    USER_DEFINED_1     = 1 << 20  # Bit 20: Enable User Defined Indicator 1.

# VRT Headers Definitions --------------------------------------------------------------------------

# Helper.
# -------

def natural_to_header_fields(natural, total_bytes=4):
    fields = {}
    for name, (msb, lsb) in natural.items():
        width = msb - lsb + 1
        # Compute byte: count bytes from the MSB (big-endian)
        byte = (total_bytes - 1) - (msb // 8)
        # If the field fits in one byte, compute offset within that byte;
        # otherwise, assume the field is aligned to the start of a byte.
        offset = (msb % 8) - width + 1 if msb // 8 == lsb // 8 else 0
        fields[name] = HeaderField(byte, offset, width)
    return fields

# Common Header.
# --------------

def get_common_header_fields():
    """Define the 32-bit VRT common header fields in big-endian format."""
    return natural_to_header_fields({
        "packet_type"  : (31, 28), # Bits 31-28 : Packet Type.
        "c"            : (27, 27), # Bit     27 : Class ID Indicator (1: present, 0: not present).
        "t"            : (26, 26), # Bit     26 : Trailer Indicator (1: present, 0: not present).
        "r"            : (25, 24), # Bits 25-24 : Reserved (typically 0).
        "tsi"          : (23, 22), # Bits 23-22 : Integer Timestamp Type.
        "tsf"          : (21, 20), # Bits 21-20 : Fractional Timestamp Type.
        "packet_count" : (19, 16), # Bits 19-16 : Packet Count (modulo-16 counter).
        "packet_size"  : (15,  0), # Bits  15-0 : Packet Size (total 32-bit words, max 65,535).
    }, total_bytes = 4)

# Signal Packet Header.
# ---------------------

signal_header_fields = {
    **get_common_header_fields(),
    "stream_id"     : HeaderField(4,  0, 32), # Bits   95-64.
    "timestamp_int" : HeaderField(8,  0, 32), # Bits  127-96.
    "timestamp_fra" : HeaderField(12, 0, 64), # Bits 159-128.
}

signal_header_length = 20  # 20 bytes = 160 bits (5 32-bit words)
signal_header = Header(signal_header_fields,
    length           = signal_header_length,
    swap_field_bytes = True,
)

# Signal Packet Description.
# --------------------------

def vrt_signal_packet_description(data_width):
    param_layout   = signal_header.get_layout()
    payload_layout = [
        ("data", data_width),
    ]
    return stream.EndpointDescription(payload_layout, param_layout)

def vrt_signal_packet_user_description(data_width):
    param_layout = [
        ("stream_id",     32),
        ("timestamp_int", 32),
        ("timestamp_fra", 64),
    ]
    payload_layout = [
        ("data",       data_width),
        ("data_words",         16),
    ]
    return stream.EndpointDescription(payload_layout, param_layout)

# VRT Signal Packet Inserter -----------------------------------------------------------------------

class VRTSignalPacketInserter(LiteXModule):
    """Prepends VRT signal data headers to an input stream of IQ samples using Pacfetizer."""
    def __init__(self, data_width=32):
        # Input and output streams.
        self.sink   = sink   = stream.Endpoint(vrt_signal_packet_user_description(data_width))
        self.source = source = stream.Endpoint([("data", data_width)])

        # # #

        # Signals.
        _packet_count = Signal(4)
        _packet_size  = Signal(16)

        # Packetizer.
        self.packetizer = packetizer = Packetizer(
            sink_description   = vrt_signal_packet_description(data_width),
            source_description = [("data", data_width)],
            header             = signal_header,
        )

        # Connect sink to packetizer.
        self.comb += [
            sink.connect(packetizer.sink, omit={"data_words"}),
            # Common header fields.
            packetizer.sink.packet_type.eq(VRT_PacketType.SIG_DATA_WITH_STREAM_ID),
            packetizer.sink.c.eq(VRT_Bool.DISABLED), # No Class Identifier.
            packetizer.sink.t.eq(VRT_Bool.DISABLED), # No Trailer.
            packetizer.sink.r.eq(0),
            packetizer.sink.tsi.eq(VRT_TSI.UTC),
            packetizer.sink.tsf.eq(VRT_TSF.REAL_TIME),
            packetizer.sink.packet_count.eq(_packet_count),
            packetizer.sink.packet_size.eq(self.sink.data_words + signal_header_length//4),
        ]

        # Connect packetizer to source.
        self.comb += packetizer.source.connect(source)

        # Packet count increment.
        self.sync += [
            If(source.valid & source.ready & source.last,
                _packet_count.eq(_packet_count + 1)
            )
        ]

# VRT Signal Packet Streamer -----------------------------------------------------------------------

class VRTSignalPacketStreamer(LiteXModule):
    """Integrates VRT signal packet insertion with UDP streaming."""
    def __init__(self, udp_crossbar, ip_address, udp_port, data_width=32, with_csr=True):
        self.sink = stream.Endpoint(vrt_signal_packet_user_description(data_width))

        # # #

        # VRT Streamer.
        vrt_streamer_port = udp_crossbar.get_port(udp_port, dw=data_width, cd="sys")
        self.vrt_streamer = LiteEthStream2UDPTX(
            ip_address = ip_address,
            udp_port   = udp_port,
            fifo_depth = 1024,
            data_width = data_width,
            with_csr   = with_csr,
        )

        # VRT Inserter.
        self.vrt_inserter = VRTSignalPacketInserter(data_width=data_width)

        # Connections.
        self.comb += [
            # Connect sink to inserter.
            self.sink.connect(self.vrt_inserter.sink),

            # Connect inserter to streamer.
            self.vrt_inserter.source.connect(self.vrt_streamer.sink),

            # Connect streamer to UDP port.
            self.vrt_streamer.source.connect(vrt_streamer_port.sink),
        ]
