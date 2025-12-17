#!/usr/bin/env python3

import time
import argparse
import numpy as np

import SoapySDR
from SoapySDR import SOAPY_SDR_RX, SOAPY_SDR_CS16

def main():
    parser = argparse.ArgumentParser(description="LiteXM2SDR init script.")
    parser.add_argument("--samplerate", type=float, default=4e6,               help="RX Sample rate in Hz")
    parser.add_argument("--freq",       type=float, default=2.4e9,             help="RX frequency in Hz")
    parser.add_argument("--gain",       type=float, default=-20.0,             help="RX gain in dB")
    parser.add_argument("--bandwidth",  type=float, default=56e6,              help="RX filter bandwidth in Hz")
    parser.add_argument("--channel",    type=int,   choices=[0, 1], default=0, help="RX channel index (0 or 1)")
    parser.add_argument("--remote",     action="store_true",                   help="Use remote mode via SoapySDRServer (default: local)")
    parser.add_argument("--remote-ip",  type=str,   default="127.0.0.1:55132", help="Remote server IP and port (default: 127.0.0.1:55132)")

    args = parser.parse_args()

    # Prepare Remote/Local Arguments.
    if args.remote:
        device_args = {
            "driver":           "remote",
            "remote":           args.remote_ip,
            "remote:driver":    "LiteXM2SDR",
            "eth_mode":         "vrt",
        }
        mode = "remote"
    else:
        device_args = {
            "driver":   "LiteXM2SDR",
            "eth_mode": "vrt",
        }
        mode = "local"

    # Initialize the LiteXM2SDR device.
    sdr = SoapySDR.Device(','.join(f'{key}={value}' for key, value in device_args.items()))
    sdr.setSampleRate(SOAPY_SDR_RX, args.channel, args.samplerate)
    sdr.setFrequency( SOAPY_SDR_RX, args.channel, args.freq)
    sdr.setGain(      SOAPY_SDR_RX, args.channel, args.gain)
    sdr.setBandwidth( SOAPY_SDR_RX, args.channel, args.bandwidth)

    print(f"LiteXM2SDR device initialized in {mode} mode with:")
    print(f"  Sample Rate: {args.samplerate} Hz")
    print(f"  Frequency  : {args.freq} Hz")
    print(f"  Gain       : {args.gain} dB")
    print(f"  Bandwidth  : {args.bandwidth} Hz")
    print(f"  Channel    : {args.channel}")
    if args.remote:
        print(f"  Remote IP  : {args.remote_ip}\n")
    else:
        print()

    # Open and Activate the Rx Stream.
    rx_stream = sdr.setupStream(SOAPY_SDR_RX, SOAPY_SDR_CS16, [0])
    sdr.activateStream(rx_stream)

    # Buffer for receiving samples (CS16: 2x 16-bit ints = 4 bytes per sample)
    buff = np.zeros(1024, dtype=np.complex64)  # 1024 samples
    total_bytes = 0

    # Record the initialization time.
    start_time = time.time()
    last_display_time = start_time

    # Keep the device open, receive data continuously, and display every second.
    try:
        while True:
            # Read samples from the stream
            status = sdr.readStream(rx_stream, [buff], len(buff))
            if status.ret > 0:
                total_bytes += status.ret * 4  # 4 bytes per CS16 sample

            # Update display only once per second
            current_time = time.time()
            if current_time - last_display_time >= 1.0:
                elapsed = current_time - start_time
                print(f"Device has been active for {elapsed:.2f} seconds, Received: {total_bytes} bytes", end="\r")
                last_display_time = current_time

    except KeyboardInterrupt:
        print("\nExiting and closing the device.")

    # Deactivate and close the RX Stream.
    sdr.deactivateStream(rx_stream)
    sdr.closeStream(rx_stream)

if __name__ == "__main__":
    main()
