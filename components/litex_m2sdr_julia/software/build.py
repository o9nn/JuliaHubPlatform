#!/usr/bin/env python3

# This file is part of LiteX-M2SDR.
#
# Copyright (c) 2024-2025 Enjoy-Digital <enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

import os
import argparse
import subprocess

# Build Utilities ----------------------------------------------------------------------------------

nproc = os.cpu_count()

def run_command(command):
    subprocess.run(command, shell=True, check=True)

def build_driver(path, cmake_options=""):
    base_dir   = os.path.dirname(os.path.abspath(__file__))
    source_dir = os.path.join(base_dir, path)
    build_dir = os.path.join(base_dir, path, 'build')
    os.makedirs(build_dir, exist_ok=True)
    commands = [
        f"cmake -B{build_dir} -S{source_dir} {cmake_options}",
        f"cmake --build {build_dir} --target clean",
        f"cmake --build {build_dir} --parallel {nproc}",
        f"sudo cmake --build {build_dir} --target install"
    ]
    for command in commands:
        run_command(command)

# Main ---------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="LiteX-M2SDR Software build.", formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--interface", default="liteeth", help="Control/Data path interface", choices=["litepcie", "liteeth"])
    build_group = parser.add_mutually_exclusive_group()
    build_group.add_argument("--debug", action="store_const", dest="build_type",
                            const="debug", help="Build with debug configuration")
    build_group.add_argument("--release", action="store_const", dest="build_type",
                            const="release", help="Build with release configuration")
    parser.set_defaults(build_type="debug")

    args = parser.parse_args()

    # Control/Data path flags.
    if args.interface == "litepcie":
        flags = "-DUSE_LITEETH=OFF"
    else:
        flags = "-DUSE_LITEETH=ON"

    # Kernel compilation.
    if args.interface == "litepcie":
        run_command(f"make -C kernel clean")
        run_command(f"make -C kernel -j{nproc} all BUILD_TYPE={args.build_type}")

    # Utilities compilation.
    run_command(f"make -C user clean")
    run_command(f"make -C user -j{nproc} all BUILD_TYPE={args.build_type}")

    # SoapySDR Driver compilation.
    cmake_build_type = "Release" if args.build_type == "release" else "Debug"
    build_driver("soapysdr", f"-DCMAKE_INSTALL_PREFIX=/usr -DCMAKE_BUILD_TYPE={cmake_build_type} {flags}")

if __name__ == "__main__":
    main()
