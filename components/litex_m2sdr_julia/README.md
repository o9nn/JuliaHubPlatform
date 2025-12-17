            __   _ __      _  __    __  ______  _______  ___         __     ___
           / /  (_) /____ | |/_/___/  |/  /_  |/ __/ _ \/ _ \______ / /_ __/ (_)__ _
          / /__/ / __/ -_)>  </___/ /|_/ / __/_\ \/ // / , _/___/ // / // / / / _ `/
         /____/_/\__/\__/_/|_|   /_/  /_/____/___/____/_/|_|    \___/\_,_/_/_/\_,_/

                          LiteX M2 SDR Customization for Julia Computing.
                               Copyright (c) 2025 Julia Computing.
                               Copyright (c) 2025 Enjoy-Digital.

![LiteX M2SDR in Baseboard with PoE](https://github.com/user-attachments/assets/6d158238-3334-4390-90b1-ce454e6088ea "LiteX M2SDR in Baseboard with PoE")

[> Intro
--------

This project reuses the LiteX M2 SDR design and customizes it for Ethernet connectivity with VRT
(VITA Radio Transport) streaming.

Key network parameters can be configured via the command line:
- **--board-ip**: Assigns the IP address to the board (or simulation).
- **--vrt-dst-ip**: Specifies the destination IP for VRT data transmission.
- **--vrt-dst-port**: Defines the UDP port used for VRT data transmission.

[> Architecture
---------------
Below is the top-level architecture diagram for the **M2SDR Ethernet SoC VRT Variant**, showing how
data flows through the system and how the various components interact:

![M2SDR Ethernet SoC VRT Variant Diagram](https://github.com/user-attachments/assets/138ec672-0cf1-4ad1-9502-252d4dd7a49a "M2SDR Ethernet SoC VRT Variant")

1. **Physical Setup**
   - **M2 SDR Board**: The core SDR board is inserted into a LiteX baseboard.
   - **Two SFP Connectors**: The baseboard provides two SFP ports. An **SFP-to-RJ45** transceiver
     (or direct fiber SFP modules) can be used to run either **1GbE** or **2.5GbE** Ethernet.
     You can use one of these ports to connect to your host system.

2. **X86/ARM Host**
   - Communicates with the FPGA over one SFP port (via an SFP-to-RJ45 module or fiber).
   - Runs user applications (e.g., a Julia-based SDR toolchain), drivers (SoapySDR, `libm2sdr`, or custom),
     and standard OS networking.

3. **LiteX SoC**
   - **LiteEth + UDP**: Handles Ethernet MAC/PHY interfacing (via the SFP transceiver) and UDP
     packet processing.
   - **MMAP (via Etherbone)**: Provides a memory-mapped mechanism for the host to configure and
     control FPGA cores using simple read/write operations across Ethernet.
   - **VITA Radio Transport**: Implements VRT streaming (via UDP) for **RX** data.
   - **AD9361 RFIC Core/PHY (RX Only)**: Configured for receive-only operation, capturing data
     from the AD9361’s receive path and feeding it into the FPGA for processing and streaming.
   - **SI5351 I2C/PWM + Low Pass Filter**: Generates and tunes the clock for the AD9361 radio
     (via I2C + PWM/VCXO control).
   - **LiteScope** (Debug): Optional integrated logic analyzer for on-FPGA signal inspection.
   - **JTAGBone** (Debug): Provides JTAG-based debug access via LiteX’s JTAG-to-Wishbone bridge.
   - **Identifier Core**: Supplies board-specific information to higher-level software.
   - **SPIFlash Core** / **ICAP Core**: Handle FPGA configuration, updates, and partial
     reconfiguration.
   - **XADC Core**: Monitors FPGA voltages, temperatures, and other analog signals.

4. **RF Connections**
   - **AD9361 (RX Only)**: Only the receive channels from the AD9361 are used, exposed via
     u.FL connectors.
   - **SYNC**: Optional sync signal for multi-board synchronization in phased-array or MIMO systems
     (if multiple boards are used).

By leveraging the LiteX baseboard’s **dual SFP** connectors (with SFP->RJ45 modules or fiber SFP
modules), the design provides a straightforward way to achieve either 1GbE or 2.5GbE speeds for SDR
data. On the FPGA side, **Etherbone** over LiteEth offers a convenient memory-mapped mechanism for
register access, while VRT streaming is handled through UDP for the **RX** data coming from the
AD9361 transceiver.


[> Simulation
-------------
Run the simulation using the `litex_m2sdr_sim.py` script. For example:
```bash
python3 litex_m2sdr_sim.py --board-ip 192.168.1.50 --vrt-dst-ip 192.168.1.100 --vrt-dst-port 4991
```

[> Hardware Build
-----------------
Build and flash the design to your FPGA board with the `litex_m2sdr_julia.py` script. For example:
```bash
python3 litex_m2sdr_julia.py --board-ip 192.168.1.50 --vrt-dst-ip 192.168.1.100 --vrt-dst-port 4991 --build --flash
```
