#  Haskellator9000

Haskellator9000 is a simple calculator written in (you guessed it) Haskell. Its main
feature is the support for multiple SI base and derived units. For a complete list of
supported units and features, see below.

## Supported Units
- SI base units
    - [x] Time (ns, µs, ms, s, min, h, d)
    - [x] Length (nm, µm, mm, cm, m, km)
    - [x] Mass (ng, µg, mg, g, kg, t)
- SI derived units
    - [x] Area (m², dm², cm², mm², a, ha, km²)
    - [x] Volume (m³, dm³, cm³, mm³, km³)
    - [x] Velocity (combination of length and time)
    - [x] Acceleration (combination of length and time²)
    - [x] Force (N, m/s², kN)
    - [x] Pressure (Pa, hPa)
    - [x] Frequency (Hz, kHz, MHz, GHz)
    - [x] Energy (J, kJ, MJ)
    - [x] Power (W, mW, kW, MW, GW, TW)

## Features
- Rich REPL with persisted history and CTRL-R search
- Explicit unit conversion (e.g. `2km [m]`)
- Implicit multiplication (e.g. `2m(3m+4m)` instead of `2m*(3m+4m)`)
- Support for variable bindings (e.g. `a = 1+2, b = 3 -> a + b`)
    - Of course, they are evaluated lazily :wink:

## Usage
1. To run the calculator you first have to make sure that you got a working Haskell
   installation
    1. You can use, e.g., [GHCup](https://www.haskell.org/ghcup/) to install all
       necessary tools
    2. We will need the GHC and Stack. So go ahead and install those
2. Next, you can clone the repository
   ```sh
   git clone https://github.com/LukasPietzschmann/Haskellator9000.git
   ```
   Or if you prefer SSH:
   ```sh
   git clone git@github.com:LukasPietzschmann/Haskellator9000.git
   ```
3. After that, navigate into the project:
   ```sh
   cd Haskellator9000
   ```
4. Now you have two options: You can simply run the calculator using
   ```sh
   stack run
   ```
   Or you could install it into your system by running
   ```sh
   stack install
   ```
