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
