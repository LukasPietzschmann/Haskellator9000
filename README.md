#  Haskellator9000

## Features
### Required

- Operations
    - [x] Addition, Subtraction
    - [x] Multiplication, Division
    - [x] Power
    - [x] Unary negation
    - [ ] Explicit unit conversion
- SI base units
    - [x] Time (ns, µs, ms, s, min, h, d)
    - [x] Length (nm, µm, mm, cm, m, km)
    - [x] Mass (ng, µg, mg, g, kg, t)
- SI derived units
    - [ ] Area (m², dm², cm², mm², a, ha, km²)
    - [ ] Volume (m³, dm³, cm³, mm³, l, ml, cl, dl, km³)
    - [ ] Velocity (combination of length and time)
    - [ ] Acceleration (combination of length and time²)
    - [ ] Force (N, m/s², kN)
    - [ ] Pressure (Pa, hPa)
    - [ ] Frequency (Hz, kHz, MHz, GHz)
    - [ ] Energy (J, kJ, MJ, Wh, KWh, MWh, GWh, TWh)
    - [ ] Power (W, mW, kW, MW, GW, TW)

### Optional

- Doctests, unit tests and property tests
- Extensive CI pipeline ([here](https://gitlab.uni-ulm.de/sp/fp-2/ss24/team1/-/pipelines))
- Deployed documentation ([here](https://haskellator.pietzschmann.org))
- Rich REPL with persisted history and <CTRL>-R search
- Implicit multiplication (e.g. `2m(3m+4m)` instead of `2m*(3m+4m)`)
- Support for variable bindings (e.g. `a = 1+2 -> a + 3`)
    - Of course, they are evaluated lazily
    - And one can bind multiple variables in one scope (e.g. `a = 1, b = 2 -> a + b`)

## Usage

Check out the [documentation](https://haskellator.pietzschmann.org) for more
information.

## Development

TODO
