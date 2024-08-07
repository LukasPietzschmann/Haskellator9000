module End2End (end2endTests) where

import Math.Haskellator

import Test.Tasty
import Test.Tasty.HUnit

-- TODO Add examples using derived units!
end2endTests :: TestTree
end2endTests = testGroup "End-to-End Tests" [
    testsFromSpec,
    arithmetic,
    units,
    unitConversion,
    --nonSiUnits,
    variables,
    physics,
    physicsDerived
    ]

testsFromSpec :: TestTree
testsFromSpec = testGroup "Tests from the Project description" [
    testCase "36 km/h * 50s = 500m"       $ calc "36 km/h * 50s"   @?= "500.0 m",
    testCase "(12 * 13m) [km] = 0.156 km" $ calc "(12 * 13m) [km]" @?= "0.156 km",
    testCase "1 = 1"                      $ calc "1"               @?= "1.0",
    testCase "1m + 2 m = 3m"              $ calc "1m + 2 m"        @?= "3.0 m",
    testCase "1000m/5s + 3m/s"            $ calc "1000m/5s + 3m/s" @?= "203.0 m/s"
    ]

arithmetic :: TestTree
arithmetic = testGroup "Arithmetic" [
    testCase "Arithmetic expression"
        $ calc "2 * (3 + 4) / 7" @?= "2.0",
    testCase "Arithmetic expression"
        $ calc "2 * (3 + 4) / 7" @?= "2.0",
    testCase "Power!"
        $ calc "2^3"             @?= "8.0",
    testCase "Fractional Power!"
        $ calc "2^3.5"           @?= "11.313708498984761",
    testCase "Arithmetic expression with implicit multiplier"
        $ calc "2(3 + 4)/7"      @?= "2.0",
    testCase "Multi exponentiation"
        $ calc "2^(3^4)" @?= "2.4178516392292583e24"
    ]

units :: TestTree
units = testGroup "Units" [
    testCase "Base units are left as-is I"
        $ calc "5s^2" @?= "5.0 s^2",
    testCase "Base units are left as-is II"
        $ calc "5s^(-2)" @?= "5.0 1/s^2",
    testCase "Units get normalized I"
        $ calc "5min^2" @?= "18000.0 s^2",
    testCase "Units get normalized II"
        $ calc "5km^3" @?= "5.0e9 m^3",
    testCase "Units get normalized III"
        $ calc "1ns^3" @?= "1.0000000000000002e-27 s^3",
    testCase "Multiple units of same type are combined I"
        $ calc "1000 km * 1m" @?= "1000000.0 m^2",
    testCase "Multiple units of same type are combined II"
        $ calc "1s * 1m*s" @?= "1.0 s^2*m",
    testCase "Simple unit calculation"
        $ calc "(30km) / (2km/h)" @?= "54000.0 s",
    testCase "Simple unit calculation without parenthesis"
        $ calc "30km / 2km/h"     @?= "54000.0 s",
    testCase "Calculating with derived units"
        $ calc "1s * 1N" @?= "1.0 kg*m/s"
    ]

physics :: TestTree
physics = testGroup "Some classics from physics class" [
    testCase "Newton's second law of motion: F = m * a"
        $ calc "1000kg * 2m/s^2"      @?= "2000.0 kg*m/s^2",
    testCase "Constant acceleration: v = u + at"
        $ calc "0m/s + (3m/s^2*5s)"   @?= "15.0 m/s",
    testCase "Gravitational potential energy: U = mgh"
        $ calc "10kg * 9.8m/s^2 * 5m" @?= "490.0 m^2*kg/s^2",
    testCase "Kinetic Energy: KE=1/2mv^2"
        $ calc "1/2*800kg*(10m/s)^2"  @?= "40000.0 kg*m^2/s^2",
    testCase "Work done: W = Fd"
        $ calc "50kg*m*s^(-2) * 10m"    @?= "500.0 m^2*kg/s^2"
    ]

physicsDerived :: TestTree
physicsDerived = testGroup "Physics equations using derived units" [
    testCase "Pressure is force applied over an area: P = F / A"
        $ calc "F = 0.2 kN, A = 0.5m^2 -> F/A" @?= "0.4 kg/(m*s^2)",
    testCase "Power as energy over time: P = E / t"
        $ calc "E = 1000J, t=50s -> E / t"     @?= "20.0 kg*m^2/s^3",
    testCase "Mechanical power: P = F / v"
        $ calc "F = 50N, v=3m/s -> F*v"        @?= "150.0 m^2*kg/s^3",
    testCase "Work done: W = Fd"
        $ calc "F = 50N, d = 10m -> F*d"       @?= "500.0 m^2*kg/s^2"
    ]

-- TODO
--nonSiUnits :: TestTree
--nonSiUnits = testGroup "Conversion of Non-SI units" [
--    testCase "Wh" $ calc "1Wh" @?= "3600.0kg*m^2/s^2",
--    testCase "a"  $ calc "1a"  @?= "100 m^2",
--    testCase "ha" $ calc "1ha" @?= "10000 m^2",
--    testCase "l"  $ calc "1l"  @?= "0.001 m^3"
--    ]

unitConversion :: TestTree
unitConversion = testGroup "Unit Conversion" [
    testCase "Simple unit conversion I"
        $ calc "2000m [km]"             @?= "2.0 km",
    testCase "Simple unit conversion II"
        $ calc "54000s [h]"             @?= "15.0 h",
    testCase "Compound unit conversion"
        $ calc "2m/s [km/h]"            @?= "7.2 km/h",
    testCase "Unit conversion with exponent"
        $ calc "2km^2 [m^2]"            @?= "2000000.0 m^2",
    testCase "Conversion after calculation I"
        $ calc "((30km) / (2km/h)) [h]" @?= "15.0 h",
    testCase "Conversion after calculation II"
        $ calc "(30km) / (2km/h) [h]"   @?= "15.0 h",
    testCase "Conversion after calculation III"
        $ calc "30km / 2km/h [h]"       @?= "15.0 h"
    ]

variables :: TestTree
variables = testGroup "Variables" [
    testCase "Simple assignment"
        $ calc "m = 800 -> m"                      @?= "800.0",
    testCase "Assignment with unit"
        $ calc "m = 800kg -> m"                    @?= "800.0 kg",
    testCase "Multi assignment"
        $ calc "M = 800, v=100 -> M * v"           @?= "80000.0",
    testCase "Multi assignment with units"
        $ calc "M = 800kg, v=100km/h -> 0.5*M*v^2" @?= "308641.9753086419 kg*m^2/s^2",
    testCase "Nested assignment"
        $ calc "M = 60 -> M * (v = 3 -> v)"        @?= "180.0",
    testCase "Nested assignment with units"
        $ calc "M = 60kg -> M * (a = 3m/s^2 -> a)" @?= "180.0 kg*m/s^2",
    testCase "Assignment with expression"
        $ calc "F = 1t * 2m/s^2, d = 10m -> F * d" @?= "20000.0 m^2*kg/s^2"
    ]

calc::String->String
calc s = case calculate s of
            Right res -> show res
            Left e    -> error $ show e
