module End2End (end2endTests) where

import Math.SiConverter

import Test.Tasty
import Test.Tasty.HUnit

-- TODO Add examples using derived units!
end2endTests :: TestTree
end2endTests = testGroup "End-to-End Tests" [
    testsFromSpec,
    arithmetic,
    units,
    unitConversion,
    nonSiUnits,
    variables,
    physics,
    physicsDerived
    ]

-- | Test cases based on the project description
-- Decimal places are allowed (discussed in our meeting on 2024-07-09)
testsFromSpec :: TestTree
testsFromSpec = testGroup "Tests from the Project description" [
    testCase "36 km/h * 50s = 500m"       $ calc "36 km/h * 50s"   @?= "500.0m",
    testCase "(12 * 13m) [km] = 0.156 km" $ calc "(12 * 13m) [km]" @?= "0.156km",
    testCase "1 = 1"                      $ calc "1"               @?= "1.0",
    testCase "1m + 2 m = 3m"              $ calc "1m + 2 m"        @?= "3.0m",
    testCase "1000m/5s + 3m/s"            $ calc "1000m/5s + 3m/s" @?= "203.0m*s^-1"
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
        $ calc "2(3 + 4)/7"      @?= "2.0"
    ]

units :: TestTree
units = testGroup "Units" [
    testCase "Simple unit calculation"
        $ calc "(30km) / (2km/h)" @?= "54000.0s",
    testCase "Simple unit calculation without parenthesis"
        $ calc "30km / 2km/h"     @?= "54000.0s"
    ]

physics :: TestTree
physics = testGroup "Some classics from physics class" [
    testCase "Newton's second law of motion: F = m * a"
        $ calc "1000kg * 2m/s^2"      @?= "2000.0kg*m*s^-2",
    testCase "Constant acceleration: v = u + at"
        $ calc "0m/s + (3m/s^2*5s)"   @?= "15.0m*s^-1",
    testCase "Gravitational potential energy: U = mgh"
        $ calc "10kg * 9.8m/s^2 * 5m" @?= "490.0kg*m^2*s^-2",
    -- TODO Need to be sanity checked once exponentiation works again
    testCase "Kinetic Energy: KE=1/2mv^2"
        $ calc "1/2*800kg*(10m/s)^2"  @?= "40000.0kg*m^2*s^-2",
    testCase "Work done: W = Fd"
        $ calc "50kg*m*s^-2 * 10m"    @?= "500.0kg*m^2*s^-2"
    ]

physicsDerived :: TestTree
physicsDerived = testGroup "Physics equations using derived units" [
    testCase "Pressure is force applied over an area: P = F / A"
        $ calc "F = 0.2 kN, A = 0.5m^2 -> F/A" @?= "400.0kg*m^-1*s^-2",
    testCase "Power as energy over time: P = E / t"
        $ calc "E = 1000J, t=50s -> E / t"     @?= "20.0kg*m^2*s^-3",
    testCase "Mechanical power: P = F / v"
        $ calc "F = 50N, v=3m/s -> F*v"        @?= "150.0kg*m^2*s^-3",
    testCase "Work done: W = Fd"
        $ calc "F = 50N, d = 10m -> F*d"       @?= "500.0kg*m^2*s^-2"
    ]

nonSiUnits :: TestTree
nonSiUnits = testGroup "Conversion of Non-SI units" [
    testCase "Wh" $ calc "1Wh" @?= "3600.0kg*m^2*s^-2",
    testCase "a"  $ calc "1a"  @?= "100m^2",
    testCase "ha" $ calc "1ha" @?= "10000m^2",
    testCase "l"  $ calc "1l"  @?= "0.001m^3"
    ]

unitConversion :: TestTree
unitConversion = testGroup "Unit Conversion" [
    testCase "Simple unit conversion I"
        $ calc "2000m [km]"             @?= "2.0km",
    testCase "Simple unit conversion II"
        $ calc "54000s [h]"             @?= "15.0h",
    testCase "Compound unit conversion"
        $ calc "2m/s [km/h]"            @?= "7.2km*h^-1",
    testCase "Unit conversion with exponent"
        $ calc "2km^2 [m^2]"            @?= "2000000.0m^2",
    testCase "Conversion after calculation I"
        $ calc "((30km) / (2km/h)) [h]" @?= "15.0h",
    testCase "Conversion after calculation II"
        $ calc "(30km) / (2km/h) [h]"   @?= "15.0h",
    testCase "Conversion after calculation III"
        $ calc "30km / 2km/h [h]"       @?= "15.0h"
    ]

variables :: TestTree
variables = testGroup "Variables" [
    testCase "Simple assignment"
        $ calc "m = 800 -> m"                      @?= "800.0",
    testCase "Assignment with unit"
        $ calc "m = 800kg -> m"                    @?= "800.0kg",
    testCase "Multi assignment"
        $ calc "M = 800, v=100 -> M * v"           @?= "80000.0",
    testCase "Multi assignment with units"
        $ calc "M = 800kg, v=100km/h -> 0.5*M*v^2" @?= "308641.9753086419kg*m^2*s^-2",
    testCase "Nested assignment"
        $ calc "M = 60 -> M * (v = 3 -> v)"        @?= "180.0",
    testCase "Nested assignment with units"
        $ calc "M = 60kg -> M * (a = 3m/s^2 -> a)" @?= "180.0kg*m*s^-2",
    testCase "Assignment with expression"
        $ calc "F = 1t * 2m/s^2, d = 10m -> F * d" @?= "20000.0kg*m^2*s^-2"
    ]

calc::String->String
calc s = case calculate s of
            Right res -> show res
            Left e    -> error $ show e
