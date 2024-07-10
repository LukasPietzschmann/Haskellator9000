module End2End (end2endTests) where

import Math.SiConverter

import Test.Tasty
import Test.Tasty.HUnit

-- TODO Add examples using derived units!
end2endTests :: TestTree
end2endTests = testGroup "End-to-End Tests" [testsFromSpec, physics, calculations]

-- | Test cases based on the project description
-- Decimal places are allowed (discussed in our meeting on 2024-07-09)
testsFromSpec :: TestTree
testsFromSpec = testGroup "Tests from the Project description" [
    testCase "36 km/h * 50s = 500m" $ calc "36 km/h * 50s" @?= "500.0m",
    testCase "(12 * 13m) [km] = 0.156 km" $ calc "(12 * 13m) [km]" @?= "0.156km",
    testCase "1 = 1" $ calc "1" @?= "1.0",
    testCase "1m + 2 m = 3m" $ calc "1m + 2 m" @?= "3.0m",
    testCase "1000m/5s + 3m/s" $ calc "1000m/5s + 3m/s" @?= "203.0m/s"
    ]

physics :: TestTree
physics = testGroup "Some classics from physics class" [
    testCase "Newton's second law of motion: F = m * a"
        $ calc "1000kg * 2m/s^2" @?= "2000.0kg*m*s^-2",
    testCase "Constant acceleration: v = u + at"
        $ calc "0m/s + (3m/s^2*5s)" @?= "15.0m*s^-1",
    testCase "Gravitational potential energy: U = mgh"
        $ calc "10kg * 9.8m/s^2 * 5m" @?= "490.0kg*m^2*s^-2",
    -- TODO Need to be sanity checked once exponentiation works again
    testCase "Kinetic Energy: KE=1/2mv^2"
        $ calc "1/2*800kg*(10m/s)^2" @?= "40000g*m^2*s^-2",
    testCase "Work done: W = Fd"
        $ calc "50kg*m*s^-2 * 10m" @?= "500000g*m^2*s^-2"
    ]

calculations :: TestTree
calculations = testGroup "Misc Tests" [
    -- Arithmetic
    testCase "Arithmetic expression" 
        $ calc "2 * (3 + 4) / 7" @?= "2.0",
    testCase "Power!"                                         -- TODO See issue #32
        $ calc "2^3" @?= "8.0", 
    testCase "Arithmetic expression with implicit multiplier" -- TODO See issue #30
        $ calc "2(3 + 4)/7" @?= "2.0", 
    
    -- Calculations with units
    testCase "Simple unit calculation"
        $ calc "(30km) / (2km/h)" @?= "54000.0s",
    testCase "Simple unit calculation without parenthesis"    -- TODO See issue #31
        $ calc "30km / 2km/h" @?= "54000.0s",

    -- Unit conversion
    testCase "Simple unit conversion I" 
        $ calc "2000m [km]" @?= "2.0km",
    testCase "Simple unit conversion II" 
        $ calc "54000s [h]" @?= "15.0h",
    testCase "Compound unit conversion"                       -- TODO Should this be possible? 
        $ calc "2m/s [km/h]" @?= "7.2km/h",
    testCase "Conversion after calculation I"
        $ calc "((30km) / (2km/h)) [h]" @?= "15.0h",
    testCase "Conversion after calculation II"
        $ calc "(30km) / (2km/h) [h]" @?= "15.0h",
    testCase "Conversion after calculation III"
        $ calc "30km / 2km/h [h]" @?= "15.0h"
    ]

calc::String->String
calc s = case calculate s of
            Right res -> show res
            Left e    -> error $ show e