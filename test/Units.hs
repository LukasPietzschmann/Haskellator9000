module Units(printUnitWithFractions) where
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty
import Math.SiConverter.Internal.Units ( meter, second, kilogram )

printUnitWithFractions::TestTree
printUnitWithFractions = testGroup "Prints unit with fraction" [
    testCase "One negative exponent" $ show (meter (-2)) @?= "1/m^2",
    testCase "Two negative exponents" $ show (meter (-2) ++ second (-2)) @?= "1/(m^2*s^2)",

    testCase "Positive and negative exponent I" $ show (meter 1 ++ second (-1)) @?= "m/s",
    testCase "Positive and negative exponent II" $ show (meter 1 ++ second (-2)) @?= "m/s^2",
    testCase "Positive and negative exponent III" $ show (meter (-2) ++ second 2) @?= "s^2/m^2",

    testCase "Multiple positive, one negative exponent" $ show (meter 2 ++ second 1 ++ kilogram (-2)) @?= "m^2*s/kg^2",
    testCase "One positive, multiple negative exponents I" $ show (meter (-2) ++ second 1 ++ kilogram (-2)) @?= "s/(m^2*kg^2)",
    testCase "One positive, multiple negative exponents II" $ show (meter (-2) ++ second 2 ++ kilogram (-2)) @?= "s^2/(m^2*kg^2)"
    ]