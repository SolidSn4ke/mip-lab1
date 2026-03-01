import Intensity (Intensity (Intensity))
import Lib (colorIntensity)
import RGB (RGB (RGB))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Text.Printf (printf)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Module Tests" [colorIntensityTests]

eps :: Double
eps = (10 :: Double) ** (-15)

colorIntensityTests :: TestTree
colorIntensityTests =
    testGroup
        "Color Intensity"
        [ testCase "Direct light source" $ colorIntensity (Intensity (RGB 1 1 1)) 0 @?= Intensity (RGB 1 1 1),
          testCase "60 degree angle source" $
            let
                Intensity (RGB r g b) = colorIntensity (Intensity (RGB 1 0.5 0)) (pi / 3)
             in
                all (\x -> abs x < eps) [r - 0.5, g - 0.25, b] @? printf "expected (0.5, 0.25, 0), but got (%f, %f, %f)" r g b,
          testCase "Wrong angle" $ colorIntensity (Intensity (RGB 100 100 100)) pi @?= Intensity (RGB 0 0 0),
          testCase "90 degree angle source" $
            let
                (Intensity (RGB r g b)) = colorIntensity (Intensity (RGB 0.8 0.3 0.6)) (pi / 2)
             in
                all (\x -> abs x < eps) [r, g, b] @? printf "expected (0, 0, 0), but got (%f, %f, %f)" r g b,
          testCase "45 degree angle source" $ colorIntensity (Intensity (RGB 1 0 0)) (pi / 4) @?= Intensity (RGB (sqrt 2 / 2) 0 0),
          testCase "89 degree angle source" $ colorIntensity (Intensity (RGB 1 1 1)) (pi / 180 * 89) @?= Intensity (RGB 1.74524064372836e-2 1.74524064372836e-2 1.74524064372836e-2)
        ]
