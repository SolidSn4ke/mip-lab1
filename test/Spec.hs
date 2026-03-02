import Illuminance (Illuminance (Illuminance))
import Intensity (Intensity (Intensity))
import Lib
import LightSource (LightSource (..))
import Point (Point (..))
import RGB (RGB (RGB))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Module Tests" [colorIntensityTests, colorIlluminanceTests, localToGlobalTest, normalVecTest]

eps :: Double
eps = (10 :: Double) ** (-15)

colorIntensityTests :: TestTree
colorIntensityTests =
    testGroup
        "Color Intensity"
        [ testCase "Direct light source (point along axis)" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 1
             in colorIntensity source point @?= Intensity (RGB 1 1 1),
          testCase "60 degree angle from axis" $
            let source = LightSource (Intensity (RGB 1 0.5 0)) (Point 0 0 0) (Point 0 0 1)
                point = Point (sin (pi / 3)) 0 (cos (pi / 3)) -- точка под 60° к оси Z
                expectedR = 0.5 -- 1 * cos(60°)
                expectedG = 0.25 -- 0.5 * cos(60°)
                expectedB = 0.0
                Intensity (RGB r g b) = colorIntensity source point
             in assertBool "Should match expected values" $
                    abs (r - expectedR) < eps
                        && abs (g - expectedG) < eps
                        && abs (b - expectedB) < eps,
          -- testCase "Light behind source (theta > 90°)" $
          --   let source = LightSource (Intensity (RGB 100 100 100)) (Point 0 0 0) (Point 0 0 1)
          --       point = Point 0 0 (-1) -- позади источника
          --    in colorIntensity source point @?= Intensity (RGB 0 0 0),
          testCase "90 degree angle from axis" $
            let source = LightSource (Intensity (RGB 0.8 0.3 0.6)) (Point 0 0 0) (Point 0 0 1)
                point = Point 1 0 0 -- перпендикулярно оси
             in colorIntensity source point @?= Intensity (RGB 0 0 0),
          testCase "45 degree angle from axis" $
            let source = LightSource (Intensity (RGB 1 0 0)) (Point 0 0 0) (Point 0 0 1)
                point = Point (sin (pi / 4)) 0 (cos (pi / 4))
                expected = sqrt 2 / 2
                Intensity (RGB r g b) = colorIntensity source point
             in assertBool "Should be (sqrt2/2, 0, 0)" $
                    abs (r - expected) < eps && abs g < eps && abs b < eps,
          testCase "89 degree angle from axis" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                angle = pi / 180 * 89
                point = Point (sin angle) 0 (cos angle)
                expected = cos angle -- ~0.01745
                Intensity (RGB r g b) = colorIntensity source point
             in assertBool "Should match cos(89°)" $
                    abs (r - expected) < eps
                        && abs (g - expected) < eps
                        && abs (b - expected) < eps
        ]

colorIlluminanceTests :: TestTree
colorIlluminanceTests =
    testGroup
        "Color Illuminance"
        [ testCase "Direct light, perpendicular surface, unit distance" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 1
                normal = Point 0 0 1 -- нормаль смотрит на источник
             in colorIlluminance source point normal @?= Illuminance (RGB 1 1 1),
          testCase "Direct light, only red channel" $
            let source = LightSource (Intensity (RGB 1 0 0)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 1
                normal = Point 0 0 1
             in colorIlluminance source point normal @?= Illuminance (RGB 1 0 0),
          testCase "Inverse square law (distance 10)" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 10
                normal = Point 0 0 1
             in colorIlluminance source point normal @?= Illuminance (RGB 0.01 0.01 0.01),
          testCase "60 degree surface angle (alpha)" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 1
                normal = Point (sin (pi / 3)) 0 (cos (pi / 3)) -- нормаль под 60° к направлению света
                expected = 0.5 -- cos(60°)
                Illuminance (RGB r g b) = colorIlluminance source point normal
             in assertBool "Should be attenuated by cos(60°)" $
                    abs (r - expected) < eps
                        && abs (g - expected) < eps
                        && abs (b - expected) < eps,
          testCase "90 degree surface angle (grazing light)" $
            let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
                point = Point 0 0 1
                normal = Point 1 0 0 -- нормаль перпендикулярна свету
             in colorIlluminance source point normal @?= Illuminance (RGB 0 0 0)
            -- testCase "Combined effects: source angle + surface angle + distance" $
            --   let source = LightSource (Intensity (RGB 1 0 1)) (Point 0 0 0) (Point 0 0 1)
            --       -- Точка под 45° к оси источника
            --       theta = pi / 4
            --       point = Point (sin theta) 0 (cos theta) -- (0.707, 0, 0.707)
            --       -- Нормаль под 30° к направлению на точку
            --       alpha = pi / 6
            --       -- Направление от источника к точке: (0.707, 0, 0.707) уже единичный?
            --       -- Но нам нужна нормаль, повернутая на alpha от этого направления
            --       -- Упростим: возьмем нормаль = (sin(alpha)*cos(phi), sin(alpha)*sin(phi), cos(alpha))
            --       -- с phi = 0, чтобы было в плоскости XZ
            --       normal = Point (sin alpha) 0 (cos alpha) -- (0.5, 0, 0.866)
            --       -- Расстояние = 1 (уже единичный вектор)

            --       -- Ожидание:
            --       -- 1. source angle: cos(theta) = 0.7071
            --       -- 2. surface angle: cos(alpha) = 0.8660
            --       -- 3. distance: 1/dist^2 = 1
            --       -- Итог: (1 * 0.7071 * 0.8660, 0, 1 * 0.7071 * 0.8660) ≈ (0.6124, 0, 0.6124)
            --       expected = 0.7071 * 0.8660 -- ≈ 0.6124
            --       Illuminance (RGB r g b) = colorIlluminance source point normal
            --    in assertBool "Should combine all effects correctly" $
            --           abs (r - expected) < 10 ** (-3)
            --               && abs g < 10 ** (-3)
            --               && abs (b - expected) < 10 ** (-3)
            -- testCase "Light behind surface (alpha > 90°)" $
            --   let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
            --       point = Point 0 0 1
            --       normal = Point 0 0 (-1) -- нормаль смотрит от источника
            --    in colorIlluminance source point normal @?= Illuminance (RGB 0 0 0),
            -- testCase "Point not in front of source (theta > 90°)" $
            --   let source = LightSource (Intensity (RGB 1 1 1)) (Point 0 0 0) (Point 0 0 1)
            --       point = Point 0 0 (-1) -- точка позади источника
            --       normal = Point 0 0 1
            --    in colorIlluminance source point normal @?= Illuminance (RGB 0 0 0)
        ]

localToGlobalTest :: TestTree
localToGlobalTest =
    testGroup
        "Local To Global"
        [ testCase "Starting point" $ localToGlobal 0 0 (Point 0 0 0) (Point 2 0 0) (Point 0 3 0) @?= Point 0 0 0,
          testCase "On the edge" $ localToGlobal 2 0 (Point 1 1 1) (Point 4 1 1) (Point 1 5 1) @?= Point 3 1 1,
          testCase "Inside triangle" $ localToGlobal 1.5 0.5 (Point 0 0 0) (Point 2 0 0) (Point 0 2 0) @?= Point 1.5 0.5 0,
          testCase "Normalize check" $ localToGlobal 2 3 (Point 0 0 0) (Point 0 5 0) (Point 0 0 10) @?= Point 0 2 3
        ]

normalVecTest :: TestTree
normalVecTest =
    testGroup
        "Normal Vector"
        [ testCase "Triangle in XY plane (order 1)" $ normalVec (Point 0 0 0) (Point 1 0 0) (Point 0 1 0) @?= Point 0 0 (-1),
          testCase "Triangle in XY plane (order 2)" $ normalVec (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) @?= Point 0 0 1,
          testCase "Triangle in XZ plane" $ normalVec (Point 0 0 0) (Point 1 0 0) (Point 0 0 1) @?= Point 0 1 0,
          testCase "Triangle in YZ plane" $ normalVec (Point 0 0 0) (Point 0 1 0) (Point 0 0 1) @?= Point (-1) 0 0,
          testCase "Normalize check (scaled triangle)" $ normalVec (Point 0 0 0) (Point 2 0 0) (Point 0 3 0) @?= Point 0 0 (-1),
          testCase "Offset triangle (parallel to XY)" $ normalVec (Point 1 1 1) (Point 4 1 1) (Point 1 5 1) @?= Point 0 0 (-1),
          testCase "" $ normalVec (Point 1 1 1) (Point 4 1 1) (Point 1 5 1) @?= Point 0 0 (-1)
        ]
