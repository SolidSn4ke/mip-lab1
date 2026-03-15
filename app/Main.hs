import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Intensity ()
import Lib (calcBrightness, calcIlluminance)
import Scene
import Text.Printf (printf)

main :: IO ()
main = mainLoop

mainLoop :: IO ()
mainLoop = do
    putStrLn "enter file path"
    input <- getLine
    content <- BL.readFile input
    case eitherDecode content :: Either String Scene of
        Left err -> putStrLn err
        Right params ->
            let
                Scene ls t v s ps = params
             in
                do
                    mapM_
                        ( \(l, i) -> do
                            putStrLn $ printf "Illuminance E_%d:" i
                            mapM_ (\(x, y) -> print $ calcIlluminance l t (x, y)) ps
                        )
                        $ zip ls [1 .. length ls]

                    putStrLn "Brightness:"
                    mapM_ (\(x, y) -> print $ calcBrightness ls t v s (x, y)) ps
    mainLoop
