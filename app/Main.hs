import Brightness
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Illuminance
import Intensity ()
import Lib (calcBrightness, calcIlluminance)
import RGB
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
                            putStrLn $ printf "\nIlluminance E_%d:" i
                            putStr $ printf "|  Y/X  "
                            mapM_ ((putStr . printf "| %21.3f ") . fst) ps
                            putStrLn ""
                            mapM_
                                ( ( \y -> do
                                        putStr $ printf "| %4.3f " y
                                        mapM_ ((\x -> let Illuminance (RGB r' g' b') = calcIlluminance l t (x, y) in putStr $ printf "| (%5.2f, %5.2f, %5.2f) " r' g' b') . fst) ps
                                        putStrLn ""
                                  )
                                    . snd
                                )
                                ps
                        )
                        $ zip ls [1 .. length ls]

                    putStrLn "\nBrightness:"
                    putStr $ printf "|  Y/X  "
                    mapM_ ((putStr . printf "| %21.3f ") . fst) ps
                    putStrLn ""
                    mapM_
                        ( ( \y -> do
                                putStr $ printf "| %4.3f " y
                                mapM_ ((\x -> let Brightness (RGB r' g' b') = calcBrightness ls t v s (x, y) in putStr $ printf "| (%5.2f, %5.2f, %5.2f) " r' g' b') . fst) ps
                                putStrLn ""
                          )
                            . snd
                        )
                        ps
    mainLoop
