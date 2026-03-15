import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Intensity ()
import Lib (calc)
import Scene

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
                mapM_ (\(x, y) -> print $ calc ls t v s (x, y)) ps
    mainLoop
