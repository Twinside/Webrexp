
import System.Environment
import Webrexp

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ args !! 1
    _ <- evalWebRexp file
    return ()

