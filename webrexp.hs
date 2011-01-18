import System.Environment
import Text.Parsec

import Webrexp.Parser
import Webrexp.Eval()

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ args !! 1
    print $ runParser webRexpParser () (args !! 1) file
    return ()

