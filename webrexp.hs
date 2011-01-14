import System.Environment
import Text.Parsec

import Webrexp.Parser
import Webrexp.WebContext

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ args !! 1
    print $ runParser webrexp () (args !! 1) file
    return ()

