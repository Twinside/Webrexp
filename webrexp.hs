import Control.Monad
import System.Environment
import Text.Parsec

import Webrexp.Exprtypes
import Webrexp.Parser

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ args !! 1
    print $ runParser webrexp () (args !! 1) file
    return ()

