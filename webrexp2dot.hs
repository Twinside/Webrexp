import Webrexp.Exprtypes
import Webrexp.WebRexpAutomata
import Webrexp.Parser
import Text.Parsec
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    file <- readFile $ args !! 0
    case runParser webRexpParser () "expr" file of
        Left _ -> return ()
        Right e ->
            dumpAutomata file stdout 
                    . buildAutomata 
                    . snd
                    $ setUniqueIndices e

