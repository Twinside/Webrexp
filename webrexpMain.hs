
import System.Environment
import System.Directory
import Webrexp

evaluator :: String -> IO ()
evaluator expr = do
    putStrLn $ "Executing :\n" ++ expr
    valid <- evalWebRexp expr
    if valid
       then putStrLn "SUCCESS"
       else putStrLn "ERROR"

main :: IO ()
main = do
    args <- getArgs
    let filename = args !! 0
    real <- doesFileExist filename
    if real
       then do putStrLn $ "Executing file : " ++ filename
               file <- readFile $ filename
               evaluator file
       else evaluator filename

