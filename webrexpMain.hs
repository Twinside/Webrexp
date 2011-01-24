
import Control.Monad
import System.Console.GetOpt
import System.Environment
import System.IO

import Webrexp

data Flag = Verbose
          | Quiet
          | Input String
          | Output String
          | Delay Int
          deriving Eq

options :: [OptDescr Flag]
options =
    [ Option "o"  ["output"] (ReqArg Output "FILE") "output FILE"
    , Option "f"  ["file"] (ReqArg Input "FILE") "input FILE, use - for stdin"
    , Option "v"  ["verbose"] (NoArg Verbose) "Display many information"
    , Option "q"  ["quiet"] (NoArg Quiet) "Remain practically silent"
    , Option "d"  ["delay"] (ReqArg (Delay . read) "Delay")
            "Time to wait between HTTP request (ms)"
    ]

parseArgs :: [String] -> IO Conf
parseArgs args = 
  let (opt, left:_, _) = getOpt Permute options args in do
    conf <- foldM configurator defaultConf opt
    if expr conf == "" then return $ conf{expr = left}
                      else return conf
     where configurator c Verbose = return $ c{ verbose = True }
           configurator c Quiet = return $ c{ quiet = True }
           configurator c (Output "-") = return $ c { output = stdout }
           configurator c (Delay i) = return $ c { hammeringDelay = i }
           configurator c (Input fname) = do
               file <- readFile fname
               return $ c{ expr = file }
           configurator c (Output fname) = do
               file <- openFile fname WriteMode
               return $ c{ output = file }

main :: IO ()
main = do
    args <- getArgs
    c <- parseArgs args
    valid <- evalWebRexpWithConf c
    if valid
       then putStrLn "SUCCESS"
       else putStrLn "ERROR"

