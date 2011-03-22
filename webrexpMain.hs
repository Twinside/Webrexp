
import Control.Monad
import qualified Control.Exception as E
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import Webrexp

data Flag = Verbose
          | Quiet
          | Input String
          | Output String
          | BfsEval
          | Help
          | Graphviz
          | Delay Int
          deriving Eq

version :: String
version = "1.0"

options :: [OptDescr Flag]
options =
    [ Option "o"  ["output"] (ReqArg Output "FILE") "output FILE"
    , Option "f"  ["file"] (ReqArg Input "FILE") "input FILE, use - for stdin"
    , Option "v"  ["verbose"] (NoArg Verbose) "Display many information"
    , Option "q"  ["quiet"] (NoArg Quiet) "Remain practically silent"
    , Option "h"  ["help"]  (NoArg Help) "Display help (this screen)"
    , Option "d"  ["delay"] (ReqArg (Delay . read) "Delay")
            "Time to wait between HTTP request (ms)"
    , Option ""   ["bfs"] (NoArg BfsEval)
             "Evaluate in BFS order - This option might not remain in the future"
    , Option ""   ["dot"] (NoArg Graphviz)
             "Output the evaluation automata in graphviz format."
    ]

hasInput :: Flag -> Bool
hasInput (Input _) = True
hasInput _ = False

parseArgs :: [String] -> IO Conf
parseArgs args =
    case getOpt Permute options args of
     (opt, [], _) -> do
            conf <- foldM configurator defaultConf opt
            return $ conf { showHelp = not $ any hasInput opt }
     (opt, left:_, _) -> do
        conf <- foldM configurator defaultConf opt
        if expr conf == "" then return $ conf{expr = left}
                          else return conf
     where configurator c Verbose = return $ c{ verbose = True }
           configurator c Quiet = return $ c{ quiet = True }
           configurator c Help = return $ c{ showHelp = True }
           configurator c Graphviz = return $ c { outputGraphViz = True }
           configurator c BfsEval = return $ c { depthEvaluation = False }
           configurator c (Output "-") = return $ c { output = stdout }
           configurator c (Delay i) = return $ c { hammeringDelay = i }
           configurator c (Input fname) = do
               file <- readFile fname
               return $ c{ expr = file }
           configurator c (Output fname) = do
               file <- openFile fname WriteMode
               return $ c{ output = file }

mainProgram :: IO ()
mainProgram = do
    args <- getArgs
    c <- parseArgs args
    if showHelp c
       then do putStrLn $ usageInfo ("Webrexp v" ++ version) options
               exitWith ExitSuccess

       else do valid <- evalWebRexpWithConf c
               if valid
                  then exitWith ExitSuccess
                  else exitWith $ ExitFailure 1

main :: IO ()
main =
    E.catch mainProgram
          (\e -> do
              let err = show (e :: E.AsyncException)
              putStrLn $ "Error : " ++ err
              return ())
