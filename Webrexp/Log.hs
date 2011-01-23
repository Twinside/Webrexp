module Webrexp.Log (
        errorLog,
        infoLog,
        debugLog,

        networkInfo,
        networkError,
    ) where

errorLog :: String -> IO ()
errorLog = putStrLn

infoLog :: String -> IO ()
infoLog = putStrLn

networkInfo :: String -> IO ()
networkInfo = putStrLn

networkError :: String -> IO ()
networkError = putStrLn

debugLog :: String -> IO ()
debugLog = putStrLn

