
import Control.Monad
import Text.Parsec

import Webrexp.Exprtypes
import Webrexp.Parser

tests :: [String]
tests = [ "img[4]"
        , "img[12-16]"
        , "img[5,12,40-72, 21]"
        , "div{@height * 3 + 3 > 12}"
        , "img{@width=3; @height > 4; .}"
        , "div ; span"
        , "div; span"
        , "div ;span"
        , "div;span"
        , "div.comic-pane img"
        , "(> (div.comic-pane img {.}; div.nav-next a))+"
        , "(> (div.comic-pane   img {.} ; div.nav-next a))+"
        , "dabada"
        , "dabada.poum"
        , "dabada#bang"
        , "dabada.poum#bang"
        , "rootpage > div | span |[4] ^"
        ]

main :: IO ()
main = do
    mapM_ (\a -> do
        putStrLn "\n--------------------------------------------"
        putStrLn $ "expr: " ++ a
        putStrLn "---------------"
        print $ runParser webrexp () a a) tests
    return ()

