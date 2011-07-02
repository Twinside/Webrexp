
import Control.Monad
import Text.Parsec

import Text.Webrexp
import Text.Webrexp.Exprtypes

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
        , "\"http://www.google.com\" >"
        , "\"http://www.stringtheorycomic.com/comics/chapterone/chapterone/\" > (div.comic img {.}; div.nav-next a >)*"
        ]

main :: IO ()
main = do
    mapM_ (\a -> do
        putStrLn "\n--------------------------------------------"
        putStrLn $ "expr: " ++ a
        putStrLn "---------------"
        print $ parseWebRexp a) tests
    return ()

