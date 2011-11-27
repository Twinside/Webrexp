{-# LANGUAGE QuasiQuotes #-}
import Text.Webrexp
import Text.Webrexp.Quote
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = print $ queryDocument ParseableJson document [webrexpParse| some things [.] |]
    where document = B.pack "{ \"some\": { \"things\": \"a phrase\" } }"

