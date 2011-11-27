-- | Module implementing the parsing of webrexp.
-- It shouldn't be used directly.
module Text.Webrexp.Parser( webRexpParser ) where

import Control.Applicative( (<$>), (<$), (<*>) )
import Control.Monad.Identity
import qualified Data.Map as Map

import Text.Webrexp.Exprtypes

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Language( haskellStyle )
import qualified Text.Parsec.Token as P

-- | Parser used to parse a webrexp.
-- Use just like any 'Parsec' 3.0 parser.
webRexpParser :: ParsecT String st Identity WebRexp
webRexpParser = webrexp 

-- | Little shortcut.
type Parsed st b = ParsecT String st Identity b

-----------------------------------------------------------
--          Lexing defs
-----------------------------------------------------------
reservedOp :: String -> Parsed st ()
reservedOp = P.reservedOp lexer

natural :: Parsed st Integer
natural = P.natural lexer

stringLiteral :: Parsed st String
stringLiteral = P.stringLiteral lexer

parens :: ParsecT String u Identity a 
       -> ParsecT String u Identity a
parens = P.parens lexer

brackets :: ParsecT String u Identity a 
         -> ParsecT String u Identity a
brackets = P.brackets lexer

whiteSpace :: Parsed st ()
whiteSpace = P.whiteSpace lexer

lexer :: P.GenTokenParser String st Identity
lexer  = P.makeTokenParser 
         (haskellStyle { P.reservedOpNames = [ "&", "|", "<", ">"
                                             , "*", "/", "+", "-"
                                             , "^", "=", "!", ":"
                                             , "_", "$", "~", "?"
                                             ]
                       , P.identStart = letter
                       } )

-----------------------------------------------------------
--          Real "grammar"
-----------------------------------------------------------
webrexpCombinator :: OperatorTable String st Identity WebRexp
webrexpCombinator =
    [ [ postfix "*" Star
      , Postfix repeatOperator ]
    , [ binary "|" Alternative AssocLeft ]
    ]

operatorDefs :: OperatorTable String st Identity ActionExpr
operatorDefs = 
    [ [binary "/" (BinOp OpDiv) AssocLeft
      ,binary "*" (BinOp OpMul) AssocLeft]
    , [binary "+" (BinOp OpAdd) AssocLeft
      ,binary "-" (BinOp OpSub) AssocLeft
      ,binary ":" (BinOp OpConcat) AssocLeft]
    , [binary "=" (BinOp OpEq)  AssocRight
      ,binary "!=" (BinOp OpNe) AssocLeft
      ,binary "=~" (BinOp OpMatch) AssocLeft
      ,binary "~=" (BinOp OpContain) AssocLeft

      -- CSS compatibility...
      ,binary "~=" (BinOp OpContain) AssocLeft
      ,binary "^=" (BinOp OpBegin) AssocLeft
      ,binary "$=" (BinOp OpEnd) AssocLeft
      ,binary "*=" (BinOp OpSubstring) AssocLeft
      ,binary "|=" (BinOp OpHyphenBegin) AssocLeft

      ,binary "<" (BinOp OpLt)  AssocLeft
      ,binary ">"  (BinOp OpGt) AssocLeft
      ,binary "<=" (BinOp OpLe) AssocLeft
      ,binary ">=" (BinOp OpGe) AssocLeft]
    , [binary "&" (BinOp OpAnd) AssocLeft
      ,binary "|" (BinOp OpOr) AssocLeft]
    , [prefix "$" NodeReplace]
    ]

functionMap :: Map.Map String BuiltinFunc
functionMap = Map.fromList
    [ ("trim"   , BuiltinTrim)
    , ("replace", BuiltinSubsitute)
    , ("to_num" , BuiltinToNum)
    , ("to_str" , BuiltinToString)
    , ("format" , BuiltinFormat)
    , ("sys"    , BuiltinSystem)
    ]


-- | Parse some range
noderange :: Parsed st NodeRange
noderange = do
    n <- fromInteger <$> natural
    (do _ <- char '-' 
        m <- fromInteger <$> natural
        return $ Interval n m) <|> return (Index n)

rangeParser :: Parsed st WebRexp
rangeParser = do
    string "#{" >> whiteSpace
    vals <- sepBy noderange separator
    _ <- whiteSpace >> char '}'
    return . Range (-1) $ simplifyNodeRanges vals
     where separator = whiteSpace >> char ',' >> whiteSpace

webrexpOp :: Parsed st WebRexp
webrexpOp = (DiggLink <$ string ">>")
         <|> (DumpLink <$ string "->")
         <|> (PreviousSibling <$ char '~')
         <|> (NextSibling <$ char '+')
         <|> (Parent <$ char '<')
         <|> (Unique (-1) <$ char '!')
         <?> "webrexpOp"

repeatCount :: Parsed st RepeatCount
repeatCount = do
    begin <- fromInteger <$> natural
    parseComma begin <|> return (RepeatTimes begin)
     where parseComma firstNum = do
             whiteSpace
             _ <- char ','
             whiteSpace
             parseLastNumber firstNum <|> return
                                        (RepeatAtLeast firstNum) 
                
           parseLastNumber firstNum = do
              endNum <- fromInteger <$> natural
              return $ RepeatBetween firstNum endNum

repeatOperator :: Parsed st (WebRexp -> WebRexp)
repeatOperator = (do
    whiteSpace
    _ <-  char '{' >> whiteSpace
    counts <- repeatCount
    _ <- whiteSpace >> char '}' >> whiteSpace
    return $ Repeat counts) <?> "#{repeat}"

webident :: Parsed st String
webident = many1 (alphaNum <|> char '-' <|> char '_')
        <?> "webident"

webrefop :: Parsed st (WebRef -> String -> WebRef)
webrefop = (OfClass <$ char '.')
        <|> (Attrib <$ char '@')
        <|> (OfName <$ char '#')
        <?> "webrefop"

webref :: Parsed st WebRef
webref = do
    initialIdent <- webident
    let initial = if initialIdent == "_"
            then Wildcard
            else Elem initialIdent 
    (do op <- webrefop
        next <- webident
        return $ op initial next) <|> return initial

actionCall :: Parsed st ActionExpr
actionCall = do
    ident <- webident
    (char '(' >> funParser ident) <|> return (ARef ident)
        where funParser ident = do
                  args <- sepBy1 actionExpr (spaceSurrounded $ char ',')
                  _ <- whiteSpace >> char ')'
                  case Map.lookup ident functionMap of
                     Nothing -> error $ "Unknown function " ++ ident
                     Just b -> return $ Call b args

actionTerm :: Parsed st ActionExpr
actionTerm = (CstI . fromIntegral <$> natural)
          <|> parens actionExpr
          <|> (CstS <$> stringLiteral)
          <|> (OutputAction <$ char '.')
          <|> (NodeNameOutputAction <$ char '?')
          <|> (DeepOutputAction <$ char '#')
          <|> actionCall
          <?> "actionTerm"

actionExpr :: Parsed st ActionExpr
actionExpr = (char '$' >> whiteSpace >> NodeReplace <$> wholeExpr)
          <|> wholeExpr
          <?> "actionExpr"
    where wholeExpr = buildExpressionParser operatorDefs (spaceSurrounded actionTerm)

actionList :: Parsed st ActionExpr
actionList = (aexpr <$>
    sepBy actionExpr (whiteSpace >> char ';' >> whiteSpace))
             <?> "actionList"
     where aexpr [a] = a
           aexpr b = ActionExprs b

webrexp :: Parsed st WebRexp
webrexp = (do path <- exprUnion
              rest <- recParser <|> return []
              return . aBrancher $ path : rest) <?> "webrexp"
    where separator = whiteSpace >> char ';' >> whiteSpace
          aBrancher [a] = a
          aBrancher a = Branch a
          recParser = separator >>
           ((do p <- exprUnion
                ((p:) <$> recParser) <|> return [p]) <|> return [List []])


exprUnion :: Parsed st WebRexp
exprUnion = unioner <$> exprPath `sepBy1` separator
    where separator = whiteSpace >> char ',' >> whiteSpace
          unioner [a] = a
          unioner a = Unions a

exprPath :: Parsed st WebRexp
exprPath = (list <$> many1 expr)
        <?> "exprPath"
    where list [a] = a
          list a = List a

expr :: Parsed st WebRexp
expr = buildExpressionParser webrexpCombinator (spaceSurrounded expterm)
    <?> "expr"

expterm :: Parsed st WebRexp
expterm = parens webrexp
       <|> brackets (Action <$> actionList)
       <|> rangeParser
       <|> (try (DirectChild <$ (char '>' >> whiteSpace) <*> webref) <|> webrexpOp)
       <|> (Str <$> stringLiteral)
       <|> (Ref <$> webref)
       <?> "expterm"
        
spaceSurrounded :: Parsed st a -> Parsed st a
spaceSurrounded p = do
    whiteSpace
    something <- p
    whiteSpace
    return something

-----------------------------------------------
----        Little helpers
-----------------------------------------------
binary :: String -> (a -> a -> a) -> Assoc -> Operator String st Identity a
binary name fun = Infix (do{ reservedOp name; return fun })

prefix :: String -> (a -> a) -> Operator String st Identity a
prefix  name fun = Prefix (do{ reservedOp name; return fun })

postfix :: String -> (a -> a) -> Operator String st Identity a
postfix name fun = Postfix (do{ reservedOp name; return fun })

