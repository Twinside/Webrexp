module Webrexp.Parser( webRexpParser ) where

import Control.Applicative( (<$>), (<*), (<$) )
import Control.Monad.Identity

import Webrexp.Exprtypes

import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Language( haskellStyle )
import qualified Text.Parsec.Token as P

-- | Parser used to parse a webrexp.
-- Use just like any 'Parsec' 3.0 parser.
webRexpParser :: ParsecT String st Identity WebRexp
webRexpParser = webrexp 

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

braces :: ParsecT String u Identity a 
       -> ParsecT String u Identity a
braces = P.braces lexer

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
                                             , "_"
                                             ]
                       , P.identStart = letter
                       } )

-----------------------------------------------------------
--          Real "grammar"
-----------------------------------------------------------
webrexpCombinator :: OperatorTable String st Identity WebRexp
webrexpCombinator =
    [ [ postfix "*" Star
      , postfix "+" Plus ] ]

operatorDefs :: OperatorTable String st Identity ActionExpr
operatorDefs = 
    [ [binary "/" (BinOp OpDiv) AssocLeft
      ,binary "*" (BinOp OpMul) AssocLeft]
    , [binary "+" (BinOp OpAdd) AssocLeft
      ,binary "-" (BinOp OpSub) AssocLeft]
    , [binary "=" (BinOp OpEq)  AssocRight
      ,binary "!=" (BinOp OpNe) AssocLeft
      ,binary "<" (BinOp OpLt)  AssocLeft
      ,binary ">"  (BinOp OpGt) AssocLeft
      ,binary "<=" (BinOp OpLe) AssocLeft
      ,binary ">=" (BinOp OpGe) AssocLeft]
    , [binary "&" (BinOp OpAnd) AssocLeft
      ,binary "|" (BinOp OpOr) AssocLeft]
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
    Range . simplifyNodeRanges <$> 
            sepBy noderange (whiteSpace >> char ',' >> whiteSpace)
                            <* whiteSpace

webrexpOp :: Parsed st WebRexp
webrexpOp =  spaceSurrounded ops
    where ops =  (DiggLink <$ char '>')
             <|> (PreviousSibling <$ char '^')
             <|> (NextSibling <$ char '/')
             <|> (Parent <$ char '<')
             <|> (Unique (-1) <$ char '!')
             <?> "webrexpOp"

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
    initial <- Elem <$> webident
    (do op <- webrefop
        next <- webident
        return $ op initial next) <|> return initial

attribute :: Parsed st String
attribute = char '@' >> webident

actionTerm :: Parsed st ActionExpr
actionTerm = (ARef <$> attribute)
          <|> parens actionExpr
          <|> (CstS <$> stringLiteral)
          <|> (CstI . fromIntegral <$> natural)
          <|> (OutputAction <$ char '.')
          <?> "actionTerm"

actionExpr :: Parsed st ActionExpr
actionExpr =
    buildExpressionParser operatorDefs
                        (spaceSurrounded actionTerm)

actionList :: Parsed st ActionExpr
actionList = (aexpr <$>
    sepBy actionExpr (whiteSpace >> char ';' >> whiteSpace))
             <?> "actionList"
     where aexpr [a] = a
           aexpr b = ActionExprs b

webrexp :: Parsed st WebRexp
webrexp = (branch <$> sepBy1 exprPath
                     (whiteSpace >> char ';' >> whiteSpace))
       <?> "webrexp"
    where branch [a] = a
          branch a = Branch a

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
       <|> braces (Action <$> actionList)
       <|> brackets rangeParser
       <|> webrexpOp
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

{-prefix :: String -> (a -> a) -> Operator String st Identity a-}
{-prefix  name fun = Prefix (do{ reservedOp name; return fun })-}

postfix :: String -> (a -> a) -> Operator String st Identity a
postfix name fun = Postfix (do{ reservedOp name; return fun })

