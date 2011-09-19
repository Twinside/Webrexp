{-# LANGUAGE FlexibleContexts #-}
module Text.Webrexp.Eval.Action( evalAction 
                          , dumpActionVal
                          , isActionResultValid ) where

import Control.Applicative
import Control.Monad
import Data.List
import Text.Regex.PCRE

import Text.Webrexp.GraphWalker
import Text.Webrexp.Exprtypes
import Text.Webrexp.WebContext
import Text.Webrexp.Eval.ActionFunc
import Text.Webrexp.IOMock

import qualified Text.Webrexp.ProjectByteString as B

binArith :: ( GraphWalker node rezPath
            , IOMockable (WebContextT node rezPath m)
            , Functor m
            , Monad m)
         => (ActionValue -> ActionValue -> ActionValue) -- ^ Function to cal result
         -> Maybe (EvalState node rezPath) -- Actually evaluated element
         -> ActionExpr       -- Left subaction (tree-like)
         -> ActionExpr      -- Right subaction (tree-like)
         -> WebContextT node rezPath m (ActionValue, Maybe (EvalState node rezPath))
binArith _ Nothing _ _ = return (ATypeError, Nothing)
binArith f e sub1 sub2 = do
    (v1,e') <- evalAction sub1 e
    case e' of
      Nothing -> return (ATypeError, Nothing)
      Just _ -> do
        (v2, e'') <- evalAction sub2 e'
        return (v1 `f` v2, e'')

intOnly :: (Int -> Int -> Int) -> ActionValue -> ActionValue -> ActionValue
intOnly f (AInt a) (AInt b) = AInt $ f a b
intOnly _ _ _ = ATypeError

stringOnly :: (String -> String -> String) -> ActionValue -> ActionValue
           -> ActionValue
stringOnly f (AString a) (AString b) = AString $ f a b
stringOnly _ _ _ = ATypeError

stringPredicate :: (String -> String -> Bool) -> ActionValue 
                -> ActionValue -> ActionValue
stringPredicate f (AString a) (AString b) = ABool $ f a b
stringPredicate _ _ _= ATypeError

intComp :: (Int -> Int -> Bool) -> ActionValue -> ActionValue -> ActionValue
intComp f (AInt a) (AInt b) = ABool $ f a b
intComp _ _ _ = ATypeError

binComp :: ActionValue -> ActionValue -> ActionValue
binComp (AInt a) (AInt b) = ABool $ a == b
binComp (ABool a) (ABool b) = ABool $ a == b
binComp (AString a) (AString b) = ABool $ a == b
binComp ATypeError _ = ATypeError
binComp _ ATypeError = ATypeError
binComp _ _ = ATypeError

boolComp :: (Bool -> Bool -> Bool) -> ActionValue -> ActionValue -> ActionValue
boolComp f (ABool a) (ABool b) = ABool $ f a b
boolComp _ _                _ = ABool False

isActionResultValid :: ActionValue -> Bool
isActionResultValid (ABool False) = False
isActionResultValid (AInt 0) = False
isActionResultValid ATypeError = False
isActionResultValid _ = True

dumpActionVal :: (IOMockable (WebContextT node rezPath m), Monad m)
              => ActionValue -> WebContextT node rezPath m ()
dumpActionVal (AString s) = textOutput s
dumpActionVal (AInt i) = textOutput $ show i
dumpActionVal _ = return ()

dumpContent :: ( GraphWalker node rezPath
               , IOMockable (WebContextT node rezPath m)
               , Functor m
               , Monad m)
            => Bool     -- ^ If we convert recursively data.
            -> Maybe (EvalState node rezPath)   -- ^ Node to be dumped
            -> WebContextT node rezPath m (ActionValue, Maybe (EvalState node rezPath))
dumpContent _ Nothing = return (ABool False, Nothing)
dumpContent recursive e@(Just (Node ns)) =
  case (indirectLinks (this ns), recursive) of
    ([], False) -> return (AString $ valueOf (this ns), e)
    ([], True) -> (\a -> (AString a, e)) <$> deepValueOf (this ns)
    (links, _) -> do
        loggers <- prepareLogger
        mapM_ (\l -> dumpDataAtPath loggers $
                            rootRef ns <//> l) links
        return (ABool True, e)
dumpContent _ e@(Just (Text str)) = return (AString str, e)
dumpContent _ e@(Just (Blob b)) = do
    (norm, _, _) <- prepareLogger
    let filename = localizePath $ sourcePath b
    norm $ "Dumping blob in " ++ filename
    _ <- performIO $ B.writeFile filename (blobData b)
    return (ABool True, e)

-- | Evaluate embedded action in WebRexp
evalAction :: ( GraphWalker node rezPath
              , IOMockable (WebContextT node rezPath m)
              , Functor m
              , Monad m )
           => ActionExpr
           -> Maybe (EvalState node rezPath)
           -> WebContextT node rezPath m
                        (ActionValue, Maybe (EvalState node rezPath))
evalAction (ActionExprs actions) e = do
    rez <- foldM eval (ABool True, e) actions
    debugLog $ "\t>" ++ show (fst rez)
    return rez
    where eval v@(ABool False, _) _ = do
              debugLog "\t|False"
              return v
          eval v@(ATypeError, _) _ = do
              debugLog "\t|ATypeError"
              return v
          eval (actionVal, el) act = do
              debugLog $ "\t>" ++ show actionVal
              dumpActionVal actionVal
              evalAction act el

evalAction (NodeReplace sub) e = do
    (val, el) <- evalAction sub e
    case val of
         AInt i -> return (ABool True, Just . Text $ show i)
         ABool True -> return (ABool True, Just $ Text "1")
         ABool False -> return (ABool True, Just $ Text "0")
         AString s -> return (ABool True, Just $ Text s)
         ATypeError -> return (val, el)
         
evalAction (CstI i) n = return (AInt i, n)
evalAction (CstS s) n = return (AString s, n)
evalAction OutputAction e = dumpContent False e
evalAction DeepOutputAction e = dumpContent True e

evalAction (ARef r) e@(Just (Node n)) =
    case attribOf r (this n) of
      Nothing -> return (ABool False, e)
      Just s -> return (AString s, e)

evalAction (ARef _) _ =
    return (ATypeError, Nothing)

evalAction (BinOp OpMatch a b) e =
    binArith (stringPredicate (=~)) e a b
evalAction (BinOp OpAdd a b) e = binArith (intOnly (+)) e a b
evalAction (BinOp OpSub a b) e = binArith (intOnly (-)) e a b
evalAction (BinOp OpMul a b) e = binArith (intOnly (*)) e a b
evalAction (BinOp OpDiv a b) e = binArith (intOnly div) e a b

evalAction (BinOp OpLt a b) e = binArith (intComp (<)) e a b
evalAction (BinOp OpLe a b) e = binArith (intComp (<=)) e a b
evalAction (BinOp OpGt a b) e = binArith (intComp (>)) e a b
evalAction (BinOp OpGe a b) e = binArith (intComp (>=)) e a b

evalAction (BinOp OpEq a b) e = binArith binComp e a b
evalAction (BinOp OpNe a b) e = binArith (\a' b' -> valNot $ binComp a' b') e a b
    where valNot (ABool f) = ABool $ not f
          valNot el = el

evalAction (BinOp OpAnd a b) e = binArith (boolComp (&&)) e a b
evalAction (BinOp OpOr  a b) e = binArith (boolComp (||)) e a b
evalAction (BinOp OpConcat a b) e = binArith (stringOnly (++)) e a b

evalAction (BinOp OpContain a b) e =
    binArith (stringPredicate contain) e a b
        where contain att val = val `elem` words att
evalAction (BinOp OpHyphenBegin a b) e = 
    binArith (stringPredicate contain) e a b
      where contain att val = val == fst (break ('-' ==) att)
evalAction (BinOp OpBegin a b) e =
    binArith (stringPredicate $ flip isPrefixOf) e a b
evalAction (BinOp OpEnd a b) e =
    binArith (stringPredicate $ flip isSuffixOf) e a b
evalAction (BinOp OpSubstring a b) e =
    binArith (stringPredicate $ flip isInfixOf) e a b


-- We list every possibility for now to be sure to implement
-- everything.
evalAction (Call BuiltinToNum subs) e = actionFunEval toNum subs e
evalAction (Call BuiltinToString subs) e = actionFunEval funToString subs e
evalAction (Call BuiltinTrim subs) e = actionFunEval trimString subs e
evalAction (Call BuiltinFormat subs) e = actionFunEval formatString subs e
evalAction (Call BuiltinSubsitute subs) e = actionFunEval substituteFunc subs e
evalAction (Call BuiltinSystem subs) e = actionFunEvalM funcSysCall subs e

actionFunEval :: ( GraphWalker node rezPath, IOMockable (WebContextT node rezPath m)
                 , Functor m
                 , Monad m )
              => ActionFunc node rezPath
              -> [ActionExpr] -> Maybe (EvalState node rezPath)
              -> WebContextT node rezPath m
                          (ActionValue, Maybe (EvalState node rezPath))
actionFunEval f actions st =  do
    vals <- mapM (`evalAction` st) actions
    let values = map fst vals
    if all (/= ATypeError) values
       then return $ f values st
       else return (ATypeError, Nothing)


actionFunEvalM :: ( GraphWalker node rezPath
                  , IOMockable (WebContextT node rezPath m)
                  , Functor m
                  , Monad m )
               => ActionFuncM node rezPath m
               -> [ActionExpr] -> Maybe (EvalState node rezPath)
               -> WebContextT node rezPath m
                          (ActionValue, Maybe (EvalState node rezPath))
actionFunEvalM f actions st = do
    vals <- mapM (`evalAction` st) actions
    let values = map fst vals
    if all (/= ATypeError) values
       then f values st
       else return (ATypeError, Nothing)

