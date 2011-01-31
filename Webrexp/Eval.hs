module Webrexp.Eval
    (
    -- * Functions
    evalWebRexp,
    evalAction
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe

import Webrexp.GraphWalker
import Webrexp.Exprtypes
import Webrexp.WebContext

import Webrexp.Log
import qualified Data.ByteString.Lazy as B

evalList :: (GraphWalker node rezPath)
         => Bool -> [WebRexp] -> WebCrawler node rezPath Bool
evalList _ [] = return True
evalList isTail [x] = breadthFirstEval isTail x
evalList isTail (x:xs) = do
    valid <- breadthFirstEval False x
    if valid
       then evalList isTail xs
       else do debugLog "> FALSE [ .. ]"
       	       return False

-- | Evaluate a list of webrexp with a rollback system (reverting
-- to the initial state for every list element).
evalBranches :: (GraphWalker node rezPath)
             => Bool -> [WebRexp] -> WebCrawler node rezPath Bool
evalBranches _ [] = return True
evalBranches isTail [x] = do
    popCurrentState
    -- If we are the tail, we can drop
    -- the context without problem
    when (not isTail) pushCurrentState
    breadthFirstEval isTail x

evalBranches isTail (x:xs) = do
    popCurrentState
    pushCurrentState
    valid <- breadthFirstEval False x
    if valid
       then evalBranches isTail xs
       else return False

applyFunTillFalse :: (GraphWalker node rezPath)
                  => (Bool -> a -> WebCrawler node rezPath Bool) -> Bool -> a
                  -> WebCrawler node rezPath Bool
applyFunTillFalse f isTail obj = do
    valid <- f isTail obj
    if valid then applyFunTillFalse f isTail obj
             else return True

-- | For the current state, filter the value to keep
-- only the values which are included in the node
-- range.
filterNodes :: [NodeRange] -> WebCrawler node rezPath ()
filterNodes ranges = getEvalState >>= setEvalState . filtered
      where filtered = discardLockstep ranges . zip [0..]
            discardLockstep [] _  = []
            discardLockstep _  [] = []
            discardLockstep rlist@(Index i:xs) elist@((i2,e):ys)
                | i2 == i = e : discardLockstep xs ys
                | i2 < i = discardLockstep rlist ys
                -- i2 > i (should not arrise in practice)
                | otherwise = discardLockstep xs elist
            discardLockstep rlist@(Interval a b:xs) elist@((i,e):ys)
                | i < a = discardLockstep rlist ys
                -- i >= a
                | i < b = e : discardLockstep rlist ys
                | i == b = e : discardLockstep xs ys
                -- i > b
                | otherwise = discardLockstep xs elist


-- | repeatidly eval the webrexp until a false is returned.
evalTillFalse :: (GraphWalker node rezPath)
              => Bool -> WebRexp -> WebCrawler node rezPath Bool
evalTillFalse = applyFunTillFalse breadthFirstEval


-- | Given a node search for valid children, check for their
-- validity against the requirement.
searchRefIn :: (GraphWalker node rezPath)
            => WebRef -> NodeContext node rezPath
            -> [NodeContext node rezPath]
searchRefIn (Elem s) n =
    [ NodeContext {
        parents = subP ++ parents n,
        this = sub,
        rootRef = rootRef n
    }  | (sub, subP) <- findNamed s $ this n]
searchRefIn (OfClass r s) n =
    [v | v <- searchRefIn r n, attribOf "class" (this v) == Just s]
searchRefIn (Attrib  r s) n =
    [v | v <- searchRefIn r n, attribOf s (this v) /= Nothing]
searchRefIn (OfName  r s) n =
    [v | v <- searchRefIn r n, attribOf "id" (this v) == Just s]

evalWebRexp :: (GraphWalker node rezPath)
            => WebRexp -> WebCrawler node rezPath Bool
evalWebRexp rexp = do
    setUniqueBucketCount count
    debugLog $ "Parsed as: " ++ show neorexp
    breadthFirstEval True neorexp
    where (count, neorexp) = foldWebRexp uniqueCounter 0 rexp
          uniqueCounter acc (Unique _) = (acc + 1, Unique acc)
          uniqueCounter acc e = (acc, e)

-- | Evaluate an expression, the boolean is here to propagate
-- the idea of 'tail' call, if we are at the tail of the expression
-- we can discard some elements safely and thus reduce memory
-- usage (which can be important)
breadthFirstEval :: (GraphWalker node rezPath)
             => Bool -> WebRexp -> WebCrawler node rezPath Bool
breadthFirstEval isTail (Branch subs) = do
    debugLog "> '[... ; ...]'"
    pushCurrentState
    evalBranches isTail subs

breadthFirstEval isTail (List subs) = do
    debugLog "> '[...]'"
    evalList isTail subs

breadthFirstEval isTail (Star subs) = do
    debugLog "> '...*'"
    evalTillFalse isTail subs

breadthFirstEval isTail (Plus subs) = do
    debugLog "> '...+'"
    once <- breadthFirstEval isTail subs
    if once then do _ <- evalTillFalse isTail subs
                    return True
            else return False

breadthFirstEval isTail (Alternative a b) = do
    debugLog "> '...|...'"
    leftValid <- breadthFirstEval False a
    if leftValid
       then return True
       else breadthFirstEval isTail b

breadthFirstEval _ (Str str) = do
    debugLog "> '\"...\"'"
    setEvalState $ [Text str]
    return True

breadthFirstEval _ (Action action) = do
    debugLog "> '{...}'"
    st <- getEvalState
    rez <- mapM (evalAction action . Just) st
    mapM_ (dumpActionVal . fst) rez
    let justValids  = [el | (actRez, e) <- rez 
                        , isActionResultValid actRez
                        , let Just el = e]
    debugLog $ ">>> left " ++ show (length justValids)
    setEvalState justValids
    hasNodeLeft

breadthFirstEval _ (Unique bucket) = do
    debugLog $ "> '!' (" ++ show bucket ++ ")"
    st <- getEvalState
    st' <- filterM visited st
    if null st'
       then return False
       else setEvalState st' >> hasNodeLeft

     where visited (Node n) = checkUnique . show $ rootRef n
           visited (Text s) = checkUnique s
           visited (Blob b) = checkUnique . show $ sourcePath b
           checkUnique s = do
               seen <- hasResourceBeenVisited bucket s
               when (not seen)
                    (setResourceVisited bucket s)
               return $ not seen

breadthFirstEval _ (Ref ref) = do
    debugLog $ "> 'ref' : " ++ show ref
    st <- getEvalState
    let st' = concatMap diggNode st
    debugLog $ ">>> found " ++ show (length st)
             ++ "->"
             ++ show (length st')
    setEvalState st'
    hasNodeLeft
     where diggNode (Node n) = map Node $ searchRefIn ref n
           diggNode _ = []

breadthFirstEval _ (Range subs) = do
    debugLog "> '[...]'"
    filterNodes subs
    return True

breadthFirstEval _ DiggLink = do
    debugLog "> '>'"
    st <- getEvalState 
    concat <$> mapM diggLinks st >>= setEvalState
    hasNodeLeft

breadthFirstEval _ NextSibling = do
  debugLog "> '/'"
  st <- getEvalState
  setEvalState . catMaybes $ map (siblingAccessor 1) st
  hasNodeLeft

breadthFirstEval _ PreviousSibling = do
  debugLog "> '^'"
  st <- getEvalState
  setEvalState . catMaybes $ map (siblingAccessor (-1)) st
  hasNodeLeft

breadthFirstEval _ Parent = do
  debugLog "> '<'"
  st <- getEvalState
  setEvalState $ concatMap parentExtractor st
  hasNodeLeft
    where parentExtractor (Node node) =
            case parents node of
                []       -> []
                (n,_):ps ->
                    [Node $ node { parents = ps, this = n }]
          parentExtractor _ = []

downLinks :: (GraphWalker node rezPath)
          => rezPath
          -> WebCrawler node rezPath [EvalState node rezPath]
downLinks path = do
    loggers <- prepareLogger
    down <- accessGraph loggers path
    case down of
         AccessError -> return []
         DataBlob u b -> return [Blob $ BinBlob u b]
         Result u n -> return [Node $
                    NodeContext { parents = []
         	                    , rootRef = u
         	                    , this = n }]

diggLinks :: (GraphWalker node rezPath)
          => EvalState node rezPath
          -> WebCrawler node rezPath [EvalState node rezPath]
diggLinks (Node n) = do
    concat <$> sequence
            [ downLinks $ rootRef n <//> indir
                                | indir <- indirectLinks $ this n ]
diggLinks (Text str) = case importPath str of
        Nothing -> return []
        Just p -> downLinks p
diggLinks _ = return []

-- | Let access sibling nodes with a predefined index.
siblingAccessor :: (GraphWalker node rezPath)
                => Int -> EvalState node rezPath
                -> Maybe (EvalState node rezPath)
siblingAccessor 0   node@(Node _) = Just $ node
siblingAccessor idx (Node node)=
    case parents node of
      [] -> Nothing
      (n,i):ps ->
          let children = childrenOf n
              childrenCount = length children
              neoIndex = i + idx
          in if neoIndex < 0 || neoIndex >= childrenCount
                then Nothing
                else Just . Node $ NodeContext
                        { parents = (n, neoIndex):ps
                        , this = children !! neoIndex
                        , rootRef = rootRef node
                        }
siblingAccessor _ _ = Nothing

--------------------------------------------------
----            Action Evaluation
--------------------------------------------------

-- | Data used for the evaluation of actions. Represent the
-- whole set of representable data at runtime.
data ActionValue =
      AInt    Int
    | ABool   Bool
    | AString String
    | ATypeError
    deriving (Show)

binArith :: (GraphWalker node rezPath)
         => (ActionValue -> ActionValue -> ActionValue) -- ^ Function to cal result
         -> Maybe (EvalState node rezPath) -- Actually evaluated element
         -> ActionExpr       -- Left subaction (tree-like)
         -> ActionExpr      -- Right subaction (tree-like)
         -> WebCrawler node rezPath (ActionValue, Maybe (EvalState node rezPath))
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
boolComp f a         (AInt b) = boolComp f a (ABool $ b /= 0)
boolComp f (AInt a)         b = boolComp f (ABool $ a /= 0) b
boolComp _ _                _ = ABool $ False

isActionResultValid :: ActionValue -> Bool
isActionResultValid (ABool False) = False
isActionResultValid (AInt 0) = False
isActionResultValid ATypeError = False
isActionResultValid _ = True

dumpActionVal :: ActionValue -> WebCrawler node rezPath ()
dumpActionVal (AString s) = textOutput s
dumpActionVal _ = return ()

dumpContent :: (GraphWalker node rezPath)
            => Maybe (EvalState node rezPath)
            -> WebCrawler node rezPath (ActionValue, Maybe (EvalState node rezPath))
dumpContent Nothing = return (ABool False, Nothing)
dumpContent e@(Just (Node ns)) =
  case indirectLinks (this ns) of
    [] -> return (AString $ valueOf (this ns), e)
    links -> do
        loggers <- prepareLogger
        mapM_ (\l -> dumpDataAtPath loggers $
                            (rootRef ns) <//> l) links
        return (ABool True, e)
dumpContent e@(Just (Text str)) = return (AString str, e)
dumpContent e@(Just (Blob b)) = do
    (norm, _, _) <- prepareLogger
    let filename = localizePath $ sourcePath b
    liftIO . norm $ "Dumping blob in " ++ filename
    liftIO $ B.writeFile filename (blobData b)
    return (ABool True, e)

-- | Evaluate embedded action in WebRexp
evalAction :: (GraphWalker node rezPath)
           => ActionExpr
           -> Maybe (EvalState node rezPath)
           -> WebCrawler node rezPath
                        (ActionValue, Maybe (EvalState node rezPath))
evalAction (ActionExprs actions) e = foldM eval (ABool True, e) actions
    where eval v@(ABool False, _) _ = return v
          eval (_, el) act = evalAction act el

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
evalAction OutputAction e =
    dumpContent e

evalAction (ARef r) e@(Just (Node n)) = do
    case attribOf r (this n) of
      Nothing -> return (ABool False, e)
      Just s -> return $ (AString s, e)

evalAction (ARef _) _ =
    return (ATypeError, Nothing)

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

