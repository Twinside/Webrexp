module Webrexp.Eval
    (
    -- * Functions
    evalWebRexp,
    evalAction
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe ( catMaybes )

import Webrexp.ResourcePath
import Webrexp.GraphWalker
import Webrexp.Exprtypes
import Webrexp.WebContext

import Webrexp.Log

evalList :: (GraphWalker node)
         => Bool -> [WebRexp] -> WebCrawler node Bool
evalList _ [] = return True
evalList isTail [x] = evalWebRexp isTail x
evalList isTail (x:xs) = do
    valid <- evalWebRexp False x
    if valid
       then evalList isTail xs
       else return False

-- | Evaluate a list of webrexp with a rollback system (reverting
-- to the initial state for every list element).
evalBranches :: (GraphWalker node)
             => Bool -> [WebRexp] -> WebCrawler node Bool
evalBranches _ [] = return True
evalBranches isTail [x] = do
    popCurrentState
    -- If we are the tail, we can drop
    -- the context without problem
    when (not isTail) pushCurrentState
    evalWebRexp isTail x

evalBranches isTail (x:xs) = do
    popCurrentState
    pushCurrentState
    valid <- evalWebRexp False x
    if valid
       then evalBranches isTail xs
       else return False

applyFunTillFalse :: (GraphWalker node)
                  => (Bool -> a -> WebCrawler node Bool) -> Bool -> a
                  -> WebCrawler node Bool
applyFunTillFalse f isTail obj = do
    valid <- f isTail obj
    if valid then applyFunTillFalse f isTail obj
             else return True

-- | For the current state, filter the value to keep
-- only the values which are included in the node
-- range.
filterNodes :: [NodeRange] -> WebCrawler node Bool
filterNodes ranges = getEvalState >>= \state ->
    case state of
         None -> return False
         Strings s -> setEvalState (Strings $ filtered s)
                   >> return True
         Nodes n -> setEvalState (Nodes $ filtered n)
                 >> return True
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
evalTillFalse :: (GraphWalker node)
              => Bool -> WebRexp -> WebCrawler node Bool
evalTillFalse = applyFunTillFalse evalWebRexp


-- | Given a node search for valid children, check for their
-- validity against the requirement.
searchRefIn :: (GraphWalker node)
            => WebRef -> [NodeContext node] -> [NodeContext node]
searchRefIn ref = concatMap (searchRef ref)
  where searchRef (Elem s) n = -- (\a -> trace ("ELEM: " ++ show(length a)) a)
            [ NodeContext {
                parents = subP ++ parents n,
                this = sub,
                rootRef = rootRef n
            }  | (sub, subP) <- findNamed s $ this n]
        searchRef (OfClass r s) n = -- (\a -> trace ("CLASS: " ++ show(length a)) a)
            [v | v <- searchRef r n, attribOf "class" (this v) == Just s]
        searchRef (Attrib  r s) n = -- (\a -> trace ("ATTRIB: " ++ show(length a)) a)
            [v | v <- searchRef r n, attribOf s (this v) /= Nothing]
        searchRef (OfName  r s) n = -- (\a -> trace ("NAME: " ++ show(length a)) a)
            [v | v <- searchRef r n, attribOf "id" (this v) == Just s]

-- | Evaluate an expression, the boolean is here to propagate
-- the idea of 'tail' call, if we are at the tail of the expression
-- we can discard some elements safely and thus reduce memory
-- usage (which can be important)
evalWebRexp :: (GraphWalker node) => Bool -> WebRexp -> WebCrawler node Bool
evalWebRexp isTail (Branch subs) = do
    debugLog "> '[... ; ...]'"
    pushCurrentState
    evalBranches isTail subs

evalWebRexp isTail (List subs) = do
    debugLog "> '[...]'"
    evalList isTail subs

evalWebRexp isTail (Star subs) = do
    debugLog "> '...*'"
    evalTillFalse isTail subs

evalWebRexp isTail (Plus subs) = do
    debugLog "> '...+'"
    once <- evalWebRexp isTail subs
    if once then do _ <- evalTillFalse isTail subs
                    return True
            else return False

evalWebRexp _ (Str str) = do
    debugLog "> '\"...\"'"
    setEvalState $ Strings [str]
    return True

evalWebRexp _ (Action action) = do
    debugLog "> '{...}'"
    st <- getEvalState
    case st of
      None -> return False

      Strings strs -> do
        rez <- filterM (\e -> do rez <- evalAction action $ ElemString e
                                 return $ isActionResultValid rez) strs
        setEvalState $ Strings rez
        hasNodeLeft

      Nodes nds -> do
        rez <- filterM (\e -> do rez <- evalAction action $ ElemNode e
                                 return $ isActionResultValid rez) nds
        setEvalState $ Nodes rez
        hasNodeLeft

evalWebRexp _ (Unique _subs) = do
    debugLog "> '!'"
    error "Unimplemented - Unique (webrexp)"
    -- return False

evalWebRexp _ (Ref ref) = do
    debugLog $ "> 'ref' : " ++ show ref
    st <- getEvalState
    case st of
         Nodes ns -> do
             let rezNode = searchRefIn ref ns
             {-liftIO . debugLog $ show $ map (nameOf . this) ns-}
             {-liftIO . debugLog $ show $ map (nameOf . this) rezNode-}
             {-liftIO . debugLog $ show $ map (map nameOf . childrenOf . this) rezNode-}
             debugLog $ ">>> found " ++ show (length ns)
                                     ++ "->"
                                     ++ show (length rezNode)
                                     ++ " nodes"
             setEvalState $ Nodes rezNode
             return . not $ null rezNode
         _ -> do debugLog ">>> No nodes"
                 setEvalState None
                 return False

evalWebRexp _ (Range subs) = do
    debugLog "> '[...]'"
    _ <- filterNodes subs
    return True

evalWebRexp _ DiggLink = do
    debugLog "> '>'"
    getEvalState >>= diggLinks

evalWebRexp _ NextSibling = do
  debugLog "> '|'"
  mapCurrentNodes $ siblingAccessor 1
  hasNodeLeft

evalWebRexp _ PreviousSibling = do
  debugLog "> '^'"
  mapCurrentNodes $ siblingAccessor (-1)
  hasNodeLeft

evalWebRexp _ Parent = do
  debugLog "> '<'"
  mapCurrentNodes parentExtractor
  hasNodeLeft
    where parentExtractor node =
           case parents node of
             []       -> Nothing
             (n,_):ps ->
              Just $ node { parents = ps
                          , this = n }

downLinks :: (GraphWalker node)
          => Maybe ResourcePath -> WebCrawler node (Maybe (NodeContext node))
downLinks (Just path) = do
    (norm, err, verbo) <- prepareLogger
    down <- accessGraph norm err verbo path
    case down of
         Nothing -> return Nothing
         Just (u,n) -> return . Just $
                    NodeContext { parents = []
         	                    , rootRef = u
         	                    , this = n }
downLinks Nothing = return Nothing

diggLinks :: (GraphWalker node) => EvalState node -> WebCrawler node Bool
diggLinks (Nodes subs) = do
    neoNodes <- catMaybes <$>
                    sequence [ downLinks $ (rootRef s <//>) <$> url
                                | s <- subs
                                , let href = attribOf "href" $ this s
                                , href /= Nothing
                                , let Just ref = href
                                , let url = toRezPath ref ]
    case neoNodes of
      [] -> setEvalState None >> return False
      lst -> setEvalState (Nodes lst) >> return True

diggLinks (Strings str) = do
    newDocs <- mapM (downLinks . toRezPath) str
    case catMaybes newDocs of
         [] -> setEvalState None >> return False
         lst -> do setEvalState $ Nodes lst
                   return True

diggLinks _ = do
    setEvalState None
    return False

-- | Let access sibling nodes with a predefined index.
siblingAccessor :: (GraphWalker node)
                => Int -> NodeContext node -> Maybe (NodeContext node)
siblingAccessor 0   node = Just $ node
siblingAccessor idx node =
    case parents node of
      [] -> Nothing
      (n,i):ps ->
          let children = childrenOf n
              childrenCount = length children
              neoIndex = i + idx
          in if neoIndex < 0 || neoIndex >= childrenCount
                then Nothing
                else Just $ NodeContext
                        { parents = (n, neoIndex):ps
                        , this = children !! neoIndex
                        , rootRef = rootRef node
                        }


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

binArith :: (Int -> Int -> Int) -> ActionValue -> ActionValue -> ActionValue
binArith f (AInt a) (AInt b) = AInt $ f a b
binArith _ _ _ = ATypeError

valComp :: (Int -> Int -> Bool) -> ActionValue -> ActionValue -> ActionValue
valComp f (AInt a) (AInt b) = ABool $ f a b
valComp _ _ _ = ATypeError

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

data Elem node = ElemNode (NodeContext node)
               | ElemString String

dumpContent :: (GraphWalker node) => Elem node -> WebCrawler node ActionValue
dumpContent (ElemNode ns) =
  case attribOf "src" (this ns) >>= toRezPath of
    Nothing -> do
        textOutput $ valueOf (this ns)
        return (ABool True)
    Just r -> do
        dumpResourcePath (infoLog) $ (rootRef ns) <//> r
        return (ABool True)

dumpContent (ElemString str) =
    textOutput str >> return (ABool True)

-- | Evaluate embedded action in WebRexp
evalAction :: (GraphWalker node)
           => ActionExpr -> Elem node -> WebCrawler node ActionValue
evalAction (ActionExprs actions) e = evaluator actions
    where evaluator [] = return $ ABool True
          evaluator [x] = evalAction x e
          evaluator (x:xs) = do
              rez <- evalAction x e
              if isActionResultValid rez
              	 then evaluator xs
              	 else return rez


evalAction (CstI i) _ =
    debugLog "@ int" >>
    return (AInt i)
evalAction (CstS s) _ =
    debugLog "@ str" >>
    return (AString s)
evalAction OutputAction e =
    debugLog "@ ." >>
    dumpContent e

evalAction (ARef _) (ElemString _) = do
    debugLog "@ @..."
    return ATypeError

evalAction (ARef r) (ElemNode n) = do
    case attribOf r (this n) of
      Nothing ->
        debugLog "@ @..." >> return (ABool False)
      Just s -> do
          debugLog $ "@ @" ++ r ++ "=" ++ s
          return $ AString s

evalAction (BinOp OpAdd a b) e =
    debugLog "@ '+'" >>
    binArith (+) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpSub a b) e =
    debugLog "@ '-'" >>
    binArith (-) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpMul a b) e =
    debugLog "@ '*'" >>
    binArith (*) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpDiv a b) e =
    debugLog "@ '/'" >>
    binArith div <$> evalAction a e <*> evalAction b e

evalAction (BinOp OpLt a b) e =
    debugLog "@ '<'" >>
    valComp (<) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpLe a b) e =
    debugLog "@ '<='" >>
    valComp (<=) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpGt a b) e =
    debugLog "@ '>'" >>
    valComp (>) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpGe a b) e =
    debugLog "@ '>='" >>
    valComp (>=) <$> evalAction a e <*> evalAction b e

evalAction (BinOp OpEq a b) e =
    debugLog "@ '='" >>
    binComp <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpNe a b) e =
    debugLog "@ '/='" >>
    valNot <$> (binComp <$> evalAction a e <*> evalAction b e)
        where valNot (ABool f) = ABool $ not f
              valNot el = el

evalAction (BinOp OpAnd a b) e =
    debugLog "@ '&'" >>
    boolComp (&&) <$> evalAction a e <*> evalAction b e
evalAction (BinOp OpOr  a b) e =
    debugLog "@ '|'" >>
    boolComp (||) <$> evalAction a e <*> evalAction b e

