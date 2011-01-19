module Webrexp.Eval where

import Data.List

import Webrexp.GraphWalker
import Webrexp.Exprtypes
import Webrexp.WebContext

isVerifiedIn :: (a -> Bool) -> [a] -> Bool
isVerifiedIn f lst = case find f lst of
                        Nothing -> False
                        Just _ -> True

evalList :: (GraphWalker node)
         => Bool -> [WebRexp] -> WebCrawler node Bool
evalList _ [] = return True
evalList isTail [x] = evalWebRexp isTail x
evalList isTail (x:xs) = do
    valid <- evalWebRexp False x
    if valid
       then evalList isTail xs
       else return False

evalBranches :: (GraphWalker node) => Bool -> [WebRexp] -> WebCrawler node Bool
evalBranches _ [] = return True
evalBranches isTail [x] = do
    popCurrentState
    pushCurrentState 
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
             else return False

filterNodes :: [NodeRange] -> WebCrawler node Bool
filterNodes ranges = getEvalState >>= \state ->
    case state of
         None -> return False
         Strings s -> setEvalState (Strings $ filtered s)
                   >> return True
         Nodes n -> setEvalState (Nodes $ filtered n)
                 >> return True
         Blob b -> setEvalState (Blob $ filtered b)
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


-- | Evaluate an expression while it's still valid.
-- @param@ isTail Tell if the list is from a tail node
evalListTillFalse :: (GraphWalker node)
                  => Bool -> [WebRexp] -> WebCrawler node Bool
evalListTillFalse = applyFunTillFalse evalList

-- | repeatidly eval the webrexp until a false is returned.
evalTillFalse :: (GraphWalker node)
              => Bool -> WebRexp -> WebCrawler node Bool
evalTillFalse = applyFunTillFalse evalWebRexp

evalAction :: (GraphWalker node)
           => ActionExpr -> WebCrawler node Bool
evalAction _ = return False

searchRefIn :: (GraphWalker node)
            => WebRef -> [NodeContext node] -> [NodeContext node]
searchRefIn ref = concatMap (searchRef ref)
  where searchRef (Elem s) n =
            [ NodeContext {
                parents = subP ++ parents n,
                this = sub
              }  | (sub, subP) <- findNamed (this n) s]
        searchRef (OfClass r s) n = 
            [n | v <- searchRef r n, attribOf (this v) "class" == Just s]
        searchRef (Attrib  r s) n =
            [n | v <- searchRef r n, attribOf (this v) s /= Nothing]
        searchRef (OfName  r s) n =
            [n | v <- searchRef r n, attribOf (this v) "name" == Just s]

-- | Evaluate an expression, the boolean is here to propagate
-- the idea of 'tail' call, if we are at the tail of the expression
-- we can discard some elements safely and thus reduce memory
-- usage (which can be important)
evalWebRexp :: (GraphWalker node) => Bool -> WebRexp -> WebCrawler node Bool
evalWebRexp isTail (Branch subs) = -- [WebRexp]
    pushCurrentState >> evalBranches isTail subs

evalWebRexp isTail (List subs) =
    evalList isTail subs

evalWebRexp isTail (Star subs) =
    evalTillFalse isTail subs

evalWebRexp isTail (Plus subs) = do
    once <- evalWebRexp isTail subs
    if once then do _ <- evalTillFalse isTail subs
                    return True
            else return False

evalWebRexp _ (Str str) = do
    setEvalState $ Strings [str]
    return True

evalWebRexp _ (Action action) =
    evalAction action
    
evalWebRexp _ (Unique _subs) = -- WebRexp
    error "Unimplemented - Unique (webrexp)"
    -- return False

evalWebRexp _ (Ref ref) = getEvalState >>= \st ->
    case st of
         Nodes ns -> do
             setEvalState . Nodes $ searchRefIn ref ns
             hasNodeLeft
         _ -> do setEvalState None
                 return False

evalWebRexp _ (Range subs) = do
    _ <- filterNodes subs
    return True

evalWebRexp _ DiggLink =
    return False

evalWebRexp _ NextSibling = do
  mapCurrentNodes $ siblingAccessor 1
  hasNodeLeft

evalWebRexp _ PreviousSibling = do
  mapCurrentNodes $ siblingAccessor (-1)
  hasNodeLeft

evalWebRexp _ Parent =
  mapCurrentNodes parentExtractor >> hasNodeLeft
    where parentExtractor node = 
           case parents node of
             []       -> Nothing
             (n,_):ps ->
              Just $ NodeContext { parents = ps
                                 , this = n }


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
                        }

