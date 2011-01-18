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


-- | Evaluate an expression while it's still valid.
-- @param@ isTail Tell if the list is from a tail node
evalListTillFalse :: (GraphWalker node)
                  => Bool -> [WebRexp] -> WebCrawler node Bool
evalListTillFalse = applyFunTillFalse evalList

evalTillFalse :: (GraphWalker node)
              => Bool -> WebRexp -> WebCrawler node Bool
evalTillFalse = applyFunTillFalse evalWebRexp

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

evalWebRexp _ DiggLink =
    return False
evalWebRexp _ (Unique _subs) = -- WebRexp
    return False

evalWebRexp _ (Str str) = do
    setEvalState $ Strings [str]
    return True

evalWebRexp _ (Action _subs) = -- ActionExpr
    return False
evalWebRexp _ (Range _subs) = -- [NodeRange]
    return False
evalWebRexp _ (Ref _subs) = -- WebRef
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

