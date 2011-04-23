{-# LANGUAGE ScopedTypeVariables #-}
module Text.Webrexp.Eval
    (
    -- * Functions
    evalAction,
    evalWebRexpFor 
    ) where

import Control.Applicative
import Control.Monad
import Data.List

import Text.Webrexp.GraphWalker
import Text.Webrexp.Exprtypes
import Text.Webrexp.WebContext
import Text.Webrexp.Eval.Action
import Text.Webrexp.Log

-- | Given a node search for valid children, check for their
-- validity against the requirement.
searchRefIn :: (GraphWalker node rezPath)
            => Bool                         -- ^ Do we recurse?
            -> WebRef                       -- ^ Ref to find
            -> NodeContext node rezPath     -- ^ The root nood for the search
            -> WebCrawler node rezPath
                        [NodeContext node rezPath]   -- ^ The found nodes.
searchRefIn False Wildcard n = do
    children <- childrenOf $ this n
    return [ NodeContext {
        parents = (this n, idx) ^: parents n,
        this = sub,
        rootRef = rootRef n
     } | (sub, idx) <- zip children [0..]]

searchRefIn True Wildcard n = do
    subs <- descendants $ this n
    return [ NodeContext {
        parents = subP ^+ parents n,
        this = sub,
        rootRef = rootRef n
    }  | (sub, subP) <- subs ]

searchRefIn True (Elem s) n = do
    subs <- findNamed s $ this n
    return [ NodeContext {
        parents = subP ^+ parents n,
        this = sub,
        rootRef = rootRef n
    }  | (sub, subP) <- subs ]

searchRefIn False (Elem s) n = do
    subs <- searchRefIn False Wildcard n
    return [v | v <- subs, nameOf (this v) == Just s]

searchRefIn recurse (OfClass r s) n = do
    subs <- searchRefIn recurse r n
    return [v | v <- subs, attribOf "class" (this v) == Just s]
searchRefIn recurse (Attrib  r s) n = do
    subs <- searchRefIn recurse r n
    return [v | v <- subs, attribOf s (this v) /= Nothing]
searchRefIn recurse (OfName  r s) n = do
    subs <- searchRefIn recurse r n
    return [v | v <- subs, attribOf "id" (this v) == Just s]

-- | Evaluate the leaf nodes of a webrexp, this way the code
-- can be shared between the Breadth first evaluator and the
-- Depth first one.
evalWebRexpFor :: (GraphWalker node rezPath)
               => WebRexp -> EvalState node rezPath
               -> WebCrawler node rezPath (Bool, [EvalState node rezPath])
evalWebRexpFor (Str str) _ = do
    debugLog "> '\"...\"'"
    return (True, [Text str])

evalWebRexpFor (Action action) e = do
    debugLog "> '[...]'"
    (rez, neoNode) <- evalAction action $ Just e
    dumpActionVal rez
    if isActionResultValid rez
       then case neoNode of
        Nothing -> return (True, [e])
        Just new -> return (True, [new])
       else return (False, [])

evalWebRexpFor (Unique bucket) e = do
    debugLog $ "> '!' (" ++ show bucket ++ ")"
    beenVisited <- visited e
    return (beenVisited, [e])
     where visited (Node n) = checkUnique . show $ rootRef n
           visited (Text s) = checkUnique s
           visited (Blob b) = checkUnique . show $ sourcePath b
           checkUnique s = do
               seen <- hasResourceBeenVisited bucket s
               unless seen
                    (setResourceVisited bucket s)
               return $ not seen

evalWebRexpFor (ConstrainedRef s action) e = do
    ref@(valid, lst) <- evalWebRexpFor (Ref s) e
    if not valid
      then return ref
      else do
          lst'  <- mapM (evalWebRexpFor $ Action action) lst
          return (any fst lst', concatMap snd lst')
            

evalWebRexpFor (DirectChild ref) (Node n) = do
    debugLog $ "> direct 'ref' : " ++ show ref
    subs <- searchRefIn False ref n
    let n' = map Node subs
    debugLog $ ">>> found ->" ++ show (length n')
    return (not $ null n', n')

evalWebRexpFor (DirectChild _) _ = return (False, [])

evalWebRexpFor (Ref ref) (Node n) = do
    debugLog $ "> 'ref' : " ++ show ref
    subs <- searchRefIn True ref n
    let n' = map Node subs
    debugLog $ ">>> found ->" ++ show (length n')
    return (not $ null n', n')

evalWebRexpFor (Ref _) _ = return (False, [])

evalWebRexpFor DiggLink e = do
    debugLog "> '>>'"
    e' <- diggLinks e
    return (not $ null e', e')

evalWebRexpFor NextSibling e = do
  debugLog "> '+'"
  subs <- siblingAccessor 1 e 
  case subs of
    Nothing -> return (False, [])
    Just e' -> return (True, [e'])

evalWebRexpFor PreviousSibling e = do
  debugLog "> '~'"
  subs <- siblingAccessor (-1) e
  case subs of
    Nothing -> return (False, [])
    Just e' -> return (True, [e'])

evalWebRexpFor Parent (Node e) = do
  debugLog "> '<'"
  case parents e of
      ImmutableHistory [] -> return (False, [])
      MutableHistory   [] -> return (False, [])
      ImmutableHistory ((n,_):ps) ->
          return (True, [Node $ e { parents = ImmutableHistory ps, this = n }])
      MutableHistory (n:ps) ->
          return (True, [Node $ e { parents = MutableHistory ps, this = n }])

evalWebRexpFor Parent _ = return (False, [])

-- Exaustive definition to get better warning from compiler in case
-- of modification
evalWebRexpFor (Branch _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (Unions _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (List _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (Star _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (Repeat _ _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (Alternative _ _) _ =
     error "evalWebRexpFor - non terminal in terminal function."
evalWebRexpFor (Range _ _) _ =
     error "evalWebRexpFor - non terminal in terminal function."

downLinks :: (GraphWalker node rezPath)
          => rezPath
          -> WebCrawler node rezPath [EvalState node rezPath]
downLinks path = do
    loggers <- prepareLogger
    down <- accessGraph loggers path
    case down of
         AccessError -> return []
         DataBlob u b -> return [Blob $ BinBlob u b]
         Result u n -> return [Node 
                    NodeContext { parents = hist
                                , rootRef = u
                                , this = n }]
                     where hist = if isHistoryMutable n
                                    then MutableHistory []
                                    else ImmutableHistory []

--------------------------------------------------
----            Helper functions
--------------------------------------------------
diggLinks :: (GraphWalker node rezPath)
          => EvalState node rezPath
          -> WebCrawler node rezPath [EvalState node rezPath]
diggLinks (Node n) =
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
                -> WebCrawler node rezPath
                             (Maybe (EvalState node rezPath))
siblingAccessor 0   node@(Node _) = return $ Just node
siblingAccessor idx (Node node)=
    case parents node of
      ImmutableHistory [] -> return Nothing
      MutableHistory [] -> return Nothing
      ImmutableHistory ((n,i):ps) -> do
          children <- childrenOf n
          let childrenCount = length children
              neoIndex = i + idx
          if neoIndex < 0 || neoIndex >= childrenCount
                then return Nothing
                else return . Just . Node $ NodeContext
                        { parents = ImmutableHistory $ (n, neoIndex):ps
                        , this = children !! neoIndex
                        , rootRef = rootRef node
                        }
      MutableHistory (n:_) -> do
          children <- childrenOf n
          let childrenCount = length children
          case elemIndex (this node) children of
            Nothing -> error "Sibling access - root file removed"
            Just i ->
                let neoIndex = i + idx
                in if neoIndex < 0 || neoIndex >= childrenCount
                    then return Nothing
                    else return . Just . Node $ node
                        { this = children !! neoIndex }
siblingAccessor _ _ = return Nothing

