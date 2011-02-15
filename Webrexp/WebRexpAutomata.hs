module Webrexp.WebRexpAutomata ( -- * Types
                                 Automata
                               , StateIndex

                               -- * Automata manipulation
                               , buildAutomata
                               , dumpAutomata

                               -- * Automata evaluation
                               , evalAutomataDFS
                               , evalAutomataBFS

                               , evalDepthFirst 
                               , evalBreadthFirst 
                               ) where

import Control.Monad
import Data.Array
import qualified Data.Array.Unboxed as U
import System.IO

import Webrexp.Log
import Webrexp.Eval
import Webrexp.GraphWalker
import Webrexp.WebContext
import Webrexp.Exprtypes

type AutomataSink = (Int, Int)

data AutomataAction =
      Push
    | PopPush
    | Pop
    | AutoTrue
    | AutoSimple WebRexp
    | Scatter (U.UArray Int Int)
    | Gather (U.UArray Int Int)
    deriving (Show)
    

data AutomataState = 
    -- | Action to perform, action on True
    -- action on False
    AutoState !AutomataAction !Int !Int

-- | The automata representing a WebRexp,
-- ready to be executed.
data Automata = Automata
    { autoStates :: Array Int AutomataState
    , beginState :: Int
    }

type StateListBuilder =
    [(Int, AutomataState)] -> [(Int, AutomataState)]

type FreeId = Int
type FirstState = Int


-- | Simply the index of the state in a table.
type StateIndex = Int

--------------------------------------------------
----            Automata building
--------------------------------------------------

nodeCount :: Automata -> Int
nodeCount = sizer . bounds . autoStates
    where sizer (low, high) = high - low + 1

-- | General function to translate a webrexp to an evaluation
-- automata.
buildAutomata :: WebRexp -> Automata
buildAutomata expr = Automata
  { beginState = 0
  , autoStates = array (0, lastFree - 1) $ start : end : sts []
  }
   where start = (0, AutoState Push begin begin)
         end = (1, AutoState Pop (-1) (-1))
         (lastFree, begin, sts) = toAutomata expr 2 (1, 1)


-- | Debug function dumping the automata in form of a
-- graphviz file. The file can be used with the \'dot\'
-- tool to produce a visualisation of the graph.
dumpAutomata :: String      -- ^ Text used as title for the automata.
             -> Handle      -- ^ Where the graphviz representation will be written.
             -> Automata    -- ^ Automata to dump
             -> IO ()
dumpAutomata label h auto = do
    hPutStrLn h $ "// begin:" ++ show (beginState auto)
                 ++ " count:" ++ show (nodeCount auto)
    hPutStrLn h "digraph debug {"
    hPutStrLn h $ "    graph [fontname=\"Helvetica\", root=\"i" ++ show (beginState auto) 
                        ++ "\" label=\"" ++ concatMap subster label ++ "\"]"
    mapM_ printInfo . assocs $ autoStates auto
    hPutStrLn h "}"
     where printInfo (idx, AutoState act@(Scatter arr) t f) = do
               let idxs = 'i' : show idx
               hPutStrLn h $ idxs ++ " [label=\"" ++ show idx
                            ++ " : Scatter\"," ++ shaper act ++ "];"
               dumpLink idxs t f
               dumpAllLinks idxs arr

           printInfo (idx, AutoState act@(Gather arr) t f) = do
               let idxs = 'i' : show idx
               hPutStrLn h $ idxs ++ " [label=\"" ++ show idx
                            ++ " : Gather\"," ++ shaper act ++ "];"
               dumpLink idxs t f
               dumpAllLinks idxs arr

           printInfo (idx, AutoState act t f) = do
               let idxs = 'i' : show idx
               hPutStrLn h $ idxs ++ " [label=\"" ++ show idx ++ " : " ++ cleanShow act 
                                  ++ "\"," ++ shaper act ++ "];"
               dumpLink idxs t f

           dumpLink idxs t f =
               if t == f && t >= 0
               	 then hPutStrLn h $ idxs ++ " -> i" ++ show t    
               	                    ++ "[label=\"t/f\"];"
                 else do
                   when (t >= 0)
                        (hPutStrLn h $ idxs ++ " -> i"
                                    ++ show t ++ "[label=\"t\"];")
                   when (f >= 0)
                        (hPutStrLn h $ idxs ++ " -> i"
                                   ++ show f ++ "[label=\"f\"];")

           cleanShow (AutoSimple DiggLink) = ">>"
           cleanShow (AutoSimple (Unique i)) = '!' : show i
           cleanShow (AutoSimple (Ref ref)) = "<" ++ prettyShowWebRef ref ++ ">"
           cleanShow (AutoSimple (Str str)) = "\\\"" ++ concatMap subster str ++ "\\\""
           cleanShow (AutoSimple (Action _)) = "[ ]"
           cleanShow (AutoSimple (ConstrainedRef ref _)) =
               "<" ++ prettyShowWebRef ref ++ "> []"
           cleanShow a = concatMap subster $ show a

           dumpAllLinks idx arr = mapM_ (\i ->
               hPutStrLn h $ idx ++ " -> i"
                            ++ show i ++ "[style=\"dotted\"]"
               ) $ U.elems arr

           shaper (AutoSimple _) = ""
           shaper _ = "shape=\"box\", color=\"yellow\", style=\"filled\""

           subster '"' = "\\\""
           subster '\n' = "\\n"
           subster '\r' = "\\r"
           subster '\\' = "\\\\"
           subster a = [a]

-- | Main transformation function.
-- Assume that each state has two output, one for
-- true and one for false, simplifying the design
-- of the function.
--
-- The idea is to be able to store the automata in an
-- array after the generation, hence the propagation
-- of different indexes.
toAutomata :: WebRexp       -- ^ Expression to be transformed into an automata
           -> StateIndex    -- ^ Last free index
           -> AutomataSink  -- ^ The input/output for the current automata
           -- | The first unused, the index of the beggining state
           -- of the converted webrexp, and finaly the list of states.
           -> (FreeId, FirstState, StateListBuilder) 
toAutomata (Unions lst) free (onTrue, onFalse) =
  (contentFree, scatterId, ([scatterState, gatherState] ++) . states)
    where scatterId = free
          gatherId = free + 1

          scatterState = (scatterId, AutoState (Scatter beginList) (head beginIndices) gatherId)
          gatherState = (gatherId, AutoState (Gather beginList) onTrue onFalse)

          beginList = U.listArray (0, length lst - 2) $ tail beginIndices

          transformExprs expr (new, beginIds, st) =
            let (freeId, first, newStates) =
                        toAutomata expr new (gatherId, gatherId)
            in (freeId, first : beginIds, newStates . st)

          (contentFree, beginIndices, states) =
              foldr transformExprs (gatherId + 1, [], id) lst

toAutomata (List lst) free (onTrue, onFalse) =
  foldr transformExprs (free, onTrue, id) lst
    where transformExprs expr (new, toTrue, states) =
           let (freeId, first, newStates) =
                    toAutomata expr new (toTrue, onFalse)
           in (freeId, first, newStates . states)

toAutomata (Branch []) _ _ =
    error "toAutomata - Empty Branch statement"
toAutomata (Branch (x:lst)) free (onTrue, onFalse) =
  (lastFree, firstSink
  ,firstPush . finalStates . listStates)
    where firstSink = free
          firstPush = ((firstSink, AutoState Push branchBegin branchBegin):)

          -- Code used for the last branch
          transformExprs expr (True, new, (toTrue, toFalse), states) =
              let (freeId, subBegin, newStates) =
                    toAutomata expr new (toTrue, toFalse)
                  stackChange = ((freeId, AutoState Pop subBegin toFalse):)
              in (False, freeId + 1, (freeId, freeId), stackChange . newStates . states)

          -- all except last and first
          transformExprs expr (_, new, (toTrue, toFalse), states) =
              let (freeId, subBegin, newStates) = toAutomata expr new (toTrue, toFalse)
                  stackChange = ((freeId, AutoState PopPush subBegin onFalse):)
              in (False, freeId + 1, (freeId, freeId), stackChange . newStates . states)

          (_, listFree, (listBegin, lastFalseSink), listStates) =
              foldr transformExprs (True, free + 1, (onTrue, onFalse), id) lst

          (lastFree, branchBegin, finalStates) =
              toAutomata x listFree (listBegin, lastFalseSink)

-- For repetition, we simply create a list replicated n times
-- and convert it to an automata
toAutomata (Repeat (RepeatTimes n) expr) free sinks =
    toAutomata (List $ replicate n expr) free sinks
-- For a minimum occurence count, we replicate the minimum
-- and star the maximum.
toAutomata (Repeat (RepeatAtLeast n) expr) free sinks =
    toAutomata (List [List $ replicate n expr
                     ,Star expr]) free sinks
-- My favorite...
toAutomata (Repeat (RepeatBetween n m) expr) free (onTrue, onFalse) =
  (minFree, minBegin, states . middleStates)
    where (minFree, minBegin, states) =
              toAutomata (List $ replicate n expr)
                         middleFree (middleBegin, onFalse)

          (middleFree, middleBegin, middleStates) =
              toAutomata (List $ replicate (m - n) expr)
                         free
                         (onTrue, onTrue)

toAutomata (Star expr) free (onTrue, _onFalse) =
  (lastFree, beginning, (trueState :) . states)
    where (lastFree, beginning, states) =
              toAutomata expr (free + 1) (beginning, free)
          trueState =
              (free, AutoState AutoTrue onTrue onTrue)

toAutomata (Alternative a b) free (onTrue, onFalse) =
  (aFree, abeg, aStates . bStates)
    where (bFree, bbeg, bStates) = toAutomata b free (onTrue, onFalse)
          (aFree, abeg, aStates) = toAutomata a bFree (onTrue, bbeg)

-- Like other places in the software, we explicitely list all possibilites
-- to let the compiler help us when refactoring/modifying the types by
-- emitting a warning.
toAutomata rest@(Unique _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(Str _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(Action _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(Range _ _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(Ref _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(DirectChild _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@(ConstrainedRef _ _)  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@DiggLink  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@NextSibling  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@PreviousSibling  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))
toAutomata rest@Parent  free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))

--------------------------------------------------
----            DFS
--------------------------------------------------

-- | Simple function performing a depth first evaluation
evalDepthFirst :: (GraphWalker node rezPath)
               => WebRexp -> WebCrawler node rezPath Bool
evalDepthFirst expr = do
    debugLog $ "[Depth first, starting at " ++ show begin ++ "]"
    setBucketCount count rangeCount
    evalAutomataDFS auto (beginState auto) True (Text "")
        where auto = buildAutomata neorexp
              begin = beginState auto
              (count, rangeCount, neorexp) = assignWebrexpIndices expr

-- | Main Evaluation function
evalAutomataDFS :: (GraphWalker node rezPath)
             => Automata                 -- ^ Automata to evaluate
             -> StateIndex               -- ^ State to evaluate
             -> Bool                     -- ^ Are we coming from a true link.
             -> EvalState node rezPath   -- ^ Current evaluated element
             -> WebCrawler node rezPath Bool
evalAutomataDFS auto i fromTrue e
    | i < 0 = return fromTrue
    | otherwise = do
        debugLog $ "] State " ++ show i
        evalStateDFS auto 
                    (autoStates auto ! i) fromTrue e

-- | Pop a record and start evaluation for him.
scheduleNextElement :: (GraphWalker node rezPath)
                    => Automata -> WebCrawler node rezPath Bool
scheduleNextElement a = do
    (e, idx) <- popLastRecord
    evalAutomataDFS a idx True e


-- | Evaluation function for an element.
evalStateDFS :: (GraphWalker node rezPath)
                  => Automata       -- ^ Evaluation automata
                  -> AutomataState  -- ^ Current state in the automata
                  -> Bool           -- ^ If we are coming from a True link or a False one
                  -> EvalState node rezPath -- ^ Currently evaluated element
                  -> WebCrawler node rezPath Bool
evalStateDFS a (AutoState (Gather _) onTrue onFalse) valid e = do
    debugLog $ "> Gather"
    evalAutomataDFS a (if valid then onTrue else onFalse) valid e

evalStateDFS a (AutoState (Scatter idxs) onTrue _) True e = do
    debugLog $ "> Scattering " ++ show (U.bounds idxs)
    mapM_ (\idx -> do debugLog $ "  > Scatter " ++ show idx
                      recordNode (e, idx)) . reverse $ U.elems idxs
    addToBranchContext (1 + snd (U.bounds idxs)) 0
    evalAutomataDFS a onTrue True e

evalStateDFS a (AutoState (Scatter _) _ onFalse) False e = do
    debugLog "> Scatter FALSE"
    evalAutomataDFS a onFalse False e

evalStateDFS a (AutoState Push onTrue _) _ e = do
    debugLog "> Push"
    pushToBranchContext (e, 1, 0)
    evalAutomataDFS a onTrue True e
    
evalStateDFS a (AutoState AutoTrue onTrue _) _ e = do
    debugLog "> True"
    evalAutomataDFS a onTrue True e

evalStateDFS a (AutoState Pop onTrue onFalse) fromValid _ = do
    debugLog "> Pop"
    (e', left, valid) <- popBranchContext
    let validAdd = if fromValid then 1 else 0
        neoValid = valid + validAdd
        neoCount = left - 1
    if neoCount == 0
       then let nextState = if neoValid > 0 then onTrue else onFalse
            in evalAutomataDFS a nextState (neoValid > 0) e'

       else do pushToBranchContext (e', left - 1, neoValid)
       	       scheduleNextElement a

    

evalStateDFS a (AutoState PopPush onTrue onFalse) fromValid _ = do
    debugLog "> PopPush"
    (e', left, valid) <- popBranchContext
    let validAdd = if fromValid then 1 else 0
        neoValid = valid + validAdd
        neoCount = left - 1
    if neoCount == 0
       then if neoValid > 0
               then do pushToBranchContext (e', 1, 0)
                       evalAutomataDFS a onTrue True  e'        
               -- we don't push if we failed.
               else evalAutomataDFS a onFalse False e'

       else do pushToBranchContext (e', left - 1, neoValid)
       	       scheduleNextElement a

evalStateDFS a (AutoState (AutoSimple (Range bucket ranges)) 
                                onTrue onFalse) _ e = do
    count <- incrementGetRangeCounter bucket
    debugLog $ show ranges ++ " - [" ++ show bucket ++  "]" ++ show count ++ "  :"
                ++ show (count `isInNodeRange` ranges)
    if count `isInNodeRange` ranges
       then evalAutomataDFS a onTrue True e
       else evalAutomataDFS a onFalse False e

evalStateDFS a (AutoState (AutoSimple rexp) onTrue onFalse) _ e = do
    (valid, subList) <- evalWebRexpFor rexp e
    let nextState = if valid then onTrue else onFalse
    case subList of
      [] -> evalAutomataDFS a onFalse False e
      (x:xs) -> do
          mapM_ (recordNode . flip (,) nextState) $ reverse xs
          addToBranchContext (length xs) 0
          evalAutomataDFS a nextState valid x


--------------------------------------------------
----            BFS evaluation
--------------------------------------------------
evalBreadthFirst :: (GraphWalker node rezPath)
                 => WebRexp -> WebCrawler node rezPath Bool
evalBreadthFirst expr = do
    debugLog $ "[Breadth first, starting at " ++ show begin ++ "]"
    setBucketCount count 0
    evalAutomataBFS auto (beginState auto) True [Text ""]
        where auto = buildAutomata neorexp
              begin = beginState auto
              (count, _, neorexp) = assignWebrexpIndices expr

evalAutomataBFS :: (GraphWalker node rezPath)
                => Automata                 -- ^ Automata to evaluate
                -> StateIndex               -- ^ State to evaluate
                -> Bool                     -- ^ Are we coming from a true link.
                -> [EvalState node rezPath] -- ^ Current evaluated element
                -> WebCrawler node rezPath Bool
evalAutomataBFS auto i fromTrue e
    | i < 0 = return fromTrue
    | otherwise = do
        debugLog $ "] State " ++ show i
        evalStateBFS auto 
                    (autoStates auto ! i) fromTrue e


-- | Main evaluation function for BFS evaluation.
evalStateBFS :: (GraphWalker node rezPath)
             => Automata       -- ^ Evaluation automata
             -> AutomataState  -- ^ Current state in the automata
             -> Bool           -- ^ If we are coming from a True link or a False one
             -> [EvalState node rezPath]   -- ^ Currently evaluated elements
             -> WebCrawler node rezPath Bool

evalStateBFS a (AutoState (Gather idxs) onTrue onFalse) valid e = do
    debugLog $ "> Gather"
    (st, idx) <- popAccumulation
    (beginSt, _) <- popAccumulation
    let (_, maxId) = U.bounds idxs
        toConcat = if valid then e else []
    if idx <= maxId
       then do
           accumulateCurrentState (beginSt, 0)
           accumulateCurrentState (st ++ toConcat, idx + 1)
           evalAutomataBFS a (idxs U.! idx) True beginSt

       else do
       	   let finalSt = st ++ toConcat
       	       finalValid = not $ null finalSt
       	   debugLog $ "    > gathered " ++ show (length finalSt)
       	   evalAutomataBFS a (if finalValid then onTrue else onFalse)
       	                     finalValid finalSt

evalStateBFS a (AutoState (Scatter _) onTrue _) True e = do
    debugLog $ "> Scatter"
    accumulateCurrentState (e, 0)
    accumulateCurrentState ([], 0)
    evalAutomataBFS a onTrue True e

evalStateBFS a (AutoState (Scatter _) _ onFalse) False e = do
    debugLog $ "> Scatter FALSE"
    evalAutomataBFS a onFalse False e

evalStateBFS a (AutoState Push onTrue _) True e = do
    debugLog "> Push"
    pushCurrentState e
    evalAutomataBFS a onTrue True e
    
evalStateBFS a (AutoState Push _ onFalse) False e = do
    debugLog "> Push"
    pushCurrentState e
    evalAutomataBFS a onFalse False e

evalStateBFS a (AutoState AutoTrue onTrue _) _ e = do
    debugLog "> True"
    evalAutomataBFS a onTrue True e

evalStateBFS a (AutoState Pop onTrue _) True _ = do
    debugLog "> Pop"
    newList <- popCurrentState
    evalAutomataBFS a onTrue True newList

evalStateBFS a (AutoState Pop _ onFalse) False _ = do
    debugLog "> Pop"
    newList <- popCurrentState
    evalAutomataBFS a onFalse False newList

evalStateBFS a (AutoState PopPush _ onFalse) False _ = do
    debugLog "> PushPop"
    newList <- popCurrentState
    pushCurrentState newList
    evalAutomataBFS a onFalse False newList

evalStateBFS a (AutoState PopPush onTrue _) True _ = do
    debugLog "> PopPush"
    newList <- popCurrentState
    pushCurrentState newList
    evalAutomataBFS a onTrue True newList

evalStateBFS a (AutoState (AutoSimple (Range _ ranges)) 
                                onTrue onFalse) _ e = do
    let nodes = filterNodes ranges e
        nextState = if null nodes then onFalse else onTrue
    evalAutomataBFS a nextState (not $ null nodes) nodes

evalStateBFS a (AutoState (AutoSimple rexp) onTrue onFalse) _ e = do
    e' <- mapM (evalWebRexpFor rexp) e
    let valids = concat [ lst | (v, lst) <- e', v ]
        nextState = if null valids then onFalse else onTrue
    evalAutomataBFS a nextState (not $ null valids) valids

-- | For the current state, filter the value to keep
-- only the values which are included in the node
-- range.
filterNodes :: [NodeRange] -> [a] -> [a]
filterNodes ranges = filtered
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
