module Webrexp.WebRexpAutomata ( -- * Types
                                 Automata
                               , StateIndex

                               -- * Automata manipulation
                               , buildAutomata
                               , dumpAutomata

                               -- * Automata evaluation
                               , evalAutomata
                               , evalDepthFirst 
                               ) where

import Control.Monad
import Data.Array
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

nodeCount :: Automata -> Int
nodeCount = sizer . bounds . autoStates
    where sizer (low, high) = high - low + 1


-- | Simple function performing a depth first evaluation
evalDepthFirst :: (GraphWalker node rezPath)
               => WebRexp -> WebCrawler node rezPath Bool
evalDepthFirst expr = do
    debugLog $ "[Depth first, starting at " ++ show begin ++ "]"
    evalAutomata auto (beginState auto) True (Text "")
        where auto = buildAutomata expr
              begin = beginState auto



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
    hPutStrLn h $ "    graph [root=\"i" ++ show (beginState auto) 
                        ++ "\" label=\"" ++ concatMap subster label ++ "\"]"
    mapM_ printInfo . assocs $ autoStates auto
    hPutStrLn h "}"
     where printInfo (idx, AutoState act t f) = do
               let idxs = "i" ++ show idx
               hPutStrLn h $ idxs ++ " [label=\"" ++ cleanShow act 
                                  ++ "\"," ++ shaper act ++ "];"
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

           cleanShow (AutoSimple DiggLink) = ">"
           cleanShow (AutoSimple (Unique i)) = "!" ++ show i
           cleanShow (AutoSimple (Ref ref)) = "<" ++ prettyShowWebRef ref ++ ">"
           cleanShow (AutoSimple (Str str)) = "\\\"" ++ concatMap subster str ++ "\\\""
           cleanShow (AutoSimple (Action _)) = "{ }"
           cleanShow a = concatMap subster $ show a

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
toAutomata (List lst) free (onTrue, onFalse) =
  foldr transformExprs (free, onTrue, id) lst
    where transformExprs expr (new, toTrue, states) =
           let (freeId, first, newStates) =
                    toAutomata expr new (toTrue, onFalse)
           in (freeId, first, newStates . states)

    
toAutomata (Branch [x]) free (onTrue, onFalse) =
  (lastFree, free, ([pushSt, popSt] ++) . states)
    where (lastFree, begin, states) = toAutomata x (free + 2) (free + 1, free + 1)
          pushSt = (free, AutoState Push begin begin)
          popSt = (free + 1, AutoState Pop onTrue onFalse)

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

toAutomata (Plus expr) free sinks =
    toAutomata (List [expr, Star expr]) free sinks

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

toAutomata (Range _) _free (_onTrue, _onFalse) =
    error "Range - not compilable yet"

toAutomata rest free (onTrue, onFalse) =
    (free + 1, free, ((free, AutoState (AutoSimple rest) onTrue onFalse):))

-- | Main Evaluation function
evalAutomata :: (GraphWalker node rezPath)
             => Automata                 -- ^ Automata to evaluate
             -> StateIndex               -- ^ State to evaluate
             -> Bool                     -- ^ Are we coming from a true link.
             -> EvalState node rezPath   -- ^ Current evaluated element
             -> WebCrawler node rezPath Bool
evalAutomata auto i fromTrue e
    | i < 0 = return fromTrue
    | otherwise = evalAutomataState auto 
                    (autoStates auto ! i) fromTrue e

-- | Pop a record and start evaluation for him.
scheduleNextElement :: (GraphWalker node rezPath)
                    => Automata -> WebCrawler node rezPath Bool
scheduleNextElement a = do
    (e, idx) <- popLastRecord
    evalAutomata a idx True e


-- | Evaluation function for an element.
evalAutomataState :: (GraphWalker node rezPath)
                  => Automata       -- ^ Evaluation automata
                  -> AutomataState  -- ^ Current state in the automata
                  -> Bool           -- ^ If we are coming from a True link or a False one
                  -> EvalState node rezPath -- ^ Currently evaluated element
                  -> WebCrawler node rezPath Bool
evalAutomataState a (AutoState Push onTrue _) _ e = do
    debugLog "> Push"
    pushToBranchContext (e, 1, 0)
    evalAutomata a onTrue True e
    
evalAutomataState a (AutoState AutoTrue onTrue _) _ e = do
    debugLog "> True"
    evalAutomata a onTrue True e

evalAutomataState a (AutoState Pop onTrue onFalse) fromValid _ = do
    debugLog "> Pop"
    (e', left, valid) <- popBranchContext
    let validAdd = if fromValid then 1 else 0
        neoValid = valid + validAdd
        neoCount = left - 1
    if neoCount == 0
       then let nextState = if neoValid > 0 then onTrue else onFalse
            in evalAutomata a nextState (neoValid > 0) e'

       else do pushToBranchContext (e', left - 1, neoValid)
       	       scheduleNextElement a

    

evalAutomataState a (AutoState PopPush onTrue onFalse) fromValid _ = do
    debugLog "> PopPush"
    (e', left, valid) <- popBranchContext
    let validAdd = if fromValid then 1 else 0
        neoValid = valid + validAdd
        neoCount = left - 1
    if neoCount == 0
       then if neoValid > 0
               then do pushToBranchContext (e', 1, 0)
                       evalAutomata a onTrue True  e'        
               -- we don't push if we failed.
               else evalAutomata a onFalse False e'

       else do pushToBranchContext (e', left - 1, neoValid)
       	       scheduleNextElement a


evalAutomataState a (AutoState (AutoSimple rexp) onTrue onFalse) _ e = do
    (valid, subList) <- evalWebRexpFor rexp e
    let nextState = if valid then onTrue else onFalse
    case subList of
      [] -> evalAutomata a onFalse False (Text "")
      (x:xs) -> do
          mapM_ (recordNode . flip (,) nextState) xs
          addToBranchContext (length xs) 0
          evalAutomata a nextState valid x

