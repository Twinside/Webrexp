module Webrexp.WebRexpAutomata where

import Data.Array
import System.IO

import Webrexp.Exprtypes

type AutomataSink = (Int, Int)

data AutomataAction =
      AutoStr String
    | Push
    | PopPush
    | Pop
    | Drop

    | AutoTrue
    | AutoFalse
    | AutoUnique Int
    | AutoString String
    | AutoAction ActionExpr
    | AutoRef    WebRef
    | AutoDigg
    | AutoNextSibling 
    | AutoPreviousSibling 
    | AutoParent 

    | AutoEnd
    deriving (Show)
    

data AutomataState = 
    -- | Action to perform, action on True
    -- action on False
    AutoState !AutomataAction !Int !Int

data Automata = Automata
    { autoStates :: Array Int AutomataState
    , nodeCount :: Int
    , beginState :: Int
    }

type StateListBuilder =
    [(Int, AutomataState)] -> [(Int, AutomataState)]

type FreeId = Int
type FirstState = Int

buildAutomata :: WebRexp -> Automata
buildAutomata expr = Automata
  { beginState = begin
  , nodeCount = lastFree
  , autoStates = array (0, lastFree - 1) $ end : sts []
  }
   where end = (0, AutoState AutoEnd (-1) (-1))
         (lastFree, begin, sts) = toAutomata expr 1 (0, 0)

dumpAutomata :: Handle -> Automata -> IO ()
dumpAutomata h auto = do
    hPutStrLn h $ "// begin:" ++ show (beginState auto)
                 ++ " count:" ++ show (nodeCount auto)
    hPutStrLn h "digraph debug {"
    mapM_ printInfo . assocs $ autoStates auto
    hPutStrLn h "}"
     where printInfo (idx, AutoState act t f) = do
               let idxs = "i" ++ show idx
               hPutStrLn h $ idxs ++ " [label=\"" ++ cleanShow act ++ "\"];"
               hPutStrLn h $ idxs ++ " -> i" ++ show t ++ "[label=\"t\"];"
               hPutStrLn h $ idxs ++ " -> i" ++ show f ++ "[label=\"f\"];"

           cleanShow (AutoRef ref) = "<" ++ prettyShowWebRef ref ++ ">"
           cleanShow (AutoString str) = "\\\"" ++ concatMap subster str ++ "\\\""
           cleanShow (AutoAction _) = "{ }"
           cleanShow a = concatMap subster $ show a
           subster '"' = "\\\""
           subster a = [a]

simpleState :: (Int, AutomataState) -> (FreeId, FirstState, StateListBuilder)
simpleState (idx, st) = (idx + 1, idx, ((idx, st):))

toAutomata :: WebRexp       -- ^ Expression to be transformed into an automata
           -> Int           -- ^ Last free index
           -> AutomataSink  -- ^ The input/output for the current automata
           -> (FreeId, FirstState, StateListBuilder)
toAutomata (List lst) free (onTrue, onFalse) =
  foldr transformExprs (free, onTrue, id) lst
    where transformExprs expr (new, toTrue, states) =
           let (freeId, first, newStates) =
                    toAutomata expr new (toTrue, onFalse)
           in (freeId, first, newStates . states)

    
toAutomata (Branch lst) free (onTrue, onFalse) =
  (lastFree, firstSink
  ,([(firstSink, AutoState Push branchBegin branchBegin)
    ,(falseSink, AutoState Pop  onFalse     onFalse)
    ,(trueSink,  AutoState Drop onTrue      onTrue)] ++) . finalStates)
    where firstSink = free
          falseSink = free + 1
          trueSink = free + 2
          transformExprs expr (new, toTrue, states) =
              let (freeId, subBegin, newStates) = toAutomata expr new (toTrue, falseSink)
                  stackChange = ((freeId, AutoState PopPush subBegin subBegin):)
              in (freeId + 1, freeId, stackChange . newStates . states)
          (lastFree, branchBegin, finalStates) =
              foldr transformExprs (free + 3, trueSink, id) lst

toAutomata (Plus expr) free sinks =
    toAutomata (List [expr, Star expr]) free sinks

toAutomata (Star expr) free (onTrue, _onFalse) =
  (lastFree, trueSink
  ,([(trueSink, AutoState AutoTrue beginning beginning)
    ,(falseSink, AutoState AutoTrue onTrue    onTrue)
    ]++) . states)
    where trueSink = free
          falseSink = free + 1
          (lastFree, beginning, states) =
              toAutomata expr (free + 2) (trueSink, falseSink)

toAutomata (Alternative a b) free (onTrue, onFalse) =
  (aFree, abeg, aStates . bStates)
    where (bFree, bbeg, bStates) = toAutomata b free (onTrue, onFalse)
          (aFree, abeg, aStates) = toAutomata a bFree (onTrue, bbeg)

toAutomata (Unique i) free (onTrue, onFalse) =
    simpleState (free, AutoState (AutoUnique i) onTrue onFalse)
toAutomata (Str str) free (onTrue, onFalse) =
    simpleState (free, AutoState (AutoString str) onTrue onFalse)
toAutomata (Action act) free (onTrue, onFalse) =
    simpleState (free, AutoState (AutoAction act) onTrue onFalse)
toAutomata (Ref ref) free (onTrue, onFalse) =
    simpleState (free, AutoState (AutoRef ref) onTrue onFalse)
toAutomata (DiggLink) free (onTrue, onFalse) =
    simpleState (free, AutoState AutoDigg onTrue onFalse)
toAutomata (NextSibling) free (onTrue, onFalse) =
    simpleState (free, AutoState AutoNextSibling onTrue onFalse)
toAutomata (PreviousSibling) free (onTrue, onFalse) =
    simpleState (free, AutoState AutoPreviousSibling onTrue onFalse)
toAutomata (Parent) free (onTrue, onFalse) =
    simpleState (free, AutoState AutoParent onTrue onFalse)

toAutomata (Range _) _free (_onTrue, _onFalse) =
    error "Range - not compilable yet"

