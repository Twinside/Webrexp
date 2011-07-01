{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Datatypes used to describe webrexps, and some helper functions.
module Text.Webrexp.Exprtypes
    ( 
    -- * Types
      WebRef (..)
    , NodeRange (..)
    , Op (..)
    , ActionExpr (..)
    , WebRexp (..)
    , RepeatCount  (..)
    , BuiltinFunc (..)
    -- * Functions
    -- ** Transformations
    , simplifyNodeRanges 
    , foldWebRexp
    , assignWebrexpIndices 
    , prettyShowWebRef
    , packRefFiltering 
    -- ** Predicates
    , isInNodeRange
    , isOperatorBoolean
    , isActionPredicate
    ) where

import Data.List( sort, mapAccumR )
import Language.Haskell.TH.Syntax

-- | represent an element
data WebRef =
    -- | \'*\' Any subelement.
      Wildcard
    -- | ... Search for a named element.
    | Elem String
    -- | ... . ...  Check the value of the \'class\' attribute
    | OfClass WebRef String
    -- | \@... Check for the presence of an attribute
    | Attrib WebRef String
    -- | #...  Check the value of the \'id\' attribute
    | OfName WebRef String
    deriving Show

-- | Ranges to be able to filter nodes by position.
data NodeRange =
    -- | ...
      Index Int
    -- | min-max
    | Interval Int Int
    deriving (Eq, Show)


instance Ord NodeRange where
    compare (Index a) (Index b) = compare a b
    compare (Index a) (Interval b c)
        | a  < b = LT
        | a  > c = GT
        -- index is within the interval, put interval
        -- before
        | otherwise = GT
    compare (Interval a _) (Index c)
        | a < c = LT
        | a > c = GT
        | otherwise = LT -- favor interval before
    compare (Interval a b) (Interval c d) =
        case compare a c of
             EQ -> compare b d
             e -> e

simplifySortedNodeRanges :: [NodeRange] -> [NodeRange]
simplifySortedNodeRanges [] = []
simplifySortedNodeRanges [a] = [a]
simplifySortedNodeRanges (i@(Interval a b):idx@(Index c):xs)
    | a <= c && c <= b = simplifySortedNodeRanges (i:xs)
    | otherwise = i : simplifySortedNodeRanges (idx:xs)
simplifySortedNodeRanges (i1@(Index a):i2@(Index b):xs)
    | a == b = simplifySortedNodeRanges (i1:xs)
    | otherwise = i1 : simplifySortedNodeRanges (i2:xs)
simplifySortedNodeRanges (i1@(Index _):i2@(Interval _ _):xs) =
    i1 : simplifySortedNodeRanges (i2:xs)
simplifySortedNodeRanges (i1@(Interval a b):i2@(Interval c d):xs)
    | a <= c && c <= b = -- there is intersection, pick the union
        simplifySortedNodeRanges $ Interval a (max b d) : xs
    | otherwise = i1 : simplifySortedNodeRanges (i2:xs)

-- | This function is an helper function to simplify
-- the handling the node range. After simplification,
-- the ranges are sorted in ascending order and no
-- node range overlap.
simplifyNodeRanges :: [NodeRange] -> [NodeRange]
simplifyNodeRanges = simplifySortedNodeRanges . sort . map rangeRearranger
    where rangeRearranger i@(Index _) = i
          rangeRearranger i@(Interval a b)
            | a == b = Index a
            | a > b = Interval b a
            | otherwise = i

-- | Definitions of the operators available in
-- the actions of the webrexp.
data Op =
      OpAdd -- ^ '+'
    | OpSub -- ^ '-'
    | OpMul -- ^ '*'
    | OpDiv -- ^ 'div'
    | OpLt  -- ^ '<'
    | OpLe  -- ^ '<='
    | OpGt  -- ^ '>'
    | OpGe  -- ^ '>='
    | OpEq  -- ^ \'=\' in webrexp ('==' in Haskell)
    | OpNe  -- ^ \'!=\' ('/=' in Haskell)
    | OpAnd -- ^ \'&\' ('&&' in Haksell)
    | OpOr  -- ^ \'|\' ('||' in Haskell)
    | OpMatch -- ^ \'=~\' regexp matching

    | OpContain   -- ^ \'~=\' op contain, as the CSS3 operator.
    | OpBegin     -- ^ \'^=\' op beginning, as the CSS3 operator.
    | OpEnd       -- ^ \'$=\' op beginning, as the CSS3 operator.
    | OpSubstring -- ^ \'^=\' op beginning, as the CSS3 operator.
    | OpHyphenBegin -- ^ \'|=\' op beginning, as the CSS3 operator.

    | OpConcat -- ^ \':\' concatenate two strings
    deriving (Eq, Show, Enum)

-- | Type used to index built-in functions 
-- in actions.
data BuiltinFunc =
      BuiltinTrim
    | BuiltinSubsitute
    | BuiltinToNum
    | BuiltinToString
    | BuiltinFormat
    | BuiltinSystem
    deriving (Eq, Show, Enum)

-- | Represent an action Each production
-- of the grammar more or less map to a
-- data constructor of this type.
data ActionExpr =
    -- | { ... ; ... ; ... ; ... }
    -- A list of action to execute, each
    -- one must return a 'valid' value to
    -- continue the evaluation
      ActionExprs [ActionExpr]
    -- | Basic binary opertor application
    | BinOp Op ActionExpr ActionExpr

    -- | Find a value of a given attribute for
    -- the current element.
    | ARef String
    -- | An integer constant.
    | CstI Int

    -- | A string constant
    | CstS String

    -- | \'$\'... operator
    -- Used to put the action value back into
    -- the evaluation pipeline.
    | NodeReplace ActionExpr

    -- | the '.' action. Dump the content of
    -- the current element.
    | OutputAction

    -- | funcName(..., ...)
    | Call BuiltinFunc [ActionExpr]
    deriving (Eq, Show)

data RepeatCount =
      RepeatTimes Int
    | RepeatAtLeast Int
    | RepeatBetween Int Int
    deriving (Show)

-- | Type representation of web-regexp,
-- main type.
data WebRexp =
    -- | ( ... ; ... ; ... )
      Branch [WebRexp]
    -- | ( ... , ... , ... )
    | Unions [WebRexp]
    -- | ... ... (each action followed, no rollback)
    | List [WebRexp]
    -- | ... *
    | Star WebRexp
    -- | ... #{  }
    | Repeat RepeatCount WebRexp
    -- | \'|\' Represent two alternative path, if
    -- the first fail, the second one is taken
    | Alternative WebRexp WebRexp
    -- | \'!\'
    -- Possess an unique index to differentiate all the differents
    -- uniques. Negative value are considered invalid, all positive or
    -- null one are accepted.
    | Unique Int
    -- | \"...\" A string constant in the source expression.
    | Str String
    -- | \"{ ... }\"
    | Action ActionExpr
    -- | \'[ ... ]\' Filtering Range
    -- The Int is used as an index for a counter 
    -- in the DFS evaluator.
    | Range Int [NodeRange]
    -- | every tag/class name
    | Ref WebRef
    -- | Find children who are the different descendent of
    -- the current nodes.
    | DirectChild WebRef
    -- | This constructor is an optimisation, it
    -- combine an Ref followed by an action, where
    -- every action is a predicate. Help pruning
    -- quickly the evaluation tree in DFS evaluation.
    | ConstrainedRef WebRef ActionExpr

    -- | \'>>\' operator in the language, used
    -- to follow hyper link
    | DiggLink

    -- | \'+\' operator in the language, used
    -- to select the next sibling node.
    | NextSibling

    -- | \'~\' operator in the language, used
    -- to select the previous sibling node.
    | PreviousSibling

    -- | \'<\' operator in the language. 
    -- Select the parent node
    | Parent
    deriving Show

-- | Tell if an action operator return a boolean
-- operation. Useful to tell if an action is a
-- predicate. See 'isActionPredicate'
isOperatorBoolean :: Op -> Bool
isOperatorBoolean op = op `elem`
    [ OpLt, OpLe, OpGt, OpGe, OpEq, OpNe
    , OpAnd, OpOr, OpMatch
    
    -- All CSS 3 operators
    , OpContain, OpBegin, OpEnd, OpSubstring
    , OpHyphenBegin ]

-- | Tell if an action is a predicate and is only
-- used to filter nodes. Expression can be modified
-- with this information to help prunning as soon
-- as possible with the DFS evaluator.
isActionPredicate :: ActionExpr -> Bool
isActionPredicate (BinOp op _ _) = isOperatorBoolean op
isActionPredicate _ = False

-- | This function permit the rewriting of a wabrexp in a depth-first
-- fashion while carying out an accumulator.
foldWebRexp :: (a -> WebRexp -> (a, WebRexp)) -> a -> WebRexp -> (a, WebRexp)
foldWebRexp f acc (Repeat count sub) = f acc' $ Repeat count sub'
    where (acc', sub') = foldWebRexp f acc sub
foldWebRexp f acc (Alternative a b) = f acc'' $ Alternative a' b'
    where (acc', a') = foldWebRexp f acc a
          (acc'', b') = foldWebRexp f acc' b
foldWebRexp f acc (Unions subs) = f acc' $ Unions subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (Branch subs) = f acc' $ Branch subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (List subs) = f acc' $ List subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (Star sub) = f acc' $ Star sub'
    where (acc', sub') = foldWebRexp f acc sub
-- This part is exaustive to let the compiler emit a warning if
-- we modify the definition of the WebRexp type
foldWebRexp f acc e@(ConstrainedRef _ _) = f acc e
foldWebRexp f acc e@(DirectChild _) = f acc e
foldWebRexp f acc e@(Unique _) = f acc e
foldWebRexp f acc e@(Str _) = f acc e
foldWebRexp f acc e@(Action _) = f acc e
foldWebRexp f acc e@(Range _ _) = f acc e
foldWebRexp f acc e@(Ref _) = f acc e
foldWebRexp f acc e@DiggLink = f acc e
foldWebRexp f acc e@NextSibling = f acc e
foldWebRexp f acc e@PreviousSibling = f acc e
foldWebRexp f acc e@Parent = f acc e

-- | Preparation function for webrexp, assign all indices
-- used for evaluation as an automata.
assignWebrexpIndices :: WebRexp -> (Int, Int, WebRexp)
assignWebrexpIndices expr = (uniqueCount, rangeCount, packRefFiltering fexpr)
    where (uniqueCount, expr') = setUniqueIndices expr
          (rangeCount, fexpr) = setRangeIndices expr'

packRefFiltering :: WebRexp -> WebRexp
packRefFiltering = snd . foldWebRexp packer ()
  where packer () (List lst) = ((), List $ refActionFind lst)
        packer () a = ((), a)

        refActionFind [] = []
        refActionFind (Ref a: Action act: xs) =
            case actionSpliter act of
              ([], _) -> Ref a : Action act : refActionFind xs
              (some, []) ->
                ConstrainedRef a (actioner some) : refActionFind xs
              (some, [rest]) -> 
                ConstrainedRef a (actioner some) : Action rest 
                                                 : refActionFind xs
              (some, rest) -> 
                ConstrainedRef a (actioner some) : Action (actioner rest)
                                                 : refActionFind xs

        refActionFind (x:xs) = x : refActionFind xs

        actioner [a] = a
        actioner x = ActionExprs x

        actionSpliter (ActionExprs exprs) =
            break isActionPredicate exprs
        actionSpliter a = if isActionPredicate a
            then ([a], [])
            else ([], [a])

-- | Set the index for every unique, return the
-- new webrexp and the count of unique element
setUniqueIndices :: WebRexp -> (Int, WebRexp)
setUniqueIndices expr = foldWebRexp uniqueCounter 0 expr
    where uniqueCounter acc (Unique _) = (acc + 1, Unique acc)
          uniqueCounter acc e = (acc, e)


-- | Set the indices for the Range constructor (filtering
-- by ID).
setRangeIndices :: WebRexp -> (Int, WebRexp)
setRangeIndices expr = foldWebRexp uniqueCounter 0 expr
    where uniqueCounter acc (Range _ r) = (acc + 1, Range acc r)
          uniqueCounter acc e = (acc, e)

-- | Pretty printing for 'WebRef'. It's should be reparsable
-- by the WebRexp parser.
prettyShowWebRef :: WebRef -> String
prettyShowWebRef Wildcard = "_"
prettyShowWebRef (Elem s) = s
prettyShowWebRef (OfClass r s) = prettyShowWebRef r ++ "." ++ s
prettyShowWebRef (Attrib r s) = prettyShowWebRef r ++ "@" ++ s
prettyShowWebRef (OfName r s) = prettyShowWebRef r ++ "#" ++ s

-- | Helper function to check if a given in dex is within
-- all the ranges
isInNodeRange :: Int -> [NodeRange] -> Bool
isInNodeRange _ [] = False
isInNodeRange i (Index ii:xs)
    | i == ii = True
    | i < ii = False
    | otherwise = isInNodeRange i xs
isInNodeRange i (Interval beg end:xs)
    | beg <= i && i <= end = True
    | beg >= i = False
    | otherwise = isInNodeRange i xs

--------------------------------------------------
----            Quasi quotation instances
--------------------------------------------------
instance Lift WebRef where
    lift Wildcard = [| Wildcard |]
    lift (Elem str) =  [| Elem str |]
    lift (OfClass ref str) = [| OfClass ref str |]
    lift (Attrib ref str) = [| Attrib ref str |]
    lift (OfName ref str) = [| OfName ref str |]

instance Lift NodeRange  where
    lift (Index i) = [| Index i |]
    lift (Interval i1 i2) = [| Interval i1 i2 |]

instance Lift Op where
    lift = return . ConE . mkName . show

instance Lift BuiltinFunc where
    lift = return . ConE . mkName . show

instance Lift ActionExpr where
    lift OutputAction = [| OutputAction |]
    lift (ActionExprs lst) = [| ActionExprs lst |]
    lift (ARef str) = [| ARef str |]
    lift (CstI i) = [| CstI i |]
    lift (CstS str) = [| CstS str |]
    lift (NodeReplace a) = [| NodeReplace a |]
    lift (Call b lst) = [| Call b lst |]
    lift (BinOp op a1 a2) = [| BinOp op a1 a2 |]

instance Lift RepeatCount where
    lift (RepeatTimes i) = [| RepeatTimes i |] 
    lift (RepeatAtLeast i) = [| RepeatAtLeast i |] 
    lift (RepeatBetween i1 i2) = [| RepeatBetween i1 i2 |]

instance Lift WebRexp where
    lift (Branch lst) = [| Branch lst |]
    lift (Unions lst) = [| Unions lst |]
    lift (List lst) = [| List lst |]
    lift (Star w) = [| Star w |]
    lift (Repeat count w) = [| Repeat count w |]
    lift (Alternative w1 w2) = [| Alternative w1 w2 |]
    lift (Unique i) = [| Unique i |]
    lift (Str i) = [| Str i |]
    lift (Action a) = [| Action a |]
    lift (Range i lst) = [| Range i lst |]
    lift (Ref ref) = [| Ref ref |]
    lift (DirectChild ref) = [| DirectChild ref |]
    lift (ConstrainedRef ref action) = [| ConstrainedRef ref action |]
    lift a = return . ConE . mkName $ show a
