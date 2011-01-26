module Webrexp.Exprtypes
    ( 
    -- * Types
      WebRef (..)
    , NodeRange (..)
    , Op (..)
    , ActionExpr (..)
    , WebRexp (..)
    -- * Functions
    , simplifyNodeRanges 
    , foldWebRexp
    ) where

import Data.List( sort, mapAccumR )

-- | represent an element
data WebRef =
    -- | ...
      Elem String
    -- | ... . ...
    | OfClass WebRef String
    -- | @...
    | Attrib WebRef String
    -- | #...
    | OfName WebRef String
    deriving (Eq, Show)


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

data Op =
      OpAdd | OpSub | OpMul | OpDiv
    | OpLt  | OpLe  | OpGt  | OpGe
    | OpEq  | OpNe  | OpAnd | OpOr
    deriving (Eq, Show)

-- | Represent an action Each production
-- of the grammar more or less map to a
-- data constructor of this type.
data ActionExpr =
    -- | { ... ; ... ; ... ; ... }
      ActionExprs [ActionExpr]
    -- | Basic binary opertor
    | BinOp Op ActionExpr ActionExpr
    -- | Often find an attribute
    | ARef String
    -- | Any number
    | CstI Int
    -- | a string constant
    | CstS String

    -- | the '.' action
    | OutputAction
    deriving (Eq, Show)


-- | Type representation of web-regexp,
-- main type.
data WebRexp =
    -- | ( ... ; ... ; ... )
      Branch [WebRexp]
    -- | ... ... (each action followed, no rollback)
    | List [WebRexp]
    -- | ... *
    | Star WebRexp
    -- | ... +
    | Plus WebRexp

    -- | !
    -- Possess an unique index to differentiate all the differents
    -- uniques. Negative value are considered invalid, all positive or
    -- null one are accepted.
    | Unique Int

    -- | "..."
    | Str String
    -- { ... }
    | Action ActionExpr
    -- [ ... ]
    | Range [NodeRange]

    -- every tag/class name
    | Ref WebRef

    -- | '>' operator in the language, used
    -- to follow hyper link
    | DiggLink

    -- | '|' operator in the language, used
    -- to select the next sibling node.
    | NextSibling

    -- | '^' operator in the language, used
    -- to select the previous sibling node.
    | PreviousSibling

    -- | '<' operator in the language. 
    -- Select the parent node
    | Parent
    deriving (Eq, Show)

foldWebRexp :: (a -> WebRexp -> (a, WebRexp)) -> a -> WebRexp -> (a, WebRexp)
foldWebRexp f acc (Branch subs) = f acc' $ Branch subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (List subs) = f acc' $ List subs'
    where (acc', subs') = mapAccumR (foldWebRexp f) acc subs
foldWebRexp f acc (Star sub) = f acc' $ Star sub'
    where (acc', sub') = foldWebRexp f acc sub
foldWebRexp f acc (Plus sub) = f acc' $ Plus sub'
    where (acc', sub') = foldWebRexp f acc sub
foldWebRexp f acc e = f acc e

