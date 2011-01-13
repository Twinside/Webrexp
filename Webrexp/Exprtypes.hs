module Webrexp.Exprtypes where

data InlineOperator =
    DiggLink


-- | represent an element
data WebRef =
    -- | ...
      Elem String
    -- | ... . ...
    | OfClass WebRef String
    -- | @...
    | Attrib String
    -- | #...
    | OfName WebRef String


data NodeRange =
    -- | 1-5,8,10
      Ranges [WebRange]
    -- | ...
    | Index Int
    -- | min-max
    | Interval Int Int

data Op =
      OpAdd | OpSub | OpMul | OpDiv
    | OpLt  | OpLe  | OpGt  | OpGe
    | OpEq  | OpNe

-- | Represent action grammar
data ActionExpr kind =
    -- | { ... ; ... ; ... ; ... }
      ActionExprs [ActionExpr]
    | BinOp Op ActionExpr


-- | Type representation of web-regexp,
-- main type.
data WebRexp =
    -- | ( ... ; ... ; ... )
      Group [WebRexp]
    -- | ... *
    | Star WebRexp
    -- | ... +
    | Plus WebRexp
    -- | ... !
    | Unique WebRexp
    -- | "..."
    | Str String
    -- { ... }
    | Action ActionExpr

