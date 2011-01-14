module Webrexp.Exprtypes where

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

data Op =
      OpAdd | OpSub | OpMul | OpDiv
    | OpLt  | OpLe  | OpGt  | OpGe
    | OpEq  | OpNe  | OpAnd | OpOr
    deriving (Eq, Show)

-- | Represent action grammar
data ActionExpr =
    -- | { ... ; ... ; ... ; ... }
      ActionExprs [ActionExpr]
    -- Basic binary opertor
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
    -- | ... !
    | Unique WebRexp
    -- | "..."
    | Str String
    -- { ... }
    | Action ActionExpr
    -- [ ... ]
    | Range [NodeRange]

    -- every tag/class name
    | Ref WebRef

    | DiggLink
    | NextSibling
    | PreviousSibling
    | Parent
    deriving (Eq, Show)

