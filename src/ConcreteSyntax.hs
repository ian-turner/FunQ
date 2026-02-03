module ConcreteSyntax where


data Exp
  = Unit                                -- Unit type and value
  | Num Integer                         -- Number constants
  | Var String                          -- Variable identifiers
  | Tuple [Exp]                         -- Tuple of objects (any size)
  | App Exp Exp                         -- Function application
  | Let [String] Exp Exp                -- Let expression
  | Lam [String] Exp                    -- Lambda expressions
  | IfExp Exp Exp Exp                   -- If/then/else expressions
  | Arrow Exp Exp                       -- Arrow for type declarations
  deriving (Show, Eq)

data Decl
  = TypeDecl String Exp                 -- Type declarations
  | VarDecl String Exp                  -- Variable declarations
  | FunDecl String [String] Exp         -- Function declarations
  deriving (Show, Eq)
