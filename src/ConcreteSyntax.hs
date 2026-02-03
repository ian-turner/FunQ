module ConcreteSyntax where


data Expr = Unit
            | Num Integer
            | Var String
            | Tuple [Expr]
            | App Expr Expr
            | Let [String] Expr Expr
            | Lambda [String] Expr
            | IfExpr Expr Expr Expr
            deriving (Show, Eq)

data Decl = VarDecl String Expr
            | FunDecl String [String] Expr
            deriving (Show, Eq)
