module Parser where

import Text.Parsec
import Control.Monad
import qualified Text.Parsec.Expr as E

import ConcreteSyntax


type Parser a = Parsec String () a

reservedNames :: [String]
reservedNames = ["let", "in", "if", "then", "else"]

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme (try (p >>= check))
  where
    p = (:) <$> firstChar <*> many nonFirstChar
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

    check x
      | x `elem` reservedNames =
        unexpected ("reserved word " ++ show x)
      | otherwise = return x

reserved :: String -> Parser ()
reserved w = lexeme . try $ do
    string w
    notFollowedBy (alphaNum <|> char '_')

symbol :: String -> Parser String
symbol s = lexeme $ string s

num :: Parser Expr
num = Num <$> integer

var :: Parser Expr
var = Var <$> identifier

unitParensOrTuple :: Parser Expr
unitParensOrTuple = unitPattern <|> (between (symbol "(") (symbol ")") inner)
  where
    unitPattern = do
        reserved "()"
        return Unit
    inner = do
        es <- expr `sepBy1` symbol ","
        case es of
            [e] -> return e
            ps  -> return $ Tuple ps

atom :: Parser Expr
atom = var <|> num <|> unitParensOrTuple

term :: Parser Expr
term = do
    f    <- atom
    args <- many atom
    return $ foldl App f args

pattern :: Parser [String]
pattern = tuplePattern <|> simplePattern
  where
    simplePattern = do
        id <- identifier
        return [id]
    tuplePattern = do
        ps <- between (symbol "(") (symbol ")") (identifier `sepBy1` symbol ",")
        return ps

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    p <- pattern
    symbol "="
    e1 <- expr
    reserved "in"
    e2 <- expr
    return $ Let p e1 e2

lamExpr :: Parser Expr
lamExpr = do
    reserved "\\"
    p <- pattern
    symbol "->"
    e <- expr
    return $ Lambda p e

ifElseExpr :: Parser Expr
ifElseExpr = do
    reserved "if"
    eBool <- expr
    reserved "then"
    trueExp <- expr
    reserved "else"
    falseExp <- expr
    return $ IfExpr eBool trueExp falseExp
    
expr :: Parser Expr
expr = lamExpr <|> letExpr <|> ifElseExpr <|> term

varOrFunDecl :: Parser Decl
varOrFunDecl = do
    ids <- many identifier
    symbol "="
    e <- expr
    case ids of
        [id] -> return $ VarDecl id e
        (funId : vars) -> return $ FunDecl funId vars e

decls :: Parser Decl
decls = varOrFunDecl

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = do
        whitespace
        p
