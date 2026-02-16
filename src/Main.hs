module Main where

import System.IO
import System.Environment (getArgs)
import Text.Parsec

import Parser
import Resolve
import TopMonad


parserIO :: Either ParseError a -> IO a
parserIO (Left e) = error $ show e
parserIO (Right a) = return a


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Please provide input filename"
        (srcName : args) -> do
            -- Reading file contents
            fileContents <- readFile srcName
            -- Parsing file into declarations
            (decls, _) <- parserIO $ parseModule srcName fileContents initialParserState
            mapM_ (\x -> putStrLn (show x)) decls
            -- Resolving concrete syntax into abstract syntax
            -- ...
