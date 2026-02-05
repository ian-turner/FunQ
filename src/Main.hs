module Main where

import System.Environment (getArgs)
import Text.Parsec

import Parser


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Please provide input filename"
        (a:as) -> do
            fileContent <- readFile a
            let fileLines = lines fileContent
                decls = map (parseWithWhitespace decl) fileLines
            mapM_ (\x -> putStrLn (show x)) decls
