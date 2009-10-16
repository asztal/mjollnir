module Main where

import Control.Applicative
import Data.List (sortBy)
import Data.Ord (comparing)

import System.Environment.UTF8 (getArgs, getProgName)
import System.IO (stderr)
import System.IO.UTF8 (readFile, putStrLn, hPutStrLn)
import Prelude hiding (readFile, putStrLn)
import System.FilePath (takeFileName)

import Compiler
import Parser
import Eval
import Env
import Located

-- TODO: Use a library made for this. This is just temporary.
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "No arguments given." >> usage
        ["-v"] -> version
        ["--version"] -> version
        ["--help"] -> version >> putStrLn "" >> usage
        [path] -> run path
        _ -> hPutStrLn stderr "Too many arguments." >> usage

usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " /path/to/program.fjo"

version = do
    hPutStrLn stderr "Fjölnir Ensk útgáfa 0.00  Raðnúmer: 00000000"
    hPutStrLn stderr "Copyright (c) 2008,09. Öll réttindi áskilin"

run path = do
    prog <- parseProgram (takeFileName path) <$> readFile path
    case prog of
        Left e -> hPutStrLn stderr (show e)
        Right prog -> do
            env <- runCompiler (buildEnv prog)
            case env of
                Left e -> hPutStrLn stderr (show e)
                Right (entryPoints, _) -> do
                    case entryPoints of
                        [] -> hPutStrLn stderr $ "No entry points in " ++ show path
                        [(_, fun)] -> do
                            result <- runEval . evalE $ AppE (Right fun) [] []
                            either (hPutStrLn stderr) (const (return ())) result
                        xs -> hPutStrLn stderr $ "Multiple entry points defined:\n" ++ showEntryPoints xs

showEntryPoints xs = unlines . map f . sortBy (comparing $ unLoc . fst) $ xs where
    f (L loc name, _) = "  " ++ name ++ atLoc (L loc name)
