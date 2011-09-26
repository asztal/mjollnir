module Main where

import Data.List
import Data.Ord

-- Seems the default System.Environment.getProgName doesn't do conversion.
import System.Environment.UTF8 (getArgs, getProgName)
import System.FilePath (takeFileName)
import System.IO (stderr, hPutStrLn)
import Prelude

import Compiler
import Eval
import Env
import Exp

-- TODO: Use a library made for this. This is just temporary.
main = do
    args <- getArgs
    case args of
        [] -> hPutStrLn stderr "No arguments given." >> usage
        ["-v"] -> version
        ["--version"] -> version
        ["--help"] -> version >> putStrLn "" >> usage
        paths -> run paths

usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " /path/to/program.fjo [/path/to/other.fjo ..]"

version = do
    hPutStrLn stderr "Fjölnir Ensk útgáfa 0.00  Raðnúmer: 00000000"
    hPutStrLn stderr "Copyright (c) 2008,09. Öll réttindi áskilin"

run paths = do
    r <- runCompiler $ do
        programs <- mapM parseProgram paths
        runDefaultEnv $ mapM_ perform programs

    case r of
        Left e -> hPutStrLn stderr (show e)
        Right (entryPoints, _) -> do
            case entryPoints of
                [] -> hPutStrLn stderr $ "No entry points in " ++ intercalate ", " (map takeFileName paths)
                [(_, fun)] -> do
                    result <- runEval . evalE $ AppE (Right fun) [] []
                    either (hPutStrLn stderr) (const (return ())) result
                xs -> hPutStrLn stderr $ "Multiple entry points defined:\n" ++ showEntryPoints xs

showEntryPoints xs = unlines . map f . sortBy (comparing $ unLoc . fst) $ xs where
    f (L loc name, _) = "  " ++ name ++ atLoc (L loc name)
