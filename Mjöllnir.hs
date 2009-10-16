module Main where

import Control.Applicative
import Data.List (sortBy)
import Data.Ord (comparing)

import System.Environment (getArgs, getProgName)
import System.IO (stderr)
import System.IO.UTF8 (readFile, hPutStrLn)
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
        [path] -> run path
        _ -> hPutStrLn stderr "Too many arguments." >> usage

usage = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " /path/to/program.fjo"

version = do
    hPutStrLn stderr "Fjölnir Ensk útgáfa 0.00  Raðnúmer: 00000000"
    hPutStrLn stderr "Copyright (c) 2008,09. Öll réttindi áskilin"
    hPutStrLn stderr "Lee Houghton, Anonymous, Gerald Jay Sussman"

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

{- import Grunnur

main = do
    putStrLn "Mjöllnir 0.01"

    grunnur <- makeGrunnur

    x <- liftIO $ newIORef (Word 0)
    y <- liftIO $ newIORef (Word 0)
    let env = (Env (M.fromList [("x",x),("y",y)]) grunnur Nothing)
    runInputT (setComplete (kwCompletion grunnur) defaultSettings) $ loop env

    where
        kwCompletion mod = completeWord Nothing " \t\f\r\n,;" (keywordCompletions mod)
        keywordCompletions mod prefix = return $ do
            w <- sort $ if "\\" `isPrefixOf` prefix
                            then map ('\\':) $ moduleNames mod
                            else reservedNames ++ moduleNames mod
            
            guard $ prefix `isPrefixOf` w
            return $ Completion w w True
            
        moduleNames mod = map fst $ M.assocs mod
        loop env = do
            input <- getInputLine "Mjöllnir> "
            case input of
                Nothing -> return ()
                Just input -> do
                    case parseExpression "<interactive>" input of
                        Left e -> outputStrLn $ "*** Parse error: " ++ show e
                        Right expr -> do
                            outputStrLn (show expr)
                            outputStrLn (show (desugar expr))
                            val <- liftIO $ runEvalIn env (eval expr) 
                            case val of
                                Left e -> outputStrLn $ "*** Error: " ++ show e
                                Right r -> outputStrLn =<< mshow r
                    loop env
-}
