import System.IO (stderr, hPutStrLn)
import System.IO.UTF8 (readFile)
import Prelude hiding (readFile, putStrLn)

import Data.List (intercalate)

import Parser
import Eval
import Env

main :: IO ()
main = do
    prog <- either (error . show) return . parseProgram "test.fjo" =<< readFile "/home/asztal/Grunnur/fjo/test.fjo"
    (entryPoints, _) <- either error return =<< runCompiler (buildEnv prog)
    case entryPoints of
        [] -> hPutStrLn stderr "No entry points"
        [(_, fun)] -> do
            result <- runEval . evalE $ AppE (Right fun) [] []
            either (hPutStrLn stderr) (const (return ())) result
        xs -> hPutStrLn stderr $ "Multiple entry points defined: " ++ intercalate ", " (map fst xs)

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
