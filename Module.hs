{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, RecordWildCards,
             MultiParamTypeClasses, ViewPatterns, RankNTypes, PatternGuards,
             StandaloneDeriving, RecursiveDo #-}

module Module
    ( Module (Module)
    , emptyModule
    , plusModule
    , combineModule
    , composeModule
    , iterateModule
    , singleModule
    , lookupExport
    , moduleImports
    , Export (..)
    , debugModule
    , debugExports
    , debugImports
    ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((***))
import qualified Control.Monad.State as SM
import Control.Monad (msum, ap, forM_, when)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Error (MonadError (..))

import Data.Array.Unboxed (listArray)
import Data.Char (ord)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.List (intercalate, nub, union, sort, sortBy, (\\), genericLength, groupBy)
import Data.Monoid (Monoid(..))
import qualified Data.Traversable as T

import AST
import Eval
import Compiler

import Prelude hiding (init)

data Export
    = ExportVar (IORef Value)
    | ExportAgain Name
    | ExportFun (IORef (Function Var Fun))
    | ExportNative Arity ([Var] -> [Value] -> Eval Value)

data ImportType
    = ImportVar
    | ImportFun Arity
    | ImportAny
    | ImportError [ImportType]
    deriving (Ord, Eq)

instance Show ImportType where
    show ImportVar = "breyta"
    show (ImportFun (io,i)) = "stef (" ++ show io ++ ";" ++ show i ++ ")"
    show ImportAny = "*"
    show (ImportError xs) = "Imports with same name but different types: " ++ intercalate ", " (map show xs)

instance Monoid ImportType where
    mappend ImportAny x = x
    mappend x ImportAny = x

    mappend (ImportError es) (ImportError e)
        = ImportError (e `union` es)

    mappend (ImportError es) x
        | x `elem` es = ImportError es
        | otherwise = ImportError (x:es)

    mappend x y
        | x == y = x
        | otherwise = ImportError [x,y]

    mempty = ImportAny

newtype Module = Module (Map Name Export)

------------------------------------------------------------------------------

-- Stupid mtl lacking basic instances.
instance (Monad f, Applicative f) => Applicative (SM.StateT s f) where
    pure = return
    (<*>) = ap

emptyModule :: Module
emptyModule = Module M.empty

graphNodes :: Map Name Export -> Compiler [IORef (Function Var Fun)]
graphNodes m = do
    let roots = scannableExports (M.elems m)
    SM.execStateT (mapM_ scan roots) []
    where
        scan ref = do
            scanned <- SM.get
            when (ref `notElem` scanned) $ do
                SM.modify (ref:)
                fun <- readMValue ref
                let refs = scannableRefs (references (funBody fun))
                mapM_ scan refs

        scannableExports es = [ ref | ExportFun ref <- es ]
        scannableRefs (_, fs) = [ ref | ResolvedFun _ ref <- fs ]

iterateModule :: Module -> Compiler Module
composeModule, combineModule :: Module -> Module -> Compiler Module

lookupExport :: Name -> Module -> Maybe Export
lookupExport name (Module m) = M.lookup name m

(iterateModule, composeModule, combineModule) = (iterateModule', composeModule', combineModule')
    where

    -- This is the "!" module operator, which links a module with itself, thus
    -- allowing recursive functions.
    iterateModule' m = do
        Module m' <- cloneModule m
        nodes <- graphNodes m'

        mapM_ (resolveFun m') nodes
        iterated <- Module <$> T.mapM (resolveOther m') m'

        -- This shouldn't produce errors, but it's best to be sure.
        newImports <- moduleImports iterated
        checkImportList "Conflicting imports when composing two modules" newImports

        return iterated

        where
            resolveOther src (ExportAgain name) = resolveChain src [] (ExportAgain name)
            resolveOther _ x = return x
            
            resolveChain src seen (ExportAgain z)
                | z `elem` seen = throwError $
                    "Re-export cycle detected during module iteration: " ++ intercalate " -> " (head seen : reverse seen)
                | otherwise = case M.lookup z src of
                    Just x -> resolveChain src (z : seen) x
                    Nothing -> return (ExportAgain z)
            resolveChain _ _ x = return x

    -- TODO: Test if this works.
    -- Links a module with another module, but mixes in exports from the right-hand
    -- module into the exports of the result, where no clashes would occur.
    combineModule' left right@(Module r) = do
        Module composed <- composeModule' left right

        -- M.union is left-biased, so this works in our favour.
        return $ Module (M.union composed r)

    -- Links a module with another module, but does not bring exports from the second module
    -- into the exports of the result.
    composeModule' left (Module right) = do
        Module left' <- cloneModule left

        nodes <- graphNodes left'
        mapM_ (resolveFun right) nodes
    
        composed <- Module <$> T.mapM (resolveOther right) left'

        newImports <- moduleImports composed
        checkImportList "Conflicting imports when composing two modules" newImports

        return composed

        where
            resolveOther src (ExportAgain n) = case M.lookup n src of
                Just ex -> return ex
                Nothing -> return (ExportAgain n)
            resolveOther _ x = return x

    resolveFun src ref = do
        fun <- readMValue ref
        body' <- modifyExpM (resolveExp src) (funBody fun)
        ref $= fun { funBody = body' }

    resolveExp src e = case e of
        FunE fun@(NamedFun name arity) _ ->
            FunE <$> (fromMaybe fun <$> lookupFun name arity) <*> pure arity
        ReadE var@(NamedVar name) ->
            ReadE <$> (fromMaybe var <$> lookupVar name)
        WriteE var@(NamedVar name) x ->
            WriteE <$> (fromMaybe var <$> lookupVar name) <*> pure x
        AppE (Left var@(NamedVar name)) refs args ->
            AppE <$> (Left . fromMaybe var <$> lookupVar name) <*> pure refs <*> pure args
        AppE (Right fun@(NamedFun name arity)) refs args ->
            AppE <$> (Right . fromMaybe fun <$> lookupFun name arity) <*> pure refs <*> pure args
        x -> pure x

        where

        lookupVar name = case M.lookup name src of
            Nothing -> return Nothing
            Just (ExportVar ref) -> return . Just $ ResolvedVar ref
            Just (ExportAgain name') -> return . Just $ NamedVar name'
            Just (ExportNative arity _) -> throwError $
                "Resolving " ++ name ++ ": Expected a variable; found a function of arity " ++ show arity
            Just (ExportFun ref) -> do
                fun <- readMValue ref
                throwError $ "Resolving " ++ name ++ ": Expected a variable; found a function of arity " ++ show (funArity fun)

        lookupFun name arity = do
            case M.lookup name src of
                Nothing -> return Nothing
                Just (ExportVar _) -> throwError $ "Resolving " ++ name ++ ": Expected a function of arity " ++ show arity ++ "; found a variable"
                Just (ExportAgain name') -> return . Just $ NamedFun name' arity
                Just (ExportFun ref) -> return . Just $ ResolvedFun arity ref
                Just (ExportNative arity' fun) ->
                    if arity == arity'
                        then return . Just $ NativeFun arity fun
                        else throwError $ "Resolving " ++ name ++
                            ": Expected a function of arity " ++ show arity ++
                            "; found a function of arity " ++ show arity'
                            

-- When cloning a module, we may have to rewrite some of the function bodies
-- if they refer to another function in the module (i.e., we are cloning an
-- iterated module).
cloneModule :: Module -> Compiler Module
cloneModule (Module oldExports) = do
    nodes <- graphNodes oldExports
    mappings <- mapM cloneNode nodes

    let newExports = fmap (updateExport mappings) oldExports
    SM.evalStateT (rewriteExports mappings (rewritableExports (M.elems newExports))) []

    return $ Module newExports
    where
        updateExport assocs (ExportFun ref) = case lookup ref assocs of
            Nothing -> ExportFun ref
            Just ref' -> ExportFun ref'
        updateExport _ x = x
    
        cloneNode ref = do
            ref' <- newMValue =<< readMValue ref
            return (ref, ref')

        rewritableExports xs = [ ref | ExportFun ref <- xs ]

        rewriteExports assocs refs = forM_ refs $ \ref-> do
            fun <- readMValue ref
            rewritten <- SM.get
            when (ref `notElem` rewritten) $ do
                SM.modify (ref:)
                body' <- rewriteRefs assocs (funBody fun)
                ref $= fun { funBody = body' }

        rewriteRefs assocs body = modifyExpM f body where
            f (FunE fun arity) = FunE <$> rewriteF fun <*> pure arity
            f (AppE (Right fun) refs args) = AppE <$> (Right <$> rewriteF fun) <*> pure refs <*> pure args
            f x = return x

            rewriteF (ResolvedFun arity ref) = case lookup ref assocs of
                Nothing -> ResolvedFun arity ref <$ rewriteExports assocs [ref]
                Just ref' -> ResolvedFun arity ref' <$ rewriteExports assocs [ref']
            rewriteF x = return x

-- Adds two modules. If there are duplicate exports, an error is raised.
plusModule :: Module -> Module -> Compiler Module
plusModule l@(Module left) r@(Module right) = do
    li <- moduleImports l
    ri <- moduleImports r
    checkImportList "Conflicting imports found when adding two modules" (li ++ ri)

    case M.toList (M.intersection left right) of
        [] -> return ()
        xs -> throwError . unlines $
            "Export conflicts found in a module sum:" : map showError xs

    return $ Module (M.union left right)

    where
        showError (name, _) = show name ++ " is exported by both sides of the sum"

checkImportList :: String -> [(Name, ImportType)] -> Compiler ()
checkImportList msg imports = do
    let sorted = sort imports
        grouped = groupBy ((==) `on` fst) sorted
        checked = map ((head *** mconcat) . unzip) grouped

    case importErrors checked of
        [] -> return ()
        xs -> throwError . unlines $ msg : xs

    where
        importErrors xs = [ "  " ++ name ++ ": " ++ show err
                          | (name, err@(ImportError _)) <- xs ]

debugModule :: Name -> Module -> Compiler ()
debugModule name m = debugImports name m >> debugExports name m

debugExports :: Name -> Module -> Compiler ()
debugExports moduleName (Module exports) =
    liftIO $ do
        putStrLn $ "Exports for module " ++ show moduleName ++ ":"
        mapM_ (uncurry printExport) (M.toList exports)
    where
        printExport name export = do
            display <- showExport export
            putStrLn $ "  " ++ name ++ " -> " ++ display

        showExport (ExportVar _) = return "breyta"
        showExport (ExportAgain n) = return n
        showExport (ExportNative arity _) = return $ show (ImportFun arity)
        showExport (ExportFun f) = showFun f
        showFun r = do
            arity <- funArity <$> readMValue r
            return $ show (ImportFun arity)
            
debugImports :: Name -> Module -> Compiler ()
debugImports moduleName m = do
    imports <- moduleImports m
    liftIO $ do
        putStrLn $ "Imports for module " ++ show moduleName ++ ":"
        mapM_ (uncurry printImport) imports
    where
        printImport name importType = putStrLn $ "  " ++ name ++ " :: " ++ show importType

-- TODO: If a variable is imported but not actually used, it is disregarded
--       by the module system. I think the Rejkjavík compiler might do this
--       too, it's best to check.
moduleImports :: Module -> Compiler [(Name, ImportType)]
moduleImports (Module m) = 
    concat <$> SM.evalStateT (mapM imports (M.elems m)) []

    where
        --type S a = StateT [IORef (Function UVar UFun)] Compiler a
    
        --imports :: UExport -> S [(Name, ImportType)]
        imports (ExportVar _) = return []
        imports (ExportNative _ _) = return []
        imports (ExportAgain n) = return [(n, ImportAny)]
        imports (ExportFun ref) = do
            seen <- SM.get
            if ref `elem` seen
                then return []
                else imports' =<< readMValue ref

        --imports' :: Function UVar UFun -> S [(Name, ImportType)]
        imports' fun = do
            (resolved, named) <- ufunImports fun
            nestedImports <- mapM imports' resolved
            return (named ++ concat nestedImports)

        -- ufunImports :: Function UVar UFun -> S ([Function UVar UFun], [(Name, ImportType)])
        ufunImports fun = do
            let (vn, vi) = (concat *** concat) . partitionEithers $ map f vars
            (fn, fi) <- (concat *** concat) . partitionEithers <$> mapM g funs
            return $ (vn ++ fn, vi ++ fi)
            
            where
                (vars, funs) = references (funBody fun)

                -- Given a variable reference, it's easy to figure out what imports that
                -- variable reference causes.
                f (NamedVar name) = Right [(name, ImportVar)]
                f _ = Right []

                -- If we see a resolved function, check if that function has
                -- already been scanned for imports.
                g (NamedFun name arity) = return $ Right [(name, ImportFun arity)]
                g (ResolvedFun _ ref) = do
                    seen <- SM.get
                    if ref `elem` seen
                        then return (Left [])
                        else do
                            SM.modify (ref:)
                            fun' <- readMValue ref
                            return $ Left [fun']
                g (NativeFun _ _) = return $ Right []

-- Converts a module declaration to a Module. A module declaration has only
-- unresolved imports.
singleModule :: [(Name, ExportDecl)] -> Compiler Module
singleModule decls = do
    checkExportsUnique

    return . Module . M.fromList =<< compileExports
        
    where
        checkExportsUnique = case sorted \\ nub sorted of
            [] -> return ()
            xs -> throwError $ "Module literal exports names more than once: " ++ intercalate ", " (nub xs)
            where
                sorted = sort (map fst decls)
    
        -- Get the list of exported functions from an export declaration list.
        compileExports = mapM (uncurry f) decls where
            f name (ExportFunDecl decl) = do
                compiled <- compileFunction decl
                ref <- newMValue compiled
                return (name, ExportFun ref)
            f name ExportVarDecl = do
                ref <- newMValue Nil
                return (name, ExportVar ref)
            f name (ExportAgainDecl source) = return (name, ExportAgain source)

compileFunction :: FunctionDecl -> Compiler (Function Var Fun)
compileFunction FunctionDecl {..} = do
    checkVarNames $ fnInOutParams ++ fnInParams ++ concatMap variableDeclNames fnVariables

    body <- compileBody
    
    return $ Function
        (genericLength fnInOutParams, genericLength fnInParams)
        (length allLocals)
        (length fnInParams)
        body

    where
        compileBody = do
            bodyExp <- toExp (BlockS fnBody)
            initExps <- compileInitializers
            return $ simplifyE (ManyE (initExps ++ [bodyExp]))

        simplifyE (ManyE []) = ConstE Nil
        simplifyE (ManyE [x]) = x
        simplifyE x = x

        -- Convert the initializer list to a list of statements to be inserted
        -- before the function body.
        compileInitializers = catMaybes <$> (sequence $ zipWith f [0..] allLocals) where
            f i (_, Just init) = Just . WriteE (LocalVar i) <$> toExp init
            f _ (_, Nothing) = return Nothing

        (allLocals, importedVars) = (concat *** concat) . partitionEithers $ map f fnVariables where
            f (LocalVarDecl xs) = Left xs
            f (ImportVarDecl xs) = Right xs
    
        checkVarNames xs = case xs \\ nub xs of
            [] -> return ()
            dups -> throwError $
                "Variable declared multiple times: " ++ intercalate ", " dups

        argMap = indexMap fnInParams
        refArgMap = indexMap fnInOutParams
        localMap = indexMap locals

        indexMap = M.fromList . sortBy (comparing snd) . (`zip` [0..])

        (locals, _) = partitionEithers $ concatMap f fnVariables where
            f (LocalVarDecl xs) = map (Left . fst) xs
            f (ImportVarDecl names) = map Right names

        toExp = syntaxToExp localMap argMap refArgMap importedVars

syntaxToExp :: Map VarName Int -> Map VarName Int -> Map VarName Int -> [VarName]
            -> Syntax
            -> Compiler (Exp Var Fun)
syntaxToExp localIndices argIndices refArgIndices importedVars expr = case expr of
    LiteralS lit -> return $ transformLiteral lit
    ListS [] -> return $ ConstE Nil
    ListS (x:xs) -> recur $ OperatorS ":" x (ListS xs)

    FunRefS name arity -> return $ FunE (NamedFun name arity) arity
    VarRefS name -> ReadE <$> lookupVarD name

    GetterS a is -> recur $ OperatorCallS ("fylkissækja" ++ show (length is)) (a:is)
    SetterS a is e -> recur $ OperatorCallS ("fylkissetja" ++ show (length is)) (a:is ++ [e])
    AssignS name e -> WriteE <$> lookupVarD name <*> recur e

    FunCallS name refs args -> app name refs args
    OperatorS name x y -> app name [] [x, y]
    OperatorCallS name args -> app name [] args
    OperatorUnaryS name x -> app name [] [x]

    AndS x y -> AndE <$> recur x <*> recur y
    OrS x y -> OrE <$> recur x <*> recur y
    NotS x -> NotE <$> recur x

    CaseS cond cases defaultCase -> CaseE
        <$> recur cond
        <*> mapM toBranch cases
        <*> maybe (pure (ConstE Nil)) (recur . BlockS) defaultCase

    IfS cond thenExp elseIfs otherwiseExp ->
        IfE <$> recur cond <*> recur (BlockS thenExp) <*> case elseIfs of
            ((cond', thenExp') : rest) -> recur $ IfS cond' thenExp' rest otherwiseExp
            [] -> maybe (return (ConstE Nil)) (recur . BlockS) otherwiseExp

    WhileLoopS cond body -> recur . LoopS $
        IfS cond [ListS []] [] (Just [BreakS]) : body
        
    ForLoopS inits cond incs body -> recur . BlockS $
        BlockS inits : WhileLoopS cond (body ++ incs) : []
    
    LoopS xs -> LoopE . ManyE <$> mapM recur xs

    BlockS [] -> return $ ConstE Nil
    BlockS [x] -> recur x
    BlockS xs -> ManyE <$> mapM recur xs
    
    BreakS -> return BreakE
    ReturnS x -> ReturnE <$> recur x
    
    where
        recur = syntaxToExp localIndices argIndices refArgIndices importedVars
        
        app name refs args = AppE
            <$> pure (possiblyFun name (genericLength refs, genericLength args))
            <*> mapM toRefVar refs
            <*> mapM recur args

        toRefVar e = case e of
            VarRefS name -> Right <$> lookupVarD name
            x -> Left <$> recur x

        lookupVarD name = 
            maybe (tryImport name) return $ lookupVar name

        tryImport name
            | name `elem` importedVars = return $ NamedVar name
            | otherwise = throwError $
                "Unbound variable " ++ show name ++ " used (did you mean to import it?)"

        lookupVar name = msum $ zipWith find
                [localIndices, argIndices, refArgIndices]
                [LocalVar, ArgVar, RefArgVar]
            where
                find vars constr = constr <$> M.lookup name vars

        possiblyFun name arity = maybe (Right (NamedFun name arity)) Left $ lookupVar name

        transformLiteral (CharLit c) = ConstE $ Word (fromIntegral (ord c))
        transformLiteral (NatLit i) = ConstE $ Word (fromIntegral i)
        transformLiteral (IntLit i) = ConstE $ Word (fromIntegral i)
        transformLiteral (FloatLit f) = ConstE $ Real f
        transformLiteral (StringLit xs) = StrE $ let len = length xs in
            listArray (0, len) (fromIntegral len : map (fromIntegral . ord) xs)

        toBranch (ranges, body) = do
            bodyExp <- recur (BlockS body)
            return (map toRange ranges, bodyExp)
            
        toRange (from, to) =
            (literalAsWord16 from, literalAsWord16 $ fromMaybe from to)

        literalAsWord16 (CharLit c) = fromIntegral (ord c)
        literalAsWord16 (NatLit i) = fromIntegral i
        literalAsWord16 _ = error "Literal in case range wasn't CharLit/NatLit"
