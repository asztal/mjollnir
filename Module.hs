{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, RecordWildCards,
             MultiParamTypeClasses, ViewPatterns, RankNTypes, PatternGuards,
             StandaloneDeriving, RecursiveDo #-}

module Module
    ( Module (Module)
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
import Data.List (intercalate, union, sort, sortBy, genericLength, group, groupBy)
import Data.Monoid (Monoid(..))
import qualified Data.Traversable as T

import AST
import Eval
import Compiler
import Located

import Prelude hiding (init)

data Export
    = ExportVar (IORef Value)
    | ExportAgain LName
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

data Module = Module SrcSpan (Map LName Export)

------------------------------------------------------------------------------

-- Stupid mtl lacking basic instances.
instance (Monad f, Applicative f) => Applicative (SM.StateT s f) where
    pure = return
    (<*>) = ap

graphNodes :: Map LName Export -> Compiler [IORef (Function Var Fun)]
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

lookupExport :: LName -> Module -> Maybe Export
lookupExport name (Module _ m) = M.lookup name m

(iterateModule, composeModule, combineModule) = (iterateModule', composeModule', combineModule')
    where

    -- This is the "!" module operator, which links a module with itself, thus
    -- allowing recursive functions.
    iterateModule' (Module loc m) = context $ do
        Module _ m' <- cloneModule (Module loc m)
        nodes <- graphNodes m'

        mapM_ (resolveFun m') nodes
        iterated <- Module loc <$> T.mapM (resolveOther m') m'

        -- This shouldn't produce errors, but it's best to be sure.
        newImports <- moduleImports iterated
        checkImportList loc "Conflicting imports when composing two modules" newImports

        return iterated

        where
            context = withErrorContext $
                "When iterating the module at " ++ show loc
                
            resolveOther src (ExportAgain name) = resolveChain src [] (ExportAgain name)
            resolveOther _ x = return x
            
            resolveChain src seen (ExportAgain z)
                | z `elem` seen = throwAt loc $
                    "Re-export cycle detected during module iteration: "
                    : showCycle (head seen : reverse seen)
                | otherwise = case M.lookup z src of
                    Just x -> resolveChain src (z : seen) x
                    Nothing -> return (ExportAgain z)
            resolveChain _ _ x = return x

            showCycle xs = [ "  " ++ unLoc name ++ atLoc name | name <- xs ]

    -- TODO: Test if this works.
    -- Links a module with another module, but mixes in exports from the right-hand
    -- module into the exports of the result, where no clashes would occur.
    combineModule' left@(Module lloc _) right@(Module rloc r) = context $ do
        Module _ composed <- composeModule' left right

        -- M.union is left-biased, so this works in our favour.
        return $ Module (combineSpan lloc rloc) (M.union composed r)
        where
            context = withErrorContext $
                "When combining modules at " ++ show lloc ++ " and " ++ show rloc

    -- Links a module with another module, but does not bring exports from the second module
    -- into the exports of the result.
    composeModule' left@(Module lloc _) (Module rloc right) = context $ do
        Module _ left' <- cloneModule left

        nodes <- graphNodes left'
        mapM_ (resolveFun right) nodes
    
        composed <- Module lloc <$> T.mapM (resolveOther right) left'

        newImports <- moduleImports composed
        checkImportList lloc "Conflicting imports when composing two modules" newImports

        return composed

        where
            context = withErrorContext $
                "When composing modules at " ++ show lloc ++ " and " ++ show rloc

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
            Just (ExportNative arity _) -> throwAt (getLoc name) $
                [ "Resolving " ++ unLoc name ++
                  ": Expected a variable; found a function of arity " ++
                  show arity
                ]
            Just (ExportFun ref) -> do
                fun <- readMValue ref
                throwAt (getLoc name)
                    [ "Resolving " ++ unLoc name ++
                      ": Expected a variable; found a function of arity " ++
                       show (funArity fun)
                    ]

        lookupFun name arity = do
            case M.lookup name src of
                Nothing -> return Nothing
                Just (ExportVar _) -> throwAt (getLoc name)
                    [ "Resolving " ++ unLoc name ++
                      ": Expected a function of arity " ++ show arity ++
                      "; found a variable"
                    ]
                Just (ExportAgain name') -> return . Just $ NamedFun name' arity
                Just (ExportFun ref) -> return . Just $ ResolvedFun arity ref
                Just (ExportNative arity' fun) ->
                    if arity == arity'
                        then return . Just $ NativeFun arity fun
                        else throwAt (getLoc name)
                            [ "Resolving " ++ unLoc name ++
                              ": Expected a function of arity " ++ show arity ++
                              "; found a function of arity " ++ show arity'
                            ]

-- When cloning a module, we may have to rewrite some of the function bodies
-- if they refer to another function in the module (i.e., we are cloning an
-- iterated module).
cloneModule :: Module -> Compiler Module
cloneModule (Module loc oldExports) = do
    nodes <- graphNodes oldExports
    mappings <- mapM cloneNode nodes

    let newExports = fmap (updateExport mappings) oldExports
    SM.evalStateT (rewriteExports mappings (rewritableExports (M.elems newExports))) []

    return $ Module loc newExports
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
plusModule l@(Module lloc left) r@(Module rloc right) = context $ do
    li <- moduleImports l
    ri <- moduleImports r
    
    checkImportList newLoc "Conflicting imports found when adding two modules" (li ++ ri)

    throwClashes
        "Export conflicts found in a module sum"
        newLoc
        (findClashes $ map fst (M.toList left ++ M.toList right))

    return $ Module newLoc (M.union left right)

    where
        context = withErrorContext $ "When adding two modules at " ++ show lloc ++ " and " ++ show rloc
        newLoc = combineSpan lloc rloc

checkImportList :: SrcSpan -> String -> [(LName, ImportType)] -> Compiler ()
checkImportList loc msg imports = do
    let sorted = sort imports
        grouped = groupBy ((==) `on` fst) sorted
        checked = map ((head *** mconcat) . unzip) grouped

    case importErrors checked of
        [] -> return ()
        xs -> throwAt loc $ msg : xs

    where
        importErrors xs = [ name ++ ": " ++ atLoc (L loc name)
                          | (L loc name, ImportError _) <- xs ]

debugModule :: Name -> Module -> Compiler ()
debugModule name m = debugImports name m >> debugExports name m

debugExports :: Name -> Module -> Compiler ()
debugExports moduleName (Module loc exports) =
    liftIO $ do
        putStrLn $ "Exports for module " ++ show moduleName ++ " (" ++ show loc ++ "):"
        mapM_ (uncurry printExport) (M.toList exports)
    where
        printExport name export = do
            display <- showExport export
            putStrLn $ "  " ++ unLoc name ++ " -> " ++ display
            putStrLn $ "    (at " ++ show (getLoc name) ++ ")"

        showExport (ExportVar _) = return "breyta"
        showExport (ExportAgain n) = return $ unLoc n
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
        printImport name importType = putStrLn $ "  " ++ unLoc name ++ " :: " ++ show importType

-- TODO: If a variable is imported but not actually used, it is disregarded
--       by the module system. I think the Rejkjavík compiler might do this
--       too, it's best to check.
moduleImports :: Module -> Compiler [(LName, ImportType)]
moduleImports (Module _ m) = 
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
singleModule :: Located [(LName, ExportDecl)] -> Compiler Module
singleModule (L declLoc decls) = do
    checkExportsUnique
    return . Module declLoc . M.fromList =<< compileExports        
    where
        checkExportsUnique = throwClashes
            "Module exports names more than once: "
            declLoc
            (findClashes $ map fst decls)
    
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

findClashes xs = filter (not . null . tail) . group . sort $ xs

throwClashes :: String -> SrcSpan -> [[LName]] -> Compiler ()
throwClashes msg loc xs = case xs of
    [] -> return ()
    xs -> throwAt loc $ msg : concatMap showClash xs
    where
        showClash names = [ "  " ++ unLoc name ++ atLoc name | name <- names ]

compileFunction :: Located FunctionDecl -> Compiler (Function Var Fun)
compileFunction (L funLoc FunctionDecl {..}) = context $ do
    checkVarNames $ fnInOutParams ++ fnInParams ++ concatMap variableDeclNames fnVariables

    body <- compileBody
    
    return $ Function
        (genericLength fnInOutParams, genericLength fnInParams)
        (length allLocals)
        (length fnInParams)
        body

    where
        context = withErrorContext $ "When compiling function at " ++ show funLoc
    
        compileBody = do
            bodyExps <- mapM toExp fnBody
            initExps <- compileInitializers
            return $ simplifyE (ManyE (initExps ++ bodyExps))

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

        checkVarNames xs = throwClashes "Variable declared multiple times: " funLoc (findClashes xs)

        argMap = indexMap fnInParams
        refArgMap = indexMap fnInOutParams
        localMap = indexMap locals

        indexMap = M.fromList . sortBy (comparing snd) . (`zip` [0..])

        (locals, _) = partitionEithers $ concatMap f fnVariables where
            f (LocalVarDecl xs) = map (Left . fst) xs
            f (ImportVarDecl names) = map Right names

        toExp = syntaxToExp localMap argMap refArgMap importedVars

syntaxToExp :: Map LVarName Int -> Map LVarName Int -> Map LVarName Int -> [LVarName]
            -> Located Syntax
            -> Compiler (Exp Var Fun)
syntaxToExp localIndices argIndices refArgIndices importedVars (L loc expr) =
    context $ case expr of
        LiteralS lit -> return $ transformLiteral lit
        ListS [] -> return $ ConstE Nil
        ListS (x:xs) -> app (withLoc x ":") [] [x, L loc $ ListS xs] -- Location info isn't perfect here.

        FunRefS name (L _ arity) -> return $ FunE (NamedFun name arity) arity
        VarRefS name -> ReadE <$> lookupVarD name

        GetterS a is -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissækja" ++ show (length is)) (a:is)
        SetterS a is e -> recur . L loc $ OperatorCallS (withLoc a $ "fylkissetja" ++ show (length is)) (a:is ++ [e])
        AssignS name e -> WriteE <$> lookupVarD name <*> recur e

        FunCallS name refs args -> app name refs args
        OperatorS name x y -> app name [] [x, y]
        OperatorCallS name args -> app name [] args
        OperatorUnaryS name x -> app name [] [x]

        AndS x y -> AndE <$> recur x <*> recur y
        OrS x y -> OrE <$> recur x <*> recur y
        NotS x -> NotE <$> recur x

        CaseS cond cases defaultCase -> do
            CaseE
            <$> recur cond
            <*> mapM toBranch cases
            <*> maybe (pure (ConstE Nil)) recurs defaultCase

        IfS cond thenExp elseIfs otherwiseExp ->
            fold $ (cond, thenExp) : elseIfs
            where
                fold [] = maybe (pure (ConstE Nil)) recurs otherwiseExp
                fold ((cond, thenExp) : rest) = IfE <$> recur cond <*> recurs thenExp <*> fold rest

        WhileLoopS cond body -> while cond body
        ForLoopS inits cond incs body -> for inits cond incs body 
        
        LoopS xs -> LoopE . ManyE <$> mapM recur xs

        BlockS [] -> return $ ConstE Nil
        BlockS [x] -> recur x
        BlockS xs -> ManyE <$> mapM recur xs
        
        BreakS -> return BreakE
        ReturnS x -> ReturnE <$> recur x
    
    where
        context = withErrorContext $ "When compiling function at " ++ show loc
    
        recur = syntaxToExp localIndices argIndices refArgIndices importedVars

        recurs [] = return $ ConstE Nil
        recurs [x] = recur x
        recurs xs = ManyE <$> mapM recur xs

        while cond body = do
            break <- IfE <$> recur cond <*> pure (ConstE Nil) <*> pure BreakE
            LoopE . insertBreak break <$> recurs body
            where
                insertBreak break (ManyE xs) = ManyE (break : xs)
                insertBreak break (ConstE Nil) = break
                insertBreak break x = ManyE [break, x]

        for inits cond incs body = do
            inits' <- recurs inits
            body' <- while cond (body ++ incs)
            return . simplifyE $ ManyE [inits', body']

        simplifyE (ManyE xs) = ManyE $ concatMap f xs where
            f (ManyE xs) = xs
            f x = [x]
        simplifyE e = e
        
        app name refs args = AppE
            <$> pure (possiblyFun name (genericLength refs, genericLength args))
            <*> mapM toRefVar refs
            <*> mapM recur args

        toRefVar (L l e) = case e of
            VarRefS name -> Right <$> lookupVarD name
            x -> Left <$> recur (L l x)

        lookupVarD name = 
            maybe (tryImport name) return $ lookupVar name

        tryImport name
            | name `elem` importedVars = return $ NamedVar name
            | otherwise = throwAt (getLoc name) [ "Unbound variable " ++ show name ++ " used (did you mean to import it?)" ]

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
            bodyExp <- recurs body
            return (map toRange ranges, bodyExp)
            
        toRange (L _ (from, to)) =
            (literalAsWord16 from, literalAsWord16 $ fromMaybe from to)

        literalAsWord16 (CharLit c) = fromIntegral (ord c)
        literalAsWord16 (NatLit i) = fromIntegral i
        literalAsWord16 _ = error "Literal in case range wasn't CharLit/NatLit"
