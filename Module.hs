{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, RecordWildCards,
             MultiParamTypeClasses, ViewPatterns, RankNTypes, PatternGuards,
             StandaloneDeriving, TypeFamilies #-}

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
import Control.Monad
import Control.Monad.Trans (MonadIO(..))

import Data.Either
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (Monoid(..))
import qualified Data.Traversable as T

import AST
import Exp
import Eval
import Compiler
import Located
import Var

import Prelude hiding (init)

-- `r' and 'f' correspond with the `r' and 'f' parameters of the FunR type class.
data Export r f
    = ExportVar r
    | ExportAgain LName
    | ExportFun f

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

data Module r v f = Module SrcSpan (Map LName (Export r f))

------------------------------------------------------------------------------

graphNodes :: FunR r v f => Map LName (Export r f) -> Compiler [IORef (Function v f)]
iterateModule, cloneModule :: FunR r v f => Module r v f -> Compiler (Module r v f)
composeModule, combineModule, plusModule :: FunR r v f => Module r v f -> Module r v f -> Compiler (Module r v f)
lookupExport :: FunR r v f => LName -> (Module r v f) -> Maybe (Export r f)
checkImportList :: SrcSpan -> String -> [(LName, ImportType)] -> Compiler ()
debugModule, debugExports, debugImports :: FunR r v f => Name -> Module r v f -> Compiler ()
moduleImports :: FunR r v f => Module r v f -> Compiler [(LName, ImportType)]
singleModule :: FunR r v f => Located [(LName, ExportDecl)] -> Compiler (Module r v f)

-- Stupid mtl lacking basic instances.
instance (Monad f, Applicative f) => Applicative (SM.StateT s f) where
    pure = return
    (<*>) = ap


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

        scannableExports es = [ ref | ExportFun (viewFun -> VResolvedFun ref) <- es ]
        scannableRefs (_, fs) = [ ref | VResolvedFun ref <- map viewFun fs ]

lookupExport name (Module _ m) = M.lookup name m

-- This is the "!" module operator, which links a module with itself, thus
-- allowing recursive functions.
iterateModule (Module loc m) = context $ do
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
combineModule left@(Module lloc _) right@(Module rloc r) = context $ do
    Module _ composed <- composeModule left right

    -- M.union is left-biased, so this works in our favour.
    return $ Module (combineSpan lloc rloc) (M.union composed r)
    where
        context = withErrorContext $
            "When combining modules at " ++ show lloc ++ " and " ++ show rloc

-- Links a module with another module, but does not bring exports from the second module
-- into the exports of the result.
composeModule left@(Module lloc _) (Module rloc right) = context $ do
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

-- WARN There's a warning about overlapping patterns here and non-exhaustive
-- patterns too, but that's clearly not the case.
-- It will apparently be fixed in GHC 6.12.
-- http://hackage.haskell.org/trac/ghc/ticket/2395
resolveExp src e = case e of
    FunE fun@(viewFun -> VNamedFun name arity) _ ->
        FunE <$> (fromMaybe fun <$> lookupFun name arity) <*> pure arity
    ReadE var@(viewVar -> VNamedVar name) ->
        ReadE <$> (fromMaybe var <$> lookupVar name)
    WriteE var@(viewVar -> VNamedVar name) x ->
        WriteE <$> (fromMaybe var <$> lookupVar name) <*> pure x
    AppE (Left var@(viewVar -> VNamedVar name)) refs args ->
        AppE <$> (Left . fromMaybe var <$> lookupVar name) <*> pure refs <*> pure args
    AppE (Right fun@(viewFun -> VNamedFun name arity)) refs args ->
        AppE <$> (Right . fromMaybe fun <$> lookupFun name arity) <*> pure refs <*> pure args
    x -> pure x

    where

    lookupVar name = case M.lookup name src of
        Nothing -> return Nothing
        Just (ExportVar ref) -> return . Just $ mkResolvedVar ref
        Just (ExportAgain name') -> return . Just $ mkNamedVar name'
        Just (ExportFun f) -> do
            arity <- getFunArity f
            throwAt (getLoc name) [
                "Resolving " ++ show name ++ ": expected a variable, found\
                \a function of arity " ++ show arity
                ]

    lookupFun name arity = do
        case M.lookup name src of
            Nothing -> return Nothing
            Just (ExportVar _) -> throwAt (getLoc name)
                [ "Resolving " ++ unLoc name ++
                  ": Expected a function of arity " ++ show arity ++
                  "; found a variable"
                ]
            Just (ExportAgain name') -> return . Just $ mkNamedFun name' arity
            Just (ExportFun fun) -> do
                realArity <- getFunArity fun
                when (arity /= realArity) $ throwAt (getLoc name) [
                    "Resolving " ++ unLoc name ++
                    ": Expected a function of arity " ++ show arity ++
                    "; found a function of arity " ++ show realArity
                    ]

                return $ Just fun

-- When cloning a module, we may have to rewrite some of the function bodies
-- if they refer to another function in the module (i.e., we are cloning an
-- iterated module).
cloneModule (Module loc oldExports) = do
    nodes <- graphNodes oldExports
    mappings <- mapM cloneNode nodes

    let newExports = fmap (updateExport mappings) oldExports
    SM.evalStateT (rewriteExports mappings (rewritableExports (M.elems newExports))) []

    return $ Module loc newExports
    where
        updateExport assocs (ExportFun (viewFun -> VResolvedFun ref)) = case lookup ref assocs of
            Nothing -> ExportFun (mkResolvedFun ref)
            Just ref' -> ExportFun (mkResolvedFun ref')
        updateExport _ x = x
    
        cloneNode ref = do
            ref' <- newMValue =<< readMValue ref
            return (ref, ref')

        rewritableExports xs = [ ref | ExportFun (viewFun -> VResolvedFun ref) <- xs ]

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

            rewriteF (viewFun -> VResolvedFun ref) = case lookup ref assocs of
                Nothing -> mkResolvedFun ref <$ rewriteExports assocs [ref]
                Just ref' -> mkResolvedFun ref' <$ rewriteExports assocs [ref']
            rewriteF x = return x

-- Adds two modules. If there are duplicate exports, an error is raised.
plusModule l@(Module lloc left) r@(Module rloc right) = context $ do
    li <- moduleImports l
    ri <- moduleImports r
    
    checkImportList newLoc "Conflicting imports found when adding two modules" (li ++ ri)

    checkNameClashes
        newLoc
        "Export conflicts found in a module sum"
        (map fst (M.toList left ++ M.toList right))

    return $ Module newLoc (M.union left right)

    where
        context = withErrorContext $ "When adding two modules at " ++ show lloc ++ " and " ++ show rloc
        newLoc = combineSpan lloc rloc

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


debugModule name m = debugImports name m >> debugExports name m

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
        showExport (ExportFun f) = show . ImportFun <$> getFunArity f

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

moduleImports (Module _ m) = 
    concat <$> SM.evalStateT (mapM imports (M.elems m)) []

    where
        imports (ExportVar _) = return []
        imports (ExportAgain n) = return [(n, ImportAny)]
        imports (ExportFun fun) = do
            case viewFun fun of
                VResolvedFun ref -> do
                    seen <- SM.get
                    if ref `elem` seen
                        then return []
                        else imports' =<< readMValue ref
                _ -> return []            

        imports' fun = do
            (resolved, named) <- ufunImports fun
            nestedImports <- mapM imports' resolved
            return (named ++ concat nestedImports)

        ufunImports fun = do
            let (vn, vi) = (concat *** concat) . partitionEithers $ map f vars
            (fn, fi) <- (concat *** concat) . partitionEithers <$> mapM g funs
            return $ (vn ++ fn, vi ++ fi)
            
            where
                (vars, funs) = references (funBody fun)

                -- Given a variable reference, it's easy to figure out what imports that
                -- variable reference causes.
                f (viewVar -> VNamedVar name) = Right [(name, ImportVar)]
                f _ = Right []

                -- If we see a resolved function, check if that function has
                -- already been scanned for imports.
                g x = case viewFun x of
                    VNamedFun name arity -> return $ Right [(name, ImportFun arity)]
                    VResolvedFun ref -> do
                        seen <- SM.get
                        if ref `elem` seen
                            then return (Left [])
                            else do
                                SM.modify (ref:)
                                fun' <- readMValue ref
                                return $ Left [fun']
                    VNativeFun _ _ -> return $ Right []

-- Converts a module declaration to a Module. A module declaration has only
-- unresolved imports.
singleModule (L declLoc decls) = do
    checkExportsUnique
    return . Module declLoc . M.fromList =<< compileExports        
    where
        checkExportsUnique = checkNameClashes
            declLoc
            "Module exports names more than once: "
            (map fst decls)
    
        -- Get the list of exported functions from an export declaration list.
        compileExports = mapM (uncurry f) decls where
            f name (ExportFunDecl decl) = do
                compiled <- compileFunction decl
                ref <- newMValue compiled
                return (name, ExportFun (mkResolvedFun ref))
            f name ExportVarDecl = do
                ref <- liftIO newResolvedVar
                return (name, ExportVar ref)
            f name (ExportAgainDecl source) = return (name, ExportAgain source)
