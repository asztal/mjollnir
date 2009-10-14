{-# LANGUAGE Rank2Types, GADTs, TypeSynonymInstances, DeriveDataTypeable,
             RecordWildCards #-}

module AST
    ( -- * Syntax Tree
      Literal (..)
    , Syntax (..)
    , VariableDecl (..)
    , FunctionDecl (..)
    , ExportDecl (..)
    , ModuleDecl (..)
    , Program
    , ProgramStatement (..)
      -- * Types
    , module Types
      -- * AST operations
    , variableDeclNames
    , functionImports
    , deepModifyAST
    , subexpressions
    ) where

import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf)
import Data.Generics (Data, Typeable, everywhere, mkT, listify)

import Language
import Types

-- | A value literal of type Char (stafur), Natural number (Fjöldatala), Integer (Heiltala)
-- | Floating point number (Fleytitala), or String (Strengur).
data Literal where
    CharLit   :: Char    -> Literal
    NatLit    :: Integer -> Literal
    IntLit    :: Integer -> Literal
    FloatLit  :: Double  -> Literal
    StringLit :: String  -> Literal
    deriving (Data, Typeable)

-- TODO: Use Fjölnir escaping rules for CharLit/StringLit instead of Haskell's.
instance Show Literal where
    show (CharLit c) = show c
    show (StringLit str) = show str
    show (NatLit n) = show n
    show (IntLit n) = show n
    show (FloatLit f) = show f

data Syntax
    = LiteralS Literal
    | ListS [Syntax]
    
    | FunRefS FunName Arity    -- stef f(0,2)
    | VarRefS VarName          -- Reference to a variable

    | GetterS Syntax [Syntax]
    | SetterS Syntax [Syntax] Syntax
    | AssignS VarName Syntax

    | FunCallS FunName [Syntax] [Syntax]  -- f(inout;in)
    | OperatorS FunName Syntax Syntax     -- x ++ y OR x \fgcd y
    | OperatorCallS FunName [Syntax]      -- \hali(;xs)
    | OperatorUnaryS FunName Syntax       -- \hali xs

    -- ekki, og, and eða are treated separately from normal operators.
    -- AFAICS it is not possible to redefine their behaviour.
    -- This is most likely due to short-circuiting behaviour.
    | AndS Syntax Syntax
    | OrS Syntax Syntax
    | NotS Syntax

    | CaseS Syntax [([(Literal, Maybe Literal)], Block)] (Maybe Block)
    | IfS Syntax Block [(Syntax, Block)] (Maybe Block)

    -- meðan cond lykkja body lykkjulok
    | WhileLoopS Syntax Block                 
    -- fyrir (initializers; condition; increments) lykkja body lykkjulok
    | ForLoopS [Syntax] Syntax [Syntax] Block
    -- lykkja body lykkjulok
    | LoopS Block

    | BlockS Block
    | BreakS
    | ReturnS Syntax            
    deriving (Data, Typeable)

squarize, parenize :: String -> String
squarize x = ('[' : x) ++ "]"
parenize x = ('(' : x) ++ ")"

type Block = [Syntax]

-- Corresponds to a single staðvær or innflutt statement.
data VariableDecl
    = LocalVarDecl [(VarName, Maybe Syntax)]  -- staðvær name := expr
    | ImportVarDecl [VarName]                -- innflutt name
    deriving Show

variableDeclNames :: VariableDecl -> [VarName]
variableDeclNames (LocalVarDecl xs) = map fst xs
variableDeclNames (ImportVarDecl xs) = xs

data FunctionDecl = FunctionDecl
    { fnInOutParams :: [VarName]
    , fnInParams :: [VarName]
    , fnVariables :: [VariableDecl]
    , fnBody :: Block
    } deriving Show

data ExportDecl
    = ExportAgainDecl Name
    | ExportVarDecl
    | ExportFunDecl FunctionDecl
    deriving Show

data ModuleDecl
    = ModuleDecl [(Name, ExportDecl)]
    | NamedModule Name
    | RecursiveModule ModuleDecl
    | CombinedModule ModuleDecl Name ModuleDecl
    deriving Show

type Program = [ProgramStatement]

data ProgramStatement
    = ModuleAssign Name ModuleDecl
    | ModuleFrom Name Name ModuleDecl
    deriving Show

deepModifyAST :: (Syntax -> Syntax) -> (Syntax -> Syntax)
deepModifyAST f = everywhere (mkT f)

functionImports :: FunctionDecl -> ([VarName], [FunName])
functionImports FunctionDecl {..} =
    partitionEithers . concatMap f . concatMap subexpressions $ fnBody
    where
        f (FunRefS n _) = fun n
        f (VarRefS n) = var n
        f (AssignS n _) = var n
        f (FunCallS n _ _) = fun n 
        f (OperatorS n _ _) = fun n
        f (OperatorCallS n _) = fun n
        f (OperatorUnaryS n _) = fun n
        f _ = []
        filterLocals constr n
            | n `elem` localNames = []
            | otherwise = [constr n]
        fun = filterLocals Right
        var = filterLocals Left
        localNames = fnInOutParams ++ fnInParams ++ concatMap variableDeclNames fnVariables

subexpressions :: Syntax -> [Syntax]
subexpressions = listify (const True)

-- Sadly, this can't be automated.
instance Show Syntax where
    showList xs p = p ++ intercalate "," (map show xs)
    
    show (LiteralS l) = show l
    show (FunRefS n (io, i)) = "stef " ++ n ++ parenize (show io ++ ";" ++ show i)
    show (VarRefS n) = n
    show (ListS xs) = squarize (show xs)
    show (NotS x) = "ekki " ++ parenize (show x)
    show (AndS x y) = parenize (show x) ++ " og " ++ parenize (show y)
    show (OrS x y) = parenize (show x) ++ " eða " ++ parenize (show y)
    show (CaseS scrutinee cases defCase) =
        "val " ++ show scrutinee ++ concatMap showCase cases ++ maybe "" showDefCase defCase ++ " vallok " where
            showCase (r, code) = " kostur " ++ intercalate "," (map showRange r) ++ " þá " ++ show code
            showRange (x, y) = show x ++ maybe "" ((".."++).show) y
            showDefCase code = " annars " ++ show code
    show (IfS cond then' elseifs else') =
        "ef " ++ show cond ++ " þá " ++ show then' ++ concatMap showElseIf elseifs ++ maybe "" showElse else' ++ " eflok " where
            showElseIf (condition, code) = " annarsef " ++ show condition ++ " þá " ++ show code
            showElse code = " annars " ++ show code
    show (WhileLoopS cond code) = "meðan " ++ show cond ++ show (LoopS code)
    show (ForLoopS inits cond incs code) = "fyrir(" ++ show inits ++ ";" ++ show cond ++ ";" ++ show incs ++ ") " ++ show (LoopS code)
    show (LoopS code) = " lykkja " ++ show code ++ " lykkjulok "
    show (BlockS code) = " stofn " ++ show code ++ " stofnlok "
    show BreakS = "út "
    show (ReturnS expr) = " skila " ++ show expr
    show (FunCallS n io i) = n ++ parenize (show io ++ ";" ++ show i)
    show (OperatorS op left right)
        | take 1 op `isPrefixOf` operatorAlphabet =
            parenize $ show left ++ " " ++ op ++ " " ++ show right
        | otherwise = parenize $ show left ++ " \\" ++ op ++ " " ++ show right
    show (OperatorCallS op args) = parenize $ op ++ "(;" ++ show args ++ ")"
    show (OperatorUnaryS op arg) = parenize $ "\\" ++ op ++ " " ++ show arg
    show (GetterS arr i) = parenize $ show arr ++ squarize (show i)
    show (SetterS arr i val) = parenize $ show arr ++ squarize (show i) ++ " := " ++ show val
    show (AssignS n val) = parenize $ n ++ " := " ++ show val

