{-# LANGUAGE ScopedTypeVariables #-}

module ModuleHelpers
    ( simpleModule
    , IModule, IExport
    , var, fun, fun', unimplemented
    , NativeFun, Args
    , T0(T0), T1(T1), T2(T2), T3(T3), T4(T4)
    , T5(T5), T6(T6), T7(T7), T8(T8), T9(T9), T10(T10)
    , module Eval
    , module Compiler
    , module Types
    , module Module
    ) where

import Control.Applicative
import qualified Data.Map as M

import Eval
import Module
import Compiler
import Types
import Var

type IExport = Export (IORef Value) IFun
type IModule = Module (IORef Value) IVar IFun
type NativeFun = [IVar] -> [Value] -> Eval Value

simpleModule :: FunR r v f => Name -> [Compiler (LName, Export r f)] -> Compiler (Module r v f)
simpleModule name exports = Module (unknownSpan $ "module \"" ++ name ++ "\"") . M.fromList <$> sequence exports

var :: Name -> Compiler (LName, IExport)
var name = (,) (genLoc name) . ExportVar <$> newMValue Nil

unimplemented :: Name -> Arity -> Compiler (LName, IExport)
unimplemented name _ = 
    return (genLoc name, ExportFun (INativeFun (-1, -1) func)) where
        func _ _ = throw $ "Unimplemented function used: " ++ name

class Args t where
    args :: [a] -> t a
    argCount :: t a -> Integer

-- Feel the power of Haskell. Actually, I'm just too lazy to learn Template Haskell.
data T0 a = T0
data T1 a = T1 a
data T2 a = T2 a a
data T3 a = T3 a a a
data T4 a = T4 a a a a
data T5 a = T5 a a a a a
data T6 a = T6 a a a a a a
data T7 a = T7 a a a a a a a
data T8 a = T8 a a a a a a a a
data T9 a = T9 a a a a a a a a a
data T10 a = T10 a a a a a a a a a a

-- I'm too lazy to use template haskell.
instance Args T0 where
    args [] = T0
    args _ = error "Expected no arguments"
    argCount _ = 0

instance Args T1 where
    args [a] = T1 a
    args _ = error "Expected 1 argument"
    argCount _ = 1

instance Args T2 where
    args [a, b] = T2 a b
    args _ = error "Expected 2 arguments"
    argCount _ = 2

instance Args T3 where
    args [a, b, c] = T3 a b c
    args _ = error "Expected 3 arguments"
    argCount _ = 3

instance Args T4 where
    args [a, b, c, d] = T4 a b c d
    args _ = error "Expected 4 arguments"
    argCount _ = 4

instance Args T5 where
    args [a, b, c, d, e] = T5 a b c d e
    args _ = error "Expected 5 arguments"
    argCount _ = 5

instance Args T6 where
    args [a, b, c, d, e, f] = T6 a b c d e f
    args _ = error "Expected 6 arguments"
    argCount _ = 6

instance Args T7 where
    args [a, b, c, d, e, f, g] = T7 a b c d e f g
    args _ = error "Expected 7 arguments"
    argCount _ = 7

instance Args T8 where
    args [a, b, c, d, e, f, g, h] = T8 a b c d e f g h
    args _ = error "Expected 8 arguments"
    argCount _ = 8
    
instance Args T9 where
    args [a, b, c, d, e, f, g, h, i] = T9 a b c d e f g h i
    args _ = error "Expected 9 arguments"
    argCount _ = 9
    
instance Args T10 where
    args [a, b, c, d, e, f, g, h, i, j] = T10 a b c d e f g h i j
    args _ = error "Expected 10 arguments"
    argCount _ = 10

fun :: forall io i. (Args io, Args i) => Name -> (io IVar -> i Value -> Eval Value) -> Compiler (LName, IExport)
fun name body = do
    let inArity = argCount (undefined :: i Value)
        inoutArity = argCount (undefined :: io IVar)
        func io i = body (args io) (args i)
    return (genLoc name, ExportFun (INativeFun (inoutArity, inArity) func))

fun' :: Name -> Arity -> NativeFun -> Compiler (LName, IExport)
fun' name arity body = return (genLoc name, ExportFun (INativeFun arity body))
