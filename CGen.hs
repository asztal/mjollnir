module Main where

import Data.List (intercalate)

import Exp
import IR
import Var
import Types

sampleExp = AndE (WordE 2) (OrE (NotE (RealE 7.0)) (IfE (ReadE (CLocalVar 5)) (ReadE (CRefArgVar 0)) (WordE 6)))

generateC :: Name -> Arity -> [Instruction Int] -> String
generateC name arity instructions = 
    "Value gen_" ++ name ++ " (" ++ argList ++ ") {\n"
    ++ stackVars
    ++ localVars
    ++ concatMap (uncurry genInstr) (zip [0..] instructions)
    ++ "  I" ++ show (length instructions) ++ ": return S0;\n"
    ++ "}\n"
    where
        argList = intercalate ", " . filter (not . null) $
            [ nameList "Value *R" (fst arity)
            , nameList "Value A" (snd arity)]
        stackVars = "  Value " ++ nameList "S" (stackVarCount instructions) ++ ";\n"
        localVars = case localVarCount instructions of
            0 -> ""
            count -> "  Value " ++ nameList "L" count ++ ";\n"
    
        nameList prefix count = intercalate ", " [ prefix ++ show i | i <- [0..count-1]]
    
        stackVarCount xs = foldr1 max (0 : concatMap getStackRefs xs) + 1
        localVarCount xs = foldr max (-1) (concatMap getLocalRefs xs) + 1
        
        getStackRefs x = concatMap f (varRefs x) where
            f (CStackVar _ i) = [i]
            f _ = []
            
        getLocalRefs x = concatMap f (varRefs x) where
            f (CLocalVar i) = [i]
            f _ = []
    
genInstr :: Int -> Instruction Int -> String
genInstr i instruction = "  I" ++ show i ++ ": " ++ f instruction ++ ";\n" where
    f (WordI v w) = showVar v ++ " = makeWord(" ++ show w ++ ")"
    f (StringI v s) = showVar v ++ " = makeString(" ++ show s ++ ")"
    f (NilI v) = showVar v ++ " = nil"
    f (RealI v x) = showVar v ++ " = makeReal(" ++ show x ++ "d)"
    f (AssignI u v) = showVar u ++ " = " ++ showVar v
    f (JumpI l) = "goto I" ++ show l
    f (ConditionalI v l) = "if (" ++ showVar v ++ ") goto I" ++ show l
    f _ = "/* TODO */"
    
    showVar (CNamedVar _) = error "CNamedVar present at C generation stage"
    showVar (CLocalVar i) = "L" ++ show i
    showVar (CArgVar i) = "A" ++ show i
    showVar (CRefArgVar i) = "*R" ++ show i
    showVar (CStackVar Direct i) = "S" ++ show i
    showVar (CStackVar Indirect i) = "*S" ++ show i
    showVar (CModuleVar id) = error "Can't do CModuleVar yet" 

main = putStrLn $ "#include \"mjollnir.h\"\n\n" ++ generateC "sample" (2,3) (compileToIR sampleExp)
