{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor,
             RankNTypes #-}

module IR {-(
    Instruction(..),
    generateIR
    ) -}where

import Control.Applicative
import Control.Monad (mapM, ap)
import Control.Monad.ST
import Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Data.STRef
import Data.Word (Word16)

import Exp
import Types
import Var
import Located

instance Applicative (ST s) where
    pure = return
    (<*>) = ap

-- IR, like Exp, represents the control flow of a function,
-- but transformed into an imperative style intermediate
-- representation that is easier to compile to byte code.
-- 
-- IR does not need to be generalised to work with both IVar 
-- and CVar because only the compiler uses this representation.
data Instruction label
    = WordI CVar Word16
    | StringI CVar String
    | NilI CVar
    | RealI CVar Double
    
    | AssignI CVar CVar
    | JumpI label
    | ConditionalI CVar label
    | CallI (Maybe CVar) CFun [CVar] [CVar]
    | CallVI (Maybe CVar) CVar [CVar] [CVar] 
    deriving (Show)

newtype Label s = Label (STRef s Int) 

newtype GenIR s a = GenIR { unGenIR :: StateT (GenState s) (ST s) a } 
    deriving (Monad, Applicative, Functor)
    
data GenState s = GenState 
    { gsInstructions :: S.Seq (Instruction (Label s)) }

runGenIR :: (forall s. GenIR s a) -> [Instruction Int]
runGenIR x = runST (do
    gs <- execStateT (unGenIR x) (GenState S.empty)
    mapM resolveLabel (F.toList (gsInstructions gs)))

resolveLabel :: Instruction (Label s) -> ST s (Instruction Int)
resolveLabel (JumpI (Label r)) = JumpI <$> readSTRef r
resolveLabel (ConditionalI v (Label r)) = ConditionalI v <$> readSTRef r
resolveLabel (WordI v x) = return $ WordI v x
resolveLabel (RealI v x) = return $ RealI v x
resolveLabel (StringI v x) = return $ StringI v x
resolveLabel (NilI v) = return $ NilI v
resolveLabel (AssignI u v) = return $ AssignI u v
resolveLabel (CallI r f vs vs') = return $ CallI r f vs vs'
resolveLabel (CallVI r v vs vs') = return $ CallVI r v vs vs'

generateIR :: Exp CVar CFun -> CVar -> GenIR s ()
generateIR expr output = case expr of
    NilE -> do
        write $ NilI output
    AndE x y -> do
        x' <- stackVar
        generateIR x x'
        end <- label
        skip <- label
        write $ ConditionalI x' skip
        write $ AssignI x' output
        write $ JumpI end
        defineLabel skip
        generateIR y output
        defineLabel end
        
    _ -> return ()

label :: GenIR s (Label s)
label = GenIR . lift $ (Label <$> newSTRef (-1))

stackVar :: GenIR s CVar
stackVar = return (CNamedVar (genLoc "<stackVar>"))

retVal :: GenIR s CVar
retVal = return (CNamedVar (genLoc "<retVal>"))

write :: Instruction (Label s) -> GenIR s ()
write instr = GenIR . modify $ \ gs -> 
    gs { gsInstructions = gsInstructions gs S.|> instr }

-- TODO: get the current writer position and use that.
defineLabel :: Label s -> GenIR s ()
defineLabel (Label r) = do
    instrs <- GenIR $ gets gsInstructions
    GenIR . lift $ writeSTRef r (S.length instrs)

infiniteLoop = do
    x <- label
    defineLabel x
    write $ JumpI x

branchTest = do
    skip <- label
    end <- label
    write $ ConditionalI (CLocalVar 0) skip
    write $ CallI Nothing (CNamedFun (genLoc "puts") (0,1)) [] [CLocalVar 1]
    write $ JumpI end
    defineLabel skip
    write $ CallI Nothing (CNamedFun (genLoc "puts") (0,1)) [] [CLocalVar 2]
    defineLabel end
