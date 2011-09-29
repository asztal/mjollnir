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
import qualified Data.Array.Unboxed
import Data.Char (chr)
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
    deriving (Monad, Applicative, Functor, MonadState (GenState s))
    
data GenState s = GenState 
    { gsInstructions :: S.Seq (Instruction (Label s)) 
    , gsBreakLabel :: Maybe (Label s)
    , gsReturnLabel :: Label s
    }
    
runGenIR :: (forall s. GenIR s a) -> [Instruction Int]
runGenIR x = runST (do
    end <- Label <$> newSTRef (-1)
    gs <- execStateT
        (unGenIR (x >> defineLabel end))
        (GenState S.empty Nothing end)
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

generateIR :: CVar -> Exp CVar CFun -> GenIR s ()
generateIR output expr = case expr of
    NilE -> do
        write $ NilI output
    WordE w -> write $ WordI output w
    StrE s -> write $ StringI output 
        (map (chr . fromIntegral) (Data.Array.Unboxed.elems s))
    RealE r -> write $ RealI output r
    
    ReadE v -> write $ AssignI output v
    WriteE v e -> do
        generateIR v e
        write $ AssignI v output
    
    AppE (Left v) rs xs -> do
        rs' <- mapM evalRefArg rs
        xs' <- mapM evalValArg xs
        write $ CallVI (Just output) v rs' xs'
    
    AppE (Right f) rs xs -> do
        rs' <- mapM evalRefArg rs
        xs' <- mapM evalValArg xs
        write $ CallI (Just output) f rs' xs'
        
    ManyE xs -> do
        mapM_ (generateIR output) xs
    
    AndE x y -> do
        x' <- stackVar
        generateIR x' x
        end <- label
        skip <- label
        write $ ConditionalI x' skip
        write $ AssignI x' output
        write $ JumpI end
        defineLabel skip
        generateIR output y
        defineLabel end

    OrE x y -> do
        x' <- stackVar
        done <- label
        end <- label
        generateIR x' x
        write $ ConditionalI x' done
        generateIR output y
        write $ JumpI end
        defineLabel done
        write $ AssignI output x'
        defineLabel end
        
    NotE x -> do
        x' <- stackVar
        done <- label
        end <- label
        generateIR x' x
        write $ ConditionalI x' done
        write $ WordI output 1
        write $ JumpI end
        defineLabel done
        write $ NilI output
        defineLabel end
        
    LoopE x -> do
        startOfLoop <- label
        endOfLoop <- label
        defineLabel startOfLoop
        withBreakLabel endOfLoop $ do
            generateIR output x
            write $ JumpI startOfLoop
        defineLabel endOfLoop 
        
    BreakE -> do
        break <- gets gsBreakLabel
        case break of 
            -- TODO: add error handling to GenIR?
            Nothing -> error "\"út\" outside of loop"
            Just l -> write $ JumpI l
    
    -- TODO: IfE/CaseE    
    _ -> return ()
    
    where
        -- TODO: this should change local var references to 
        -- closure references (for Right cases).
        evalRefArg :: Either (Exp CVar CFun) CVar -> GenIR s CVar
        evalRefArg (Right var) = return var
        evalRefArg (Left arg) = do
            x <- stackVar
            generateIR x arg
            return x
        
        evalValArg :: Exp CVar CFun -> GenIR s CVar 
        evalValArg arg = do
            x <- stackVar
            generateIR x arg
            return x
                        
        withBreakLabel :: Label s -> GenIR s () -> GenIR s ()
        withBreakLabel breakLabel action = do
            oldBreakLabel <- gets gsBreakLabel
            modify (\gs -> gs { gsBreakLabel = Just breakLabel })
            action
            modify (\gs -> gs { gsBreakLabel = oldBreakLabel })
             

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
