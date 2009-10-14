module Types
    ( Name
    , VarName
    , FunName
    , Arity
    ) where

type Name = String
type VarName = Name
type FunName = Name
type Arity = (Integer, Integer)
