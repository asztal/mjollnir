module Language
    ( alphabet
    , operatorAlphabet
    , reservedNames
    , reservedOperators
    ) where

alphabet = ['a'..'z'] ++ "þæöðáéýúíó" ++ ['A'..'Z'] ++ "ÞÆÖÐÁÝÚÍÓ_"
operatorAlphabet = "+-*/%!&=?<>|:^@"

reservedOperators = words "-> := ( ) [ ] { } , ; .."
reservedNames = words "annars annarsef breyta eða eflok ef ekki \
    \ fyrir innflutt kostur lykkja lykkjulok meðan og skila staðvær \
    \ stef stofn stofnlok úr út val vallok þá"
