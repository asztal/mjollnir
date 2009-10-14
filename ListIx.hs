module ListIx
    ( ListIx (..)
    , module Data.Ix
    ) where

import Data.Ix

-- | An Ix instance for N-dimensional arrays, where N is not known at
-- | compile-time. It works like the (Ix a, Ix b, ...) => Ix (a, b, ...) instances
-- | but for an unknown amount of dimensions, and all dimensions must use the same Ix
-- | instance.
newtype ListIx a = ListIx [a]
    deriving (Show, Eq, Ord)

instance (Ix a, Enum a) => Ix (ListIx a) where
    range (ListIx from, ListIx to)
        | length from /= length to = error "ListIx.range: from/to have different lengths"
        | otherwise = map ListIx . sequence $ zipWith enumFromTo from to
    rangeSize (ListIx from, ListIx to)
        | length from /= length to = error "ListIx.rangeSize: from/to have different lengths"
        | otherwise = product $ zipWith (curry rangeSize) from to
    inRange (ListIx from, ListIx to) (ListIx x)
        | lf /= lx || lt /= lx = error "ListIx.inRange: lists have different lengths"
        | otherwise = and $ zipWith inRange (zip from to) x
            where (lf, lt, lx) = (length from, length to, length x)
    index (ListIx from, ListIx to) (ListIx x)
        | lf /= lx || lt /= lx = error "ListIx.index: lists have different lengths"
        | otherwise = sum $ zipWith (*) mults ixs
            where
                (lf, lt, lx) = (length from, length to, length x)
                sizes = zipWith (curry rangeSize) from to
                mults = tail (scanr1 (*) sizes) ++ [1]
                ixs = zipWith (curry (pred . rangeSize)) from x
