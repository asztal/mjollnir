{-# LANGUAGE ScopedTypeVariables #-}

module Kjarni (kjarni, ut, strengir, snua, skrifalin, lesalinu, inn) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)

import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Convertible (convert)
import Data.Int (Int32)
import Data.Word (Word8, Word16, Word32)
import System.Exit (exitWith, ExitCode(..))
import System.Mem (performGC)

import ModuleHelpers

kjarni, ut, snua, strengir, skrifalin :: Compiler IModule

inn = simpleModule "INN"
    [ unimplemented "lesa" (0,0)
    -- Clashes with LESALINU
    -- , unimplemented "lesalínu" (0,0)
    , unimplemented "lesastaf" (0,0)
    , unimplemented "næstistafur" (0,0)
    ]

------------------------------------------------------------------------------

lesalinu = simpleModule "LESALINU" [ unimplemented "lesalínu" (0,0) ]

------------------------------------------------------------------------------

skrifalin = simpleModule "SKRIFALIN" [ fun "skrifalínu" skrifalínu ]

skrifalínu T0 (T1 x) = do
    skrifastreng T0 (T1 x)
    liftIO $ putStrLn ""
    return Nil

------------------------------------------------------------------------------

snua = simpleModule "SNUA" [ fun "snúa" snúa ]

snúa T0 (T1 p@(Pair _ _)) = makeList . reverse =<< fromList p
    where
        fromList :: Value -> Eval [Value]
        fromList (Pair x xs) = 
            (:) <$> readMValue x <*> (fromList =<< readMValue xs)
        fromList _ = return []  -- Yes, snúa discards the last part of an
                                -- improper list.
                                -- The moral of the story is not to use
                                -- snúa on improper lists.

        makeList [] = return Nil
        makeList (x:xs) = Pair <$> newMValue x <*> (newMValue =<< makeList xs)
snúa T0 (T1 x) = expectedPair x   

------------------------------------------------------------------------------

strengir = simpleModule "STRENGIR"
    [ unimplemented "erstafurístreng" (0,2)
    , unimplemented "lengd" (0,1)
    , unimplemented "strengskeyta" (0,2)
    , unimplemented "hlutstrengur" (0,3)
    , unimplemented "strengurístreng" (0,3)
    ]

------------------------------------------------------------------------------

ut = simpleModule "UT"
    [ fun "nýlína" nýlína
    , fun "skrifa" skrifa
    , fun "skrifafjöl" skrifafjöl
    , fun "skrifastreng" skrifastreng
    ]

nýlína T0 T0 = do
    liftIO $ putStrLn ""
    return Nil

skrifa T0 (T1 x) = do
    case x of
        Word x -> liftIO . putStr . show =<< signed (Word x)
        Str arr -> liftIO $ do
            putStr "\""
            putStr =<< map (toEnum . fromIntegral) . drop 1 <$> getElems arr
            putStr "\""
        _ -> liftIO . putStr =<< mshow x
    return Nil

skrifafjöl T0 (T1 x) = do
    liftIO . putStr . show =<< unsigned x
    return Nil

skrifastreng T0 (T1 x) = do
    arr <- string x
    liftIO $ do
        putStr "\""
        putStr =<< map (toEnum . fromIntegral) . drop 1 <$> getElems arr
        putStr "\""
    return Nil

------------------------------------------------------------------------------

kjarni = simpleModule "KJARNI"
    [
      fun "!" not'
    , fun "%" natRem
    , fun "%%" intRem
    , fun "&" $ bitwiseOp (.&.)
    , fun "*" $ wordBinOp unsigned (*)
    , fun "**" $ wordBinOp signed (*)
    , fun "***" $ realBinOp (*)
    , fun "+" $ wordBinOp unsigned (+)
    , fun "++" $ wordBinOp signed (+)
    , fun "+++" $ realBinOp (+)
    , fun "-" $ wordBinOp unsigned (-)
    , fun "--" $ wordBinOp signed (-)
    , fun "---" $ realBinOp (-)
    , fun "/" natQuot
    , fun "//" intQuot
    , fun "///" $ realBinOp (/)
    , fun ":" cons
    , fun "<" $ lessThan compareUnsigned
    , fun "<<" $ lessThan compareSigned
    , fun "<<<" $ lessThan compareFloats
    , fun "<=" $ lessThanEqual compareUnsigned
    , fun "<=<=" $ lessThanEqual compareSigned
    , fun "<=<=<=" $ lessThanEqual compareFloats
    , fun "<>" $ notEqual compareUnsigned
    , fun "<><>" $ notEqual compareSigned
    , fun "<><><>" $ notEqual compareFloats
    , fun "=" $ equalTo compareUnsigned
    , fun "==" $ equalTo compareSigned
    , fun "===" $ equalTo compareFloats
    , fun ">" $ greaterThan compareUnsigned
    , fun ">>" $ greaterThan compareSigned
    , fun ">>>" $ greaterThan compareFloats
    , fun ">=" $ greaterThanEqual compareUnsigned
    , fun ">=>=" $ greaterThanEqual compareSigned
    , fun ">=>=>=" $ greaterThanEqual compareFloats
    , fun "|" $ bitwiseOp (.|.)
    , fun "||" $ bitwiseOp xor
    , tbi "brjóta" (0,1)
    , fun "brot" brot
    , tbi "bætfylla" (0,1)
    , fun "erfleyt" erfleyt
    , fun "erfleytneikvæd" erfleytneikvæd
    , fun "erfleytnúll" erfleytnúll
    , fun "erhlunkur" erhlunkur
    , fun "erpar" erpar
    , fun "erstef" erstef
    , fun "erstrengur" erstrengur
    , fun "ertala" ertala
    , fun "ertóm" ertóm
    , fun "fdeiling" fdeiling
    , tbi "fjöltilfleyt" (0,1)
    , fun "fleytitala" fleytitala
    , fun "fleytmínus" fleytmínus
    , fun "fleyttilfjöl" fleyttilfjöl
    , tbi "flytja" (0,5)
    , fun "fmargfeldi" fmargfeldi
    , fun "fmismunur" fmismunur
    , fun "formerki" formerki
    , fun "fsumma" fsumma
    , fun "fylkissetja1" fylkissetja1
    , fun "fylkissækja1" fylkissækja1
    , tbi "fylla" (0,4)
    , fun "halasetja" halasetja
    , fun "hali" hali
    , fun "haus" haus
    , fun "haussetja" haussetja
    , fun "hábæti" hábæti
    , tbi "hdeiling" (2,3)
    , tbi "hhliðra" (0,2)
    , fun "hlunksetja" hlunksetja
    , fun "hlunkstærð" hlunkstærð
    , fun "hlunksækja" hlunksækja
    , fun "hlunkur" hlunkur
    , tbi "hmargfeldi" (2,2)
    , tbi "hmismunur" (2,2)
    , tbi "hsumma" (2,2)
    , fun "hætta" hætta
    , tbi "innbæti" (0,1)
    , fun "innfjöldi" innfjöldi
    , fun "innútfjöldi" innútfjöldi
    , tbi "ígrip10" (4,0)
    , tbi "kafli" (0,1)
    , tbi "kíkjabæti" (0,2)
    , tbi "kíkjaorð" (0,2)
    , tbi "laus_K" (0,0)
    , fun "lágbæti" lágbæti
    , tbi "lesastaf" (0,0)
    , fun "minnka" minnka
    , tbi "msdos" (10,0)
    , tbi "orðfylla" (0,4)
    , fun "RSskilti" rsskilti
    , fun "safna" safna
    , fun "skalsafna" skalsafna
    , tbi "skrifastaf" (0,1)
    , tbi "strengflytja" (0,5)
    , tbi "strengfylla" (0,4)
    , fun "strengsetjabæti" strengsetjabæti
    , fun "strengsetjaorð" strengsetjaorð
    , fun "strengstærð" strengstærð
    , fun "strengsækjabæti" strengsækjabæti
    , fun "strengsækaorð" strengsækjaorð
    , fun "strengur" strengur
    , fun "stækka" stækka
    , tbi "útbæti" (0,2)
    , fun "veldi" veldi
    , tbi "vhliðra" (0,2)
    , tbi "vistfang" (0,1)
    ]
    where tbi = unimplemented

not' T0 (T1 x) = return (convert (not (convert x)))

natRem T0 (T2 _ (Word 0)) = throw $ "Divide by zero error"
natRem T0 (T2 x y) = wordBinOp unsigned rem T0 (T2 x y)

intRem T0 (T2 _ (Word 0)) = throw $ "Divide by zero error"
intRem T0 (T2 x y) = wordBinOp signed rem T0 (T2 x y)

natQuot T0 (T2 _ (Word 0)) = throw $ "Divide by zero error"
natQuot T0 (T2 x y) = wordBinOp unsigned quot T0 (T2 x y)

intQuot T0 (T2 _ (Word 0)) = throw $ "Divide by zero error"
intQuot T0 (T2 x y) = wordBinOp signed quot T0 (T2 x y)

brot T0 (T1 x) = do
    x <- real x
    -- Fjölnir represents floats as (1+b/65536)*2^w, not (c/65536)*(2^(w+1)).
    let b = truncate ((significand x * 2 - 1) * 65536) - 1
    -- Why this is also necessary, I am not sure.
    return . Word . (if x < 0 then negate else id) $ b + 1

cons T0 (T2 x y) = Pair <$> newMValue x <*> newMValue y

erfleyt T0 (T1 (Real _)) = return $ convert True
erfleyt _ _ = return $ convert False

erfleytneikvæd T0 (T1 (Real x)) = return $ convert (x < 0)
erfleytneikvæd _ _ = return $ convert False

erfleytnúll T0 (T1 (Real 0)) = return $ convert True
erfleytnúll _ _ = return $ convert False

erhlunkur T0 (T1 (Array _)) = return $ convert True
erhlunkur _ _ = return $ convert False

erpar T0 (T1 (Pair _ _)) = return $ convert True
erpar _ _ = return $ convert False

erstef T0 (T1 (Fun _)) = return $ convert True
erstef _ _ = return $ convert False

erstrengur T0 (T1 (Str _)) = return $ convert True
erstrengur _ _ = return $ convert False

ertala T0 (T1 (Word _)) = return $ convert True
ertala T0 (T1 (Real _)) = return $ convert True
ertala _ _ = return $ convert False

ertóm T0 (T1 Nil) = return $ convert True
ertóm _ _ = return $ convert False

-- TODO: Find out why hdeiling actually does something else.
-- (It puts the quotient in a, and the remainder in b.)
fdeiling (T2 a b) (T3 ax ay az) = do
    z <- word32 az
    x <- word32 ax
    y <- word32 ay
    let xy = (x `shiftL` 16) .|. y
    let ab = xy `quot` z
    writeMValue a . Word . fromIntegral $ ab `shiftR` 16
    writeMValue b . Word . fromIntegral $ ab .&. 0xFFFF
    return Nil

fleytitala T0 (T3 f b v) = do
    f <- unsigned f
    b <- unsigned b
    v <- signed v
    return $ Real $ (if f /= 0 then negate else id) ((1+fromIntegral b/65536) * (2**fromIntegral v))

fleytmínus T0 (T1 x) = Real . negate <$> real x

fleyttilfjöl T0 (T1 x) = do
    x <- real x
    let x' :: Int32 = truncate (abs x)
    if x' > 65535
        then return (Word 0)
        else return (Word $ fromIntegral x')

fmargfeldi (T2 a b) (T2 ax ay) = do
    x <- word32 ax
    y <- word32 ay
    writeSplitWord32 a b (x * y)
    return Nil

fmismunur (T2 a b) (T2 ax ay) = do
    x <- unsigned ax
    y <- unsigned ay
    writeMValue a $ Word . fromIntegral $ (max x y) - (min x y)
    writeMValue b $ if x > y then Word 0 else Word 1
    return Nil

formerki T0 (T1 x) = do
    x <- real x
    if x >= 0 && not (isNegativeZero x)
        then return (Word 0)
        else return (Word 1)

fsumma (T2 a b)  (T2 ax ay) = do
    x <- word32 ax
    y <- word32 ay
    writeSplitWord32 a b (x + y)
    return Nil

fylkissetja1 T0 (T3 (Str s) i g) = do
    b <- liftIO $ getBounds s
    i <- unsigned i
    g' <- char g
    when (inRange b i) $ liftIO $ writeArray s i g'
    return g
fylkissetja1 T0 (T3 (Array a) i g) = do
    i <- unsigned i
    b <- liftIO $ getBounds a
    when (inRange b i) $ liftIO $ writeArray a i g
    return g
fylkissetja1 T0 (T3 x _ _) = do
    throw $ "fylkissetja1: expected array or string, given " ++ show x

fylkissækja1 T0 (T2 (Str s) i) = do
    b <- liftIO $ getBounds s
    i <- unsigned i
    if inRange b i
        then liftIO $ Word . fromIntegral <$> readArray s i
        else return Nil
fylkissækja1 T0 (T2 (Array a) i) = do
    b <- liftIO $ getBounds a
    i <- unsigned i
    if inRange b i
        then liftIO $ readArray a i
        else return Nil
fylkissækja1 T0 (T2 x _) = do
    throw $ "fylkissækja1: expected array or string, given " ++ show x

halasetja T0 (T2 (Pair _ b) x) = writeMValue b x >> return Nil
halasetja T0 (T2 x _) = expectedPair x

hali T0 (T1 (Pair _ b)) = readMValue b
hali T0 (T1 x) = expectedPair x

haus T0 (T1 (Pair a _)) = readMValue a
haus T0 (T1 x) = expectedPair x

hábæti T0 (T1 x) = mapWord (`shiftR` 8) x

hætta T0 (T1 x) = liftIO . exitWith . ExitFailure =<< signed x

haussetja T0 (T2 (Pair a _) x) = writeMValue a x >> return Nil
haussetja T0 (T2 x _) = expectedPair x

-- TODO: The official implementation seems to have no problem reading and
-- writing outside the bounds of the array, and according to hlunkstærð it
-- didn't grow the array. Perhaps it's doing something weird, or maybe I
-- just got lucky.
hlunksetja T0 (T3 x y g) = do
    x <- array x
    y <- unsigned y
    bounds <- liftIO $ getBounds x
    when (inRange bounds y) $ liftIO $ writeArray x y g
    return Nil

hlunkstærð T0 (T1 arr) = do
    arr <- array arr
    Word . fromIntegral . rangeSize <$> liftIO (getBounds arr)

hlunksækja T0 (T2 x y) = do
    x <- array x
    y <- unsigned y
    bounds <- liftIO (getBounds x)
    if inRange bounds y
        then liftIO $ readArray x y
        else return Nil

hlunkur T0 (T1 x) = do
    x <- unsigned x
    Array <$> liftIO (newArray (0, x-1) Nil)

innfjöldi = funArityBy snd
innútfjöldi = funArityBy fst

lágbæti T0 (T1 x) = mapWord (.&. 0xFF) x

minnka T0 (T1 (Word x)) = return $ Word (x-1)
minnka T0 (T1 x) = expectedWord x

-- Whether or not the implementation displays "RS" at the top-right corner of
-- the screen during garbage collection.
rsskilti T0 (T1 _) = return $ convert False

safna T0 T0 = liftIO performGC >> return Nil

-- Whether or not GC is enabled.
skalsafna T0 (T1 _) = return $ convert True

stækka T0 (T1 (Word x)) = return $ Word (x+1)
stækka T0 (T1 x) = expectedWord x

strengsetjabæti T0 (T3 vs vi vx) = do
    arr <- string vs
    i <- unsigned vi
    x <- word vx
    (_, len) <- liftIO $ getBounds arr
    liftIO $ writeArray arr (i `mod` succ len) $ fromIntegral x
    return Nil
    
strengsetjaorð T0 (T3 vs vi vx) = do
    arr <- string vs
    i <- unsigned vi
    x <- word vx
    (_, len) <- liftIO $ getBounds arr
    liftIO $ writeArray arr (i `mod` succ len) $ fromIntegral (x .&. 0xFF)
    case succ i `mod` succ len of
        0 -> return Nil
        i' -> do
            liftIO $ writeArray arr i' $ fromIntegral (x `shiftR` 8)
            return Nil

strengstærð T0 (T1 s) = do
    bounds <- liftIO . getBounds =<< string s
    return $ Word (fromIntegral (rangeSize bounds - 1))

-- If x is outside the range of the string, its remainder module the string length is taken.
strengsækjabæti T0 (T2 s x) = do
    arr <- string s
    (_, len) <- liftIO $ getBounds arr
    i <- unsigned x
    Word . fromIntegral <$> liftIO (readArray arr (i `mod` succ len))

strengsækjaorð T0 (T2 s x) = do
    -- x is a byte position.
    -- reading at the end of the string lets the high byte of the result to 0.
    arr <- string s
    (_, len) <- liftIO $ getBounds arr
    i <- unsigned x
    lowByte <- liftIO $ readArray arr (i `mod` succ len)
    highByte <- case succ i `mod` succ len of
        0 -> pure 0
        i' -> liftIO $ readArray arr i'
    return . Word $ (fromIntegral highByte `shiftL` 8) .|. fromIntegral lowByte

strengur T0 (T1 x) = do
    len :: Int <- unsigned x
    arr <- liftIO $ newArray (0, fromIntegral len) 0
    liftIO $ writeArray arr 0 (fromIntegral len)
    return $ Str arr

-- Fjölnir represents floats as (1+b/65536)*2^w, not (c/65536)*(2^(w+1)).
veldi T0 (T1 x) = Word . fromIntegral . subtract 1 . exponent <$> real x

------------------------------------------------------------------------------

expectedWord x =   throw $ "Expected 16-bit integer, given " ++ show x
expectedWordU x =  throw $ "Expected (unsigned) 16-bit integer, given " ++ show x
expectedWordS x =  throw $ "Expected (signed) 16-bit integer, given " ++ show x
expectedFloat x =  throw $ "Expected floating-point number, given " ++ show x
expectedPair x =   throw $ "Expected pair, given " ++ show x
expectedString x = throw $ "Expected string, given " ++ show x
expectedFun x =    throw $ "Expected function, given " ++ show x
expectedArray x =  throw $ "Expected array, given " ++ show x
expectedChar x =   throw $ "Expected character, given " ++ show x

------------------------------------------------------------------------------

lessThan cmp T0 (T2 x y) = convert . (== LT) <$> cmp x y
greaterThan cmp T0 (T2 x y) = convert . (== GT) <$> cmp x y
lessThanEqual cmp T0 (T2 x y) = convert . not . (== GT) <$> cmp x y
greaterThanEqual cmp T0 (T2 x y) = convert . not . (== LT) <$> cmp x y
equalTo cmp T0 (T2 x y) = convert . (== EQ) <$> cmp x y
notEqual cmp T0 (T2 x y) = convert . not . (== EQ) <$> cmp x y

------------------------------------------------------------------------------

signed, unsigned :: Value -> Eval Int
char :: Value -> Eval Word8
word :: Value -> Eval Word16
word32 :: Value -> Eval Word32
signed32 :: Value -> Eval Int32
real :: Value -> Eval Double
wordBinOp :: (Value -> Eval Int) -> (Int -> Int -> Int) -> T0 IVar -> T2 Value -> Eval Value

signed (Word x) = let i = fromIntegral x :: Int
    in return $ if i > 32767 then i - 65536 else i
signed x = expectedWordS x

unsigned (Word x) = return (fromIntegral x)
unsigned x = expectedWordU x

char (Word x) = return $ fromIntegral x
char x = expectedChar x

word (Word x) = return x
word x = expectedWord x

word32 (Word x) = return (fromIntegral x)
word32 x = expectedWord x

signed32 x = fromIntegral <$> signed x

real (Real x) = return x
real x = expectedFloat x

string (Str x) = return x
string x = expectedString x

stef (Fun x) = return x
stef x = expectedFun x

array (Array x) = return x
array x = expectedArray x


realBinOp f T0 (T2 x y) = fmap Real . f <$> real x <*> real y
wordBinOp extract f T0 (T2 x y) = fmap (Word . fromIntegral) . f <$> extract x <*> extract y
bitwiseOp f T0 (T2 x y) = case (x, y) of
    (Word a, Word b) -> return $ Word (f a b)
    (Word _, b) -> expectedWord b
    (a, _) -> expectedWord a

mapWord f (Word x) = return $ Word (f x)
mapWord _ x = expectedWord x

compareValuesBy :: Ord a => (Value -> Eval a) -> Value -> Value -> Eval Ordering
compareValuesBy f x y = compare <$> f x <*> f y

compareSigned = compareValuesBy signed
compareUnsigned = compareValuesBy unsigned
compareFloats = compareValuesBy real

funArityBy selector T0 (T1 x) = do
    f <- stef x
    Word . fromIntegral . selector <$> getArity f
    where
        getArity (INamedFun _ ar) = return ar
        getArity (INativeFun _ ar _) = return ar
        getArity (IResolvedFun _ r) = funArity <$> readMValue r

------------------------------------------------------------------------------

writeSplitWord32 a b w = do
    writeMValue a (Word $ fromIntegral $ w `shiftR` 16)
    writeMValue b (Word $ fromIntegral $ w .&. 0xFFFF)
