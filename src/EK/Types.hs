{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- ek type system
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module EK.Types
  ( Type(..)
  , Field(..)
  , atomTy
  , functionTy
  , intTy
  , structTy
  , intRangeTy
  , intRangeFromTy
  , intRangeUpToTy
  , intRangeInfTy
  , convertible
  )
where

import Data.List (intercalate, nub)
import Data.Semigroup (stimes, stimesIdempotentMonoid)
import qualified Data.Range as Range
import Data.Range ((+=*), (+=+), Bound (boundValue))

-- | A type is a list of concrete types
data Type = AnyTy -- ^ The Any type, which can be anything
          | UnionTy UnionType -- ^ A union type, which can be any of the types in the list
          deriving (Eq)

-- | A union type is a list of atoms, functions, integer ranges, and structs
data UnionType = UnionType
  { atoms :: [String] -- ^ A list of atoms
  , functions :: [(Type, Type)] -- ^ A list of functions
  , ints :: [Range.Range Integer] -- ^ A list of integer ranges
  , structs :: [(String, [Field])] -- ^ A list of structs
  } deriving (Eq)

-- | A field is a name and a type
data Field = Field String Type

instance Show Type where
    show AnyTy = "Any"
    show (UnionTy ts) = show ts

instance Show UnionType where
    show (UnionType atoms functions ints structs) =
        intercalate " | " (atoms ++ map showFn functions ++ map (showRange . normalizeRange) ints ++ map fst structs)

showFn :: (Type, Type) -> String
showFn (arg, ret) = "(" ++ show arg ++ " -> " ++ show ret ++ ")"

normalizeRange :: Range.Range Integer -> Range.Range Integer
normalizeRange (Range.SingletonRange a) = Range.SingletonRange a
normalizeRange (Range.SpanRange l u) = span (normalizeLBound l) (normalizeUBound u)
  where span l u | l == u = Range.SingletonRange l
                 | otherwise = l +=+ u
normalizeRange (Range.LowerBoundRange l) = Range.lbi (normalizeLBound l)
normalizeRange (Range.UpperBoundRange u) = Range.ubi (normalizeUBound u)
normalizeRange Range.InfiniteRange = Range.InfiniteRange

showRange :: Range.Range Integer -> String
showRange (Range.SingletonRange a) = show a
showRange (Range.SpanRange l u) = "[" ++ show (boundValue l) ++ ".." ++ show (boundValue u) ++ "]"
showRange (Range.LowerBoundRange l) = "[" ++ show (boundValue l) ++ "..]"
showRange (Range.UpperBoundRange u) = "[.." ++ show (boundValue u) ++ "]"
showRange Range.InfiniteRange = "[..]"

normalizeLBound :: Range.Bound Integer -> Integer
normalizeLBound (Range.Bound a Range.Inclusive) = a
normalizeLBound (Range.Bound a Range.Exclusive) = a + 1

normalizeUBound :: Range.Bound Integer -> Integer
normalizeUBound (Range.Bound a Range.Inclusive) = a
normalizeUBound (Range.Bound a Range.Exclusive) = a - 1

instance Semigroup UnionType where
    UnionType a1 f1 i1 s1 <> UnionType a2 f2 i2 s2 =
        UnionType (a1 `merge` a2) (nub $ f1 ++ f2) (i1 `Range.union` i2) (s1 `merge` s2)

instance Monoid UnionType where
    mempty = UnionType [] [] [] []

instance Semigroup Type where
    AnyTy <> _ = AnyTy
    _ <> AnyTy = AnyTy
    UnionTy t1 <> UnionTy t2 = UnionTy (t1 <> t2)
    stimes = stimesIdempotentMonoid

instance Monoid Type where
    mempty = UnionTy mempty

instance Eq Field where
    Field name1 typ1 == Field name2 typ2 = name1 == name2 && typ1 == typ2

instance Ord Field where
    compare (Field name1 _) (Field name2 _) = compare name1 name2

-- | Merge two sorted lists, removing duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys

-- | Creates a new atom type with the given name
atomTy :: String -> Type
atomTy s = UnionTy $ mempty { atoms = [s] }

-- | Creates an integer type with the given value
intTy :: Integer -> Type
intTy i = intRangeTy i i

-- | Creates a new struct type with the given name and fields
structTy :: String -> [Field] -> Type
structTy name fields = UnionTy $ mempty { structs = [(name, fields)] }

-- | Creates a new function type with the given argument and return type
functionTy :: Type -> Type -> Type
functionTy arg ret = UnionTy $ mempty { functions = [(arg, ret)] }

-- | Creates an integer range type with the given lower and upper bounds, inclusive
intRangeTy :: Integer -> Integer -> Type
intRangeTy l u = UnionTy $ mempty { ints = [l +=* (u + 1)] }

-- | Creates an integer range type with the given upper bound, inclusive
intRangeUpToTy :: Integer -> Type
intRangeUpToTy u = UnionTy $ mempty { ints = [Range.ube (u + 1)] }

-- | Creates an integer range type with the given lower bound, inclusive
intRangeFromTy :: Integer -> Type
intRangeFromTy l = UnionTy $ mempty { ints = [Range.lbi l] }

-- | Creates an integer range type with no bounds
intRangeInfTy :: Type
intRangeInfTy = UnionTy $ mempty { ints = [Range.inf] }

-- | Checks if the first type is convertible to the second type
convertible :: Type -> Type -> Bool
convertible _ AnyTy = True
convertible AnyTy _ = False
convertible (UnionTy t1) (UnionTy t2) = convertibleUnion t1 t2

convertibleUnion :: UnionType -> UnionType -> Bool
convertibleUnion (UnionType a1 f1 i1 s1) (UnionType a2 f2 i2 s2)
  =  all (`elem` a2) a1
  && all (\f1 -> any (convertibleFn f1) f2) f1
  && all (`elem` s2) s1
  && Range.union i1 i2 == i2

convertibleFn :: (Type, Type) -> (Type, Type) -> Bool
convertibleFn (arg1, ret1) (arg2, ret2) = convertible arg2 arg1 && convertible ret1 ret2
