{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- ek type system
-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module EK.Types
  ( ConcreteType(..)
  , Type(..)
  , Field(..)
  , concrete
  )
where

import Data.List (intercalate, nub)
import GHC.Base (stimes)
import Data.Semigroup (stimesIdempotentMonoid)
import qualified Data.Range as Range
import Data.Range ((+=*), (+=+), Bound (boundValue))

-- | A concrete type is a type that a value can have
data ConcreteType = Atom String -- ^ An atom, or symbol
                  | Function Type Type -- ^ A function with an argument and a return type
                  | Int Integer -- ^ A specific integer
                  | Struct String [Field] -- ^ A struct with a name and a list of fields

-- | A type is a list of concrete types
data Type = TAny -- ^ The Any type, which can be anything
          | TUnion UnionType -- ^ A union type, which can be any of the types in the list
          deriving (Eq)

-- | A union type is a list of atoms, functions, integer ranges, and structs
data UnionType = UnionType
  [String] -- ^ A list of atoms
  [(Type, Type)] -- ^ A list of functions
  [Range.Range Integer] -- ^ A list of integer ranges
  [(String, [Field])] -- ^ A list of structs
  deriving (Eq)

-- | A field is a name and a type
data Field = Field String Type

instance Show ConcreteType where
    show = show . concrete

instance Show Type where
    show TAny = "Any"
    show (TUnion ts) = show ts

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
    stimes = stimesIdempotentMonoid

instance Monoid UnionType where
    mempty = UnionType [] [] [] []

instance Semigroup Type where
    TAny <> _ = TAny
    _ <> TAny = TAny
    TUnion t1 <> TUnion t2 = TUnion (t1 <> t2)
    stimes = stimesIdempotentMonoid

instance Monoid Type where
    mempty = TUnion mempty

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

-- | Creates a type from a concrete type
concrete :: ConcreteType -> Type
concrete = TUnion . concreteUnion

concreteUnion :: ConcreteType -> UnionType
concreteUnion (Atom s) = UnionType [s] [] [] []
concreteUnion (Function arg ret) = UnionType [] [(arg, ret)] [] []
concreteUnion (Int i) = UnionType [] [] [i +=* (i + 1)] []
concreteUnion (Struct name fields) = UnionType [] [] [] [(name, fields)]
