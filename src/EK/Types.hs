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
  , UnionType(..)
  , Field(..)
  )
where

import Data.List (intercalate, nub)
import GHC.Base (stimes)
import Data.Semigroup (stimesIdempotentMonoid)
import qualified Data.Range as Range

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
  { atoms :: [String] -- ^ A list of atoms
  , functions :: [(Type, Type)] -- ^ A list of functions
  , ints :: [Range.Range Integer] -- ^ A list of integer ranges
  , structs :: [(String, [Field])] -- ^ A list of structs
  } deriving (Eq)

-- | A field is a name and a type
data Field = Field String Type

instance Show ConcreteType where
    show (Atom s) = s
    show (Function arg ret) = "(" ++ show arg ++ " -> " ++ show ret ++ ")"
    show (Int i) = show i
    show (Struct name _) = name

instance Show Type where
    show TAny = "Any"
    show (TUnion ts) = show ts

instance Show UnionType where
    show (UnionType atoms functions ints structs) =
        intercalate " | " (map show atoms ++ map showFn functions ++ map showRange ints ++ map (show . fst) structs)

showFn :: (Type, Type) -> String
showFn (arg, ret) = "(" ++ show arg ++ " -> " ++ show ret ++ ")"

showRange :: Range.Range Integer -> String
showRange (Range.SingletonRange a) = show a
showRange (Range.SpanRange l u) = "[" ++ showLBound l ++ ".." ++ showUBound u ++ "]"
showRange (Range.LowerBoundRange l) = "[" ++ showLBound l ++ "..]"
showRange (Range.UpperBoundRange u) = "[.." ++ showUBound u ++ "]"
showRange Range.InfiniteRange = "[..]"

showLBound :: Range.Bound Integer -> String
showLBound (Range.Bound a Range.Inclusive) = show a
showLBound (Range.Bound a Range.Exclusive) = show (a + 1)

showUBound :: Range.Bound Integer -> String
showUBound (Range.Bound a Range.Inclusive) = show a
showUBound (Range.Bound a Range.Exclusive) = show (a - 1)

instance Semigroup UnionType where
    UnionType a1 f1 i1 s1 <> UnionType a2 f2 i2 s2 =
        UnionType (a1 `merge` a2) (nub $ f1 ++ f2) (i1 `Range.union` i2) (s1 `merge` s2)
    stimes = stimesIdempotentMonoid

instance Monoid UnionType where
    mempty = UnionType [] [] [] []

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
