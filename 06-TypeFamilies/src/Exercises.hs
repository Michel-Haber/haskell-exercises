{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':

type family (+) (a :: Nat) (b :: Nat) :: Nat where
  'Z     + b = b
  ('S n) + m = 'S (n + m)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (**) (a :: Nat) (b :: Nat) :: Nat where
   'Z    ** b = 'Z
   'S  n ** b = b + (n ** b)

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

-- | c. Write a function to add two 'SNat' values.

add :: SNat n -> SNat m -> SNat (n + m)
add  SZ    b = b
add (SS a) b = SS (add a b)



{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector m a -> (a -> Vector n b) -> Vector (m ** n) b
flatMap  VNil        _ = VNil
flatMap (VCons x xs) f = f x `append` flatMap xs f




{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.

type family (&&) (a :: Bool) (b :: Bool) :: Bool where
  'True  && b = b
  'False && _ = 'False

-- | b. Write the type-level @||@ function for booleans.

type family (||) (a :: Bool) (b :: Bool) :: Bool where
  'True  || _ = 'True
  'False || b = b

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.

type family All (ls :: [Bool]) :: Bool where
  All '[]       = 'True
  All (x ': xs) = x && All xs




{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare (a :: Nat) (b :: Nat) :: Ordering where
  Compare 'Z 'Z = 'EQ
  Compare  x 'Z = 'GT
  Compare 'Z  y = 'LT
  Compare  ('S x) ('S y) = Compare x y

-- | b. Write a 'Max' family to get the maximum of two natural numbers.

type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = Choose (Compare a b) a b

type family Choose (o :: Ordering) (a :: Nat) (b :: Nat) :: Nat where
  Choose 'GT x _ = x
  Choose  _  _ y = y

-- | c. Write a family to get the maximum natural in a list.

type family MaxList (xs :: [Nat]) :: Nat where
  MaxList '[] = 'Z
  MaxList (x ': xs) = Max x (MaxList xs)



{- FIVE -}

-- This tree is meant as a binary search tree without duplication
-- I inferred that after comparing my solution with the provided solution
data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.

type family Insert (t :: Tree) (n :: Nat) :: Tree where
  Insert 'Empty        n = 'Node 'Empty n 'Empty
  Insert ('Node l c r) n = Branch (Compare n c) ('Node l c r) n

type family Branch (o :: Ordering) (t :: Tree) (n :: Nat) :: Tree where
  Branch 'EQ t             _ = t
  Branch 'GT ('Node l c r) n = 'Node l c (Insert r n)
  Branch 'LT ('Node l c r) n = 'Node (Insert l n) c r


{- SIX -}
-- I honestly started solving this exercise but got tired because it's so long
-- and honestly a bit boring, so I copied the solution...

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.

type family Delete (x :: Nat) (xs :: Tree) where
  Delete x  'Empty       = 'Empty
  Delete x ('Node l c r) = Delete' (Compare x c) x ('Node l c r)

type family Delete' (o :: Ordering) (x :: Nat) (xs :: Tree) where
  Delete' 'LT x ('Node  l     c r) = 'Node (Delete x l) c r
  Delete' 'GT x ('Node  l     c r) = 'Node l c (Delete x r)
  Delete' 'EQ x ('Node 'Empty c r) = r
  Delete' 'EQ x ('Node  l     c r) = Repair (Biggest l) r

type family Repair (parts :: (Nat, Tree)) (xs :: Tree) where
  Repair '(c, l) r = 'Node l c r

type family Biggest (xs :: Tree) :: (Nat, Tree) where
  Biggest ('Node l c 'Empty) = '(c, l)
  Biggest ('Node l c r)      = Biggest' l c (Biggest r)

type family Biggest' (l :: Tree) (c :: Nat) (r' :: (Nat, Tree)) :: (Nat, Tree) where
  Biggest' l c '(x, r) = '(x, 'Node l c r)




{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.

type family (++) (as :: [Type]) (bs :: [Type]) :: [Type] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

(+++) :: HList as -> HList bs -> HList (as ++ bs)
HNil         +++ bs = bs
(HCons a as) +++ bs = HCons a (as +++ bs)

{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _ '[]       = ()
  Every c (x ': xs) = (c x, Every c xs)

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.

instance (Every Show a) => Show (HList a) where
  show HNil = ""
  show (HCons a as) = show a ++ "," ++ show as

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance (Every Eq a) => Eq (HList a) where
  HCons x xs == HCons y ys = x == y && xs == ys
  _ == _ = True

instance (Every Eq a, Every Ord a) => Ord (HList a) where
  compare (HCons x xs) (HCons y ys) = compare x y <> compare xs ys
  compare _ _ = EQ


{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

type family Upto (n :: Nat) :: [Nat] where
  Upto  'Z    = '[]
  Upto ('S n) = Happend (Upto n) '[('S n)]

type family Happend (xs :: [Nat]) (ys :: [Nat]) :: [Nat] where
  Happend '[]       ys = ys
  Happend (x ': xs) ys = x ': Happend xs ys

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?

-- I did not do b, because ultimately it is a boring syntax exercise.
-- These exercises have their utility, but I'm just not up to doing it.
-- The main problem with type family code is that we have no pattern matching
-- with let, and we have to redefine everything :(
