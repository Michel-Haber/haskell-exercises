{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Exercises where





{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountNil :: CountableList
  CountCons :: (Countable a) => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountNil = 0
countList (CountCons a ls) = count a + countList ls


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountNil = CountNil
dropZero (CountCons a ls) = if count a == 0 then dropZero ls
                                            else CountCons a $ dropZero ls


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList (AnyCons a ls) = appendAny a $ reverseAnyList ls
  where
    appendAny a (AnyCons x ls) = AnyCons x $ appendAny a ls
    appendAny a AnyNil = AnyCons a AnyNil

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = error "Cannot be filtered"

lengthAnyList :: AnyList -> Int
lengthAnyList (AnyCons a ls) = 1 + lengthAnyList ls
lengthAnyList AnyNil = 0

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = error " Cannot implement reasonabble fold (No mapping for elems)"

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _ = False


instance Show AnyList where
  show = error "Cannot implement meaningful Show (Unshowable elements)"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq a => Eq (TransformableTo a) where
  (TransformWith f x) == (TransformWith g y) = f x == g y


-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap g (TransformWith f x) = TransformWith g . f $ x



{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

evalPair :: EqPair -> Bool
evalPair (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqPair' a = EqPair' a a
-- Note that we lost the Eq constraint without a GADT




{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox s (IntBox i _)) = i

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers (BoolBox _ _)   = 3
countLayers (StringBox _ _) = 2
countLayers (IntBox _ _)    = 1
countLayers EmptyBox        = 0

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- The type signature cannot match. We need to express a signature like:
-- peel :: MB a -> MB b with a = Int => b = (); a = Strin => b = Int; etc...


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hListHead :: HList (a,b) -> a
hListHead (HCons h t) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe list = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

-- We cannot construct an infinite type
-- appendHList :: a -> HList (b, c)
-- appendHList e (HCons h t) = appendHList e t
-- appendHList e HNil        = HCons e HNil



{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HTNil :: HTree Empty
  HTBranch :: HTree l -> c -> HTree r -> HTree (Branch l c r)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch a b c) -> HTree (Branch Empty b c)
deleteLeft (HTBranch l c r) = HTBranch HTNil c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  (==) _ _ = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  HTBranch lft ctr rgt == HTBranch lft' ctr' rgt' =
    ctr == ctr' && lft == lft' && rgt == rgt'



{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ALNil :: AlternatingList a b
  ALCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ALNil = []
getFirsts (ALCons a l) = a:(getFirsts . skip $ l)
  where skip (ALCons _ l) = l
        skip ALNil = ALNil

getSeconds :: AlternatingList a b -> [b]
getSeconds ALNil = []
getSeconds (ALCons a l) = getFirsts l

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues ALNil = (mempty, mempty)
foldValues (ALCons a l) = (a `mappend` snd nxt, fst nxt)
  where nxt = foldValues l





{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (BoolValue x) = x
eval (IntValue  x) = x
eval (Equals  x y) = eval x == eval y
eval (Add     x y) = eval x +  eval y
eval (If    c x y) = if eval c then eval x
                               else eval y

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

-- Copied from solutions for completeness
-- Could have been done without Typed (No GADts, cf. derivation.hs)
data Typed where
  IntType  :: Expr Int  -> Typed
  BoolType :: Expr Bool -> Typed

tidy :: DirtyExpr -> Maybe Typed
tidy (DirtyEquals x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (BoolType (Equals x y))
  _                                    -> Nothing

tidy (DirtyAdd x y) = case (tidy x, tidy y) of
  (Just (IntType x), Just (IntType y)) -> Just (IntType (Add x y))
  _                                    -> Nothing

tidy (DirtyIf p t f) = case (tidy p, tidy t, tidy f) of
  (Just (BoolType p'), Just (IntType t'), Just (IntType f')) ->
    Just (IntType (If p' t' f'))
  (Just (BoolType p'), Just (BoolType t'), Just (BoolType f')) ->
    Just (BoolType (If p' t' f'))

tidy (DirtyIntValue  x) = Just (IntType  (IntValue  x))
tidy (DirtyBoolValue x) = Just (BoolType (BoolValue x))

parse :: DirtyExpr -> Maybe (Expr Int)
parse xs = case tidy xs of
  Just (IntType x) -> Just x
  Nothing          -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

-- We can add functions and function application.
-- In principle we can avoid the Maybe because everything is well-typed.



{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil :: TypeAlignedList a a
  TACons :: (a -> c) -> TypeAlignedList c b -> TypeAlignedList a b

-- | b. Which types are existential?

-- c, here, is an existential type.

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs l (TACons f fs) = TACons f $ composeTALs l fs
composeTALs l TANil = l
