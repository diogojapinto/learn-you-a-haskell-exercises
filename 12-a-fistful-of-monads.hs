import Control.Applicative

{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Fail String | Success a deriving (Show)

-- Make the Validation a Monad

instance Functor Validation where
    f `fmap` Fail s = Fail s
    f `fmap` Success a = Success (f a)

instance Applicative Validation where
    pure = Success
    Fail s <*> _ = Fail s
    Success _ <*> Fail s = Fail s
    Success f <*> Success x = Success (f x)

instance Monad Validation where
    return = Success
    Fail s >>= f = Fail s
    Success x >>= f = f x
    fail = Fail

{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x = if x >= 0
                  then Success x
                  else Fail "not positive"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x = if even x
              then Success x
              else Fail "not even"

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = return x >>= positiveCheck >>= evenCheck
