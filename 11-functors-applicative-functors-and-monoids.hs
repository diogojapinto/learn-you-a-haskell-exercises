import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
    fmap _ Empty = Empty
    fmap f (Value x xs) = Value (f x) (fmap f xs)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists a Empty = a
combineLists Empty b = b
combineLists (Value x xs) b = Value x (combineLists xs b)

-- Make our list a Monoid
instance Monoid (List a) where
    mempty = Empty
    mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
    pure a = Value a Empty
    Empty <*> _ = Empty
    (Value f fs) <*> x = (fmap f x) `mappend` (fs <*> x)

-- Make sure that the List obeys the laws for Applicative and Monoid
-- CHECKED

-- Create some lists of numbers of different lengths such as:
oneValueList = Value 9000 Empty
twoValueList = Value 10 $ Value 20 Empty
threeValueList = Value 1 $ Value 2 $ Value 3 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2) <$> threeValueList

-- Use <$> and <*> on the lists with a binary function
divide = (/) <$> oneValueList <*> threeValueList

-- Create some lists of binary functions
binFuncList = Value (+) $ Value (*) $ Value (-) Empty

-- Use <*> on the binary functions list and the number lists
finalResult = binFuncList <*> threeValueList <*> threeValueList