module Tree
(
    singleton,
    treeInsert,
    treeElem,
    goLeft,
    goRight,
    (-:),
    modify,
    attach,
    topMost,
) where

import qualified Data.Foldable as F
import Data.Monoid

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- let nums = [8,6,4,1,7,3,5]  
-- let numsTree = foldr treeInsert EmptyTree nums

-- testTree = Node 5  
--             (Node 3  
--                 (Node 1 Empty Empty)  
--                 (Node 6 Empty Empty)  
--             )  
--             (Node 9  
--                 (Node 8 Empty Empty)  
--                 (Node 10 Empty Empty)  
--             )  

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x)
                               (fmap f l)
                               (fmap f r)



instance F.Foldable Tree where  
    foldMap _ EmptyTree = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  

-- getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- F.foldMap (\x -> [x]) testTree


data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

(-:) :: a -> (a -> b) -> b
x -: f = f x

goLeft :: (Tree a, Breadcrumbs a) -> Maybe (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (EmptyTree, _) = Nothing

goRight :: (Tree a, Breadcrumbs a) -> Maybe (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (EmptyTree, _) = Nothing

goUp :: (Tree a, Breadcrumbs a) -> Maybe (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

type Zypper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zypper a -> Zypper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (EmptyTree, bs) = (EmptyTree, bs)

-- let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')  

attach :: Tree a -> Zypper a -> Zypper a
attach t (_, bs) = (t, bs)

topMost :: Maybe (Zypper a) -> Maybe (Zypper a)
topMost (Just (t, [])) = Just (t, [])
topMost (Just z) = topMost (goUp z)
topMost Nothing = Nothing

-- return (coolTree,[]) >>= goRight >>= goRight >>= goRight