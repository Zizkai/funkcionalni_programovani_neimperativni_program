module Main where

import Lib


data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

-- struktura stromu --
data Tree a = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)
--vstupmi data --
prefix = [ '-'
         , '+'
         , '6'
         , '2'
         , '3'
         ]



goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

--vraci True kdyz je char znamenko --
isTrue :: Char -> Bool
isTrue char
    | char == '-' = True
    | char == '/' = True
    | char == '*' = True
    | char == '+' = True
    | otherwise = False


treeFromList :: [Crumb Char] -> [Char] -> Tree Char
treeFromList bs prefix = 


  --treeFromList (x:xs) = Node (treeFromList (filter (<x) xs)) x (treeFromList (filter (>x) xs))



main :: IO ()
main = someFunc
