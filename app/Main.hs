module Main where

import Lib



-- struktura stromu --
data Tree a = Empty
    | Node a (Tree a) (Tree a)
    deriving (Show)

--pomocna struktura pro stavbu stromu

data Builder a = None
    | Content Int (Tree a)
    deriving (Show)

--vstupmi data --

prefix = [ '-'
         , '+'
         , '6'
         , '2'
         , '3'
         ]

freeTree :: Tree Char
freeTree =
    Node '-'
       (Node '*'
           (Node '+'
               (Node '6' Empty Empty)
               (Node '5' Empty Empty)
           )
           (Node '/'
               (Node '7' Empty Empty)
               (Node '9' Empty Empty)
           )
      )
      (Node '/'
          (Node '*'
              (Node '1' Empty Empty)
              (Node '2' Empty Empty)
          )
          (Node '+'
              (Node '3' Empty Empty)
              (Node '4' Empty Empty)
          )
      )
buildLeft :: [Char] -> Builder Char -> Builder Char
buildLeft prefix (Content a t) =
    if isTrue (prefix!!a) == True then
      let index = a + 1
          leftWithContent = buildLeft((last:prefix)(Content a t))
          newIndex = returnIndex(leftWithContent)
          right = buildRight()

--buildRight :: [Char] -> Builder Char -> Builder Char


returnTree :: Builder Char -> Tree Char
returnTree (Content _ t) = t

returnIndex :: Builder Char -> Int
returnIndex (Content a _) = a

buildTree :: [Char] -> Tree Char
buildTree prefix =
    let leftWithContent = buildLeft( head:prefix (Content 0 Empty))
        leftTree = returnTree(leftWithContent)
        leftEnd = returnIndex(leftWithContent)
        right = returnTree( buildLeft( prefix (Content leftEnd Empty))
    in Node (head:prefix) leftTree rightTree

--vraci True kdyz je char znamenko --
isTrue :: Char -> Bool
isTrue char
    | char == '-' = True
    | char == '/' = True
    | char == '*' = True
    | char == '+' = True
    | otherwise = False


printRight :: Tree Char -> IO()
printRight (Node a l r) =
  if isTrue a == True then do
            printLeft(l)
            putChar a
            printRight(r)
            putChar(')')
      else
        let c = [a]
            output = c ++ " )"
        in putStr output

--leftTree :: Tree Char -> Tree Char
--leftTree (Node _ l _) = l

printLeft :: Tree Char -> IO ()
printLeft (Node a l r) =
  if isTrue a == True then do
            putChar('(')
            printLeft(l)
            putChar a
            printRight(r)
      else
        let c = [a]
            output = "( " ++ c
        in putStr output

printInfix :: Tree Char -> IO ()
printInfix (Node a l r) = do
      printLeft(l)
      putChar a
      printRight(r)

main :: IO ()
main = do
  printInfix(buildTree(prefix))
  putStr ("\n")
