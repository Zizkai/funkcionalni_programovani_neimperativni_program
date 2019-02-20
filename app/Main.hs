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
         , '5'
         , '/'
         , '6'
         , '-'
         , '6'
         , '3'
         ]
-- ^ Zde je mozno editovat vstup

-- testovaci data na rucne vytvorenem stromu
{-
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
-}

-- stavi levou cast stromu, argumenty jsou list charu a stavebni jednotku Builder Char. Vystupem je opet builder Char.
buildLeft :: [Char] -> Builder Char -> Builder Char
buildLeft prefix (Content a t) =
    if isTrue (prefix!!a) == True then
      let index = a + 1
          leftWithContent = buildLeft prefix (Content index t)
          leftTree = returnTree leftWithContent
          newIndex = returnIndex leftWithContent
          rightWithContent = buildRight prefix (Content newIndex t)
          rightTree = returnTree rightWithContent
          newIndexRight = returnIndex rightWithContent
          tree = Node (prefix!!a) leftTree rightTree
      in (Content newIndexRight tree)
      else
        let emptyTree = Node (prefix!!a) Empty Empty
        in Content (a+1) emptyTree

-- stavi pravou cast stromu, argumenty jsou list charu a stavebni jednotku Builder Char. Vystupem je opet builder Char.
buildRight :: [Char] -> Builder Char -> Builder Char
buildRight prefix (Content a t) =
  if isTrue (prefix!!a) == True then
    let index = a + 1
        leftWithContent = buildLeft prefix (Content index t)
        leftTree = returnTree leftWithContent
        newIndex = returnIndex leftWithContent
        rightWithContent = buildRight prefix (Content newIndex t)
        rightTree = returnTree rightWithContent
        newIndexRight = returnIndex rightWithContent
        tree = Node (prefix!!a) leftTree rightTree
    in (Content newIndexRight tree)
    else
      let emptyTree = Node (prefix!!a) Empty Empty
      in Content (a+1) emptyTree

-- extrahuje z Builderu jednu polozku a tim je strom
returnTree :: Builder Char -> Tree Char
returnTree (Content _ t) = t

-- extrahuje z Builderu jednu polozku a tim je cislo
returnIndex :: Builder Char -> Int
returnIndex (Content a _) = a

-- dostava argument typu [Char] (String) nejdrive inicializuje stavbu leveho stromu a pote praveho vraci celkovy strom typu Tree Char
buildTree :: [Char] -> Tree Char
buildTree prefix =
    let leftWithContent = buildLeft prefix (Content 1 Empty)
        leftTree = returnTree leftWithContent
        leftEnd = returnIndex leftWithContent
        right = returnTree (buildRight prefix (Content leftEnd Empty))
        in Node (head prefix) leftTree right

--vraci True kdyz je char znamenko
isTrue :: Char -> Bool
isTrue char
    | char == '-' = True
    | char == '/' = True
    | char == '*' = True
    | char == '+' = True
    | otherwise = False

-- vypisuje pravy strom a vypisuje spravne zavorky. Agrumentem je neco datoveho typu Tree -> vystupem je zapis do konzole
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

-- vypisuje levy strom a vypisuje spravne zavorky. Agrumentem je neco datoveho typu Tree -> vystupem je zapis do konzole
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


-- vypisuje infixovou podobu vyrazu agrumentem je datovy typ Tree vystupem je vypis do konzole
printInfix :: Tree Char -> IO ()
printInfix (Node a l r) = do
      printLeft(l)
      putChar a
      printRight(r)

-- main funkce jejiz vystupem je zapis do konzole, take se zde vola funkce printInfix
main :: IO ()
main = do
  printInfix(buildTree(prefix)) --vypis prefixu, argumentem je funkce buildTree s argumentem prefix
  putStr ("\n") -- novy radek
