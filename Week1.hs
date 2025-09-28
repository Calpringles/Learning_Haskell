import Prelude hiding (Left, Right)
import System.Win32 (xBUTTON1)


add:: Int -> Int -> Int
add x y = x + y


--Week 1 Exercises
--Lecture 1 Questions

--Q1
data Direction = Up | Down | Left | Right deriving (Show, Eq)

-- Pair type definition


isHorizontal :: Direction -> Bool
isHorizontal Left = True
isHorizontal Right = True
isHorizontal _ = False

--Q2
flipHorizontally:: Direction -> Direction
flipHorizontally Left = Right
flipHorizontally Right = Left
flipHorizontally x = x

--Q3
data Pair a b = MkPair a b
  deriving Show

pairOfEqualDirections :: Pair Direction Direction -> Bool
pairOfEqualDirections (MkPair Up Up) = True
pairOfEqualDirections (MkPair Down Down) = True
pairOfEqualDirections (MkPair Left Left) = True
pairOfEqualDirections (MkPair Right Right) = True
pairOfEqualDirections _ = False

--Q4
data Triple a b c = MkTriple a b c
  deriving Show

get1of3 ::Triple a b c -> a
get1of3 (MkTriple a b c) = a

get2of3 ::Triple a b c -> b
get2of3 (MkTriple a b c) = b

get3of3 ::Triple a b c -> c
get3of3 (MkTriple a b c) = c

--Q5
isA :: Char -> Bool
isA 'A' = True
isA _   = False

dropSpaces:: [Char] -> [Char]
dropSpaces [] = []
dropSpaces (x:xs)
    | x == ' ' = dropSpaces xs
    | otherwise = x:xs


--Q6
dropTrailingSpaces :: [Char] -> [Char]
dropTrailingSpaces [] = []
dropTrailingSpaces xs = reverse
    (dropSpaces (reverse xs))

--Lecture 2 Questions
--Q7

{- 
         '<'  becomes  '&lt;'     ("less than")
         '>'  becomes  '&gt;'     ("greater than")
         '&'  becomes  '&amp;'    ("ampersand")
-}

htmlEscape :: String -> String
htmlEscape [] = []
htmlEscape (x:xs)
    | x == '<' = "&lt;" ++ htmlEscape xs
    | x == '>' = "&gt;" ++ htmlEscape xs
    | x == '&' = "&amp;" ++ htmlEscape xs
    | otherwise = x : htmlEscape xs

--Q8 Markups
data Markup 
    = Text String
    | Bold Markup
    | Italic Markup
    | Concat Markup Markup
    deriving(Show, Eq)

exampleMarkup :: Markup
exampleMarkup = Concat (Bold (Text "Delays")) (Concat (Text " are ") (Italic (Text "possible")))

--Write a function that takes a list of Markup's and concatenates them all together using 'Concat'.
catMarkup:: [Markup] -> Markup
catMarkup [] = Text ""
catMarkup (x:xs) = Concat x (catMarkup xs)

--Write another function that concatenates a list of 'Markup's putting spaces between them
catMarkupSpaced:: [Markup] -> Markup
catMarkupSpaced [] = Text ""
catMarkupSpaced [x] = x
catMarkupSpaced (x:xs) = Concat x (Concat (Text " ") (catMarkupSpaced xs))

removeStyle     :: Markup -> Markup
removeStyle (Text s) = Text s
removeStyle (Bold m) = removeStyle m
removeStyle (Italic m) = removeStyle m
removeStyle (Concat m1 m2) = Concat (removeStyle m1) (removeStyle m2)

markupToHTML :: Markup -> String
markupToHTML (Text s)       = htmlEscape s
markupToHTML (Bold m)       = "<strong>" ++ markupToHTML m ++ "</strong>"
markupToHTML (Italic m)     = "<em>" ++ markupToHTML m ++ "</em>"
markupToHTML (Concat m1 m2) = markupToHTML m1 ++ markupToHTML m2


