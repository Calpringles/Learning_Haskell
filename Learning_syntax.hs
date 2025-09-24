--random maths functions
leng a b = a+b

mult a b = a*b
square a = mult a a
cube a = mult a (square a)

expo a b = if b ==0 then 1 else square a * expo a (b-1)
tetrate a b = if b==0 then 1 else expo a (tetrate a (b-1))


--end of maths functions



--declaritive stuff practice


summ ::Int->Int
summ 0 = 0
summ n= n + summ(n-1)

facto :: Int->Int 
facto 0 = 1
facto n= n*facto(n-1)

checkCHar :: Char-> String
checkCHar x = if x == 'a' then "the char is a" else (if x== 'z' then "the char is z" else "the char is neither a nor z")

checkChar1:: Char-> String
checkChar1 x | x=='a' = "the char is a"
             | x=='z' = "the char is z"
             | otherwise = "the char is neither a nor z"

checkChar2:: Char-> String
checkChar2 'a' = "the char is a"
checkChar2 'z' = "the char is z"
checkChar2 _ =  "the char is neither a nor z"


--end of declaritive stuff practice

--booleans and comparators practice


morTHan5::Int->Bool
morTHan5 x = x>5

d5::Int->Bool
d5 x = x `mod` 5 == 0

gcd:: Int->Int->Int
gcd a b = | a==b = a
        | a > b = gcd(a-b) b
        | otherwise = gcd a (b-a)

