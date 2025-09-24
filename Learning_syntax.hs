--random maths functions
leng a b = a+b

mult a b = a*b
square a = mult a a
cube a = mult a (square a)

expo a b = if b ==0 then 1 else square a * expo a (b-1)


--end of maths functions



