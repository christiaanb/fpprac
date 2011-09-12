import FPPrac

-- Opgave 1
f x = 4 * (x ^ 2) - 8 * x - 5

-- Opgave 2
codeer k x | i > 64 && i < 91  = codeer' 63 i
           | i > 96 && i < 123 = codeer' 97 i
           | otherwise         = x
  where
    i         = code x
    codeer' z = decode . ((+) z) . (`mod` 26) . (+k) . ((-) z)

-- Opgave 3
rente n b r = b * (1+r) ^ n

-- Opgave 4
wortel1 a b c | z >= 0    = (-b - (sqrt z)) / (2 * a)
              | otherwise = error "discrinimant negatief"
  where
    z = discr a b c

wortel2 a b c | z >= 0    = (-b + (sqrt z)) / (2 * a)
              | otherwise = error "discriminant negatief"
  where
    z = discr a b c

discr a b c = b^2 - 4 * a * c

-- Opgave 5
extrX a b c = -b / (2 * a)

extrY a b c = a * x^2 + b * x + c
  where
    x = extrX a b c

-- Opgave 6
mysum []     = 0
mysum (x:xs) = x + mysum xs

myreverse []     = []
myreverse (x:xs) = (myreverse xs) ++ [x]

mydrop n xs    | n <= 0 = xs
mydrop _ []             = []
mydrop n (_:xs)         = mydrop (n-1) xs

mytake n xs     | n <= 0 = []
mytake _ []              = []
mytake n (x:xs)          = x : mytake (n-1) xs

mymember a []     = False
mymember a (x:xs) = if a == x then True else mymember a xs

myconcat []     = []
myconcat (x:xs) = x ++ myconcat xs

mymin []     = error "mymin: empty list"
mymin (x:xs) = mymin' x xs
  where
    mymin' a []     = a
    mymin' a (y:ys) = if a < y then mymin' a ys else mymin' y ys

mymax []     = error "mymax: empty list"
mymax (x:xs) = mymax' x xs
  where
    mymax' a []     = a
    mymax' a (y:ys) = if a > y then mymax' a ys else mymax' y ys

myzip [] ys         = []
myzip xs []         = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

-- Opgave 7
r s v = s : r (s+v) v

r1 s v n = (r s v)!!n

totaal s v a b = sum $ take (b - a) $ drop a $ r s v

-- Opgave 8
allEqual [x]      = True
allEqual (x:y:xs) = x == y && allEqual (y:xs)

allRR = allEqual . verschil
	where
		verschil [x] = []
		verschil (x:y:ys) = (x-y) : verschil (y:ys)

-- Opgave 9
testMatrix = [ [1,3,4]
						 , [5,6,7]
						 , [8,9,10]
						 ]

equalLength = allEqual . map length

rowTotals   = map sum

myTranspose = foldr f (repeat [])
	where
		f as bss = zipWith (:) as bss

columnTotals = map sum . myTranspose 
