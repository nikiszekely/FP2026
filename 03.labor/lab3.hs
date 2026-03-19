import Control.Monad.Trans.Cont (reset)
import Graphics.Win32 (eWX_LOGOFF, dEFAULT_CHARSET, pATCOPY)
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
szjosszeg n
    | n<0 = szjosszeg(abs n)
    | otherwise = mod n 10 + szjosszeg( div n 10)

szjosszeg2 n res
    | n < 0 = szjosszeg2(abs n) res
    | n < 10 = res + n
    | otherwise = szjosszeg2(div n 10)(res + mod n 10)

--- Szamjegy szam
szjSzam n res
    | n < 0 = szjSzam(abs n) res
    | n < 10 = res + 1
    | otherwise = szjSzam( div n 10) (res + 1)

szjSzam2 n 
    | n < 0 = szjSzam2 (abs n)
    | n < 10 = 1
    | otherwise = 1 + szjSzam2( div n 10)

szjSzamOsszeg n szj
    | szj > 9 = error "nem szj"
    | n < 10 = if n == szj then szj else 0
    | otherwise = if mod n 10 == szj then szj + szjSzamOsszeg ( div n 10 ) szj else szjSzamOsszeg( div n 10 ) szj

szjSzamOsszeg2 n szj elof
    | szj > 9 = error "nem szj"
    | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
    | otherwise = if mod n 10 == szj then szjSzamOsszeg2(div n 10) szj (elof + 1) else szjSzamOsszeg2( div n 10 ) szj elof


parosSzamszj n
    | n < 0 = parosSzamszj (abs n)
    | n < 10 = if even n then 1 else 0
    | otherwise =
        if even (mod n) 
            then 1 + parosSzamszj(div n 10)
            else parosSzamszj (div n 10)

parosSzamSzj2 n res
    | n < 0 = parosSzamSzj2(abs n) res
    | n < 10 = if even n then res + 1 else res
    | otherwise =
        if even (mod n 10)
            then parosSzamSzj2 (div n 10) (res + 1)
            else parosSzamSzj2 (div n 10) res

maxSzj n max
    | n < 0 = maxSzj (abs n) max
    | n < 10 = if n > max then n else max
    | otherwise =
        if mod n 10 > max
            then maxSzj (div n 10) (mod n 10)
            else maxSzj (div n 10) max

bSzameDSzj n b d
    | n < 0 = bSzameDSzj(abs n) b d
    | n < b = if n == d then 1 else 0
    | otherwise = if mod n b == d then 1 + bSzameDSzj(div n b) b d
        else bSzameDSzj (div n b) b d


--- az 1000-ik fib szam
fibo a b res n
    | n == 0 = res
    | otherwise = fibo b res (res + b) (n - 1)

fiboN n = fibo 0 1 0 n  

-- fiboN2 n = fiboSg 0 1 0 n
--   where
--     fiboSg a b res n = fiboSg b res (res + b) (n - 1)
--     fiboSg a b res n = fiboSg b res (res + b) (n - 1)

fibols n = map (fibo 0 1 0 n) [0 .. n]

fiboLs n = map(fiboN n) [0 .. n]

-- lab4

myLength (x:xs) = 1 + myLength xs

myLength2 [] res = res
myLength2 (x:xs) res = myLength2 xs (res + 1)

myLength3 ls = foldr (\x -> (+) 1) 0 ls

myLength4 ls = foldl (\db x -> (+) 1 db ) 0 ls

myLength5 ls res = foldr (\x res -> (+) 1 res) res ls

myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs ( res * xs)

myProduct3 ls = foldr1 (*) ls

myProduct4 ls = product ls

myMinimum [] = error "ures lista"
myMinimum [x] = x
myMinimum (x1:x2:xs)
    | x1 < x2 = myMinimum(x1:xs)
    | otherwise = myMinimum (x2:xs)

myMinimum2 [] = error "ures lista"
myMinimum2 [x] = x
myMinimum2 (x1 : x2 : xs) =
    if x1 < x2
        then myMinimum2(x1:xs)
        else myMinimum2(x2:xs)

myMinimum3 ls = foldl1 min ls

myMaximum [] = error "ures lista"
myMaximum [x] = x
myMaximum (x1: x2 : xs) =
    if x1 > x2 
        then myMaximum(x1:xs)
        else myMaximum(x2: xs)

myMaximum2 ls = foldr1 max ls

myMaximum3 ls = maximum ls 

listaN ls n = ls !! n 

listaNMap ls = map (\x -> listaN x 0) ls

listaN2 ls n 
    | ls == [] = error "ures lista"
    | n < 0 = error "neg. index"
    | length ls <= n = error "tul nagy index"
    | otherwise = ls !! n

lsFuz ls1 ls2 = ls1 ++ ls2

palindrom ls = ls == reverse ls

palindrom2 ls = if ls == reverse ls then "palindrom" else "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = head ls == last ls && palindrom3 ((init. tail) ls)

szjLs x
    | x < 0 = szjLs (abs x)
    | x < 10 = [x]
    | otherwise = szjLs (div x 10) ++ [mod x 10]


elsoUtolso ls = tail ls ++ [head ls]

elsoUtolso2 (x : xs ) = xs ++ [x]


decP x p
    | x < p = [x]
    | otherwise = decP (div x p) p ++ [mod x p]

pDec ls p = foldl (\x hatvany  -> x + (p * hatvany)) 0 ls

-- pDec2 x p =
--     let szamjegyek x
--         | x < 10 = [x]
--         | otherwise = mod x 10 : szamjegyek (div x 10)
--     szjIdx = zip (szamjegyek x) [0 ..]
-- in sum [i * (p ^ hatvany) | ( i, hatvany) <- szjIdx]

ls3 = [[1..10], [5.66]]

ls4 = [[23, 56, 7], [213, 56], [67, 32]]

lsFuzMap = map (uncurry lsFuz) (zip ls3 ls4)

aLs = [3, -2, 5, 7]
x0 = 2

poli [] x = 0
poli (a: aLs) x = a + x * (poli aLs x)

type Pont = (Double, Double)

lsP :: [Pont]
lsP = [(4.5, 6.2), (1.2, 3.4), (6, 8), (4.5,2.4)]

p::Pont
p = (3.4, 5.6)

tavolsag (x1, y1) (x2, y2) = sqrt((x1 - x2) ** 2 +  (y1 - y2) ** 2) 

minTavolsag lsP p = foldl(\p1 p2 -> if tavolsag p1 p < tavolsag p2 p then p1 else p2) lsP

minTavolsag2 lsP p = foldl1 aux lsP
    where 
        aux p1 p2 = if tavolsag p1 p < tavolsag p2 p then p1 else p2

minTavolsag3 [p1] _ = p1
minTavolsag3 (p1:p2:ls) p
    | tavolsag p1 p < tavolsag p2 p = minTavolsag3 (p1 : ls) p
    | otherwise = minTavolsag3 (p2 : ls) p
