import Control.Monad.Trans.Cont (reset)
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kivonas :: Double -> Double -> Double
kivonas a b = a - b

szorzat :: Int -> Int -> Int
szorzat a b = a * b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

-- - egy első fokú egyenlet gyökét,
-- a*x + b = 0 -> a,b -> x = (-b) / a
elsoF a b = (-b) / a

-- - egy szám abszulút értékét,
abszolut a
  | a < 0 = -a
  | otherwise = a

abszolut2 a = if a < 0 then -a else a

-- -egy szam elojele
elojel a = if a < 0 then "negativ" else if a > 0 then "pozitiv" else "nulla"

elojel2 n
  | n < 0 = "negativ"
  | n > 0 = "pozitiv"
  | otherwise = "nulla"

-- ket argumentum kozott a maximum
max a b = if a > b then a else b

max2 a b
    | a > b = a
    | otherwise = b

-- - két argumentuma közül a minimumot,
min_ a b
  | a < b = a
  | otherwise = b

min2 a b = if a < b then a else b


-- - egy másodfokú egyenlet gyökeit,
-- a*(x**2) + b*x + c = 0 -> a, b, c bemeneti arg.
-- delta = b**2 - 4*a*c
-- gy1 = (-b + sqrt delta) / 2*a
-- gy2 = (-b - sqrt delta) / 2*a

masodF a b c
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1, gy2)
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

-- elempar ep1 ep2 = (a == c && b == d) || (a == d && b == c)
--   where
--     (a, c) = ep1
--     (c, d) = ep2


elempar2 (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)



fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)


fakt2 n
  | n < 0 = error "neg. szam"
  | n == 0 = 1
  | otherwise = n * fakt2 (n - 1)

fakt3 n res
  | n < 0 = error "neg. szam"
  | n == 0 = res
  | otherwise = fakt3 ( n -1) ( res * n )

hatvanyX x n 
  | n < 0 = error "neg. szam"
  | otherwise = x**n

hatvanyX2 x n
  | n < 0 = error "neg. szam"
  | otherwise = x ^ n

hatvanyX3 x n
  | n < 0 = error "neg. szam"
  | n == 0 = 1
  | otherwise = x * hatvanyX3 x (n - 1)

negyzetgyok n = [sqrt i | i <- [1 .. n]]

kobszam n = [i ^ 3 | i <- [1 .. n]]

nemNegyzet n = [i | i <- [1 .. n], i /= (sqrt i ^ 2)]

hatvany x n = [x ^ i | i <- [1 .. n]]

parosOsztok x = [i | i <- [1 .. x], mod x i == 0, mod i 2 == 0]

parosOsztok2 x = [i | i <- [1 .. x], mod x i == 0, even i]

osztok x = [i | i <- [1 .. x], mod  x i == 0]

primszam x = osztok x == [1, x]

primszamN n = [i | i <- [1 .. n], primszam i]


primszamN2 n = [i | i <- [1 .. n], primszamL i]
  where 
    primszamL sn = [1, sn] == osztokL sn
    osztokL sn2 = [j | j <- [1 .. sn2], mod sn2 j == 0]

osszetett n = [i | i <- [1 .. n], primszam i == False ]

osszetett2 n = [i | i <- [1 .. n], not (primszam i) ]

paratlanOsszetett n = [i | i <- [1 .. n], not (primszam i), mod i 2 == 0 ]

paratlanOsszetett2 n = [i | i <- [1 .. n], not (primszam i), not (even i) ]


paratlanOsszetett3 n = [i | i <- [1 .. n], not (primszam i), odd i ]


pitagorasz n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a^2 + b^2 == c^2]

betuSzam = zip ['a' .. 'z'][0 .. 25]

betuSzam2 = zip ['a' .. 'z'][1, 3 ..]

szamok = zip [0 .. 5][5,4 .. 0]

szamok2 n = zip [0 .. n] [n, n-1 .. 0]

szamok3 n = [(i, n - i) | i <- [0 .. n]]

tf n = take n ls
  where 
    ls = [True, False] ++ ls

tf2 n = [mod i 2 == 0 | i <- [0 .. n]]


main = do
  putStrLn "elempar (6,7) (7,6)"
  print (elempar2 (6,7) (7, 6))
  putStrLn ("elempar (6,7) (8,6) " ++ show(elempar2 (6,7) (8, 6)))
  print(tf2 10)