
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