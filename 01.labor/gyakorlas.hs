
import Data.List (sort)
import Text.Printf

-- import GHC.Base (VecElem(Int16ElemRep))
-- -- viiszadja a lista legnagyobb elemet
-- maxElem :: [Int] -> Int
-- maxElem [x] = x
-- maxElem (x:xs) = 
--     if x > maxElem xs
--     then x
--     else maxElem xs

-- -- teszteles: maxElem [3,7,2,9,1] -> 9

-- -- paratlan es oszthato 3-al szamok
-- paratlanOszthato3 n = 
--     [x | x <- [1..n], x `mod` 2 == 1, x `mod` 3 == 0]
-- -- mindket fetltetel kell teljesuljon
-- -- teszteles: paratlanOszthato3 20 -> [3,9,15]

-- -- primszam-e? 
-- prim :: Int -> Bool
-- prim n
--   | n < 2 = False
--   | otherwise = [x | x <- [2..n-1], n `mod` x == 0] == []
--   -- ha ures lsitat ad vissza -> prim, true
--   -- ha a lista nem ures, akkor nem prim szam, false

-- -- index alapjan donti el, paros index true, paratlan index false
-- valtozo :: Int -> [Bool]
-- valtozo n = [even x | x <- [1..n]]


-- -- gyakorlas a 6. laborbol
-- --n-ig a primszamok listaja szokozzel kiirva
-- mainPrime :: IO ()
-- mainPrime = do
--     putStr "n = "
--     -- beolvas n
--     n <-readLn :: IO Int

--     -- lista 2-tol n-ig es csak azokat tartsuk meg amelyekre igaz a masodik feltetel, vagyis prim
--     let primes = [x | x <- [2..n], isPrime x]
--     --x vegye fel a [2..n] értékeit

--     putStrLn (unwords(map show primes))
--     -- map show primes [2,3,5] → ["2","3","5"]
--     -- unwords ["2","3","5"] → "2 3 5"


-- isPrime :: Int -> Bool
-- isPrime x =
--     x > 1 && null [d | d <- [2..x-1], x `mod` d == 0]
--     -- ha van oszto, lista nem ures -> False
--     -- ha nincs oszto => primszam, True


-- -- n kettes szamrendszerbeli alakja, minden bit utan szokoz
-- mainBin :: IO ()
-- mainBin = do
--     putStr "n = "
--     n <- readLn :: IO Int

--     -- atalakitjuk binarisra
--     let bin = toBinary n
--         -- csoportositjuk 4-esevel
--         grouped = group4 bin

--     putStrLn grouped


-- -- átalakítás binárisra
-- toBinary :: Int -> String
-- toBinary 0 = "0"
-- -- helper szamolja a biteket, reverse megforditjuk (fontos!)
-- toBinary n = reverse (helper n)
--   where
--      -- ha elfogyott a szam
--     helper 0 = "" 
--     helper x = show (x `mod` 2) ++ helper (x `div` 2)

-- -- mod -> maradek, div -> hany egesz darab fer bele
-- -- 10 mod 2 = 0
-- -- 10 div 2 = 5

-- -- 5 mod 2 = 1
-- -- 5 div 2 = 2

-- -- 2 mod 2 = 0
-- -- 2 div 2 = 1

-- -- 1 mod 2 = 1
-- -- 1 div 2 = 0

-- -- "0" ++ "1" ++ "0" ++ "1" = "0101" -> az utolso 4-et veszi 


-- -- 4-es csoportok
-- group4 :: String -> String
-- group4 [] = []
-- group4 xs =
--     take 4 xs ++ " " ++ group4 (drop 4 xs)


-- -- n 16os szamrendszerbeli alakja, minden 2 karakter utan szokoz 
-- -- használjuk: a b c d e f a 10–15 számokra

-- mainHex :: IO ()
-- mainHex = do
--     putStr "n = "
--     n <- readLn :: IO Int

--     let hex = toHex n
--         grouped = group2 hex

--     putStrLn grouped


-- -- átalakítás 16-os számrendszerbe
-- toHex :: Int -> String
-- toHex 0 = "0"
-- toHex n = reverse (helper n)
--   where
--     helper 0 = ""
--     helper x = digit (x `mod` 16) : helper (x `div` 16)
--     -- ugyanaz mint a binarisnal, csak 16-al


-- -- számjegy átalakítása karakterré
-- digit :: Int -> Char
-- digit x
--     | x < 10 = head (show x) -- ha kisebb 10-nel akk irja ki
--     | x == 10 = 'a'
--     | x == 11 = 'b'
--     | x == 12 = 'c'
--     | x == 13 = 'd'
--     | x == 14 = 'e'
--     | x == 15 = 'f'


-- -- 2-es csoportok
-- group2 :: String -> String
-- group2 [] = []
-- group2 xs =
--     take 2 xs ++ " " ++ group2 (drop 2 xs)


-- -- n értékének megfelelően írjuk ki ezt:
-- -- (0, 0)
-- -- (0, 1) (1, 0)
-- -- (0, 2) (1, 1) (2, 0)
-- -- ...
-- -- (0, n) (1, n-1) ... (n, 0)

-- mainPairs :: IO ()
-- mainPairs = do
--     putStr "n = "
--     n <- readLn :: IO Int

--     mapM_ putStrLn (rows n)
--     -- rows n → lista sorokkal
--     -- mapM_ putStrLn → minden sort külön sorba kiír


-- rows :: Int -> [String]
-- rows n = [row i | i <- [0..n]]


-- row :: Int -> String
-- row i =
--     unwords [ "(" ++ show x ++ ", " ++ show (i - x) ++ ")" | x <- [0..i] ]



-- -- 0-tól n-ig minden szám binárisan
-- mainAllBin :: IO ()
-- mainAllBin = do
--     putStr "n = "
--     n <- readLn :: IO Int

--     -- x felveszi n-ig a szamokat es mineniket atalakitja binarissa
--     let binaries = [toBinary2 x | x <- [0..n]]

--     putStrLn (unwords binaries)


-- -- bináris átalakítás (ugyanaz mint előbb)
-- toBinary2 :: Int -> String
-- toBinary2 0 = "0"
-- toBinary2 n = reverse (helper n)
--   where
--     helper 0 = ""
--     helper x = show (x `mod` 2) ++ helper (x `div` 2)





-- -- Hányszor szerepel egy adott szám a listában?

-- mainCount :: IO ()
-- mainCount = do
--     putStrLn "Adj meg egy listát (pl: [1,2,3,2,2,5]):"
--     ls <- readLn :: IO [Int]

--     putStr "Keresett szám: "
--     x <- readLn :: IO Int

--     let db = count x ls

--     putStrLn ("Ennyiszer szerepel: " ++ show db)


-- -- megszámolja, hányszor van benne x a listában
-- count :: Int -> [Int] -> Int
-- count x ls = length [y | y <- ls, y == x]



-- -- Mely számok kisebbek a lista átlagánál?

-- mainBelowAvg :: IO ()
-- mainBelowAvg = do
--     putStrLn "Adj meg egy listát (pl: [1,2,3,4,5]):"
--     ls <- readLn :: IO [Double]

--     let avg = atlag ls
--         kisebbek = [x | x <- ls, x < avg]

--     putStrLn ("Atlag: " ++ show avg)
--     putStrLn ("Kisebb elemek: " ++ show kisebbek)


-- -- átlag számítása
-- atlag :: [Double] -> Double
-- atlag ls = sum ls / fromIntegral (length ls)


-- Elem-előfordulási statisztika

-- import Data.List (nub)

-- stat :: Eq a => [a] -> [(a, Int)]
-- stat xs = [(x, count x xs) | x <- nub xs]


-- count :: Eq a => a -> [a] -> Int
-- count x xs = length [y | y <- xs, y == x]


-- IV. Írjunk egy-egy Haskell függvényt, amely a billentyűzetről végjelig olvas be karakterláncokat, és

-- - meghatározza a legnagyobbat,
-- - meghatározza a legnagyobb elemek indexét,
-- - az adatok rendezett sorrendjét.

-- import Data.List (sort)

-- readLines :: IO [String]
-- readLines = do
--     line <- getLine
--     if line == "vege"
--         then return []
--         else do
--             rest <- readLines
--             return (line : rest)

-- maxElem :: [String] -> String
-- maxElem = maximum   

-- maxIndexes :: [String] -> [Int]
-- maxIndexes xs = find xs (maximum xs) 0
--   where
--     find [] _ _ = []
--     find (y:ys) m i
--         | y == m = i : find ys m (i+1)
--         | otherwise = find ys m (i+1)

-- sortedList :: [String] -> [String]
-- sortedList = sort

-- main :: IO ()
-- main = do
--     putStrLn "Adj meg szavakat, 'vege'-ig:"

--     ls <- readLines

--     putStrLn ("Max elem: " ++ maxElem ls)

--     putStrLn ("Max indexek: " ++ show (maxIndexes ls))

--     putStrLn ("Rendezve: " ++ show (sortedList ls))


-- fib :: Integer -> Integer
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)

fib :: Integer -> Integer
fib n = seged n 0 1
  where
    seged 0 a _ = a
    seged n a b = seged (n-1) b (a+b)

kiirFib :: [Integer] -> IO ()
kiirFib [] = return ()
kiirFib (x:xs) = do
    putStrLn (show x ++ ": " ++ show (fib x))
    kiirFib xs

main1 :: IO ()
main1 = do
    let lista = [10, 25, 1000, 0, 15, 5000]
    kiirFib lista



nagyVarosok :: Int -> [(String, Int)] -> [String]
nagyVarosok n lista =
    sort [varos | (varos, nepesseg) <- lista, nepesseg > n]

kiirVarosok :: [String] -> IO ()
kiirVarosok [] = return ()
kiirVarosok (x:xs) = do
    putStrLn x
    kiirVarosok xs

main2 :: IO ()
main2 = do
    let n = 150000
    let lista = [("sepsiszentgyorgy",54000),
                 ("kolozsvár",330000),
                 ("marosvasarhely",130000),
                 ("temesvar",310000),
                 ("arad",160000),
                 ("gyergyoszentmiklos",18000),
                 ("nagyvarad",196000)]
    kiirVarosok (nagyVarosok n lista)


nincsNulla :: Int -> Bool
nincsNulla n = not ('0' `elem` show n)

szures :: [Int] -> [Int]
szures lista = [x | x <- lista, nincsNulla x]

main3 :: IO ()
main3 = do
    let lista = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    putStrLn (unwords (map show (szures lista)))


-- email feldarabolása
felbont :: String -> (String, String, String)
felbont email =
    let (nev, rest) = break (=='@') email
        (szerver, domain) = break (=='.') (tail rest)
    in (nev, szerver, tail domain)

-- kiírás
kiirEmail :: (String, String, String) -> IO ()
kiirEmail (nev, szerver, domain) =
    printf "%-20s %-10s %-20s\n" nev szerver domain

main4 :: IO ()
main4 = do
    let szemelyek =
            [("rosalesanthony@example.net", "03/31", "213130957725524"),
             ("robin18@example.net", "02/29", "570620146482"),
             ("bsullivan@example.org", "03/27", "4215057708441701869"),
             ("jameshughes@example.org", "09/27", "4782851642138996"),
             ("douglasjordan@example.net", "03/27", "5289954454350249"),
             ("jwells@example.net", "06/31", "342926219737676"),
             ("spotter@example.com", "01/27", "4917299108623093")]

    putStrLn "felhasználónév      mail-szerver  domain"
    mapM_ (kiirEmail . felbont . (\(e,_,_) -> e)) szemelyek


-- megszámolja a magánhangzókat egy szóban
maganhangzoSzam :: String -> Int
maganhangzoSzam s =
    length [c | c <- s, c `elem` "aeiouAEIOU"]

-- megkeresi a legnagyobb magánhangzó számot
maxMaganhangzo :: [String] -> Int
maxMaganhangzo lista =
    maximum (map maganhangzoSzam lista)

-- kiválasztja a legjobbakat
legjobbak :: [String] -> [String]
legjobbak lista =
    let maxV = maxMaganhangzo lista
    in [s | s <- lista, maganhangzoSzam s == maxV]

-- kiírás egy sorban
kiir :: [String] -> IO ()
-- kiir xs = putStrLn (unwords xs)
kiir xs = mapM_ putStrLn xs

-- MAIN
main5 :: IO ()
main5 = do
    let lista = ["marika", "ANNABELLA", "szidonia", "lorand",
                 "katalin", "feri", "terezia", "Daniel", "zsolti"]

    let eredmeny = legjobbak lista

    kiir eredmeny



-- megkeresi az indexet (Maybe nélkül!)
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = go 0 xs
  where
    go _ [] = -1
    go i (y:ys)
        | x == y    = i
        | otherwise = go (i+1) ys


szomszedok :: String -> [String] -> [String]
szomszedok s lista =
    let sorted = sort lista
        i = indexOf s sorted

        bal
            | i > 0 = [sorted !! (i - 1)]
            | otherwise = []

        jobb
            | i /= -1 && i < length sorted - 1 = [sorted !! (i + 1)]
            | otherwise = []

    in bal ++ jobb


main6 :: IO ()
main6 = do
    let s = "feri"
    let lsS = ["Mari","Zsuzsa","szidi","Lori","kata","feri","teri","Dani","zsolti"]
    mapM_ putStrLn (sort lsS)
    putStrLn(s ++ " szomszedai: ")
    putStrLn (unwords (szomszedok s lsS))


vegsoNulla :: Int -> Bool
vegsoNulla x = abs x `mod` 10 == 0

szures2 :: [Int] -> [Int]
szures2 lista = [x | x <- lista, vegsoNulla x]

kiir2 :: [Int] -> IO ()
kiir2 xs = mapM_ print xs

main7 :: IO ()
main7 = do
    let lista = [120, 456, 3213, 67, -100, -56, -20, 112, 354]

    kiir2 (szures2 lista)