
import Data.List (unwords)

-- I. feladat

main1 :: IO ()
main1 = do
    putStr "x1 = "
    x1 <- readLn :: IO Int
    putStr "x2 = "
    x2 <- readLn :: IO Int 

    putStrLn ("x1 = " ++ show x1 ++ ", x2 = " ++ show x2)

    let ls = if x1 < x2 then [x1 .. x2] else [x2 .. x1]
        voLs = legtobbVO ls

    putStrLn ("Lista elemeinek osszege: " ++ show (sum ls))
    putStrLn ("Primszamok osszege: " ++ show (primszamOsszeg ls))

    case voLs of
        [] -> putStrLn "Nincs olyan szam"
        (x:_) -> do
            putStrLn ("Legtobb valodi oszto: " ++ show (snd x))
            putStrLn "Ezzel rendelkezo szamok:"
            print (map fst voLs)

-- osztók
osztok :: Int -> [Int]
osztok x = [i | i <- [1 .. x], mod x i == 0]

-- prímszám ellenőrzés
primszam :: Int -> Bool
primszam x = x > 1 && osztok x == [1, x]

-- prímszámok összege
primszamOsszeg :: [Int] -> Int
primszamOsszeg = sum . filter primszam

-- valódi osztók
valodiOsztok :: Int -> [Int]
valodiOsztok x = [i | i <- [2 .. x `div` 2], mod x i == 0]

-- legtöbb valódi osztó
legtobbVO :: [Int] -> [(Int, Int)]
legtobbVO ls = filter (\(_, vo) -> vo == maxVO) ls2
  where
    ls2 = [(szam, length (valodiOsztok szam)) | szam <- ls]
    maxVO = maximum (map snd ls2)

--------------------------------------------------

-- II. feladat

main2 :: IO ()
main2 = do
    putStr "n = "
    n <- readLn :: IO Int

    let fiboLs = fibo n

    putStrLn "Fibonacci szamok (>=50 es < n):"
    putStrLn $ unwords (map show fiboLs)

-- Fibonacci lista
fibo :: Int -> [Int]
fibo n = takeWhile (< n) $ dropWhile (< 50) fiboLista

-- végtelen Fibonacci sorozat
fiboLista :: [Int]
fiboLista = 0 : 1 : zipWith (+) fiboLista (tail fiboLista)
-- getLine -> stringet olvas es ad at, atalakitas read x :: Int
-- readLn -> adott tipuskent ad at, atalakitas readLn :: IO Int




-- - n-ig a prímszámok listáját, úgy hogy a számok közé szóközt ír,
-- - az n kettes számrendszerbeli alakját, úgy hogy minden negyedik bit után egy szóközt ír,
-- - az n 16-os számrendszerbeli alakját, úgy hogy minden két szimbólum után egy szóközt ír, illetve az a, b, c, d, e, f szimbólumokat használja a 10-nél nagyobb számjegyek kódolására,
-- - az n értékének megfelelően a következő sorokat:

--   ```
--   (0, 0)
--   (0, 1) (1, 0)
--   (0, 2) (1, 1) (2, 0)
--   ...
--   (0, n) (1, n-1), ..., (n, 0)
--   ```
-- - az n értékének megfelelően az összes természetes szám kettes számrendszerbeli alakját,
--   például: $$n = 6:\ 0,\ 1,\ 10,\ 11,\ 100,\ 101,\ 110$$.
