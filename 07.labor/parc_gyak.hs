import Data.List
-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
-- Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
-- ("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
-- ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”

fel1 n ls = map fst (filter (\(x,y) -> y > n) ls)

fel1Main = do
    let n = 150000
        ls = [("sepsiszentgyorgy", 54000),
              ("kolozsvar", 330000),
              ("marosvasarhely", 130000),
              ("temesvar", 310000),
              ("arad", 160000),
              ("gyergyoszentmiklos", 18000),
              ("nagyvarad",196000)]
        ls2 = fel1 n ls

    if null ls2
    then putStrLn ("Nincs " ++ show n ++ " erteknel nagyobb nepesseggel rendelkezo varos.")
    else do
        putStrLn ("A(z) " ++ show n ++ " erteknel nagyobb nepesseggel rendelkezo varosok:")
        mapM_ (\v -> putStrLn ("- " ++ v)) ls2




-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

-- nincsNulla :: Int -> Bool
-- nincsNulla x = '0' `notElem` show x

-- filter(\x -> not ('0' 'elem' show x)) ls
-- show x -> szamot stringge, megnezi ha van e benne nulla (ha van true, ha nincs false),
-- ezt tagadja => csak azok maradnak amiben nincs 0, filter kivalogatja a jo szamokat
-- unwords (map show eredmeny) -> szepen szokozzel kiirja a szamokat

fel2 ls =
    -- let eredmeny = filter nincsNulla ls
    let eredmeny = filter(\x -> not ('0' `elem` show x)) ls
    in if null eredmeny
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
       else putStrLn ("A 0 szamjegyet nem tartalmazo szamok a kovetkezok: " ++ unwords (map show eredmeny))

fel2Main = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    fel2 ls

-- intercalate " " ["a","b","c"]
-- 👉 eredmény:
-- "a b c"

-- import Data.List (intercalate)

-- fel2 ls =
--     let eredmeny = filter (\x -> not ('0' `elem` show x)) ls
--     in if null eredmeny
--        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
--        else putStrLn ("A 0 szamjegyet nem tartalmazo szamok a kovetkezok: " 
--             ++ intercalate " " (map show eredmeny))


-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.
-- Például:

-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
-- class
-- float
-- function
-- Higher-order
-- monad
-- ● Amennyiben nincsenek ilyen karakterláncok, a kimenet a következő:
-- “Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.”

-- ellenőrzi, hogy van-e szám a stringben
nincsSzam :: String -> Bool
nincsSzam s = not(any (`elem` "012345678") s)
-- nincsSzam s = not (any isDigit s)
-- import Data.Char (isDigit)

fel3 ls =
    let eredmeny = sort(filter nincsSzam ls)
    in if null eredmeny
        then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
       else do
           putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
           mapM_ putStrLn eredmeny

fel3Main = do
    let ls = 
            ["2023tuple", "function", "float", "higher-order",
             "variable10", "may13be", "0recursion", "monad", "class"]

    fel3 ls 






-- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- Például:
-- ● Bemenet:
-- s = feri
-- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata

fel4 ls s =
    let r = sort ls
        idx = elemIndex s r
    in case idx of
        Nothing -> putStrLn "Nincs ilyen szo a listaban"
        Just i -> do
            let bal = if i > 0 then r !! (i - 1) else "nincs bal oldali szomszed"
                jobb = if i < length r - 1 then r !! (i + 1) else "nincs jobb oldali szomszed"
            putStrLn (s ++ " baloldali szomszedja " ++ bal ++
                      ", jobboldali szomszedja pedig " ++ jobb)

-- lista !! n -> azt adja vissza ami a lista n-ik poziciojan van



fel4Main = do
    let ls = ["Mari","Zsuzsa","szidi","Lori","kata","feri","teri","Dani","zsolti"]
        s = "feri"       
    fel4 ls s


-- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- értéket.
-- Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
-- 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
-- ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

snd3Elem (_,y,_) = y
fst3Elem (x,_,_) = x
fel5 = do
    let ls = [("iphoneS1", 20, 2500),
            ("huaweiS1", 30, 1700),
             ("huaweiS2", 25,3100), 
             ("samsungA1", 30, 2000),
              ("nokia", 10, 1900),
               ("iphoneS2", 10, 2200),
              ("samsungA2", 15, 1650),
               ("iphone3", 30, 1800)]
    let ls2 = reverse $ sortOn snd3Elem ls
        ls3 = sort ls -- elso elem szerint rendez 
        maxErtek = snd3Elem (head ls2)
        megoldas = map fst3Elem $ takeWhile (\(_,e,_) -> e == maxErtek) ls2
    putStrLn ("A maximalis eladasi ertek " ++ show maxErtek ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:")
    mapM_ putStrLn megoldas




-- 6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- példában szereplő formában, előfordulási érték szerint rendezve.
-- Például:
-- ● Bemenet: [7]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- ● Bemenet: [1, 1, 2]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- ● Bemenet: [1, 1]
-- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- ● Kimenet:
-- Elofordulas: 1 -> Ertek: 3
-- Elofordulas: 1 -> Ertek: 7
-- Elofordulas: 3 -> Ertek: 4
-- Elofordulas: 5 -> Ertek: 2