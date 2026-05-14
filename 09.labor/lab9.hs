-- # 9. labor
import Data.List (sort)
import Data.Char (isDigit, isUpper, ord)

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate


-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, 
-- amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.

formatText :: String -> String
formatText [] = []
formatText [x] = [x]
formatText (x:y:xs) 
    | x `elem` ".,!?;" && y /= ' '
        = x : ' ' : formatText (y:xs)
    | otherwise = x : formatText(y:xs)

main1 :: IO ()
main1 = do
    let text = "Hello!Hogy vagy?Jol vagyok.Nagyon jo!"

    let result = formatText text

    putStrLn result




-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- - beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
-- - bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód
-- szerepel-e az adatok között,
-- - átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$



-- --------------------------------------------------
-- 1. Beolvasás + rendezés
-- --------------------------------------------------

readAndSortIbans :: FilePath -> IO [String]
readAndSortIbans file = do
    content <- readFile file
    let ibans = lines content
    return (sort ibans)

-- --------------------------------------------------
-- 2. Bináris keresés
-- --------------------------------------------------

binarySearch :: Ord a => a -> [a] -> Bool
binarySearch _ [] = False
binarySearch x xs
    | x == middle = True
    | x < middle  = binarySearch x left
    | otherwise   = binarySearch x right
  where
    mid = length xs `div` 2
    middle = xs !! mid
    left = take mid xs
    right = drop (mid + 1) xs

-- --------------------------------------------------
-- 3. Valid karakterek
-- --------------------------------------------------

validChars :: String -> Bool
validChars = all (\c -> isDigit c || isUpper c)

-- --------------------------------------------------
-- 4. Hossz ellenőrzés
-- --------------------------------------------------

type CountryLength = (String, Int)

parseLine :: String -> CountryLength
parseLine line =
    case words line of
        [country, len] -> (country, read len)
        _ -> error ("Hibas sor: " ++ line)

validLength :: [CountryLength] -> String -> Bool
validLength db iban =
    case lookup country db of
        Nothing -> False
        Just len -> length iban == len
  where
    country = take 2 iban

-- --------------------------------------------------
-- 5. Átrendezés
-- --------------------------------------------------

rearrange :: String -> String
rearrange iban =
    drop 4 iban ++ take 4 iban

-- --------------------------------------------------
-- 6. Karakter konvertálás
-- --------------------------------------------------

convertChar :: Char -> String
convertChar c
    | isDigit c = [c]
    | isUpper c = show (ord c - ord 'A' + 10)
    | otherwise = ""

convertIban :: String -> String
convertIban = concatMap convertChar

-- --------------------------------------------------
-- 7. Mod 97 ellenőrzés
-- --------------------------------------------------

validMod97 :: String -> Bool
validMod97 iban =
    let number = read (convertIban (rearrange iban)) :: Integer
    in number `mod` 97 == 1

-- --------------------------------------------------
-- 8. Teljes validálás
-- --------------------------------------------------

validIban :: [CountryLength] -> String -> Bool
validIban db iban =
       validChars iban
    && validLength db iban
    && validMod97 iban

-- --------------------------------------------------
-- 9. Helyes IBAN-ok kiírása
-- --------------------------------------------------

writeValidIbans :: FilePath -> FilePath -> IO ()
writeValidIbans input output = do
    content <- readFile input
    lengthsContent <- readFile "ibanLength.txt"

    let ibans = lines content
        db = map parseLine (lines lengthsContent)
        okIbans = filter (validIban db) ibans

    writeFile output (unlines okIbans)

-- --------------------------------------------------
-- MAIN
-- --------------------------------------------------

main2 :: IO ()
main2 = do

    -- Beolvasás + rendezés
    sortedIbans <- readAndSortIbans "iban.txt"

    putStrLn "Rendezett IBAN-ok:"
    print sortedIbans

    -- Bináris keresés
    putStrLn "\nAdj meg egy IBAN kodot:"
    input <- getLine

    if binarySearch input sortedIbans
        then putStrLn "Az IBAN szerepel a listaban."
        else putStrLn "Az IBAN nem szerepel a listaban."

    -- Valid IBAN-ok fájlba írása
    writeValidIbans "iban.txt" "okIban.txt"

    putStrLn "\nA valid IBAN-ok bekerultek az okIban.txt fajlba."












-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: 
--vezetéknév, keresztnév, születési dátum. 
--Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból 
--és állapítsuk meg mindegyik személyről, hogy a hét milyen napján született és mikor van a névnapja. 
--A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.

-- ```haskell
-- data Datum = Datum {
--   nap :: Int,
--   honap:: Int,
--   ev :: Int
-- } deriving (Show)

-- data Szemely = Szemely {
--   vnev :: [Char],
--   knev :: [Char],
--   szdatum :: Datum
-- } deriving (Show)
-- ```


-- ==================================================
-- III. FELADAT
-- ==================================================

-- --------------------------------------------------
-- DATUM tipus
-- --------------------------------------------------

data Datum = Datum
    {
        nap :: Int,
        honap :: Int,
        ev :: Int
    } deriving (Show)

-- --------------------------------------------------
-- SZEMELY tipus
-- --------------------------------------------------

data Szemely = Szemely
    {
        vnev :: String,
        knev :: String,
        szdatum :: Datum
    } deriving (Show)

-- --------------------------------------------------
-- Egy sor feldolgozasa
-- pl:
-- "Kovacs Bela 12 5 2001"
-- --------------------------------------------------

parsePerson :: String -> Szemely
parsePerson line =
    case words line of

        [vn, kn, n, h, e] ->
            Szemely
                vn
                kn
                (Datum
                    (read n)
                    (read h)
                    (read e))

        _ -> error "Hibas szemely sor"

-- --------------------------------------------------
-- Het napjanak meghatarozasa
-- --------------------------------------------------

dayOfWeekHu :: Datum -> String
dayOfWeekHu (Datum n h e) =
    case day of
        1 -> "Hetfo"
        2 -> "Kedd"
        3 -> "Szerda"
        4 -> "Csutortok"
        5 -> "Pentek"
        6 -> "Szombat"
        7 -> "Vasarnap"
        _ -> "Hiba"
  where
    (_, _, day) =
        toWeekDate
            (fromGregorian
                (fromIntegral e)
                h
                n)

-- --------------------------------------------------
-- Nevnap sor feldolgozasa
-- pl:
-- "Bela januar-20"
-- --------------------------------------------------

parseNameday :: String -> (String, String)
parseNameday line =
    let ws = words line
    in case ws of

        [] -> ("", "")

        (name:rest) ->
            (name, unwords rest)

-- --------------------------------------------------
-- Nevnapok beolvasasa
-- --------------------------------------------------

readNamedays :: FilePath -> IO [(String, String)]
readNamedays file = do

    content <- readFile file

    let db =
            map parseNameday (lines content)

    return db

-- --------------------------------------------------
-- Nevnap keresese
-- --------------------------------------------------

findNameday :: String -> [(String, String)] -> String
findNameday name db =
    case lookup name db of

        Just day -> day

        Nothing -> "Nincs nevnap"

-- --------------------------------------------------
-- Egy szemely adatainak kiirasa
-- --------------------------------------------------

printPerson :: [(String, String)] -> Szemely -> IO ()
printPerson db person = do

    putStrLn
        ("Nev: "
            ++ vnev person
            ++ " "
            ++ knev person)

    putStrLn
        ("Szuletesi datum: "
            ++ show (nap (szdatum person))
            ++ "."
            ++ show (honap (szdatum person))
            ++ "."
            ++ show (ev (szdatum person)))

    putStrLn
        ("A het napja: "
            ++ dayOfWeekHu (szdatum person))

    putStrLn
        ("Nevnap: "
            ++ findNameday
                    (knev person)
                    db)

    putStrLn ""

-- --------------------------------------------------
-- Main
-- --------------------------------------------------

main3 :: IO ()
main3 = do

    -- szemelyek beolvasasa
    peopleContent <- readFile "szemelyek.txt"

    -- nevnapok beolvasasa
    namedays <- readNamedays "nevnapok.txt"

    -- szemely lista
    let people =
            map parsePerson
                (lines peopleContent)

    -- kiiras
    mapM_ (printPerson namedays) people