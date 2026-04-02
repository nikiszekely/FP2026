-- III. feladat
atlagTu ls = [(nev, atlag jegyek) | (nev, jegyek) <- ls]
    where 
        atlag ls2 = sum ls2 / fromIntegral (length ls2)

atlagTu2 ls = mapM_ (\(nev, atlagjegyek) -> putStrLn (nev ++ " " ++ show atlagjegyek)) ls
    where 
        ls2 = [(nev, atlag jegyek) | (nev, jegyek) <- ls]
        atlag ls2 = sum ls2 / fromIntegral (length ls2)
main = do
    let lsNevJegy = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]), ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
    -- mapM_ (\(nev, atlagjegyek) -> printf "%s" (nev ++ "%.2f" (atlagjegyek :: Float)) (atlagTu lsNevJegy)
    putStrLn "\nCsak meghivas"
    atlagTu2 lsNevJegy 
