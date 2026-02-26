szjSzorzat 0 = 1
szjSzorzat n = mod n 10 * szjSzorzat( div n 10)

szjSzorzat2 n
    | n < 0 = error "neg. szam"
    | div n 10 == 0 = n
    | otherwise = mod n 10 * szjSzorzat (div n 10)

szjSzorzat3 n res 
    | n < 0 = error "neg szam"
    | div n 10 == 0 = res * n
    | otherwise = szjSzorzat3 (div n 10) (res * (mod n 10))