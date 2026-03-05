import Control.Monad.Trans.Cont (reset)
import Graphics.Win32 (eWX_LOGOFF, dEFAULT_CHARSET)
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

