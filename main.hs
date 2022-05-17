module Equation where

-- Quick power method to compute n^m
-- E.g: ghci> quick_power 2 10
quick_power :: Integer -> Integer -> Integer
quick_power n m
    | m == 0 = 1
    | otherwise = if (rem m 2 == 1) then n * tmp * tmp
                    else tmp * tmp
        where tmp = quick_power n (div m 2)

-- Iteration
-- E.g: ghci> iter 0 (\x -> cos(x) - x) 20
iter :: Double->(Double->Double)->Integer->[Double]
iter x0 f n 
    | n == 0 = [x0]
    | otherwise = tmp ++ [f (last tmp)]
    where 
        tmp = iter x0 f (n - 1)

-- Binary Search
-- E.g: ghci> bisection (\x->(cos(x)-x)) 0 1
bisection :: (Double->Double)->Double->Double->Double 
bisection f a b
    | (b - a > 0.00001) = if ((f mid) * (f a) < 0) then (bisection f a mid) else (bisection f mid b)
    | otherwise = a
    where mid = (a + b) / 2

-- Fixed Point Iteration
-- E.g: ghci> fpIter cos 0.1 100
fpIter :: (Double->Double)->Double->Int->Double 
fpIter f x n 
    | n > 0 = f (fpIter f x (n - 1))
    | n == 0 = x

-- Newton's Method
-- E.g: ghci> ntIter (\x->(cos(x)-x)) (\x->(-sin(x)-1)) 0.1 10
ntIter :: (Double->Double)->(Double->Double)->Double->Int->Double 
ntIter f f' x n
    | n > 0 = ntIter f f' (x - (f x) / (f' x)) (n - 1)
    | n == 0 = x

-- Secant Method
-- E.g: ghci> seIter (\x->(cos(x)-x)) 0.1 0 10
seIter :: (Double->Double)->Double->Double->Int->Double 
seIter f p1 p0 n
    | delta < 1e-10 = p1
    | n > 0 = seIter f (p1 - (f p1) / (((f p1) - (f p0)) / (p1 - p0))) p1 (n - 1)
    | n == 0 = p1
    where delta = if (p1 > p0) then p1 - p0 else p0 - p1

-- False Position Method
-- E.g: ghci> falsePIter (\x->(cos(x)-x)) 0.1 0 10
falsePIter :: (Double->Double)->Double->Double->Int->Double
falsePIter f p1 p0 n
    | delta < 1e-10 = p1
    | n == 0 = p1
    | otherwise = falsePIter f p3 p2 (n - 1)
    where 
        p2 = seIter f p1 p0 1
        a = p2 - (f p2) / (((f p2) - (f p1)) / (p2 - p1))
        b = p2 - (f p2) / (((f p2) - (f p0)) / (p2 - p0))
        p3 = if ((f p2) * (f p1) < 0) then a else b
        delta = if (p1 > p2) then p1 - p2 else p2 - p1

-- Aikten Delta2 Method, speed up p_n+1 = f(p_n) iteration
-- E.g: ghci> aitken2 cos 0.1 10
aitken2 :: (Double->Double)->Double->Int->Double 
aitken2 f x n
    | delta < 1e-10 = x
    | n > 0 = aitken2 f x3 (n - 1)
    | n == 0 = x
    where 
        x1 = f x
        x2 = f x1
        x3 = x - (x1 - x) * (x1 - x) / (x2 - 2 * x1 + x)
        delta = if (x2 + x - 2 * x1 > 0) then x2 + x - 2 * x1 else -(x2 + x - 2 * x1)

-- Polynomial Value
-- E.g: ghci> polyValue [1,2,3] 3
polyValue :: [Double]->Double->Double
polyValue a x 
    | len == 1 = a0
    | otherwise = a0 + x * (polyValue (take (len - 1) a) x)
    where 
        a0 = last a
        len = length a

-- Lagrange Polynomial
-- E.g: ghci> lagrange [(1, 2), (3, 4)] 5
-- E.g: ghci> lagrange [(1, 0), (-1, 0), (0, -1)] 5
lnk :: [Double]->Double->Double->Double
lnk xs x xj
    | xs == [] = 1
    | xi == xj = lnk (tail xs) x xj
    | otherwise = (x - xi) / (xj - xi) * (lnk (tail xs) x xj)
        where  xi = head xs

lnk1 :: [Double]->Double->Double
lnk1 xs xj
    | xs == [] = 0
    | xi == xj = lnk1 (tail xs) xj
    | otherwise = (1 / (xj - xi)) + (lnk1 (tail xs) xj)
        where  xi = head xs


lagrange :: [(Double, Double)]->Double->Double
lagrange v x = sum p
    where 
        n = length v
        xs = [fst (v !! i) | i <- [0..(n-1)]]
        lnks = map (lnk xs x) xs
        p = [(snd (v !! i)) * (lnks !! i) | i <- [0..(n-1)]]

-- Muller's Method
-- E.g: ghci> mlMethod (\x->x*x-3*x+1) 0.2 0.1 0
mlMethod :: (Double->Double)->Double->Double->Double->Double
mlMethod f x2 x1 x0
    | delta < 1e-10 = x2
    | otherwise = mlMethod f x3 x2 x1
        where
            delta = if (x2 > x1) then (x2 - x1) else (x1 - x2)
            lag = lagrange [(x0, (f x0)), (x1, (f x1)), (x2, (f x2))]
            x3 = seIter lag 0.1 0 20
            
-- Hermite Interpoltion Polynomial
-- E.g: ghci> hermite [(0, -1), (1, 0), (-1, 0)] [(0, 0), (1, 2), (-1, -2)] 4
-- ans=15 since y = x^2-1
hermite :: [(Double, Double)]->[(Double, Double)]->Double->Double
hermite v v1 x = sum p
    where
        n = length v
        xs = [fst (v !! i) | i <- [0..(n-1)]]
        ys = [snd (v !! i) | i <- [0..(n-1)]]
        y1s = [snd (v1 !! i) | i <- [0..(n-1)]]
        lnks = map (lnk xs x) xs
        lnk1s = map (lnk1 xs) xs
        h = [(1 - 2 * (x - (xs !! i)) * (lnk1s !! i)) 
                * (lnks !! i) * (lnks !! i) | i <- [0..(n-1)]]
        hh = [(x - (xs !! i)) * (lnks !! i) * (lnks !! i) | i <- [0..(n-1)]]
        p = [(ys !! i) * (h !! i) + (y1s !! i) * (hh !! i) | i <- [0..(n-1)]]

-- Trapezoidal rule for numerical integration
-- E.g: ghci> trIntegrate (\x->x) 1 2
trIntegrate :: (Double->Double)->Double->Double->Double
trIntegrate f a b = (b - a) / 2 * ((f a) + (f b))

-- Simpson's rule for numerical integration
-- E.g: ghci> trIntegrate (\x->x) 1 2
simIntegrate :: (Double->Double)->Double->Double->Double
simIntegrate f a b = (b - a) / 6 * ((f a) + 4 * (f ((a + b) / 2)) + (f b))

-- Midpoint rule for numerical integration
-- E.g: ghci> mdIntegrate (\x->x) 1 2
mdIntegrate :: (Double->Double)->Double->Double->Double
mdIntegrate f a b = (b - a) * (f ((a + b) / 2))

-- Composite rule for numerical integration
-- E.g: ghci> csIntegrate (\x->x) simIntegrate [i * 0.1 | i <- [0..10]]
-- E.g: ghci> csIntegrate (\x->x) mdIntegrate [i * 0.1 | i <- [0..10]]
-- E.g: ghci> csIntegrate (\x->x) trIntegrate [i * 0.1 | i <- [0..10]]
csIntegrate :: (Double->Double)->((Double->Double)->Double->Double->Double)->[Double]->Double
csIntegrate f sim xs
    | (length xs) == 2 = ans
    | otherwise = ans + (csIntegrate f sim (tail xs))
    where
        x1 = head xs
        x2 = head (tail xs)
        ans = sim f x1 x2

integrate :: [Double]->[Double]->Double
integrate xs ys 
    | length xs == 1 = 0
    | otherwise =  csIntegrate f simIntegrate xs 
    where f = elim_f (nsInter xs ys)

-- Euler's Method for ODE
-- E.g: ghci> eulerMethod (\t y->y) 3 [0.01*i | i <- [0..3]]
-- ans: y=3*exp(t)
-- E.g: ghci> map (\x->3*exp(x)) [0.01*i | i <- [0..3]]
-- the result is close
eulerMethod :: (Double->Double->Double)->Double->[Double]->[Double]
eulerMethod f alpha xs 
    | n == 1 = [alpha]
    | otherwise = pre ++ [ytj + h * (f tj ytj)]
    where
        n = length xs
        tj1 = xs !! (n - 1)
        tj = xs !! (n - 2)
        h = tj1 - tj
        ytj = last pre
        pre = eulerMethod f alpha [(xs !! i) | i <- [0..(n-2)]]

-- Corrected Euler's Method
-- E.g: ghci> ceulerMethod (\t y->y) 3 [0.01*i | i <- [0..3]] 
-- the result of this method is more close to "map (\x->3*exp(x)) [0.01 * i | i <- [0..3]]" then Euler's Method
ceulerMethod :: (Double->Double->Double)->Double->[Double]->[Double]
ceulerMethod f alpha xs
 | n == 1 = [alpha]
 | otherwise = pre ++ [(h / 2) * ((f tj1 ytj1) + (f tj ytj)) + ytj]
 where
     n = length xs
     tj1 = xs !! (n - 1)
     tj = xs !! (n - 2)
     h = tj1 - tj
     ytj = last pre
     pre = ceulerMethod f alpha [(xs !! i) | i <- [0..(n-2)]]
     ytj1 = last pred
     pred = eulerMethod f alpha xs

-- Runge-Kutta Method of Order 4
-- E.g: ghci> rk4Method (\t y->y) 3 [0.01*i | i <- [0..3]]
-- the result of this method is more close to "map (\x->3*exp(x)) [0.01 * i | i <- [0..3]]" than other methods above
rk4Method :: (Double->Double->Double)->Double->[Double]->[Double]
rk4Method f alpha xs
 | n == 1 = [alpha]
 | otherwise = pre ++ [ytj + (k1 + 2 * k2 + 2 * k3 + k4) / 6]
 where
     n = length xs
     tj1 = xs !! (n - 1)
     tj = xs !! (n - 2)
     h = tj1 - tj
     ytj = last pre
     pre = rk4Method f alpha [(xs !! i) | i <- [0..(n-2)]]
     k1 = h * (f tj ytj)
     k2 = h * (f (tj + h / 2) (ytj + k1 / 2))
     k3 = h * (f (tj + h / 2) (ytj + k2 / 2))
     k4 = h * (f tj1 (ytj + k3))

-- Adams Fourth-Order Predictor-Corrector
-- E.g: ghci> a4pcMethod (\t y->y) 3 [0.01*i | i <- [0..10]]
a4Method :: (Double->Double->Double)->Double->[Double]->[Double]
a4Method f alpha xs
 | n == 4 = rk4Method f alpha xs
 | otherwise = pre ++ [ytj + (h / 24) * (55 * (f tj ytj) - 59 * (f tj1 ytj1) + 37 * (f tj2 ytj2) - 9 * (f tj3 ytj3))]
 where
     n = length xs
     pre = a4Method f alpha [(xs !! i) | i <- [0..(n-2)]]
     ntj = xs !! (n - 1)
     tj = xs !! (n - 2)
     tj1 = xs !! (n - 3)
     tj2 = xs !! (n - 4)
     tj3 = xs !! (n - 5)
     h = ntj - tj
     ytj = pre !! (n - 2)
     ytj1 = pre !! (n - 3)
     ytj2 = pre !! (n - 4)
     ytj3 = pre !! (n - 5)


a4pcMethod :: (Double->Double->Double)->Double->[Double]->[Double]
a4pcMethod f alpha xs
 | n == 4 = rk4Method f alpha xs
 | otherwise = pre ++ [ytj + (h / 24) * (9 * (f ntj nytj) + 19 * (f tj ytj) - 5 * (f tj1 ytj1) + (f tj2 ytj2))]
 where
     n = length xs
     pred = a4Method f alpha xs
     pre = a4pcMethod f alpha [(xs !! i) | i <- [0..(n-2)]]
     ntj = xs !! (n - 1)
     tj = xs !! (n - 2)
     tj1 = xs !! (n - 3)
     tj2 = xs !! (n - 4)
     h = ntj - tj
     nytj = pred !! (n - 1)
     ytj = pre !! (n - 2)
     ytj1 = pre !! (n - 3)
     ytj2 = pre !! (n - 4)



-- Runge Kutta 4-order for van der Pol equation:
-- y'' = (1-y^2)y' + y, y(0) = 1, y'(0) = 0
-- ghci> xs = [0.1*i | i <- [0..100]]
-- ghci> ys = rk4Method_vdP 0 xs
-- ghci> y' = nsInter xs ys
-- ghci> y1 = elim_f y'
-- ghci> rk4Method (\t y -> (y1 t)) 1 [0.1*i | i <- [0..100]]
rk4Method_vdP :: Double->[Double]->[Double]
rk4Method_vdP alpha xs
 | n == 1 = [alpha]
 | otherwise = pre ++ [ytj + (k1 + 2 * k2 + 2 * k3 + k4) / 6]
 where
     n = length xs
     tj1 = xs !! (n - 1)
     tj = xs !! (n - 2)
     h = tj1 - tj
     ytj = last pre
     pre = rk4Method_vdP alpha [(xs !! i) | i <- [0..(n-2)]]
     y = (integrate [(xs !! i) | i <- [0..(n-2)]] pre) + 1
     f = \t y' -> 1 * (1 - y ^ 2) * y' - y
     k1 = h * (f tj ytj)
     k2 = h * (f (tj + h / 2) (ytj + k1 / 2))
     k3 = h * (f (tj + h / 2) (ytj + k2 / 2))
     k4 = h * (f tj1 (ytj + k3))

ode_method = [eulerMethod, ceulerMethod, rk4Method, a4Method, a4pcMethod]



-- Natural spline interpolation
-- E.g: ghci> nsInter [1, 2, 3] [4, 5, 6] 2.5
generate_luz :: Int -> [Double] -> [Double] -> [Double] -> [[Double]]
generate_luz n xs h alpha 
    | n == 0 = [[1], [0], [0]]
    | n == (length xs) - 1 = [lst_l ++ [1], lst_u, lst_z ++ [0]]
    | otherwise = [lst_l ++ [li], lst_u ++ [ui], lst_z ++ [zi]]
    where
        lst = generate_luz (n - 1) xs h alpha
        lst_l = lst !! 0
        lst_u = lst !! 1
        lst_z = lst !! 2
        li = 2 * ((xs !! (n + 1)) - (xs !! (n - 1))) - (h !! (n - 1)) * (lst_u !! (n - 1))
        ui = (h !! n) / li
        zi = ((alpha !! n) - (h !! (n - 1)) * (lst_z !! (n - 1))) / li

generate_abcd :: Int -> [Double] -> [Double] -> [Double] -> [Double] -> [[Double]]
generate_abcd n z u h ys
    | n == (length ys) - 1 = [[ys !! n], [], [0], []]
    | otherwise = [[aj] ++ lst_a, [bj] ++ lst_b, [cj] ++ lst_c, [dj] ++ lst_d]
    where
        lst = generate_abcd (n + 1) z u h ys
        lst_a = lst !! 0
        lst_b = lst !! 1
        lst_c = lst !! 2
        lst_d = lst !! 3
        cc = [0 | i <- [0..n]] ++ lst_c
        cj = (z !! n) - (u !! n) * (cc !! (n + 1))
        bj = ((ys !! (n + 1)) - (ys !! n)) / (h !! n) - (h !! n) * ((cc !! (n + 1)) + 2 * cj) / 3
        dj = ((cc !! (n + 1)) - cj) / (3 * (h !! n))
        aj = ys !! n

find_interval :: [Double]->Double->Int->Maybe Int
find_interval xs x n
    | n == (length xs) - 1 = Nothing
    | otherwise = if (x1 <= x && x <= x2) then (Just n) else (find_interval xs x (n + 1))
    where
        x1 = xs !! n
        x2 = xs !! (n + 1)

elim :: Maybe Int -> Int
elim Nothing = 0
elim (Just a) = a

elim_d :: Maybe Double -> Double
elim_d Nothing = 0
elim_d (Just a) = a

elim_f :: (Double-> Maybe Double) -> (Double->Double)
elim_f f x = elim_d (f x)

nsInter :: [Double]->[Double]->(Double ->Maybe Double)
nsInter xs ys x  
    | i == Nothing = Nothing
    | otherwise = Just ((a !! (elim i)) + 
                        (b !! (elim i)) * (x - (xs !! (elim i))) + 
                        (c !! (elim i)) * (x - (xs !! (elim i))) ^ 2 + 
                        (d !! (elim i)) * (x - (xs !! (elim i))) ^ 3)
    where
        n = length xs - 1
        h = [(xs !! (i + 1)) - (xs !! i) | i <- [0..(n-1)]]
        alpha = [0] ++ [3 / (h !! i) * ((ys !! (i + 1)) - (ys !! i)) - 3 / (h !! (i - 1)) * ((ys !! i) - (ys !! (i - 1))) | i <- [1..(n-1)]]
        tmp = generate_luz n xs h alpha
        l = tmp !! 0
        u = tmp !! 1
        z = tmp !! 2
        tmp_abcd = generate_abcd 0 z u h ys
        a = tmp_abcd !! 0
        b = tmp_abcd !! 1
        c = tmp_abcd !! 2
        d = tmp_abcd !! 3
        i = find_interval xs x 0

-- Clamped spline interpolation
-- E.g: ghci> csInter [1, 2, 3] [4, 5, 6] 1 1 2.5
generate_luz' :: Int -> [Double] -> [Double] -> [Double] -> [[Double]]
generate_luz' i xs h alpha 
    | i == 0 = [[2 * (h !! 0)], [0.5], [(alpha !! 0) / (2 * (h !! 0))]]
    | i == n = [lst_l ++ [(h !! (n - 1)) * (2 - (lst_u !! (n - 1)))], lst_u,
                lst_z ++ [((alpha !! n) - (h !! (n - 1)) * (lst_z !! (n - 1))) / ((h !! (n - 1)) * (2 - (lst_u !! (n - 1))))]]
    | otherwise = [lst_l ++ [li], lst_u ++ [ui], lst_z ++ [zi]]
    where
        n = (length xs) - 1
        lst = generate_luz (n - 1) xs h alpha
        lst_l = lst !! 0
        lst_u = lst !! 1
        lst_z = lst !! 2
        li = 2 * ((xs !! (i + 1)) - (xs !! (i - 1))) - (h !! (i - 1)) * (lst_u !! (i - 1))
        ui = (h !! i) / li
        zi = ((alpha !! i) - (h !! (i - 1)) * (lst_z !! (i - 1))) / li

generate_abcd' :: Int -> [Double] -> [Double] -> [Double] -> [Double] -> [[Double]]
generate_abcd' n z u h ys
    | n == (length ys) - 1 = [[ys !! n], [], [z !! n], []]
    | otherwise = [[aj] ++ lst_a, [bj] ++ lst_b, [cj] ++ lst_c, [dj] ++ lst_d]
    where
        lst = generate_abcd (n + 1) z u h ys
        lst_a = lst !! 0
        lst_b = lst !! 1
        lst_c = lst !! 2
        lst_d = lst !! 3
        cc = [0 | i <- [0..n]] ++ lst_c
        cj = (z !! n) - (u !! n) * (cc !! (n + 1))
        bj = ((ys !! (n + 1)) - (ys !! n)) / (h !! n) - (h !! n) * ((cc !! (n + 1)) + 2 * cj) / 3
        dj = ((cc !! (n + 1)) - cj) / (3 * (h !! n))
        aj = ys !! n

csInter :: [Double]->[Double]->Double->Double->(Double->Maybe Double)
csInter xs ys fpo fpn x
    | i == Nothing = Nothing
    | otherwise = Just ((a !! (elim i)) + 
                        (b !! (elim i)) * (x - (xs !! (elim i))) + 
                        (c !! (elim i)) * (x - (xs !! (elim i))) ^ 2 + 
                        (d !! (elim i)) * (x - (xs !! (elim i))) ^ 3)
    where
        n = length xs - 1
        h = [(xs !! (i + 1)) - (xs !! i) | i <- [0..(n-1)]]
        alpha = [3 * ((ys !! 1) - (ys !! 0)) / (h !! 0) - 3 * fpo] ++
                [3 / (h !! i) * ((ys !! (i + 1)) - (ys !! i)) - 3 / (h !! (i - 1)) * ((ys !! i) - (ys !! (i - 1))) | i <- [1..(n-1)]] ++
                [3 * fpn - 3 * ((ys !! n) - (ys !! (n - 1))) / (h !! (n - 1))]
        tmp = generate_luz' n xs h alpha
        l = tmp !! 0
        u = tmp !! 1
        z = tmp !! 2
        tmp_abcd = generate_abcd' 0 z u h ys
        a = tmp_abcd !! 0
        b = tmp_abcd !! 1
        c = tmp_abcd !! 2
        d = tmp_abcd !! 3
        i = find_interval xs x 0



psInter_generate_abcd :: [Double]->[Double]->[[Double]]
psInter_generate_abcd x y = solve_LES_GE eq b 0
    where
        n = (length x) - 1
        eq1 = [ [0 | i <- [0..(j*4-1)]] ++ [1] ++ [0 | i <- [(j*4+1)..(4*n-1)]]
            | j <- [0..(n-1)]]
        eq2 = [ [0 | i <- [0..(j*4-1)]] ++ 
                    [1, ((x !! (j + 1)) - (x !! j)), ((x !! (j + 1)) - (x !! j)) ^ 2, ((x !! (j + 1)) - (x !! j)) ^ 3, -1]
                    ++ [0 | i <- [(j*4+5)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq3 = [ [0 | i <- [0..(j*4-1)]] ++
                    [0, 1, 2 * ((x !! (j + 1)) - (x !! j)), 3 * ((x !! (j + 1)) - (x !! j)) ^ 2, 0, -1]
                    ++ [0 | i <- [(j*4+6)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq4 = [ [0 | i <- [0..(j*4-1)]] ++
                    [0, 0, 2, 6 * ((x !! (j + 1)) - (x !! j)), 0, 0, -2]
                     ++ [0 | i <- [(j*4+7)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq5 = [[0 | i <- [0..(4*(n-1)-1)]] ++
               [1, ((x !! n) - (x !! (n - 1))), ((x !! n) - (x !! (n - 1))) ^ 2, ((x !! n) - (x !! (n - 1))) ^ 3]]
        eq6 = [[0,-1,0,0] ++ [0 | i <- [4..(4*(n-1)-1)]] ++
               [0, 1, 2 * ((x !! n) - (x !! (n - 1))), 3 * ((x !! n) - (x !! (n - 1))) ^ 2]]
        eq7 = [[0,0,-2,0] ++ [0 | i <- [4..(4*(n-1)-1)]] ++
               [0, 0, 2, 6 * ((x !! n) - (x !! (n - 1)))]]
        eq = eq1 ++ eq2 ++ eq3 ++ eq4 ++ eq5 ++ eq6 ++ eq7
        b1 = [[(y !! j)] | j <- [0..(n-1)]]
        b2 = [[0] | j <- [0..(n-2)]]
        b3 = [[0] | j <- [0..(n-2)]]
        b4 = [[0] | j <- [0..(n-2)]]
        b5 = [[y !! 0]]
        b6 = [[0]]
        b7 = [[0]]
        b = b1 ++ b2 ++ b3 ++ b4 ++ b5 ++ b6 ++ b7
        
-- Periodic spline interpolation (ys[0] == ys[n] is necessary!)
-- E.g: ghci> psInter [1,2,3] [1,2,1] 2.5
psInter :: [Double]->[Double]->(Double->Maybe Double)
psInter xs ys x 
    | i == Nothing = Nothing
    | otherwise = Just (aj + bj * (x - xj) + cj * (x - xj) ^ 2 + dj * (x - xj) ^ 3)
    where 
        tmp = psInter_generate_abcd xs ys
        i = find_interval xs x 0
        j = elim i
        xj = xs !! j
        aj = tmp !! (4 * j) !! 0
        bj = tmp !! (4 * j + 1) !! 0
        cj = tmp !! (4 * j + 2) !! 0
        dj = tmp !! (4 * j + 3) !! 0

fsInter_generate_abcd :: [Double]->[Double]->[[Double]]
fsInter_generate_abcd x y = solve_LES_GE eq b 0
    where
        n = (length x) - 1
        eq1 = [ [0 | i <- [0..(j*4-1)]] ++ [1] ++ [0 | i <- [(j*4+1)..(4*n-1)]]
            | j <- [0..(n-1)]]
        eq2 = [ [0 | i <- [0..(j*4-1)]] ++ 
                    [1, ((x !! (j + 1)) - (x !! j)), ((x !! (j + 1)) - (x !! j)) ^ 2, ((x !! (j + 1)) - (x !! j)) ^ 3, -1]
                    ++ [0 | i <- [(j*4+5)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq3 = [ [0 | i <- [0..(j*4-1)]] ++
                    [0, 1, 2 * ((x !! (j + 1)) - (x !! j)), 3 * ((x !! (j + 1)) - (x !! j)) ^ 2, 0, -1]
                    ++ [0 | i <- [(j*4+6)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq4 = [ [0 | i <- [0..(j*4-1)]] ++
                    [0, 0, 2, 6 * ((x !! (j + 1)) - (x !! j)), 0, 0, -2]
                     ++ [0 | i <- [(j*4+7)..(4*n-1)]]
            | j <- [0..(n-2)]]
        eq5 = [[0 | i <- [0..(4*(n-1)-1)]] ++
               [1, ((x !! n) - (x !! (n - 1))), ((x !! n) - (x !! (n - 1))) ^ 2, ((x !! n) - (x !! (n - 1))) ^ 3]]
        eq6 = [[0,0,0,1,0,0,0,-1] ++ [0 | i <- [8..(4*n-1)]]]
        eq7 = [[0 | i <- [8..(4*n-1)]] ++ [0,0,0,1,0,0,0,-1]]
        eq = eq1 ++ eq2 ++ eq3 ++ eq4 ++ eq5 ++ eq6 ++ eq7
        b1 = [[(y !! j)] | j <- [0..(n-1)]]
        b2 = [[0] | j <- [0..(n-2)]]
        b3 = [[0] | j <- [0..(n-2)]]
        b4 = [[0] | j <- [0..(n-2)]]
        b5 = [[y !! n]]
        b6 = [[0]]
        b7 = [[0]]
        b = b1 ++ b2 ++ b3 ++ b4 ++ b5 ++ b6 ++ b7
        
-- Forced spline interpolation 
-- E.g: ghci> fsInter [1,2,3] [1,2,1] 2.5
fsInter :: [Double]->[Double]->(Double->Maybe Double)
fsInter xs ys x 
    | i == Nothing = Nothing
    | otherwise = Just (aj + bj * (x - xj) + cj * (x - xj) ^ 2 + dj * (x - xj) ^ 3)
    where 
        tmp = psInter_generate_abcd xs ys
        i = find_interval xs x 0
        j = elim i
        xj = xs !! j
        aj = tmp !! (4 * j) !! 0
        bj = tmp !! (4 * j + 1) !! 0
        cj = tmp !! (4 * j + 2) !! 0
        dj = tmp !! (4 * j + 3) !! 0


matrix_mult :: [[Double]] -> [[Double]] -> [[Double]]
matrix_mult a b =
    [
        [(sum [((a !! i) !! k) * ((b !! k) !! j) | k <- [0..(m-1)]])| j <- [0..(p-1)]]
        | i <- [0..(n-1)] 
    ]
    where
        n = length a
        m = length b
        p = length (b !! 0)

matrix_plus :: [[Double]] -> [[Double]] -> [[Double]]
matrix_plus a b =
    [
        [((a !! i) !! j) + ((b !! i) !! j) | j <- [0..(m-1)]]
        | i <- [0..(n-1)]
    ]
    where
        n = length a
        m = length (a !! 0)

matrix_dot :: [[Double]] -> Double -> [[Double]]
matrix_dot a b =
    [
        [((a !! i) !! j) * b | j <- [0..(m-1)]]
        | i <- [0..(n-1)]
    ]
    where
        n = length a
        m = length (a !! 0)

matrix_minus :: [[Double]] -> [[Double]] -> [[Double]]
matrix_minus a b =
    [
        [((a !! i) !! j) - ((b !! i) !! j) | j <- [0..(m-1)]]
        | i <- [0..(n-1)]
    ]
    where
        n = length a
        m = length (a !! 0)

matrix_trans :: [[Double]] -> [[Double]]
matrix_trans a = 
    [
        [((a !! j) !! i) | j <- [0..n-1]]
        | i <- [0..m-1]
    ]
    where
        n = length a
        m = length (a !! 0)

elim_LU :: Maybe ([[Double]], [[Double]]) -> ([[Double]], [[Double]])
elim_LU (Just (a, b)) = (a, b)
elim_LU Nothing = ([], [])

generate_ith_row_column :: [[Double]] -> Int -> Maybe ([[Double]], [[Double]])
generate_ith_row_column a i
    | i == 0 = if (a11 == 0) then Nothing else
                Just ([[a11] ++ [((a !! 0) !! j) | j <- [1..(n-1)]]], [[1] ++ [((a !! j) !! 0) / a11| j <- [1..(n-1)]]])
    | tmp == Nothing = Nothing
    | lu_ii == 0 = Nothing
    | otherwise = Just (u ++ [ui], l ++ [li])
    where 
        n = length a
        a11 = ((a !! 0) !! 0)
        tmp = generate_ith_row_column a (i - 1)
        u = fst (elim_LU tmp)
        l = snd (elim_LU tmp)
        aii = ((a !! i) !! i)
        lu_ii = aii - (sum [((l !! k) !! i) * ((u !! k) !! i) | k <- [0..(i - 1)]])
        lii = 1
        uii = lu_ii
        ui = [0 | j <- [0..(i-1)]] ++ [uii] ++ [(((a !! i) !! j) - (sum [((u !! k) !! j) * ((l !! k) !! i) | k <- [0..(i-1)]])) / lii | j <- [(i + 1)..(n-1)]]
        li = [0 | j <- [0..(i-1)]] ++ [lii] ++ [(((a !! j) !! i) - (sum [((u !! k) !! i) * ((l !! k) !! j) | k <- [0..(i-1)]])) / uii | j <- [(i + 1)..(n-1)]]

-- LU Factorization
-- E.g: ghci> tmp = lu_fact [[4, -1, 1], [4, -8, 1], [-2, 1, 5]]
--      ghci> mult (fst tmp) (snd tmp)
lu_fact :: [[Double]]->([[Double]], [[Double]])
lu_fact a = (matrix_trans (snd tmp), fst tmp)
    where
        tmp = elim_LU (generate_ith_row_column a ((length a) - 1))

solve_l :: Int->[[Double]]->[[Double]]->[[Double]]
solve_l i l y
    | i == 0 = (if ((l !! 0 !! 0) == 0) then [[1]] else [[((y !! 0) !! 0) / ((l !! 0) !! 0)]])
    | otherwise = pre ++ (if ((l !! i !! i) == 0) then [[1]] else [[(((y !! i) !! 0) - s) / ((l !! i) !! i)]])
    where
        pre = solve_l (i - 1) l y
        s = sum [((l !! i) !! j) * ((pre !! j) !! 0) | j <- [0..(i-1)]]

solve_u :: Int->[[Double]]->[[Double]]->[[Double]]
solve_u i u y
    | i == n - 1 = (if ((u !! i !! i) == 0) then [[1]] else [[((y !! i) !! 0) / ((u !! i) !! i)]])
    | otherwise = (if ((u !! i !! i) == 0) then [[1]] else [[(((y !! i) !! 0) - s) / ((u !! i) !! i)]]) ++ pre
    where
        n = length u
        pre = solve_u (i + 1) u y
        npre = [[0] | j <- [0..i]] ++ pre
        s = sum [((u !! i) !! j) * ((npre !! j) !! 0) | j <- [(i+1)..(n-1)]]

-- Solve linear equation systems by LU factorization
-- E.g: ghci> solve_LES_LU [[4,-1,1],[4,-8,1],[-2,1,5]] [[7],[-21],[15]]
solve_LES_LU :: [[Double]]->[[Double]]->[[Double]]
solve_LES_LU a y = 
    solve_u 0 u x
    where 
        n = length a
        lu = lu_fact a
        l = fst lu
        u = snd lu
        x = solve_l (n - 1) l y 

-- Jacobi iteration to solve linear equation system
-- E.g: ghci> solve_LES_Jacobi [[4,-1,1],[4,-8,1],[-2,1,5]] [[7],[-21],[15]] [[0], [0], [0]] 40
solve_LES_Jacobi :: [[Double]]->[[Double]]->[[Double]]->Int->[[Double]]
solve_LES_Jacobi a y x nn
    | nn == 0 = x
    | otherwise = matrix_plus (matrix_mult t (solve_LES_Jacobi a y x (nn - 1))) b
    where
        n = length a
        dr = [  [0 | j <- [0..(i-1)]] ++ [1 / ((a !! i) !! i)] ++ [0 | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        lu = [  [-((a !! i) !! j) | j <- [0..(i-1)]] ++ [0] ++ [-((a !! i) !! j) | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        t = matrix_mult dr lu
        b = matrix_mult dr y

inverse_l :: [[Double]]->[[Double]]
inverse_l l = 
    matrix_trans [   (matrix_trans (solve_l (n-1) l ([[0] | j <- [0..(i-1)]] ++ [[1]] ++ [[0] | j <- [(i+1)..(n-1)]]))) !! 0
        | i <- [0..(n-1)]
    ]
    where
        n = length l

-- Gauss-Seidel iteration to solve linear equation system
-- E.g: ghci> solve_LES_GS a y [[0],[0],[0]] 20
solve_LES_GS :: [[Double]]->[[Double]]->[[Double]]->Int->[[Double]]
solve_LES_GS a y x nn
    | nn == 0 = x
    | otherwise = matrix_plus (matrix_mult t (solve_LES_GS a y x (nn - 1))) b
    where
        n = length a
        dl = [  [(a !! i) !! j | j <- [0..i]]
                | i <- [0..(n-1)]]
        u = [   [0 | j <- [0..i]] ++ [-((a !! i) !! j) | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        dlr = inverse_l dl
        t = matrix_mult dlr u
        b = matrix_mult dlr y

find_nonzero_elem :: [[Double]]->Int->Int->Int
find_nonzero_elem a j i
    | i == n = j
    | otherwise = if (abs (a !! i !! j) < 1e-10) then find_nonzero_elem a j (i + 1) else i
    where
        n = length a

swap_row :: [[Double]]->Int->Int->[[Double]]
swap_row a i1 i2
    = [(a !! i) | i <- [0..(i1-1)]] ++ [a !! i2] ++ [(a !! i) | i <- [(i1+1)..(i2-1)]] ++ [a !! i1] ++ [(a !! i) | i <- [(i2+1)..(n-1)]]
    where
        n = length a

eij :: [[Double]]->Int->Double->Int->[[Double]]
eij a i1 d i2 = 
    [a !! i | i <- [0..(i2-1)]] ++
    [[(a !! i2 !! j) + (a !! i1 !! j) * d | j <- [0..(n-1)]]] ++
    [a !! i | i <- [(i2+1)..(n-1)]]
    where 
        n = length a

-- Gauss elimination to solve linear equation system
solve_LES_GE :: [[Double]]->[[Double]]->Int->[[Double]]
solve_LES_GE a y i
    | i == (n - 1) = solve_u 0 a y
    | aii == 0 = if (main_i == i) then solve_LES_GE a y (i + 1) else (solve_LES_GE (swap_row a i main_i) (swap_row y i main_i) i)
    | otherwise = if (nxt_i == i) then solve_LES_GE a y (i + 1) else (solve_LES_GE (eij a i prop nxt_i) (eij y i prop nxt_i) i)
    where
        n = length a
        aii = a !! i !! i
        main_i = find_nonzero_elem a i i
        nxt_i = find_nonzero_elem a i (i + 1)
        anxii = a !! nxt_i !! i
        prop = -anxii / aii

poly :: [Double]->Double->Double
poly a x = sum [(a !! i) * (x ^ i) | i <- [0..(n-1)]]
    where
        n = length a

-- Least square normal equations
-- E.g: ghci> equation_LSE [-2,-1,0,1,2] [0,1,2,1,0] 2
equation_LSE :: [Double]->[Double]->Int->[Double]
equation_LSE xs ys n = [((x !! i) !! 0) | i <- [0..n]]
    where
        m = length xs
        r = [   [(xs !! i) ^ j | j <- [0..n]]
            | i <- [0..(m-1)]]
        y = [[ys !! i] | i <- [0..(m-1)]]
        a = matrix_mult (matrix_trans r) r
        b = matrix_mult (matrix_trans r) y
        x = solve_LES_LU a b

-- Householder transformation
-- E.g: ghci> hh_trans [[5],[1],[2]]
hh_trans :: [[Double]]->[[Double]]
hh_trans v = matrix_minus id v3
    where
        n = length v
        id = [ [0 | j <- [0..(i-1)]] ++ [1] ++ [0 | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        v1 = matrix_mult v (matrix_trans v)
        v2 = 2 / (((matrix_mult (matrix_trans v) v) !! 0) !! 0)
        v3 = matrix_dot v1 v2

-- QR factorization by Householder transformation
-- E.g: ghci> qrFact_Householder  [[1,-1,1],[1,-0.5,0.25],[1,0,0],[1,0.5,0.25],[1,1,1]] 0
qrFact_Householder :: [[Double]]->Int->([[Double]], [[Double]])
qrFact_Householder a j
    | j == m = (id, a)
    | otherwise = (matrix_mult h (fst nxt), snd nxt)
    where
        n = length a
        m = length (a !! 0)
        id = [ [0 | j <- [0..(i-1)]] ++ [1] ++ [0 | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        col = [[0] | i <- [0..(j-1)]] ++ [[((a !! i) !! j)] | i <- [j..(n-1)]]
        norm = sqrt(((matrix_mult (matrix_trans col) col) !! 0) !! 0)
        sign = if (((col !! j) !! 0) > 0) then (-1) else 1
        ej = [[0] | i <- [0..(j-1)]] ++ [[1]] ++ [[0] | i <- [(j+1)..(n-1)]]
        v = matrix_minus col (matrix_dot ej (norm * sign))
        h = hh_trans v
        nxt = qrFact_Householder (matrix_mult h a) (j + 1)

set_element :: [[Double]]->Int->Int->Double->[[Double]]
set_element a x y d
    = [ [if (i == x && j == y) then d else ((a !! i) !! j) | j <- [0..(m-1)]]
        | i <- [0..(n-1)]]
    where
        n = length a
        m = length (a !! 0)

-- QR factorization by Givens rotate
-- E.g: ghci> qrFact_Givens [[1,-1,1],[1,-0.5,0.25],[1,0,0],[1,0.5,0.25],[1,1,1]] 0 0
qrFact_Givens :: [[Double]]->Int->Int->([[Double]], [[Double]])
qrFact_Givens a i j
    | (i == 0 && j == m) = (idn, a) 
    | (i <= j) = qrFact_Givens a nxti nxtj
    | ((a !! i) !! j) == 0 = qrFact_Givens a nxti nxtj
    | otherwise = (matrix_mult inverse_r (fst nxt), snd nxt)
    where
        n = length a
        m = length (a !! 0)
        nxti = if (i == n - 1) then 0 else (i + 1)
        nxtj = if (i == n - 1) then (j + 1) else j
        idn = [ [0 | j <- [0..(i-1)]] ++ [1] ++ [0 | j <- [(i+1)..(n-1)]]
                | i <- [0..(n-1)]]
        aij = ((a !! i) !! j)
        ajj = ((a !! j) !! j)
        c = ajj / sqrt(ajj^2 + aij^2)
        s = aij / sqrt(ajj^2 + aij^2)
        r = set_element (set_element (set_element (set_element idn j i s) i j (-s)) j j c) i i c
        inverse_r = set_element (set_element (set_element (set_element idn j i (-s)) i j s) j j c) i i c
        nxt = qrFact_Givens (matrix_mult r a) nxti nxtj


norm_inf :: [[Double]]->Int->Double
norm_inf v i
  | i == n = 0
  | otherwise = if ((abs ((v !! i) !! 0)) > nxt) then (abs ((v !! i) !! 0)) else nxt
  where
      n = length v
      nxt = norm_inf v (i + 1)

norm_2 :: [[Double]]->Double
norm_2 v = sqrt(((matrix_mult (matrix_trans v) v) !! 0) !! 0)

find_least_elem :: [[Double]]->Double->Int->Int
find_least_elem v d i
  | i == n = -1
  | (abs ((v !! i) !! 0)) == d = i
  | otherwise = find_least_elem v d (i + 1)
  where
      n = length v

-- Power method to find the biggest eigenvalue of matrix A
-- E.g: ghci> ghci> power_method [[4,-1,1],[-1,3,-2],[1,-2,3]] [[1],[1],[1]] 40
power_method :: [[Double]]->[[Double]]->Int->(Double, [[Double]])
power_method a x n
  | n == 0 = (mu, x)
  | mu == 0 = (0, x)
  | otherwise = power_method a (matrix_dot y (1 / mu)) (n - 1)
  where
      norm = norm_inf x 0
      p = find_least_elem x norm 0
      y = matrix_mult a x
      mu = (y !! p) !! 0

-- Symmetric power method to find the biggest eigenvalue of A
-- E.g: ghci> symmetric_power_method [[4,-1,1],[-1,3,-2],[1,-2,3]] [[1],[0],[0]] 10
symmetric_power_method :: [[Double]]->[[Double]]->Int->(Double, [[Double]])
symmetric_power_method a x n
  | n == 0 = (mu, x)
  | norm == 0 = (0, x)
  | otherwise = symmetric_power_method a (matrix_dot y (1 / norm)) (n - 1)
  where
      y = matrix_mult a x
      mu = ((matrix_mult (matrix_trans x) y) !! 0) !! 0
      norm = norm_2 y

inverse_power_method_iter :: [[Double]]->[[Double]]->Double->Int->(Double, [[Double]])
inverse_power_method_iter a x q m
  | m == 0 = (mu, x)
  | otherwise  = inverse_power_method_iter a (matrix_dot y (1/(y !! p !! 0))) q (m - 1)
  where
      n = length x        
      idn = [ [0 | j <- [0..(i-1)]] ++ [1] ++ [0 | j <- [(i+1)..(n-1)]]
              | i <- [0..(n-1)]]
      t = matrix_minus a (matrix_dot idn q)
      y = solve_LES_LU t x
      norm = norm_inf y 0
      p = find_least_elem y norm 0
      mu = y !! p !! 0

-- Inverse power method to find the eigenvalue of matrix a
-- E.g: ghci> inverse_power_method [[4,-1,1],[-1,3,-2],[1,-2,3]] [[1],[1],[1]] 10
inverse_power_method :: [[Double]]->[[Double]]->Int->(Double, [[Double]])
inverse_power_method a x n
  = (1 / (fst tmp) + q, snd tmp)
  where
      q = (matrix_dot (matrix_mult (matrix_mult (matrix_trans x) a) x) (1 / ((matrix_mult (matrix_trans x) x) !! 0 !! 0))) !! 0 !! 0
      norm = norm_inf x 0
      tmp = inverse_power_method_iter a (matrix_dot x (1 / norm)) q n

