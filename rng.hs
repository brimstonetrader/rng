-- CODE BY ME
-- ALGORITHMS BY SCIENTISTS
-- EXPLAINED TO ME BY WIKIPEDIA

type Number = Integer
(//) = div
(%)  = mod



sort :: [Number] -> [Number]
sort [] = []
sort [x] = [x]
sort [x,y] = if x>y then [y,x] else [x,y]
sort list = join (sort a) (sort b)
  where (a, b) = splitAt ((fromIntegral$length(list)) // 2) list

join :: [Number] -> [Number] -> [Number]
join as [] = as
join (a:as) (b:bs) = if a>b then b:join (a:as) bs
else a:join (b:bs) as

-- sort [6,2,10,5,14,0] -> [0,2,5,6,10,14]

fallingRandoms :: Number -> Number -> [Number]
fallingRandoms r 0 = []
fallingRandoms r i = let n = next r
                     in (n % i) : (fallingRandoms n (i-1))
                     
-- fallingRandoms 123456 8 -> [7,5,3,0,3,0,1,0]

shuffle :: Number -> [Number] -> [Number]
shuffle seed xs = shuffle2 xs (fallingRandoms seed (fromIntegral$length xs))
shuffle2 :: [Number] -> [Number] -> [Number]
shuffle2 xs [] = []
shuffle2 xs (y:ys) = e : (shuffle2 (ds++es) ys)
  where (ds, e:es) = splitAt (fromIntegral y) xs

-- shuffle 123456 [1..8] -> [8,6,4,1,7,2,5,3]

next :: Number -> Number
next r = ((1664525 * r + 1013904223) % (2 ^ 32))

rng :: Number -> Number -> [Number]
rng r 0 = []
rng r c = let nrnd = next r
          in nrnd : (rng nrnd (c-1))
          
(~) :: Number -> Number -> Number
a ~ 0 = a
0 ~ b = b
a ~ b = (if ma == mb then 1 else 0) + (2 * (da ~ db))
  where ((da,ma), (db,mb)) = (divMod a 2, divMod b 2)
  
lfg :: Number -> Number -> [Number]
lfg seed runs = lfg2 (rng seed 55) runs

lfg2 :: [Number] -> Number -> [Number]
lfg2 seeds 0 = []
lfg2 seeds i =
  let n = toInteger (((seeds !! 24) ~ (last seeds)) % (2^32))
  in (n : lfg2 (n : take 54 seeds) (i-1))