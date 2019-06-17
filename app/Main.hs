module Main where
import Math.Polynomial.Interpolation
import Math.Polynomial.Lagrange
import System.Random

--s = 954 
--n = 4
--t = 3 

s = 1234 
n = 5
t = 3

si :: Int -> [Int] -> Int -> Int
si i as p = (s + (sum $ map ax (zip as [1..]))) `mod` p
    where ax (a,j) = (a * (i^j))

encode = do
    gen <- newStdGen
    gen2 <- newStdGen
    let r1 = randomRs (0::Int, 10000) gen
    --let p = 1523
    let p = head $ filter (\x -> (x>s) && (x>n)) r1
    let r2 =  randomRs (0::Int,1000) gen2
    --let as = [352,62]--take (t-1) r2
    let as = take (t-1) r2
    let codes = zip [1..n] $ map (\i -> si i as p) [1..n]
    putStrLn . show $ codes 

    putStrLn $ "p  = " ++ (show p)
    putStrLn $ "xs = " ++ (show $ map (\(a,_)->a) codes)
    putStrLn $ "ys = " ++ (show $ map (\(_,b)->b) codes)

    return ()

p0 :: (Eq a, Fractional a) => [a] -> [a] -> a
p0 xs ys = _product * _sum
    where _product = product $ map (\a -> (-a)) xs
          _sum = sum _polys
          _polys = map (\(m,n)-> n / (denominator m)) $ zip xs ys
          denominator x = (-x) * (product . filter (/=0) $ map (\a -> x-a) xs)

decode list = do
    putStrLn "p  = "
    p <- getLine
    let (a,b) = unzip list
    let aa = take (n-1) a
    let bb = take (n-1) b
    let secret = round $ p0 aa bb
    let secret' = mod secret (read p :: Int)
    putStr "s = "
    putStrLn . show $ secret'
    return ()


main :: IO ()
main = do
    return ()

dec2 list = do
    putStrLn "p  = "
    p <- getLine
    let pol = lagrangePolyFit list
    let pol2 = fmap (flip mod (read p :: Int) . round) pol
    putStrLn $ show pol2
    return()