{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}

import System.Environment   
import Data.List
import System.Random 

main = do
    args <- getArgs
    printArgs args

printArgs :: [String] -> IO ()
printArgs ("-n":args) = putStr $ intercalate " " args
printArgs aargs = putStrLn $ intercalate " " aargs

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -} 

lottery :: StdGen -> [Int]
lottery gen = pick gen 49 6

pick :: StdGen -> Int -> Int -> [Int]
pick gen n r = sort . normalize $ pick' gen n r

pick' :: StdGen -> Int -> Int -> [Int]
pick' _ 0 _ = []
pick' _ _ 0 = []
pick' gen n r 
    | r > n = []
    | r == n = [1..n]
    | otherwise = x : pick' gen' (n - 1) (r - 1)
    where (x, gen') = randomR (1, n) gen

normalize :: [Int] -> [Int]
normalize [] = []
normalize [x] = [x]
normalize (x:xs) = x : normalize ys
     where ys = map (\y -> if x <= y then y + 1 else y) xs
