parseList :: String -> [Int]
parseList = map (\x -> read [x] ::Int)

fftCycle :: [Int] -> [Int]
fftCycle [x] = [x]
fftCycle (x:xs) = (\ys -> mod (x + head ys) 10:ys) (fftCycle xs)

nthCycle :: Int -> [Int] -> [Int]
nthCycle n list = (fpow n fftCycle) list
    where fpow n = foldr (.) id . replicate n

toNumber :: [Int] -> Int
toNumber nums = read (concatMap show nums) ::Int

main :: IO ()
main = do
    input <- readFile "Day16/Input.txt"
    let list = parseList input
    let offset = (toNumber . take 7) list
    let signal = (drop offset . concat . replicate 10000) list
    print $ length signal
    print $ (toNumber . take 8 . nthCycle 100) signal
--     print $ (id . nthCycle 100) signal