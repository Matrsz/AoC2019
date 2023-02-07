parseList :: String -> [Int]
parseList = map (\x -> read [x] ::Int)

nthPattern :: Int -> [Int] -> [Int]
nthPattern n = concatMap (replicate n)

applyPattern :: [Int] -> [Int] -> [Int]
applyPattern pattern list = zipWith (*) list $ (tail . concat . repeat) pattern

sumPattern :: [Int] -> [Int] -> Int
sumPattern pattern = flip mod 10 . abs . sum . applyPattern pattern

fftCycle :: [Int] -> [Int] -> [Int]
fftCycle pattern list = map applyRightPattern (zip list [1..])
    where applyRightPattern (_, n) = sumPattern (nthPattern n pattern) list
 
nthCycle :: Int -> [Int] -> [Int] -> [Int]
nthCycle n pattern list = (fpow n (fftCycle pattern)) list
    where fpow n = foldr (.) id . replicate n

toNumber :: [Int] -> Int
toNumber nums = read (concatMap show nums) ::Int


main :: IO ()
main = do
    input <- readFile "Day16/Input.txt"
    let list = parseList input
    print $ (toNumber . take 8 . nthCycle 100 [0, 1, 0, -1]) list
    print $ 1 + (if True then 1 else 0)