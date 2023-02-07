parseList :: String -> [Int]
parseList = map (\x -> read [x] ::Int)

nthPattern :: Int  -> [Int]
nthPattern n = concatMap (replicate n) [1, 0, -1, 0]

applyPattern :: [Int] -> [Int] -> [Int]
applyPattern pattern list = (map (\(x,y) -> x*y) . filter (\(_, x) -> x /= 0)) (zip list ((concat . repeat) pattern))

sumPattern :: [Int] -> [Int] -> Int
sumPattern pattern = flip mod 10 . sum . applyPattern pattern

fftCycle :: [Int] -> [Int]
fftCycle list = map applyRightPattern [1..length list]
    where applyRightPattern n = sumPattern (nthPattern n) (drop (n-1) list)
 
nthCycle :: Int -> [Int] -> [Int]
nthCycle n list = (fpow n fftCycle) list
    where fpow n = foldr (.) id . replicate n

toNumber :: [Int] -> Int
toNumber nums = read (concatMap show nums) ::Int

main :: IO ()
main = do
    input <- readFile "Day16/Input.txt"
    let list = parseList input
    let signal = (concat . replicate 1) list
    print $ (id . nthCycle 1) signal
    print $ (id . nthCycle 2) signal
    print $ (id . nthCycle 3) signal
    print $ (id . nthCycle 4) signal
    print $ (id . nthCycle 5) signal