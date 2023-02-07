import Data.List (sortOn, elemIndex, find)
import Data.Maybe (fromJust)
import Data.Bits ( Bits(shiftR, testBit) )

modularInverse:: Integer -> Integer -> Integer
modularInverse m x = mod (aux m x) m
    where aux a b = let
            next a b = zipWith (-) a $ map (*(head $ zipWith div a b)) b
            l = [a,0] : [b,1] : zipWith next l (tail l)
            in
            head $ tail $ head $ filter ((==1).head) l

linearModN:: Integer -> Integer -> Integer -> (Integer -> Integer)
linearModN n m b x = mod (m*x+b) n

instructionArgs:: Integer -> [String] -> (Integer, Integer)
instructionArgs size ["cut", x] = (1, read x :: Integer) 
instructionArgs size ["deal", "into", "new", "stack"] = (size-1, size-1)
instructionArgs size ["deal", "with", "increment", x] = (modularInverse size (read x :: Integer), 0)

inverseArgs:: Integer -> (Integer, Integer) -> (Integer, Integer)
inverseArgs n (m, b) = (modularInverse n m, -b * modularInverse n m)

composeArgs:: Integer -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
composeArgs n (m1, b1) (m2, b2) = (mod (m1*m2) n, mod ((m1*b2)+b1) n)

composeAll:: Integer -> [(Integer, Integer)] -> (Integer, Integer)
composeAll n = foldl (composeArgs n) (1,0)

makeInstruction:: Integer -> (Integer, Integer) -> (Integer -> Integer)
makeInstruction size = uncurry (linearModN size)

superArgs:: Integer -> String -> (Integer, Integer)
superArgs size = composeAll size . map (instructionArgs size . words) . lines

applyNTimes:: Integer -> Integer -> (Integer, Integer) -> Integer -> Integer
applyNTimes n size args = fpow (fromIntegral n) (makeInstruction size args)
    where fpow n = foldr (.) id . replicate n

expModN:: Integer -> Integer -> Integer -> Integer
expModN m b 0 = 1
expModN m b e = t * expModN m ((b * b) `mod` m) (shiftR e 1) `mod` m
    where t = if testBit e 0 then b `mod` m else 1

geometricModN:: Integer -> Integer -> Integer -> Integer
geometricModN n r i = mod (arg1 * arg2) n
    where arg1 = expModN n r i - 1
          arg2 = modularInverse n (r-1)

argsNTimes:: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
argsNTimes n size (m, b) = (expModN size m n, mod (b * geometricModN size m n) size)

main :: IO ()
main = do
    input <- readFile "Day22/Input.txt"
    input2 <- readFile "Day22/Test4.txt"
    input3 <- readFile "Day22/Test3.txt"
    input4 <- readFile "Day22/Test4.txt"

    let deckSize1 = 10007
    let nShuffle1 = 200
    let nShuffle2 = 10000

    print $ (makeInstruction deckSize1 . inverseArgs deckSize1 . superArgs deckSize1) input 2019
    print $ (makeInstruction deckSize1 . superArgs deckSize1) input 6289
    print $ (applyNTimes nShuffle1 deckSize1 . superArgs deckSize1) input 6289
    print $ (makeInstruction deckSize1 . argsNTimes nShuffle1 deckSize1 . superArgs deckSize1) input 6289
    print $ (applyNTimes nShuffle2 deckSize1 . superArgs deckSize1) input 6289
    print $ (makeInstruction deckSize1 . argsNTimes nShuffle2 deckSize1 . superArgs deckSize1) input 6289

    let deckSize = 119315717514047
    let nShuffle = 101741582076661
    print $ (makeInstruction deckSize . argsNTimes nShuffle deckSize  . superArgs deckSize) input 2020