import Data.List.Split (chunksOf)
import Data.List.Extras (argmin)
import Data.Char (digitToInt)

type Layer = [[Int]]

layers :: Int -> Int -> String -> [Layer]
layers w h = map (chunksOf w . map digitToInt) . chunksOf (h*w)

numDigit:: Int -> Layer -> Int
numDigit digit = sum . map (length . filter (\x->x==digit)) 

minZeros :: [Layer] -> Layer
minZeros = argmin (numDigit 0)

result :: Layer -> Int
result layer = numDigit 1 layer * numDigit 2 layer

main :: IO ()
main = do
    input <- readFile "Day8/Input.txt"
    print $ (result . minZeros .layers 25 6) input