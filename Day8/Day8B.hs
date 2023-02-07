import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Data.Data (typeOf)

data Pixel = B | W | T deriving (Enum, Eq)
data Layer = L [[Pixel]]
    
instance Show Pixel where
    show :: Pixel -> String
    show W = "#"
    show B = "."
    show T = " "

instance Show Layer where
    show :: Layer -> String
    show (L rows) = intercalate "\n" (map (concatMap show) rows)

toPixel :: Char -> Pixel
toPixel '0' = B
toPixel '1' = W
toPixel '2' = T

layers :: Int -> Int -> String -> [Layer]
layers w h = map (L . chunksOf w . map toPixel) . chunksOf (h*w)

stackPixel:: Pixel -> Pixel -> Pixel
stackPixel T y = y
stackPixel x _ = x

stackLayer:: Layer -> Layer -> Layer
stackLayer (L xss) (L yss) = L (zipWith (zipWith stackPixel) xss yss)

stackAll:: [Layer] -> Layer
stackAll = foldr1 stackLayer 

main :: IO ()
main = do
    input <- readFile "Day8/Input.txt"
    let img = layers 25 6 input
    print $ stackAll img