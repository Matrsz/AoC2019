import Data.List 
import Data.List.Split (splitOn)

direction :: [Char] -> (Char, Int)
direction (x:xs) = (x, read xs ::Int)

directions:: String -> [(Char, Int)]
directions = map direction . splitOn ","

followDir:: (Int, Int) -> (Char, Int) -> [(Int, Int)]
followDir (x, y) (d, n) 
    | d == 'R' = map (\xo -> (xo, y)) [x+n, x+n-1..x]
    | d == 'L' = map (\xo -> (xo, y)) [x-n..x]
    | d == 'U' = map (\yo -> (x, yo)) [y+n, y+n-1..y]
    | d == 'D' = map (\yo -> (x, yo)) [y-n..y]

follow:: [(Int, Int)] -> [(Char, Int)] -> [(Int, Int)]
follow path [] = path
follow path (dir:dirs) = follow newpath dirs
    where newpath = followDir (head path) dir ++ tail path

paths :: [String] -> [[(Int, Int)]]
paths = map (follow [(0,0)] . directions) 

intersects :: [[(Int, Int)]] -> [(Int, Int)]
intersects = init . (\[x,y] -> intersect x y) 

result :: [[(Int, Int)]] -> Int
result [p1, p2] = (minimum . map wirelen . intersects) [p1, p2]
    where wirelen x = length (dropWhile (/= x) p1) + length (dropWhile (/= x) p2) - 2

main :: IO ()
main = do
    input <- readFile "Day3/Input.txt"
    let dirs = lines input
    print $ (result . paths) dirs
