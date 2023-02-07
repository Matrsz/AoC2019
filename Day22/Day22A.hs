import Data.List (sortOn, elemIndex)

deck:: [Int]
deck = [0..10006]

dealIntoNewStack:: [Int] -> [Int]
dealIntoNewStack = reverse

cutNCards:: Int -> [Int] -> [Int]
cutNCards n deck = drop m deck ++ take m deck
    where m = mod n (length deck)

dealWithIncrement:: Int -> [Int] -> [Int]
dealWithIncrement n deck = map fst (sortOn snd itemAndIndex)
    where itemAndIndex = zip deck (incrementDest n (length deck))

incrementDest:: Int -> Int -> [Int]
incrementDest inc total = map (\x -> mod (x*inc) total) [0..total-1]

parseInstruction:: [String] -> [Int] -> [Int]
parseInstruction ["cut", x] = cutNCards (read x :: Int)
parseInstruction ["deal", "with", "increment", x] = dealWithIncrement (read x :: Int)
parseInstruction ["deal", "into", "new", "stack"] = dealIntoNewStack

instructionList:: String -> [[Int] -> [Int]]
instructionList = map (parseInstruction . words) . lines

applyTechniques:: [[Int] -> [Int]] -> [Int] -> [Int]
applyTechniques list deck = foldl (flip ($)) deck list 

main :: IO ()
main = do
    input <- readFile "Day22/Input.txt"
    print $ (elemIndex 2019 . applyTechniques (instructionList input)) deck
    print $ cutNCards 4 [0..9]