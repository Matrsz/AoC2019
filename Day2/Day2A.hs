import Data.List.Split (splitOn)

parseInt x = read x ::Int

instructions = map parseInt . splitOn ","

instr pc = take 4 . drop pc

replaceAt :: Int -> Int -> [Int] -> [Int]
replaceAt dest item list = fst parts ++ [item] ++ tail (snd parts)
    where parts = splitAt dest list

execute :: [Int] -> [Int] -> [Int]
execute [1, op1, op2, dest] list = replaceAt dest (list!!op1 + list!!op2) list
execute [2, op1, op2, dest] list = replaceAt dest (list!!op1 * list!!op2) list

execAll pc list = if list!!pc == 99 then list else execAll (pc+4) (execute (instr pc list) list)

prepare = replaceAt 1 12 . replaceAt 2 2

main :: IO ()
main = do
    input <- readFile "Day2/Input.txt"
    print $ (head . execAll 0 . prepare . instructions) input