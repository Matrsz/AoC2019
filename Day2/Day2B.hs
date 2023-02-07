import Data.List.Split ( splitOn )

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

prepare v n = replaceAt 1 v . replaceAt 2 n

allOpts = [(x,y) | x <- options, y <- options]
    where options = [0..99]

findInit list = filter (\(v, n) -> test v n list == 19690720) allOpts
    where test v n = head . execAll 0 . prepare v n


main :: IO ()
main = do
    input <- readFile "Day2/Input.txt"
    print $ (head . findInit . instructions) input