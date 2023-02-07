fuelReq mass = div mass 3 - 2

parseInt x = read x ::Int

main :: IO ()
main = do
    input <- readFile "Day1/Input.txt"
    print $ (sum . map (fuelReq . parseInt)) (lines input)