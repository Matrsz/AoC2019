fuelReq mass = if fuel > 0 then fuel + fuelReq fuel else 0
    where fuel = div mass 3 - 2

parseInt x = read x ::Int

main :: IO ()
main = do
    input <- readFile "Day1/Input.txt"
    print $ (sum . map (fuelReq . parseInt)) (lines input)