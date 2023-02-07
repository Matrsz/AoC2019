group :: Eq a => [[a]] -> a -> [[a]]

group [] x = [[x]]
group (g:gs) x = if elem x g then [x:g] ++ gs else [x]:(g:gs)

subgroups :: Eq a => [a] -> [[a]]
subgroups = foldl group []

hasDouble :: Eq a => [a] -> Bool
hasDouble = any (\x -> 2 == length x) . subgroups 

noDecrease [x, y] = x <= y
noDecrease (x:xs) = x <= head xs && noDecrease xs

main :: IO ()
main = print $ (length . filter hasDouble . filter noDecrease . map show) [134792 .. 675810]