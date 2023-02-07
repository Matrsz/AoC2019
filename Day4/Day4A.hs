
hasDouble [x, y] = x == y
hasDouble (x:xs) = x == head xs || hasDouble xs

noDecrease [x, y] = x <= y
noDecrease (x:xs) = x <= head xs && noDecrease xs

main :: IO ()
main = print $ (length . filter hasDouble . filter noDecrease . map show) [134792 .. 675810]