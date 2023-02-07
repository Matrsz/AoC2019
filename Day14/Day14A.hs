import Data.List.Split (splitOn, split)
import Data.List (find)
import Data.Maybe (fromJust)

data Chemical = Q Int String deriving Show
data Reaction = R Chemical [Chemical] deriving Show

instance Eq Chemical where
    (==) :: Chemical -> Chemical -> Bool
    Q _ x == Q _ y = x == y
instance Eq Reaction where
    (==) :: Reaction -> Reaction -> Bool
    R x _ == R y _ = x == y
instance Ord Chemical where
    compare :: Chemical -> Chemical -> Ordering
    compare (Q n _) (Q m _) = compare n m

plus :: Chemical -> Chemical -> Chemical
minus :: Chemical -> Chemical -> Chemical
times :: Int -> Chemical -> Chemical
plus (Q m x) (Q n y) = if x == y then Q (m + n) x else Q m x
minus (Q m x) (Q n y) = if x == y then Q (m - n) x else Q m x
times n (Q m x) = Q (m * n) x

isNonzero :: Chemical -> Bool
isNonzero (Q n _) = n > 0

toChemical:: String -> Chemical
toChemical string = Q (read quant ::Int) chemical
    where [quant, chemical] = splitOn " " string

toReagents:: String -> [Chemical]
toReagents = map toChemical . splitOn ", "

toReaction:: String -> Reaction
toReaction string = R (toChemical right) (toReagents left)
    where [left, right] = splitOn " => " string

reactionList:: String -> [Reaction]
reactionList = map toReaction . lines

findReaction:: [Reaction] -> Chemical -> Reaction
findReaction list x = (fromJust . find (\(R y rs) -> x==y)) list

isOre:: Chemical -> Bool
isOre (Q _ x) = x == "ORE"

inStock:: Chemical -> [Chemical] -> Chemical
inStock x xs = if elem x xs then (fromJust . find (x==)) xs else Q 0 (chemname x)
    where chemname (Q _ name) = name

takeFromStock:: Chemical -> [Chemical] -> [Chemical]
takeFromStock x = map (`minus` x)

addToStock:: Chemical -> [Chemical] -> [Chemical]
addToStock x [] = [x]
addToStock x (y:ys) = if y == x then x`plus`y:ys else y:addToStock x ys

data Balance = B [Chemical] [Chemical] deriving Show

excess:: Chemical -> Chemical -> Chemical
excess x y = if x >= y then x `minus` y else Q 0 (chemname y)
    where chemname (Q _ name) = name

sufficientReaction :: [Reaction] -> Chemical -> Reaction
sufficientReaction reactions chemical = multiplyReaction (nReactions chemical reaction) reaction
    where reaction = findReaction reactions chemical
          nReactions (Q x _) (R (Q y _) _) = if mod x y == 0 then div x y else 1 + div x y
          multiplyReaction n (R p rs) = R (n `times` p) (map (n `times`) rs)

makeIngredient:: [Reaction] -> Chemical -> Balance -> Balance
makeIngredient reactions chemical (B req stock)
    | isOre chemical = B req stock
    | otherwise = B ((takeFromStock chemical . foldr addToStock req) rs) (addToStock (excess p chemical) stock)
    where (R p rs) = sufficientReaction reactions chemical

makeIngredients:: [Reaction] -> [Chemical] -> Balance -> Balance
makeIngredients reactions chemicals balance = foldr (makeIngredient reactions) balance chemicals

updateBalance1:: Chemical -> Balance -> Balance
updateBalance1 x (B req stock) = B (takeFromStock necessary req) (takeFromStock necessary stock)
    where necessary = min (inStock x stock) x

updateBalance:: Balance -> Balance
updateBalance (B req stock) = foldr updateBalance1 (B req stock) req

cleanBalance:: Balance -> Balance
cleanBalance (B req stock) = B (filter isNonzero req) (filter isNonzero stock)

makeRequired:: [Reaction] -> Balance -> Balance
makeRequired reactions (B req stock) = (cleanBalance . updateBalance . makeIngredients reactions req) (B req stock)

makeFromOre:: [Reaction] -> Balance -> Balance
makeFromOre reactions (B req stock)
    | all isOre req = B req stock
    | otherwise = (makeFromOre reactions . makeRequired reactions) (B req stock)

main :: IO ()
main = do
    let balance = B [Q 1 "FUEL"] []
    input <- readFile "Day14/Input.txt"
    print $ makeFromOre (reactionList input) balance