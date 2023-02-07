import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.List.Extras (argmin)
import Diagrams.ThreeD (mkR3, V3(..))

data MoonT = Moon (V3 Int) (V3 Int) deriving (Show, Eq)

parseXYX :: String -> [Int]
parseXYX = map (toInt . last . splitOn "=") . splitOn "," . filter (flip notElem ['<', '>'])
    where toInt x = read x ::Int

makeMoon:: [Int] -> MoonT
makeMoon [x,y,z] = Moon (mkR3 x y z) (mkR3 0 0 0)

-- this takes literally forever
signs:: V3 Int -> V3 Int
signs (V3 x y z) = V3 (signum x) (signum y) (signum z)

moonPull:: MoonT -> MoonT -> MoonT
moonPull (Moon x1 v1) (Moon x2 v2) = Moon x2 (v2 + signs (x1 - x2))

applyGravity:: [MoonT] -> [MoonT]
applyGravity moons = map (applyGravityTo moons) moons
    where applyGravityTo moons moon = foldr moonPull moon moons

moveMoons:: [MoonT] -> [MoonT]
moveMoons = map moveMoon
    where moveMoon (Moon x v) = Moon (x + v) v

evolve:: Int -> [MoonT] -> [MoonT]
evolve 0 moons = moons
evolve t moons = evolve (t-1) ((moveMoons . applyGravity) moons)

detectCycle:: Int -> [MoonT] -> [[MoonT]] -> Int
detectCycle n initial (moons:history)
    | moons == initial && n /= 0 = n
    | otherwise = detectCycle (n+1) initial ((moveMoons . applyGravity) moons : moons : history)

-- This is the efficient solution
data MoonX = Moon1 Int Int deriving (Show, Eq)

sepXYZ :: MoonT -> [MoonX]
sepXYZ (Moon (V3 x1 x2 x3) (V3 v1 v2 v3)) = [Moon1 x1 v1, Moon1 x2 v2, Moon1 x3 v3]

moonPull1:: MoonX -> MoonX -> MoonX
moonPull1 (Moon1 x1 v1) (Moon1 x2 v2) = Moon1 x2 (v2 + signum (x1 - x2))

applyGravity1:: [MoonX] -> [MoonX]
applyGravity1 moons = map (applyGravityTo moons) moons
    where applyGravityTo moons moon = foldr moonPull1 moon moons

moveMoons1:: [MoonX] -> [MoonX]
moveMoons1 = map moveMoon
    where moveMoon (Moon1 x v) = Moon1 (x + v) v

evolve1:: Int -> [MoonX] -> [MoonX]
evolve1 0 moons = moons
evolve1 t moons = evolve1 (t-1) ((moveMoons1 . applyGravity1) moons)

detectCycle1:: Int -> [MoonX] -> [[MoonX]] -> Int
detectCycle1 n initial (moons:history)
    | moons == initial && n /= 0 = n
    | otherwise = detectCycle1 (n+1) initial ((moveMoons1 . applyGravity1) moons : moons : history)

cycleLengths:: [[MoonX]] -> [Int]
cycleLengths = map (\axis -> detectCycle1 0 axis [axis]) 

main :: IO ()
main = do
    input <- readFile "Day12/Input.txt"
    let moons = (map (makeMoon . parseXYX) . lines) input
    print $ moons
    let [xs, ys, zs] = (transpose . map sepXYZ) moons
    let cycles = cycleLengths [xs, ys, zs]
    print $ foldl lcm 1 cycles