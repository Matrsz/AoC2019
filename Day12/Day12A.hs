import Data.List.Split 
import Data.List.Extras (argmin)
import Diagrams.ThreeD

data MoonT = Moon (V3 Int) (V3 Int) deriving Show

parseXYX :: String -> [Int]
parseXYX = map (toInt . last . splitOn "=") . splitOn "," . filter (flip notElem ['<', '>'])
    where toInt x = read x ::Int

makeMoon:: [Int] -> MoonT
makeMoon [x,y,z] = Moon (mkR3 x y z) (mkR3 0 0 0)

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

systemEnergy:: [MoonT] -> Int
systemEnergy = sum . map moonEnergy
    where moonEnergy (Moon x v) = sum (abs x) * sum (abs v)

main :: IO ()
main = do
    input <- readFile "Day12/Input.txt"
    let moons = (map (makeMoon . parseXYX) . lines) input
    print $ (systemEnergy . evolve 1000) moons
