import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (fromJust)

data Astro = Nil | Node String [Astro] deriving Show

addAstro:: (String, String) -> Astro -> Astro
addAstro (planet, moon) Nil = Node planet [Node moon []]
addAstro (planet, moon) (Node name orbit)
    | moon == name = Node planet [Node name orbit]
    | planet == name = Node name (Node moon []:orbit)
    | any (hasPlanet planet) orbit = Node name (map (addAstro (planet, moon)) orbit)
    | otherwise = Node name orbit

parseOrbit :: String -> (String, String)
parseOrbit = (\[x,y] -> (x, y)) . splitOn ")"

systemPass:: [(String, String)] -> Astro -> Astro
systemPass [] astro = astro
systemPass ((planet, moon):xs) astro = addAstro (planet, moon) (systemPass xs astro)

system:: [(String, String)] -> Astro -> Astro
system [] astro = astro
system queue astro = system (filter oneMissing queue) sys
    where oneMissing (p, m) = not (hasPlanet p sys) || not (hasPlanet m sys)
          sys = systemPass queue astro

hasPlanet:: String -> Astro -> Bool
hasPlanet planet (Node name orbit) = name == planet || any (hasPlanet planet) orbit

orbits:: String -> Astro -> Int
orbits planet (Node name orbit) 
    | name == planet = 0
    | otherwise = 1 + (orbits planet . fromJust . find (hasPlanet planet)) orbit

allPlanets:: Astro -> [String]
allPlanets Nil = []
allPlanets (Node name orbit) = name : concatMap allPlanets orbit

allOrbits:: Astro -> Int
allOrbits astro = sum (map (flip orbits astro) (allPlanets astro))

commonPlanet:: String -> String -> Astro -> Astro
commonPlanet p1 p2 (Node name orbit)
    | not (any hasBoth orbit) = Node name orbit 
    | otherwise = (commonPlanet p1 p2 . fromJust . find hasBoth) orbit
    where hasBoth astro = hasPlanet p1 astro && hasPlanet p2 astro

main :: IO ()
main = do
    input <- readFile "Day6/Input.txt"
    let orbitsMap = (map parseOrbit . lines) input
    let mySystem = system orbitsMap Nil
    let subsystem = commonPlanet "YOU" "SAN" mySystem
    print (orbits "YOU" subsystem + orbits "SAN" subsystem - 2)
