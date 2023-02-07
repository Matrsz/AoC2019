using OffsetArrays

spacemap = open("Day10/Input.txt") do file
    spacemap = hcat(map(collect, readlines(file))...) |> permutedims
    return OffsetArray(spacemap, -1, -1)
end

asteroids = [x for x in CartesianIndices(spacemap) .|> Tuple if spacemap[x...]=='#']

function simplify((x, y))
    if x == 0 
        return (0, sign(y))
    elseif y == 0
        return (sign(x), 0)
    else
        return (x, y) ./ gcd(x, y) .|> Int
    end
end

function directions(point, coords)
    return hcat([coord .- point for coord in coords if coord != point]...) .|> simplify |> unique
end

function linesToAsteroid(point, asteroids)
    return [ast .- point for ast in asteroids if ast != point]
end

function asteroidsSeen(point, asteroids)
    return linesToAsteroid(point, asteroids) .|> simplify |> unique |> length
end

findmax(Dict(ast => asteroidsSeen(ast, asteroids) for ast in asteroids))