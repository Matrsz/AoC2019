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

function isBlocked(ast, lines)
    return map(x -> simplify(x) == simplify(ast) && hypot(x...) < hypot(ast...), lines) |> any
end

function asteroidsSeen(point, asteroids)
    lines = linesToAsteroid(point, asteroids) 
    return [line for line in lines if !isBlocked(line, lines)]
end

num, station = findmax(Dict(ast => asteroidsSeen(ast, asteroids) |> length for ast in asteroids))

function laserDir((i, j))
    return j >= 0 ? atan(i, j) : 2π - atan(i,-j)
end

function targets(station, asteroids) 
    inLoS = asteroidsSeen(station, asteroids)
    sort!(inLoS, by=(x -> laserDir(x)))
    return [station.+target for target in inLoS]
end

function drawSpacemap(spacemap)
    return join([join(row) for row in eachrow(spacemap)], '\n')
end

allTargets = targets(station, asteroids)
vaporized = []
spacemap[station...] = 'X'
for x in eachindex(spacemap) 
    if spacemap[x] == '.'
        spacemap[x] = ' '
    end
end

while allTargets != []
    for target in allTargets
        vaporized = [vaporized..., target]
        filter!(x -> x != target, asteroids)
        spacemap[target...] = ' '
        println(drawSpacemap(spacemap), "\n")
        sleep(0.05)
    end
    allTargets = targets(station, asteroids)
end

vaporized[200] |> (x -> x[2]*100+x[1])