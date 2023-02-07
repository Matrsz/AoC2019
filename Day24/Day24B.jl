using OffsetArrays

bugs = open("Day24/Input.txt") do file
    bugs = hcat(map(collect, readlines(file))...) |> permutedims
    return OffsetArray(bugs, -2:2, -2:2)
end

emptyLevel = ['.' for x in CartesianIndices(bugs)]

levels = Dict(0 => bugs)
borders = Dict(:N => zip(repeat([-2], 5), -2:2) |> collect, 
               :S => zip(repeat([ 2], 5), -2:2) |> collect,
               :W => zip(-2:2, repeat([-2], 5)) |> collect,
               :E => zip(-2:2, repeat([ 2], 5)) |> collect)

function lowerLevelAdj(x, levels, level)
    if !haskey(levels, level-1)
        return 0
    end
    adj = Dict((1, 0) => borders[:S], (-1, 0) => borders[:N], (0, 1) => borders[:E], (0,-1) => borders[:W])
    if haskey(adj, x)
        return count(levels[level-1][a...] == '#' for a in adj[x])
    end
    return 0
end

function higherLevelAdj(x, levels, level)
    if !haskey(levels, level+1)
        return 0
    end
    n = 0
    n += x in borders[:S] && levels[level+1][1,0] == '#' 
    n += x in borders[:N] && levels[level+1][-1,0] == '#' 
    n += x in borders[:E] && levels[level+1][0,1] == '#' 
    n += x in borders[:W] && levels[level+1][0,-1] == '#'
    return n
end

function adjacentBugs(x, levels, level)
    adj = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    filter!(y -> y != (0,0) && y in CartesianIndices(bugs) .|> Tuple, adj)
    levelAdj = count(levels[level][a...] == '#' for a in adj)
    return levelAdj + lowerLevelAdj(x, levels, level) + higherLevelAdj(x, levels, level)
end

function evolveSquare(x, levels, level)
    if x == (0,0)
        return '.'
    elseif levels[level][x...] == '#' && adjacentBugs(x, levels, level) != 1 
        return '.'
    elseif levels[level][x...] == '.' && adjacentBugs(x, levels, level) in [1,2]
        return '#'
    end
    return levels[level][x...]
end

function evolveLevel(levels, level)
    return map(x -> evolveSquare(x, levels, level), CartesianIndices(bugs) .|> Tuple)
end

function evolve(levels)
    minlevel, maxlevel = extrema(keys(levels))
    if any(levels[minlevel][x...] == '#' for x in [(-1,0), (1,0), (0,-1), (0,1)])
        levels[minlevel-1] = emptyLevel
    end
    if any(levels[maxlevel][x...] == '#' for x in vcat(values(borders)...))
        levels[maxlevel+1] = emptyLevel
    end
    return Dict(l => evolveLevel(levels, l) for l in keys(levels))
end

function drawBugs(bugs)
    return join([join(row) for row in eachrow(bugs)], '\n')
end

function drawLevels(levels)
    for l in keys(levels) |> collect |> sort
        println("Level $l")
        println(drawBugs(levels[l]), '\n')
    end
end

function totalBugs(levels)
    levelBugs(bugs) = count(x -> x=='#', bugs)
    return sum(levelBugs(bugs) for (level, bugs) in levels)
end

for i in 1:200
    levels = evolve(levels)
end

println(keys(levels) |> length)

totalBugs(levels)