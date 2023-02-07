include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day13/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

instance = Instance(list=list)

instance, flag = executeAll(instance)

function addTile(id, x, y, tiles)
    shapes = Dict(0 => ' ', 1 => '#', 2 => '*', 3 => '_', 4 => 'o')
    return merge(tiles, Dict((x, y) => shapes[id]))
end

function getTiles(output, tiles)
    while output != []
        id, x, y = [pop!(output) for _ in 1:3]
        if id in 0:4
            tiles = addTile(id, x, y, tiles)
        else
            println(id)
        end
    end
    return tiles
end

function drawScreen(tiles)
    minx, maxx = [x for (x,y) in keys(tiles)] |> extrema
    miny, maxy = [y for (x,y) in keys(tiles)] |> extrema
    screen = OffsetArray(zeros(maxx-minx+1, maxy-miny+1), minx:maxx, miny:maxy) .|> Char
    for (pos, tile) in tiles
        screen[pos...] = tile
    end
    return join([join(row) for row in eachrow(screen)], '\n')
end

function findTile(tile, tiles)
    return [k for (k, v) in tiles if v == tile]
end

function followBall(tiles, lastplay)
    paddle = findTile('_', tiles)[1][2]
    ball = findTile('o', tiles)
    if isempty(ball)
        return -lastplay
    end
    return sign(ball[1][2]-paddle)
end

function numBlocks(tiles)
    return filter(x -> x == '*', values(tiles) |> collect) |> length
end

function playGame(instance)
    win = false
    tiles = Dict()
    instance.list[0] = 2
    play = 0
    while !win
        instance, flag = executeAll(instance)
        tiles = getTiles(instance.output, tiles)
        play = followBall(tiles, play)
        instance.input = [play]
        println(drawScreen(tiles), "\n")
        sleep(0.01)
        win = numBlocks(tiles) == 0
    end
end

playGame(instance)