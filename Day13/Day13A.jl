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

function getTiles(output)
    tiles = Dict()
    while output != []
        id, x, y = [pop!(output) for _ in 1:3]
        tiles = addTile(id, x, y, tiles)
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

tiles = getTiles(instance.output)

function numBlocks(tiles)
    return filter(x -> x == '*', values(tiles) |> collect) |> length
end

println(numBlocks(tiles))

drawScreen(tiles) |> println