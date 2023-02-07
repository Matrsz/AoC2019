maze = open("Day20/Input.txt") do file
    return hcat(map(collect, readlines(file))...) |> permutedims
end

function drawMaze(maze)
    return join([join(row) for row in eachrow(maze)], '\n')
end

println(maze |> drawMaze)

isAlpha(x) = x in ['A':'Z'...]

function scanColumn(maze, j, portals)
    line = maze[:,j]
    for i in 1:length(line)-2
        zone = line[i:i+2]
        if isAlpha(zone[1]) && isAlpha(zone[2]) && zone[3] == '.'
            label = String(zone[1:2])
            portals[(i+2, j)] = label
            println("Portal link $label at $(i+2), $j")
        end
        if isAlpha(zone[3]) && isAlpha(zone[2]) && zone[1] == '.'
            label = String(zone[2:3])
            portals[(i,j)] = label
            println("Portal link $label at $i, $j")
        end
    end
end

function scanRow(maze, i, portals)
    line = maze[i,:]
    for j in 1:length(line)-2
        zone = line[j:j+2]
        if isAlpha(zone[1]) && isAlpha(zone[2]) && zone[3] == '.'
            label = String(zone[1:2])        
            portals[(i, j+2)] = label     
            println("Portal link $label at $i, $(j+2)")
        end
        if isAlpha(zone[3]) && isAlpha(zone[2]) && zone[1] == '.'
            label = String(zone[2:3])
            portals[(i,j)] = label
            println("Portal link $label at $i, $j")
        end
    end
end

h, w = size(maze)

portals = Dict()

for j in 1:w
    scanColumn(maze, j, portals)
end
for i in 1:h
    scanRow(maze, i, portals)
end

function drawMazeWithPortals(maze, portals)
    for xy in keys(portals)
        maze[xy...] = 'x'
    end
    return join([join(row) for row in eachrow(maze)], '\n')
end

start = [k for (k,v) in portals if v == "AA"][1]
finish = [k for (k,v) in portals if v == "ZZ"][1]

function mazeTiles(maze)
    return Dict(Tuple(x) => maze[x] for x in CartesianIndices(maze) if maze[x] == '.')
end

function options(x, tiles, steps, portals)
    opts = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    filter!(y -> haskey(tiles,y), opts)
    if haskey(portals, x) && !(portals[x] in ["AA", "ZZ"])
        endpoint = [k for (k,v) in portals if v==portals[x] && k!=x][1]
        push!(opts, endpoint)
    end
    filter!(y -> steps[y] > steps[x]+1, opts)
    return opts
end

tiles = mazeTiles(maze)
steps = Dict(k => 100000 for (k,v) in tiles)
steps[start] = 0

function bfs(node, tiles, steps, portals)
    next = options(node, tiles, steps, portals)
    for n in next
        steps[n] = min(steps[node]+1, steps[n])
    end
    for n in next
        steps = bfs(n, tiles, steps, portals)
    end
    return steps
end

bfs(start, tiles, steps, portals)

steps[finish]