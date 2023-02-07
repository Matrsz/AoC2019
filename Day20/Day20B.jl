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
            type = zone == line[1:3] ? :o : :i
            portals[(i+2, j)] = (label, type)
            println("Portal link $label of type $type at $(i+2), $j")
        end
        if isAlpha(zone[3]) && isAlpha(zone[2]) && zone[1] == '.'
            label = String(zone[2:3])
            type = zone == line[end-2:end] ? :o : :i
            portals[(i,j)] = (label, type)
            println("Portal link $label of type $type at $i, $j")
        end
    end
end

function scanRow(maze, i, portals)
    line = maze[i,:]
    for j in 1:length(line)-2
        zone = line[j:j+2]
        if isAlpha(zone[1]) && isAlpha(zone[2]) && zone[3] == '.'
            label = String(zone[1:2])        
            type = zone == line[1:3] ? :o : :i
            portals[(i, j+2)] = (label, type)     
            println("Portal link $label of type $type at $i, $(j+2)")
        end
        if isAlpha(zone[3]) && isAlpha(zone[2]) && zone[1] == '.'
            label = String(zone[2:3])
            type = zone == line[end-2:end] ? :o : :i
            portals[(i,j)] = (label, type)
            println("Portal link $label of type $type at $i, $j")
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

start  = ([k for (k,v) in portals if v[1] == "AA"][1], 1)
finish = ([k for (k,v) in portals if v[1] == "ZZ"][1], 1)

function mazeTiles(maze)
    return Dict(Tuple(x) => maze[x] for x in CartesianIndices(maze) if maze[x] == '.')
end

function getSteps(steps, node)
    level = node[2]
    x = node[1]
    if !(haskey(steps, level))
        return 1000000 
    elseif !(haskey(steps[level], x))
        return 1000000 
    else
        return steps[level][x]
    end
end

function options(node, tiles, steps, portals)
    level = node[2]
    x = node[1]
    opts = [(x.+y, node[2]) for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    filter!(y -> haskey(tiles[level], y[1]), opts)
    if haskey(portals, x) && !(portals[x][1] in ["AA", "ZZ"])
        endpoint = [k for (k,v) in portals if v[1]==portals[x][1] && k!=x][1]
        nextLevel = portals[x][2] == :o ? level-1 : level+1
        if nextLevel > 0
            push!(opts, (endpoint, nextLevel))
        end
    end
    filter!(y -> getSteps(steps, y) > steps[level][x]+1, opts)
    return opts
end

tiles = Dict(1 => mazeTiles(maze))
steps = Dict(1 => Dict(start[1] => 0))

function bfs(nodes, finish, tiles, steps, portals, stepsNow)
    if any(node -> node == finish, nodes)
        println("I found the exit in $(steps[1][finish[1]]) steps")
        return steps, tiles, true
    end
    next = vcat([options(node, tiles, steps, portals) for node in nodes]...)
    for n in next
        l = n[2]
        if !haskey(tiles, l)
            tiles[l] = tiles[1]
            steps[l] = Dict()
        end
        steps[l][n[1]] = min(stepsNow+1, getSteps(steps, n))
    end
    return bfs(next, finish, tiles, steps, portals, stepsNow+1)
end

steps, _ = bfs([start], finish, tiles, steps, portals, 0)

println(steps[1][finish[1]])