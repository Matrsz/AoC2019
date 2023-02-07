include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day15/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

instance = Instance(list=list)

moveCommand = Dict(:N => 1, :S => 2, :W => 3, :E => 4)
moveDir = Dict(:N => (-1, 0), :S => (1, 0), :W => (0, 1), :E => (0, -1))

function attemptMove(droid, maze, instance)
    instance.input = [moveCommand[droid[:dir]]]
    dest = droid[:pos].+moveDir[droid[:dir]]
    instance, _ = executeAll(instance)
    code = pop!(instance.output)
    if code == 0
        maze[dest] = '#'
    else
        maze[dest] = code == 2 ? 'o' : ' '
        droid[:pos] = dest
    end
    return droid, maze, instance, code
end

function drawMazeWithDroid(maze, droid)
    aux = copy(maze)
    shapes = Dict(:N => '^', :S => 'v', :W => '>', :E => '<')
    aux[droid[:pos]] = shapes[droid[:dir]]
    minx, maxx = [x for (x,y) in keys(aux)] |> extrema
    miny, maxy = [y for (x,y) in keys(aux)] |> extrema
    screen = OffsetArray(zeros(maxx-minx+1, maxy-miny+1), minx:maxx, miny:maxy) .+ Int(' ') .|> Char
    for (pos, tile) in aux
        screen[pos...] = tile
    end
    screen[0,0] = 'X'
    return join([join(row) for row in eachrow(screen)], '\n')
end

function drawMaze(maze)
    minx, maxx = [x for (x,y) in keys(maze)] |> extrema
    miny, maxy = [y for (x,y) in keys(maze)] |> extrema
    screen = OffsetArray(zeros(maxx-minx+1, maxy-miny+1), minx:maxx, miny:maxy) .+ Int(' ') .|> Char
    for (pos, tile) in maze
        screen[pos...] = tile
    end
    screen[0,0] = 'x'
    return join([join(row) for row in eachrow(screen)], '\n')
end

function explore(droid, maze, instance)
    exit = false
    seen = false
    while !exit
        turnL = Dict(:N => :E, :E => :S, :S => :W, :W => :N)
        turnR = Dict(:E => :N, :S => :E, :W => :S, :N => :W)
        droid[:dir] = turnL[droid[:dir]]
        droid, maze, instance, code = attemptMove(droid, maze, instance)
        if code == 0
            droid[:dir] = turnR[droid[:dir]]
            droid, maze, instance, code = attemptMove(droid, maze, instance)
            if code == 0
                droid[:dir] = turnR[droid[:dir]]
                droid, maze, instance, code = attemptMove(droid, maze, instance)
                if code == 0
                    droid[:dir] = turnR[droid[:dir]]
                    droid, maze, instance, code = attemptMove(droid, maze, instance)
                end        
            end    
        end
        println(drawMazeWithDroid(maze, droid), "\n")
        sleep(0.05)
        exit = seen && code == 2
        if code == 2
            seen = true
        end
    end
end

droid = Dict(:pos => (0,0), :dir => :N)
maze = Dict((0,0) => '.')

explore(droid, maze, instance)
println(drawMaze(maze), "\n")

function options(x, hallways)
    opts = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]] 
    filter!(y -> haskey(hallways, y), opts)
    filter!(y -> steps[y...] > steps[x...]+1, opts)
    return opts
end

hallways = Dict((k,v) for (k,v) in maze if v != '#')
oxygen = [k for (k,v) in maze if v == 'o'][1]

steps = Dict(k => 1000000 for (k, v) in hallways)
steps[(0,0)] = 0

options((0,0), hallways)

function bfs(node)
    next = options(node, hallways)
    for n in next
        steps[n...] = min(steps[node...]+1, steps[n...])
    end
    for n in next
        bfs(n)
    end
end

bfs((0,0))

println(steps[oxygen])

function floodFill(x, hallways)
    hallways[x] = 'o'
    next = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]] 
    filter!(y -> haskey(hallways, y) && hallways[y] != 'o', next)
    if isempty(next)
        return 0
    else
        return 1 + maximum(floodFill(n, hallways) for n in next)
    end
end

println(floodFill(oxygen, hallways))