using OffsetArrays
using Parameters
using Match

struct Opcode
    code
    modes
end

@with_kw mutable struct Instance
    ic::Int = 0
    list::OffsetVector{Int}
    auxmem::Dict{Int,Int} = Dict()
    input::Vector{Int} = []
    output::Vector{Int} = []
    rel::Int = 0
end

list = open("Day15/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

ops = Dict(1 => +, 2 => *, 7 => ((x, y) -> Int(x<y)), 8 => ((x, y) -> Int(x==y)))

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1, 5 => 2, 6 => 2, 7 => 3, 8 => 3, 9 => 1, 99 => 0)

function writeAt(elem, dest, instance)
    @unpack list, auxmem = instance
    if dest in eachindex(list)
        instance.list[dest] = elem
    else
        instance.auxmem = merge(auxmem, Dict(dest => elem))
    end
    return instance
end

function readFrom(source, instance)
    @unpack list, auxmem = instance
    return source in eachindex(list) ? list[source] : (haskey(auxmem, source) ? auxmem[source] : 0)
end

function getInterpreted(opmode, arg, instance)
    @unpack rel = instance
    return opmode==1 ? arg : readFrom(opmode==2 ? arg+rel : arg, instance)
end
function getLiteral(opmode, arg, instance)
    @unpack rel = instance
    return opmode==2 ? arg+rel : arg
end

function getParams(op, args, instance)
    function getParam(i, mode, arg)
        if i==3 && op.code in [1,2,7,8] || i==1 && op.code == 3
            return getLiteral(mode, arg, instance)
        else
            return getInterpreted(mode, arg, instance)
        end
    end
    return [getParam(i, op.modes[i], args[i]) for i in eachindex(args)]
end

function execute(op, args, instance)
    arg1, arg2, dest = args
    return writeAt(ops[op.code](arg1, arg2), dest, instance)
end

function readInput(args, instance)
    @unpack input = instance
    item = pop!(input)
    instance.input = input
    return writeAt(item, args[1], instance)
end

function writeOutput(args, instance)
    @unpack output = instance
    push!(output, args[1])
    instance.output = output
    return instance
end

function jump(op, args, ic)
    arg1, arg2 = args
    jump_if = x -> op.code == 5 ? x != 0 : x == 0
    return jump_if(arg1) ? arg2 : ic
end

function executeAll(instance)
    while true
        @unpack list, ic, auxmem, input, output, rel = instance
        op = getOpcode(list[ic])
        n = nArgs[op.code]
        args = getParams(op, list[ic+1:ic+n], instance)
        if op.code == 3 && input == []
            return instance, :wait
        end
        instance.ic = ic + n + 1
        if op.code == 99
            return instance, :halt
        elseif op.code == 9
            instance.rel = rel + args[1]
        elseif op.code in [5,6]
            instance.ic = jump(op, args, instance.ic)
        elseif op.code == 3 
            instance = readInput(args, instance)
        elseif op.code == 4
            instance = writeOutput(args, instance)
        elseif op.code in [1,2,7,8]
            instance = execute(op, args, instance)
        else 
            return instance, :error
        end
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse(Int, DE), parse.(Int, [C, B, A]))
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