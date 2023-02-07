using OffsetArrays
using Parameters

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

list = open("Day11/Input.txt") do file
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

function turn(robot, dir)
    right = Dict((-1,0) => (0,1), (0,1) => (1,0), (1,0) => (0,-1), (0,-1) => (-1,0))
    left = Dict(v => k for (k,v) in right)
    return merge(robot, Dict(:dir => dir==0 ? left[robot[:dir]] : right[robot[:dir]]))
end

function move(robot)
    return merge(robot, Dict(:pos => robot[:pos] .+ robot[:dir]))
end

function look(robot, field)
    return haskey(field, robot[:pos]) ? field[robot[:pos]] : 0
end

function paint(robot, field, color)
    return merge(field, Dict(robot[:pos] => color))
end

function drawHull(field, robot)
    aux = copy(field)
    aux[robot[:pos]] = 0
    minx, maxx = [x for (x,y) in keys(aux)] |> extrema
    miny, maxy = [y for (x,y) in keys(aux)] |> extrema
    hull = OffsetArray(zeros(maxx-minx+1, maxy-miny+1), minx:maxx, miny:maxy) .|> Char
    for index in CartesianIndices(hull)
        hull[index] = '.'
    end
    for (panel, color) in field
        hull[panel...] = color == 0 ? ' ' : '#'
    end
    shapes = Dict((-1, 0) => '^', ( 0, 1) => '>', ( 1, 0) => 'v', ( 0,-1) => '<')
    hull[robot[:pos]...] = shapes[robot[:dir]] 
    return join([join(row) for row in eachrow(hull)], '\n')
end

robot = Dict(:dir => (-1,0), :pos => (0,0))
field = Dict((0,0)=>1)
instance = Instance(list=list)

hull = OffsetArray(zeros(9,9), -4:4, -4:4) .|> Char 

function runProgram(instance, robot, field)
    flag = :init
    while flag != :halt 
        instance.input = [look(robot, field)]
        instance, flag = executeAll(instance)
        dir = pop!(instance.output)
        color = pop!(instance.output)
        field = paint(robot, field, color)
        robot = turn(robot, dir)
        robot = move(robot)
        if flag != :halt
            println(drawHull(field, robot), "\n")
            sleep(0.05)
        end
    end
    return field
end

field = runProgram(instance, robot, field)

println(length(field))