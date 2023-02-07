include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day11/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
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