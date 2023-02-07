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

list = open("Day19/Input.txt") do file
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

function drawBeam(beam)
    beam = map(x -> x == 1 ? '#' : '.', beam)
    return join([join(row) for row in eachrow(beam)], '\n')
end

function testCoords(list, x, y)
    inst = Instance(list=copy(list), input=[x,y])
    inst, flag = executeAll(inst)
    return inst.output[1]
end

function makeBeam(xy, size)
    xi, yi = xy
    tractorMap = OffsetArray(zeros(size, size), xi:xi+size-1, yi:yi+size-1)
    for x in xi:xi+size-1
        for y in yi:yi+size-1
            tractorMap[x,y] = testCoords(list, x, y)
        end 
    end
    return tractorMap
end

function makeLine(dist, size, minAngle, maxAngle)
    xys = pointsAt(dist+size-1, minAngle, maxAngle)
    return [testCoords(list, xy...) for xy in xys], xys
end

function pointsAt(distance, minAngle, maxAngle)
    tol = 0.02
    return filter(xy -> minAngle-tol < atan(xy...) < maxAngle+tol, [(i, distance-i) for i in 0:distance])
end

function firstFit(dist, size, minAngle, maxAngle)
    line, xys = makeLine(dist, size, minAngle, maxAngle)
    if count(x->x==1, line) >= size
        return dist, line, xys
    else
        range = findall(x -> x==1, line)
        maxAngle = atan(xys[range[end]]...)
        minAngle = atan(xys[range[1]]...)
        return firstFit(dist+1, size, minAngle, maxAngle)
    end
end

dist, line, xys = firstFit(1,100, atan(0,1), atan(1,0))

range = xys[findall(x -> x==1, line)]

println(range[1] .- (0,99))
xy = range[end] .- (99,0)

println(drawBeam(makeBeam(xy.-(0,1), 100)), '\n')
println(drawBeam(makeBeam(xy.-(1,0), 100)), '\n')
println(drawBeam(makeBeam(xy, 100)), '\n')

println(xy[2]*10000+xy[1])