using OffsetArrays
using Parameters
using Match
using Combinatorics

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

list = open("Day25/Input.txt") do file
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

function printAscii(output)
    return join(output .|> Char)
end

function Input(prompt)
    print(printAscii(instance.output))
    print("> ")
    readline()
end

function takePath(path, instance, wait=0.5)
    for step in path
        instance.output = []
        instance.input =  [collect(step)..., '\n'] |> reverse
        print(step)
        sleep(wait)
        instance, flag = executeAll(instance)
        print(printAscii(instance.output))
        sleep(wait)
        instance.output = []
    end
    return instance
end

path1 = ["south", "take astrolabe", "west", "take hologram", "south", "take space law space brochure", "west", "take wreath", "west", "take hypercube", "east", "east", "north", "east"]
path2 = ["south", "take cake", "west", "north", "take coin", "south", "east", "east", "south", "east", "take food ration", "south"]

items = ["astrolabe", "hologram", "space law space brochure", "wreath", "hypercube", "cake", "coin", "food ration"]

correctItems = ["hologram", "hypercube", "cake", "coin"]

function dropItems(items, instance)
    return takePath(["drop $item" for item in items], instance, 0)
end

function takeItems(items, instance)
    return takePath(["take $item" for item in items], instance, 0)
end

instance = Instance(list=list)

instance, flag = executeAll(instance)
print(printAscii(instance.output))

instance = takePath(path1, instance)
instance = takePath(path2, instance)

instance = dropItems(items, instance)
instance = takeItems(correctItems, instance)

while true
    instance, flag = executeAll(instance)
    command = Input(instance.output)
    instance.output = []
    instance.input =  [collect(command)..., '\n'] |> reverse
end