using OffsetArrays

struct Opcode
    code
    modes
end

ops = Dict(1 => +, 2 => *, 7 => ((x, y) -> Int(x<y)), 8 => ((x, y) -> Int(x==y)))

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1, 5 => 2, 6 => 2, 7 => 3, 8 => 3, 9 => 1, 99 => 0)

function writeAt(elem, dest)
    if dest in eachindex(list)
        list[dest] = elem
    else
        merge!(auxmem, Dict(dest => elem))
    end
end

function readFrom(source)
    return source in eachindex(list) ? list[source] : (haskey(auxmem, source) ? auxmem[source] : 0)
end

function getInterpreted(opmode, arg, rel)
    return opmode==1 ? arg : readFrom(opmode==2 ? arg+rel : arg)
end
function getLiteral(opmode, arg, rel)
    return opmode==2 ? arg+rel : arg
end

function getParams(op, args, rel)
    function getParam(i, mode, arg)
        if i==3 && op.code in [1,2,7,8] || i==1 && op.code == 3
            return getLiteral(mode, arg, rel)
        else
            return getInterpreted(mode, arg, rel)
        end
    end
    return [getParam(i, op.modes[i], args[i]) for i in eachindex(args)]
end

function execute(op, args)
    arg1, arg2, dest = args
    writeAt(ops[op.code](arg1, arg2), dest)
end

function readInput(args, input)
    item = pop!(input)
    writeAt(item, args[1])
end

function writeOutput(args, output)
    push!(output, args[1])
end

function jump(op, args, ic)
    arg1, arg2 = args
    jump_if = x -> op.code == 5 ? x != 0 : x == 0
    return jump_if(arg1) ? arg2 : ic
end

function executeAll(ic, rel)
    while true
        op = getOpcode(list[ic])
        n = nArgs[op.code]
        args = getParams(op, list[ic+1:ic+n], rel)
        ic = ic + n + 1
        if op.code == 99
            return :halt
        elseif op.code == 9
            rel = rel + args[1]
        elseif op.code in [5,6]
            ic = jump(op, args, ic)
        elseif op.code == 3 
            if input == []
                return :waiting
            else
                readInput(args, input)
            end
        elseif op.code == 4
            writeOutput(args, output)
        elseif op.code in [1,2,7,8]
            execute(op, args)
        else 
            return :error
        end
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse(Int, DE), parse.(Int, [C, B, A]))
end

list = open("Day9/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

output = []
input = [2]
rel = 0
auxmem = Dict()

executeAll(0, rel)
output