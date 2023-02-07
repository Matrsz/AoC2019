using OffsetArrays

input = [1]
output = []

list = open("Day5/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

struct Opcode
    code
    mode1
    mode2
    mode3
end

ops = Dict(1 => +, 2 => *)

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1)

function insertAt(list, elem, dest)
    N = length(list)-1
    OffsetArray([list[0:dest-1]..., elem, list[dest+1:N]...], 0:N)
end

function execute(op, args, list)
    if op.code in [1,2]
        arg1, arg2, dest = args
        arg1 = op.mode1 == 0 ? list[arg1] : arg1
        arg2 = op.mode2 == 0 ? list[arg2] : arg2
        return insertAt(list, ops[op.code](arg1, arg2), dest)
    elseif op.code == 3
        return insertAt(list, pop!(input), args[1])
    elseif op.code == 4
        arg = args[1] 
        push!(output, op.mode1 == 0 ? list[arg] : arg)
        return list
    end
end

function executeAll(list, ic)
    op = getOpcode(list[ic])
    println(op)
    if op.code == 99
        return list
    else
        n = nArgs[op.code]
        args = list[ic+1:ic+n]
        return executeAll(execute(op, args, list), ic+n+1)
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse.(Int, [DE, C, B, A])...)
end

getOpcode(1002)

executeAll(list, 0)

output