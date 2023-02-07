using OffsetArrays
using Combinatorics

control = open("Day7/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

struct Opcode
    code
    mode1
    mode2
    mode3
end

ops = Dict(1 => +, 2 => *, 7 => ((x, y) -> Int(x<y)), 8 => ((x, y) -> Int(x==y)))

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1, 5 => 2, 6 => 2, 7 => 3, 8 => 3)

function insertAt(list, elem, dest)
    N = length(list)-1
    OffsetArray([list[0:dest-1]..., elem, list[dest+1:N]...], 0:N)
end

function execute(op, args, list, input, output)
    if op.code in [1,2,7,8]
        arg1, arg2, dest = args
        arg1 = op.mode1 == 0 ? list[arg1] : arg1
        arg2 = op.mode2 == 0 ? list[arg2] : arg2
        return insertAt(list, ops[op.code](arg1, arg2), dest), input, output
    elseif op.code == 3
        item = pop!(input)
        return insertAt(list, item, args[1]), input, output
    elseif op.code == 4
        arg = args[1] 
        push!(output, op.mode1 == 0 ? list[arg] : arg)
        return list, input, output
    end
end

function jump(op, args, list, ic)
    arg1, arg2 = args
    jump_if = x -> op.code == 5 ? x != 0 : x == 0
    arg1 = op.mode1 == 0 ? list[arg1] : arg1
    arg2 = op.mode2 == 0 ? list[arg2] : arg2
    return jump_if(arg1) ? arg2 : ic+nArgs[op.code]+1
end

function executeAll(list, ic, input, output)
    op = getOpcode(list[ic])
    if op.code == 99
        println("Finished")
        return list, input, output
    elseif op.code in [5,6]
        n = nArgs[op.code]
        args = list[ic+1:ic+n]
        return executeAll(list, jump(op, args, list, ic), input, output)
    else
        n = nArgs[op.code]
        args = list[ic+1:ic+n]
        list, input, output = execute(op, args, list, input, output)
        return executeAll(list, ic+n+1, input, output)
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse.(Int, [DE, C, B, A])...)
end


function testPhase(phases, input, output)
    for phase in phases
        signal = pop!(output)
        push!(input, signal, phase)
        _, input, output = executeAll(control, 0, input, output)
    end
    return output[1]
end

signals = Dict()

for phases in permutations([0,1,2,3,4])
    signals[phases] = testPhase(phases, [], [0])
end

findmax(signals)