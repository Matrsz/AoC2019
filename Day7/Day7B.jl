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
        return list, ic, input, output, :halt
    elseif op.code in [5,6]
        n = nArgs[op.code]
        args = list[ic+1:ic+n]
        return executeAll(list, jump(op, args, list, ic), input, output)
    elseif op.code == 3 && input == []
        return list, ic, input, output, :waiting
    elseif op.code in [1,2,3,4,7,8]
        n = nArgs[op.code]
        args = list[ic+1:ic+n]
        list, input, output = execute(op, args, list, input, output)
        return executeAll(list, ic+n+1, input, output)
    else 
        return list, input, output, :error
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse.(Int, [DE, C, B, A])...)
end

function executeArray(list, phases, s0)
    listA, listB, listC, listD, listE = repeat([list], 5)
    icA, icB, icC, icD, icE = repeat([0], 5)
    inA, inB, inC, inD, inE = [[p] for p in phases]
    outA, outB, outC, outD, outE = repeat([[]], 5)
    flagA, flagB, flagC, flagD, flagE = repeat([nothing], 5)
    push!(inA, s0) |> reverse!
    while flagE != :halt
        listA, icA, inA, outA, flagA = executeAll(listA, icA, inA, outA)
        sig = pop!(outA)
        push!(inB, sig) |> reverse!
        listB, icB, inB, outB, flagB = executeAll(listB, icB, inB, outB)
        sig = pop!(outB)
        push!(inC, sig) |> reverse!
        listC, icC, inC, outC, flagC = executeAll(listC, icC, inC, outC)
        sig = pop!(outC)
        push!(inD, sig) |> reverse!
        listD, icD, inD, outD, flagD = executeAll(listD, icD, inD, outD)
        sig = pop!(outB)
        push!(inE, sig) |> reverse!
        listE, icE, inD, outE, flagE = executeAll(listE, icE, inE, outE)
        if flagE != :halt
            sig = pop!(outE)
            push!(inA, sig) |> reverse!
        end
    end
    return outE
end

signals = Dict()

for phases in permutations([9,8,7,6,5])
    signals[phases] = executeArray(control, phases, 0)
end

findmax(signals)