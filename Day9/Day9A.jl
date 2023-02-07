using OffsetArrays

program = open("Day9/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

struct Opcode
    code
    modes
end

ops = Dict(1 => +, 2 => *, 7 => ((x, y) -> Int(x<y)), 8 => ((x, y) -> Int(x==y)))

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1, 5 => 2, 6 => 2, 7 => 3, 8 => 3, 9 => 1, 99 => 0)

function writeAt(list, elem, dest, auxmem)
    N = length(list)-1
    if dest in eachindex(list)
        return OffsetArray([list[0:dest-1]..., elem, list[dest+1:N]...], 0:N), auxmem
    else
        return list, merge(auxmem, Dict(dest => elem))
    end
end

function readFrom(list, source, auxmem)
    return source in eachindex(list) ? list[source] : (haskey(auxmem, source) ? auxmem[source] : 0)
end

function getInterpreted(list, opmode, arg, rel, auxmem)
    return opmode==1 ? arg : readFrom(list, opmode==2 ? arg+rel : arg, auxmem)
end
function getLiteral(opmode, arg, rel)
    return opmode==2 ? arg+rel : arg
end

function getParams(op, list, args, rel, auxmem)
    function getParam(i, mode, arg)
        if i==3 && op.code in [1,2,7,8] || i==1 && op.code == 3
            return getLiteral(mode, arg, rel)
        else
            return getInterpreted(list, mode, arg, rel, auxmem)
        end
    end
    return [getParam(i, op.modes[i], args[i]) for i in eachindex(args)]
end

function execute(op, args, list, input, output, auxmem)
    if op.code in [1,2,7,8]
        arg1, arg2, dest = args
        state = writeAt(list, ops[op.code](arg1, arg2), dest, auxmem)
        return state[1], input, output, state[2]
    elseif op.code == 3
        item = pop!(input)
        state = writeAt(list, item, args[1], auxmem)
        return state[1], input, output, state[2]
    elseif op.code == 4
        push!(output, args[1])
        return list, input, output, auxmem
    end
end

function jump(op, args, ic)
    arg1, arg2 = args
    jump_if = x -> op.code == 5 ? x != 0 : x == 0
    return jump_if(arg1) ? arg2 : ic+3
end

function adjustRel(arg, rel)
    return rel + arg[1]
end

function executeAll(list, ic, input, output, rel, auxmem)
    op = getOpcode(list[ic])
    n = nArgs[op.code]
    args = getParams(op, list, list[ic+1:ic+n], rel, auxmem)
    println("Auxmem: $auxmem")
    if op.code == 99
        return list, ic, input, output, auxmem, :halt
    elseif op.code == 9
        return executeAll(list, ic+n+1, input, output, adjustRel(args, rel), auxmem)
    elseif op.code in [5,6]
        return executeAll(list, jump(op, args, ic), input, output, rel, auxmem)
    elseif op.code == 3 && input == []
        return list, ic, input, output, auxmem, :waiting
    elseif op.code in [1,2,3,4,7,8]
        list, input, output, auxmem = execute(op, args, list, input, output, auxmem)
        return executeAll(list, ic+n+1, input, output, rel, auxmem)
    else 
        return list, ic, input, output, auxmem, :error
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse(Int, DE), parse.(Int, [C, B, A]))
end

executeAll(program, 0, [1], [], 0, Dict())[4] |> println