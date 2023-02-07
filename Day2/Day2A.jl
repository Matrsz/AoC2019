using OffsetArrays

list = open("Day2/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

ops = Dict(1 => +, 2 => *)

function execute(instruction, list)
    opcode, arg1, arg2, dest = instruction
    return insertAt(list, ops[opcode](list[arg1], list[arg2]), dest)
end

function insertAt(list, elem, dest)
    N = length(list)-1
    OffsetArray([list[0:dest-1]..., elem, list[dest+1:N]...], 0:N)
end

function executeAll(list, ic)
    if list[ic] == 99
        return list
    else
        return executeAll(execute(list[ic:ic+3], list), ic+4)
    end
end

function prepare(n, v, list)
    N = length(list)-1
    return OffsetArray([list[0], n, v, list[3:N]...], 0:N)
end

[(n, v) for n in 0:99, v in 0:99 if executeAll(prepare(n, v, list), 0)[0] == 19690720]