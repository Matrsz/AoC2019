include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day21/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

function printAscii(output)
    return join(output .|> Char)
end

instructions = ["NOT C T", "NOT D J", "NOT J J", "AND T J", "NOT A T", "OR T J", "WALK"]
instructions = ["NOT C T", "NOT D J", "NOT J J", "AND T J", "NOT A T", "OR T J", "WALK"]

program = join(instructions, "\n")

instance = Instance(list=copy(list))
for instruction in instructions
    instance.input=[collect(instruction)..., '\n'] |> reverse
    instance, flag = executeAll(instance)
end

try 
    println(printAscii(instance.output))
catch
    print(instance.output[1:end-1] |> printAscii)
    println(instance.output[end])
end

instructions = ["NOT C T", "NOT A J", "OR T J", "NOT B T", "OR T J", "AND D J", "NOT E T", "NOT T T", "OR H T", "AND T J", "RUN"]

program = join(instructions, "\n")

instance = Instance(list=copy(list))
for instruction in instructions
    instance.input=[collect(instruction)..., '\n'] |> reverse
    instance, flag = executeAll(instance)
end

try 
    println(printAscii(instance.output))
catch
    print(instance.output[1:end-1] |> printAscii)
    println(instance.output[end])
end
