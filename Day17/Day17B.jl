include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day17/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

function getAscii(output)
    return reduce(vcat, permutedims.(collect.(split(join(output.|>Char)))))
end

function printAscii(output)
    return join(output .|> Char)
end

routine = "A,C,A,B,A,C,B,C,B,C"

seqA = "R,8,L,10,L,12,R,4"
seqB = "R,8,L,10,R,8"
seqC = "R,8,L,12,R,4,R,4"

instance = Instance(list=list)

instance.list[0] = 2

instance.input = [collect(join([routine, seqA, seqB, seqC, "n"], '\n'))..., '\n'] |> reverse

instance, flag = executeAll(instance)

function showAllAscii(list)
    for i in 1:floor(length(list)/2157)
        println(printAscii(list[1:2157]), '\n')
        list = list[2158:end]
        sleep(0.05)
    end
end

println(printAscii(instance.output))

instance.output[end]