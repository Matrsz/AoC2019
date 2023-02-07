include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day9/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

executeAll(Instance(list=list, input=[1]))[1].output |> println

executeAll(Instance(list=list, input=[2]))[1].output |> println