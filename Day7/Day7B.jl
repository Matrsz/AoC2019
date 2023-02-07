include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays
using Combinatorics

control = open("Day7/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

function executeArray(control, phases, s0)
    instA = Instance(list=control |> copy, input=[phases[1]])
    instB = Instance(list=control |> copy, input=[phases[2]])
    instC = Instance(list=control |> copy, input=[phases[3]])
    instD = Instance(list=control |> copy, input=[phases[4]])
    instE = Instance(list=control |> copy, input=[phases[5]])
    flagA, flagB, flagC, flagD, flagE = repeat([nothing], 5)
    push!(instA.input, s0) |> reverse!
    println("Start!:", instA)
    while flagE != :halt
        instA, flagA = executeAll(instA)
        println("Ran A:", flagA)
        sig = pop!(instA.output)

        push!(instB.input, sig) |> reverse!
        instB, flagB = executeAll(instB)
        println("Ran B:", flagB)
        sig = pop!(instB.output)

        push!(instC.input, sig) |> reverse!
        instC, flagC = executeAll(instC)
        println("Ran C:", flagC)
        sig = pop!(instC.output)

        push!(instD.input, sig) |> reverse!
        instD, flagD = executeAll(instD)
        println("Ran D:", flagD)
        sig = pop!(instD.output)

        push!(instE.input, sig) |> reverse!
        instE, flagE = executeAll(instE)
        println("Ran E:", flagE)
        if flagE != :halt
            sig = pop!(instE.output)
            push!(instA.input, sig) |> reverse!
        end
    end
    return instE.output[1]
end

signals = Dict()

executeArray(control, [9,8,7,6,5], 0)

for phases in permutations([9,8,7,6,5])
    signals[phases] = executeArray(control, phases, 0)
end

println("Max sig: $(findmax(signals)[1]) from phases $(findmax(signals)[2])")