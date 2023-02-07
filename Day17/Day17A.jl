include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day17/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

instance = Instance(list=list)

instance, flag = executeAll(instance)

println(instance, flag)

function getAscii(output)
    return reduce(vcat, permutedims.(collect.(split(join(output.|>Char)))))
end

function printAscii(output)
    return join(output .|> Char)
end

function getIntersections(asciiMap)
    indices = CartesianIndices(asciiMap) .|> Tuple
    scaffolds = filter(i -> asciiMap[i...] == '#', indices)
    function isIntersection(i)
        adj = [i.+x for x in ((1,0), (0,1), (-1,0), (0,-1)) if i.+x in scaffolds]
        return length(adj) > 2
    end
    return filter(isIntersection, scaffolds)
end

asciiMap = getAscii(instance.output[1:2157])

length(instance.output)

printAscii(instance.output) |> println

sum([(x-1)*(y-1) for (x,y) in getIntersections(asciiMap)])
