include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using Combinatorics
using OffsetArrays

list = open("Day25/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

function printAscii(output)
    return join(output .|> Char)
end

function Input(prompt)
    print(printAscii(instance.output))
    print("> ")
    readline()
end

function takePath(path, instance, wait=0.5)
    for step in path
        instance.output = []
        instance.input =  [collect(step)..., '\n'] |> reverse
        print(step)
        sleep(wait)
        instance, flag = executeAll(instance)
        print(printAscii(instance.output))
        sleep(wait)
        instance.output = []
    end
    return instance
end

path1 = ["south", "take astrolabe", "west", "take hologram", "south", "take space law space brochure", "west", "take wreath", "west", "take hypercube", "east", "east", "north", "east"]
path2 = ["south", "take cake", "west", "north", "take coin", "south", "east", "east", "south", "east", "take food ration", "south"]

items = ["astrolabe", "hologram", "space law space brochure", "wreath", "hypercube", "cake", "coin", "food ration"]

correctItems = ["hologram", "hypercube", "cake", "coin"]

function dropItems(items, instance)
    return takePath(["drop $item" for item in items], instance, 0)
end

function takeItems(items, instance)
    return takePath(["take $item" for item in items], instance, 0)
end

instance = Instance(list=list)

instance, flag = executeAll(instance)
print(printAscii(instance.output))

instance = takePath(path1, instance)
instance = takePath(path2, instance)

instance = dropItems(items, instance)
instance = takeItems(correctItems, instance)

while true
    instance, flag = executeAll(instance)
    command = Input(instance.output)
    instance.output = []
    instance.input =  [collect(command)..., '\n'] |> reverse
end