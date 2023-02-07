include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day23/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

computers = Dict(k => Instance(list=copy(list), input=[k]) for k in 0:49)

for k in keys(computers)
    computers[k], flag = executeAll(computers[k])
end

println(filter(x->!isempty(x), [computers[k].output for k in 0:49]))

struct Packet
    dest::Int
    x::Int
    y::Int
end

function takeMessages(output)
    if isempty(output)
        return []
    else
        return [Packet(output[1:3]...), takeMessages(output[4:end])...]
    end
end

function networkInstant(computers, idle=0, messageQueue=[])
    if !isempty(messageQueue) 
        println(messageQueue)
    end
    for k in keys(computers)
        tx = computers[k].output 
        computers[k].output = []
        for m in takeMessages(tx)
            push!(messageQueue, m)
            println("Tx: $k \t -> Rx: $(m.dest)  \t Message: [$(m.x), $(m.y)]")
            sleep(0.01)
        end
    end
    for k in keys(computers)
        if !any(m -> m.dest == k, messageQueue)
            push!(computers[k].input, -1)
        end
    end
    idle = isempty(messageQueue) ? idle+1 : 0
    for m in reverse(messageQueue)
        if haskey(computers, m.dest)
            push!(computers[m.dest].input, m.y)
            push!(computers[m.dest].input, m.x)
            messageQueue = filter(x->m!=x, messageQueue)
        end
    end
    for k in keys(computers)
        computers[k], flag = executeAll(computers[k])
    end
    if !isempty(messageQueue) 
        println(messageQueue)
    end
    return computers, messageQueue, idle
end

function runNetwork(computers)
    while true 
        computers, messageQueue, idle = networkInstant(computers)
        if !isempty(messageQueue)
            return messageQueue
        end
    end
end

function runNATNetwork(computers)
    NAT = missing
    idle = 0
    past = 0
    messageQueue=[]
    while true
        computers, messageQueue, idle = networkInstant(computers, idle, messageQueue)
        for m in messageQueue
            if m.dest == 255
                NAT = (x = m.x, y = m.y)
                println("Received NAT message from $messageQueue")
                messageQueue = filter(x->m!=x, messageQueue)
                println("Used NAT message, now $messageQueue")
                sleep(0.1)
            end
        end
        if idle > 100
            println("We have an Idle situation: $messageQueue")
            sleep(0.5)
            messageQueue = [Packet(0, NAT.x, NAT.y)]
            if past == NAT.y
                return NAT.y
            else
                past = NAT.y
            end 
            idle = 0
        end
    end
end

runNATNetwork(computers)