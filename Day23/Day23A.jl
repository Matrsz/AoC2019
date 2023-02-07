using OffsetArrays
using Parameters
using Match

struct Opcode
    code
    modes
end

@with_kw mutable struct Instance
    ic::Int = 0
    list::OffsetVector{Int}
    auxmem::Dict{Int,Int} = Dict()
    input::Vector{Int} = []
    output::Vector{Int} = []
    rel::Int = 0
end

list = open("Day23/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

ops = Dict(1 => +, 2 => *, 7 => ((x, y) -> Int(x<y)), 8 => ((x, y) -> Int(x==y)))

nArgs = Dict(1 => 3, 2 => 3, 3 => 1, 4 => 1, 5 => 2, 6 => 2, 7 => 3, 8 => 3, 9 => 1, 99 => 0)

function writeAt(elem, dest, instance)
    @unpack list, auxmem = instance
    if dest in eachindex(list)
        instance.list[dest] = elem
    else
        instance.auxmem = merge(auxmem, Dict(dest => elem))
    end
    return instance
end

function readFrom(source, instance)
    @unpack list, auxmem = instance
    return source in eachindex(list) ? list[source] : (haskey(auxmem, source) ? auxmem[source] : 0)
end

function getInterpreted(opmode, arg, instance)
    @unpack rel = instance
    return opmode==1 ? arg : readFrom(opmode==2 ? arg+rel : arg, instance)
end
function getLiteral(opmode, arg, instance)
    @unpack rel = instance
    return opmode==2 ? arg+rel : arg
end

function getParams(op, args, instance)
    function getParam(i, mode, arg)
        if i==3 && op.code in [1,2,7,8] || i==1 && op.code == 3
            return getLiteral(mode, arg, instance)
        else
            return getInterpreted(mode, arg, instance)
        end
    end
    return [getParam(i, op.modes[i], args[i]) for i in eachindex(args)]
end

function execute(op, args, instance)
    arg1, arg2, dest = args
    return writeAt(ops[op.code](arg1, arg2), dest, instance)
end

function readInput(args, instance)
    @unpack input = instance
    item = pop!(input)
    instance.input = input
    return writeAt(item, args[1], instance)
end

function writeOutput(args, instance)
    @unpack output = instance
    push!(output, args[1])
    instance.output = output
    return instance
end

function jump(op, args, ic)
    arg1, arg2 = args
    jump_if = x -> op.code == 5 ? x != 0 : x == 0
    return jump_if(arg1) ? arg2 : ic
end

function executeAll(instance)
    while true
        @unpack list, ic, auxmem, input, output, rel = instance
        op = getOpcode(list[ic])
        n = nArgs[op.code]
        args = getParams(op, list[ic+1:ic+n], instance)
        if op.code == 3 && input == []
            return instance, :wait
        end
        instance.ic = ic + n + 1
        if op.code == 99
            return instance, :halt
        elseif op.code == 9
            instance.rel = rel + args[1]
        elseif op.code in [5,6]
            instance.ic = jump(op, args, instance.ic)
        elseif op.code == 3 
            instance = readInput(args, instance)
        elseif op.code == 4
            instance = writeOutput(args, instance)
        elseif op.code in [1,2,7,8]
            instance = execute(op, args, instance)
        else 
            return instance, :error
        end
    end
end

function getOpcode(x)
    A, B, C, DE... = lpad(string(x), 5, "0")
    return Opcode(parse(Int, DE), parse.(Int, [C, B, A]))
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