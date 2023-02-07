include("../Intcode/Intcode.jl")
using .Intcode: Instance, executeAll
using OffsetArrays

list = open("Day19/Input.txt") do file
    line = parse.(Int, split(read(file, String), ","))
    return OffsetArray(line, 0:length(line)-1)
end

function drawBeam(beam)
    beam = map(x -> x == 1 ? '#' : '.', beam)
    return join([join(row) for row in eachrow(beam)], '\n')
end

function testCoords(list, x, y)
    inst = Instance(list=copy(list), input=[x,y])
    inst, flag = executeAll(inst)
    return inst.output[1]
end

function makeBeam(xy, size)
    xi, yi = xy
    tractorMap = OffsetArray(zeros(size, size), xi:xi+size-1, yi:yi+size-1)
    for x in xi:xi+size-1
        for y in yi:yi+size-1
            tractorMap[x,y] = testCoords(list, x, y)
        end 
    end
    return tractorMap
end

function makeLine(dist, size, minAngle, maxAngle)
    xys = pointsAt(dist+size-1, minAngle, maxAngle)
    return [testCoords(list, xy...) for xy in xys], xys
end

function pointsAt(distance, minAngle, maxAngle)
    tol = 0.02
    return filter(xy -> minAngle-tol < atan(xy...) < maxAngle+tol, [(i, distance-i) for i in 0:distance])
end

function firstFit(dist, size, minAngle, maxAngle)
    line, xys = makeLine(dist, size, minAngle, maxAngle)
    if count(x->x==1, line) >= size
        return dist, line, xys
    else
        range = findall(x -> x==1, line)
        maxAngle = atan(xys[range[end]]...)
        minAngle = atan(xys[range[1]]...)
        return firstFit(dist+1, size, minAngle, maxAngle)
    end
end

dist, line, xys = firstFit(1,100, atan(0,1), atan(1,0))

range = xys[findall(x -> x==1, line)]

println(range[1] .- (0,99))
xy = range[end] .- (99,0)

#println(drawBeam(makeBeam(xy.-(0,1), 100)), '\n')
#println(drawBeam(makeBeam(xy.-(1,0), 100)), '\n')
#println(drawBeam(makeBeam(xy, 100)), '\n')

println(xy[2]*10000+xy[1])