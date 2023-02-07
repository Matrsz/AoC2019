bugs = open("Day24/Input.txt") do file
    return hcat(map(collect, readlines(file))...) |> permutedims
end

indices = CartesianIndices(bugs) .|> Tuple

function bioRating(bugs)
    return sum(2^(5*(x[1]-1)+x[2]-1) for x in CartesianIndices(bugs) .|> Tuple if bugs[x...]=='#')
end

bioRating(bugs)

function adjacentBugs(x, bugs)
    adj = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    filter!(y -> y in CartesianIndices(bugs) .|> Tuple, adj)
    return count(bugs[a...] == '#' for a in adj)
end

function evolveSquare(xy, bugs)
    if bugs[xy...] == '#' && adjacentBugs(xy, bugs) != 1 
        return '.'
    end
    if bugs[xy...] == '.' && adjacentBugs(xy, bugs) in [1,2]
        return '#'
    end
    return bugs[xy...]
end

function evolve(bugs)
    return map(xy -> evolveSquare(xy, bugs), CartesianIndices(bugs) .|> Tuple)
end

function drawBugs(bugs)
    return join([join(row) for row in eachrow(bugs)], '\n')
end

println("Initial state")
println(drawBugs(bugs),"\n")

for i in 1:4
    bugs = evolve(bugs)
    println("After $i minutes:")
    println(drawBugs(bugs),'\n')
end

function findStableLayout(bugs)
    ratings = [bioRating(bugs)]
    while true
        bugs = evolve(bugs)
        rating = bioRating(bugs)
        if rating in ratings
            return rating
        end
        push!(ratings, rating)
    end
end

findStableLayout(bugs)