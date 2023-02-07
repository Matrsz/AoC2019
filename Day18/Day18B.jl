maze = open("Day18/InputB.txt") do file
    return hcat(map(collect, readlines(file))...) |> permutedims
end

function findInMaze(item, tiles) 
    return [k for (k,v) in tiles if v == item][1]
end

function getRobots(maze)
    return findall(x -> x=='@', maze) .|> Tuple
end   

robots = getRobots(maze)

function floodFill(tiles, x)
    tiles[x] = '#'
    next = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]] 
    filter!(y -> haskey(tiles, y) && tiles[y] != '#', next)
    if isempty(next)
        return [x]
    else
        return push!(vcat([floodFill(tiles, n) for n in next]...), x)
    end
end

function mazeTiles(maze)
    return Dict(Tuple(x) => maze[x] for x in CartesianIndices(maze) if maze[x] != '#')
end

function mazeTiles(maze, robots)
    submazes = [floodFill(mazeTiles(maze), robot) for robot in robots]
    function submazeTiles(submaze) 
        return Dict(Tuple(x) => maze[x...] for x in submaze)
    end
    return [submazeTiles(maze) for maze in submazes]
end

mutable struct Path
    keys::Set{Char}
    dist::Int
    required::Set{Char}
end

function searchPath(node, dest, tiles, steps, gates)
    if node == dest
        return steps, gates, true
    end
    next = options(node, tiles, steps)
    results = []
    for n in next
        steps[n] = steps[node]+1
        result = searchPath(n, dest, tiles, steps, gates)
        push!(results, result)
    end
    if any(x -> x[3], results)
        steps, gates, _ = filter(x -> x[3], results)[1]
        if tiles[node] in ['A':'Z'...,'a':'z'...]
            push!(gates, lowercase(tiles[node]))
        end
        return steps, gates, true
    else
        return steps, gates, false
    end
end

function options(x, tiles, steps)
    opts = [x.+y for y in [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    filter!(y -> haskey(tiles,y), opts)
    filter!(y -> steps[y] > steps[x]+1, opts)
    return opts
end

function drawMaze(maze)
    return join([join(row) for row in eachrow(maze)], '\n')
end

function useKey(key::Char, mazeKeys)
    return filter(x->x != key, mazeKeys)
end

mutable struct SemiRoute
    key::Vector{Char}
    missingKeys::Set{Char}
    steps::Int
end

mutable struct Route
    data::SemiRoute
    next::Set{Route}
end

function shortestRoute(route)
    if isempty(route.next)
        if isempty(route.data.missingKeys)
            return route.data.steps
        else
            return 100000
        end
    else
        return minimum(shortestRoute.(route.next))
    end
end

function mazeKeys(tiles)
    return [v for (k,v) in tiles if v in ['a':'z'...]] |> Set
end

function allPaths(maze, robot)
    tiles = mazeTiles(maze,robots)[robot]
    allKeys = mazeKeys(tiles)
    paths = Path[]
    for k1 in union(allKeys, ['@']|>Set)
        for k2 in allKeys
            if k1 != k2 && !any(p -> p.keys == [k1, k2] |> Set, paths)
                start = findInMaze(k1, tiles)
                dest = findInMaze(k2, tiles)
                steps = Dict(k => 100000 for (k,v) in tiles)
                steps[start] = 0
                steps, gates = searchPath(start, dest, tiles, steps, [])
                push!(paths, Path([k1, k2] |> Set, steps[dest], gates |> Set))
            end
        end
    end
    return paths
end

paths = [allPaths(maze,i) for i in 1:4]
allkeys = union([mazeKeys(tiles) for tiles in mazeTiles(maze, robots)]...)

function availablePaths(paths::Vector{Path}, origin, mazeKeys)
    dest(path) = collect(filter(key -> key != origin, path.keys))[1]
    function isAvailable(path, mazeKeys) 
        return origin in path.keys && dest(path) in mazeKeys && isempty(intersect(filter(x -> x!=dest(path), path.required), mazeKeys))
    end
    return filter(p -> isAvailable(p, mazeKeys), paths)
end

function isBetterRoute(semiRoute1::SemiRoute, semiRoute2::SemiRoute)
    if semiRoute1.key != semiRoute2.key 
        return false
    end
    if semiRoute1.missingKeys == semiRoute2.missingKeys && semiRoute1.steps <= semiRoute2.steps
        return true
    end
    return false
end

function hasBetterRoute(known::Dict{Tuple{Vector{Char}, Set{Char}}, Int}, newKey::SemiRoute)
    state = (newKey.key, newKey.missingKeys)
    return haskey(known, state) && known[state] <= newKey.steps
end

function updateBestRoutes(known::Dict{Tuple{Vector{Char}, Set{Char}}, Int}, newKey::SemiRoute)
    known[(newKey.key, newKey.missingKeys)] = newKey.steps
    return known
end

function findRoute(route, allpaths, known)
    available = [availablePaths(ps, k, route.data.missingKeys) for (ps,k) in zip(allpaths, route.data.key)]
    for (robot, paths) in zip(1:4, available)
        for path in paths
            k = collect(filter(key -> key != route.data.key[robot], path.keys))[1]
            keysState = copy(route.data.key)
            keysState[robot] = k
            newRoute = Route(SemiRoute(keysState, useKey(k, route.data.missingKeys), route.data.steps+path.dist), Set())
            if !hasBetterRoute(known, newRoute.data)
                newRoute, known = findRoute(newRoute, allpaths, updateBestRoutes(known, route.data))
                push!(route.next, newRoute)
            end
        end
    end
    return route, updateBestRoutes(known, route.data)
end

semiRoute = SemiRoute(['@', '@', '@', '@'], allkeys, 0)

available = [availablePaths(ps, k, semiRoute.missingKeys) for (ps,k) in zip(paths, semiRoute.key)]

routes, known = findRoute(Route(semiRoute, Set()), paths, Dict((semiRoute.key, semiRoute.missingKeys) => 0))

routes

println(shortestRoute(routes))

known