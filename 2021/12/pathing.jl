
using DataStructures

const Path = Vector{Int}

function is_large_cave(string)
    return reduce((x,y) -> x && y < 'a', string; init=true)
end

function visited_small_twice(path, vlist, vdict)
    dict = Dict{Int, Int}()
    val = false
    # needing both the int -> string and string -> int mappings seems ill-designed
    for str in filter(x -> !is_large_cave(x), [vlist[idx] for idx in path])
        get!(dict, vdict[str], 0)
        dict[vdict[str]] += 1
        val |= (dict[vdict[str]] > 1)
    end
    return val
end

function count_paths(is_part_two)
    total = 0
    file = open("input")
    # First we pre-parse the list of vertices to fix the size of our adjacency matrix
    vlist = String[]
    for line in readlines(file)
        m = match(r"([a-zA-Z]+)-([a-zA-Z]+)", line)
        for vlabel in m.captures
            if isnothing(indexin([vlabel], vlist)[1])
                push!(vlist, vlabel)
            end
        end
    end
    println(vlist)

    adj_mat = falses(length(vlist), length(vlist))
    vdict = Dict(zip(vlist, 1:length(vlist)))
    seekstart(file)
    for line in readlines(file)
        m = match(r"([a-zA-Z]+)-([a-zA-Z]+)", line)
        (v1, v2) = (m.captures[1], m.captures[2])
        adj_mat[vdict[v1], vdict[v2]] = true
        adj_mat[vdict[v2], vdict[v1]] = true
    end

    display(adj_mat)
    println()

    # Begin our modified DFS
    traversal_stack = Stack{Path}()
    push!(traversal_stack, [vdict["start"]])

    while !isempty(traversal_stack)
        path = first(traversal_stack)
        pop!(traversal_stack)
        if last(path) == vdict["end"]
            total += 1
            continue
        end

        for i in 1:length(vlist)
            if adj_mat[i, last(path)]
                if is_large_cave(vlist[i]) || isnothing(last(indexin(i, path)))
                    push!(traversal_stack, push!(copy(path), i))
                # break this into its own branch for readability sake
                elseif is_part_two && vlist[i] != "start" && !visited_small_twice(path, vlist, vdict)
                    push!(traversal_stack, push!(copy(path), i))
                end
            end
        end
    end

    return total
end

function part_one()
    return count_paths(false)
end

function part_two()
    return count_paths(true)
end

function run()
    println(part_one())
    println(part_two())
end

run()
