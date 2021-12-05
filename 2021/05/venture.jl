
function parse_xy_pair(instring)
    list = split(instring, ',')
    return (parse(Int, list[1]), parse(Int, list[2]))
end

function get_list(inputs)
    coord_list = Tuple{Tuple{Int, Int}, Tuple{Int, Int}}[]

    for str in inputs
        coord_str_list = split(str, " -> ")
        # Is there a better way to make tuples?
        push!(coord_list, (parse_xy_pair(coord_str_list[1]), parse_xy_pair(coord_str_list[2])))
    end
    return coord_list
end

function count_overlaps(coord_list, ignore_diagonals)
    map = zeros(Int, 1000, 1000)
    for pair in coord_list
        start = pair[1]
        finish = pair[2]
        #iterators ahoy
        xrange = start[1] <= finish[1] ? (start[1]:finish[1]) : (start[1]:-1:finish[1])
        yrange = start[2] <= finish[2] ? (start[2]:finish[2]) : (start[2]:-1:finish[2])

        if start[1] != finish[1] && start[2] != finish[2]
            if ignore_diagonals
                continue
            else
                for (x,y) in zip(xrange, yrange)
                    map[y, x] = map[y, x] + 1
                end
            end
        else
            for x in xrange, y in yrange
                map[y, x] = map[y, x] + 1
            end
        end
    end

    count = 0
    for points in map
        if points > 1
            count = count + 1
        end
    end
    return count
end

function part_one(coord_list)
    return count_overlaps(coord_list, true)
end

function part_two(coord_list)
    return count_overlaps(coord_list, false)
end

function run()
    inputs = readlines(open("input"))
    coord_list = get_list(inputs)

    println(part_one(coord_list))
    println(part_two(coord_list))
end

run()
