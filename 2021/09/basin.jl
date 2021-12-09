
using DataStructures

function is_low_point(heights, i, j)
    low = true

    if i > 1
        low &= heights[i-1, j] > heights[i, j]
    end
    if i < size(heights, 1)
        low &= heights[i+1, j] > heights[i, j]
    end
    if j > 1
        low &= heights[i, j-1] > heights[i, j]
    end
    if j < size(heights, 2)
        low &= heights[i, j+1] > heights[i, j]
    end

    return low
end

function part_one()
    file = open("input")
    heights = Int[]
    for line in readlines(file)
        vec = [parse(Int, x) for x in line]
        heights = cat(heights, vec; dims=1)
    end
    heights = reshape(heights, 100, :)

    total = 0
    for i in 1:size(heights, 1), j in 1:size(heights, 2)
        if is_low_point(heights, i, j)
            total += heights[i, j] + 1
        end
    end

    close(file)
    return total
end

function part_two()
    file = open("input")
    heights = Int[]
    for line in readlines(file)
        vec = [parse(Int, x) for x in line]
        heights = cat(heights, vec; dims=1)
    end
    heights = reshape(heights, 100, :)

    basins = Array{Tuple{Int, Int}}[]
    for i in 1:size(heights, 1), j in 1:size(heights, 2)
        if is_low_point(heights, i, j)
            # Build the basin

            basin = Tuple{Int, Int}[]
            stack = Stack{Tuple{Int, Int}}()
            push!(stack, (i,j))

            while(!isempty(stack))
                (x, y) = first(stack)
                pop!(stack)
                push!(basin, (x,y))
                next_coords = Tuple{Int, Int}[]
                if x > 1
                    push!(next_coords, (x - 1, y))
                end
                if x < size(heights, 1)
                    push!(next_coords, (x + 1, y))
                end
                if y > 1
                    push!(next_coords, (x, y - 1))
                end
                if y < size(heights, 2)
                    push!(next_coords, (x, y + 1))
                end

                for (newx, newy) in next_coords
                    if heights[newx, newy] != 9 &&
                       heights[newx, newy] > heights[x, y]
                        push!(stack, (newx, newy))
                    end
                end
            end
            basin = unique(basin)
            push!(basins, basin)
        end
    end
    sizes = sort!([size(basins[i], 1) for i in 1:size(basins, 1)], rev=true)
    return foldl(*, sizes[1:3])
end

function run()
    println(part_one())
    println(part_two())
end

run()
