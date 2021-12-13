
function perform_folds(is_part_one)
    file = open("input")
    lines = readlines(file)
    folds = Tuple{Char, Int}[]
    sizes = Dict(zip("xy", [0, 0]))

    # Let's do this in a smart way. Folds always bisect the paper, so by parsing
    # the first two folds along different axes, we get the grid size. To be lazy,
    # we incur a possible extra conditional by updating the size vector every time
    for line in lines[indexin([""], lines)[1]+1:end]
        caps = match(r"([xy])=([0-9]+)", line).captures
        dim = caps[1][1]
        val = parse(Int, caps[2]) + 1
        push!(folds, (dim, val))
        sizes[dim] = max(2 * val - 1, sizes[dim])
    end

    grid = falses(sizes['y'], sizes['x'])
    for line in lines[1:indexin([""], lines)[1]-1]
        (x, y) = [parse(Int, str) + 1 for str in match(r"([0-9]+),([0-9]+)", line).captures]
        grid[y, x] = true
    end
    close(file)

    # At first we only kept track of the first size[1] x size[2] submatrix of our
    # matrix to save computational time. But now we just resize the grid each time
    # and use broadcasted OR operations to (possibly) take advantage of vectorized
    # matrix operations.
    for fold in folds
        if fold[1] == 'x'
            grid = grid[1:end, 1:fold[2]-1] .| reverse(grid[1:end, fold[2]+1:end]; dims=2)
        else
            grid = grid[1:fold[2]-1, 1:end] .| reverse(grid[fold[2]+1:end, 1:end]; dims=1)
        end
        if is_part_one
            return reduce(+, grid)
        end
    end
    display(grid)
    println()
end

function part_one()
    return perform_folds(true)
end

function part_two()
    return perform_folds(false)
end

function run()
    println(part_one())
    println(part_two())
end

run()
