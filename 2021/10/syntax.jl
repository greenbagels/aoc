
using DataStructures

function check_line_corrupt_score(line)
    ldict = Dict(zip("([{<", [3, 57, 1197, 25137]))
    rdict = Dict(zip(")]}>", [3, 57, 1197, 25137]))
    stack = Stack{Int}()
    for ch in line
        if get(ldict, ch, 0) == 0
            if rdict[ch] != first(stack)
                return rdict[ch]
            else
                pop!(stack)
            end
        else
            push!(stack, ldict[ch])
        end
    end
    return 0
end

function part_one()
    total = 0
    file = open("input")
    for line in readlines(file)
        total += check_line_corrupt_score(line)
    end
    close(file)
    return total
end

function check_line_incomp_score(line)
    ldict = Dict(zip("([{<", 1:4))
    stack = Stack{Int}()
    for ch in line
        if get(ldict, ch, 0) == 0
            pop!(stack)
        else
            push!(stack, ldict[ch])
        end
    end

    # Stacks are LIFO, so foldl is actually top-to-bottom
    return foldl((a,b) -> 5 * a + b, stack; init=0)
end

function part_two()
    total = 0
    file = open("input")
    incompletes = String[]
    for line in readlines(file)
        if check_line_corrupt_score(line) == 0
            push!(incompletes, line)
        end
    end

    scores = Int[]
    for line in incompletes
        push!(scores, check_line_incomp_score(line))
    end
    close(file)
    return sort(scores)[Int((size(scores, 1) + 1) / 2)]
end

function run()
    println(part_one())
    println(part_two())
end

run()
