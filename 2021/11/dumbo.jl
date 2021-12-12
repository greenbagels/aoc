# My god, dumbo octopi are so cute!

function evolve(synchronize)
    total = 0
    octopi = Int[]

    file = open("input")
    for line in readlines(file)
        for ch in line
            push!(octopi, parse(Int, ch))
        end
    end

    octopi = reshape(octopi, 10, :)

    # hopefully we finish finding the target step before overflow...
    # (change to int128/bigint if needed)
    step_max = synchronize ? typemax(Int) : 100

    for step in 1:step_max
        flashed = falses(size(octopi, 1), size(octopi, 2))
        octopi = map(x->x+1, octopi)

        while true
            found_flash = false
            for x in 1:size(octopi, 1), y in 1:size(octopi, 2)
                if octopi[x, y] > 9 && !flashed[x, y]
                    flashed[x, y] = true
                    found_flash = true
                    total += 1
                    for dx in -1:1, dy in -1:1
                        if min(x+dx, y+dy) >= 1 && max(x+dx, y+dy) <= 10
                            octopi[x+dx, y+dy] += 1
                        end
                    end
                end
            end
            found_flash || break
        end

        if synchronize
            if reduce(&, flashed)
                return step
            end
        end

        for x in 1:size(octopi, 1), y in 1:size(octopi, 2)
            if octopi[x, y] > 9
                octopi[x, y] = 0
            end
        end
    end
    return total
end

function part_one()
    return evolve(false)
end

function part_two()
    return evolve(true)
end

function run()
    println(part_one())
    println(part_two())
end

run()
