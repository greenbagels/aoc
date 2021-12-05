
function part_one(inputs)
    pos = 0
    depth = 0

    for i=1:size(inputs, 1)
        m = match(r"([a-z]+) ([0-9]+)", inputs[i])
        dir = m.captures[1]
        dx = parse(Int, m.captures[2])
        if dir == "forward"
            pos += dx
        elseif dir == "up"
            depth -= dx
        elseif dir == "down"
            depth += dx
        end
    end

    pos * depth
end

function part_two(inputs)
    pos = 0
    depth = 0
    aim = 0

    for i=1:size(inputs, 1)
        m = match(r"([a-z]+) ([0-9]+)", inputs[i])
        dir = m.captures[1]
        dx = parse(Int, m.captures[2])
        if dir == "forward"
            pos += dx
            depth += aim * dx
        elseif dir == "up"
            aim -= dx
        elseif dir == "down"
            aim += dx
        end
    end

    pos * depth
end

inputs = readlines(open("input"))

println(part_one(inputs))
println(part_two(inputs))

