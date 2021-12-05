
function part_one(inputs)
    n = size(inputs, 1)
    strlen = length(inputs[1])
    one_counts = zeros(Int, strlen)
    for i = 1:n
        for j = 1:strlen
            one_counts[j] += parse(Int, inputs[i][j])
        end
    end

    gamma = 0
    for i = 0:strlen-1
        if one_counts[strlen - i] > (n / 2)
            gamma += 2 ^ i
        end
    end
    # Our answer is going to be the product of this
    # result and its ones complement. But recall that
    # the max unsigned value for n bits is 2^n - 1.

    # Now, it follows that, for any x, x + comp(x) = 2^n - 1.
    # So ~x is just (2^n - 1) - x, so their product is x(2^n-1-x)
    gamma * (2 ^ strlen - 1 - gamma)
end

function part_two(inputs)
    strlen = length(inputs[1])

    ratings = [0, 0]
    operators = [>=, <]
    for i = 1:2
        list = inputs
        for j = 1:strlen
            count = 0
            n = size(list, 1)
            for k = 1:n
                count += parse(Int, list[k][j])
            end
            list = filter( (x) -> parse(Int, x[j]) == operators[i](count, n/2), list)
            if size(list, 1) == 1
                ratings[i] = parse(Int, list[1], base = 2)
                break
            end
        end
    end

    ratings[1] * ratings[2]
end

inputs = readlines(open("input"))

println(part_one(inputs))
println(part_two(inputs))
