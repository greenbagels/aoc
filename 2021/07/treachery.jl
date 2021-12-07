
function calc_total_cost(cost_fun)
    fname = "input"
    crab_positions = [parse(Int, x) for x in split(readline(open(fname)), ',')]

    first = crab_positions[1]
    last = crab_positions[1]

    for x in crab_positions
        first = min(x, first)
        last = max(x, last)
    end

    last_total = typemax(Int)
    for x in first:last
        cur_total = 0
        for crab_pos in crab_positions
            cur_total += cost_fun(abs(x - crab_pos))
        end
        last_total = min(cur_total, last_total)
    end

    return last_total
end

function part_one()
    calc_total_cost((n) -> n)
end

function part_two()
    calc_total_cost((n) -> n * (n+1) ÷ 2)
end

function run()
    println(part_one())
    println(part_two())
end

run() 
