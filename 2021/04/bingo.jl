
using BenchmarkTools

function read_draws(infile)
    [parse(Int, x) for x in split(readline(infile), ',')]
end

function read_boards(n, infile)
    boards = Array{Int}(undef, n, n, 0)
    while true
        # Skip the blank line before every table
        line = readline(infile)
        board = Array{Int}(undef, 5, 5)
        for i in 1:n
            row_vals = [parse(Int, d) for d in split(readline(infile))]
            if isempty(row_vals)
                return boards
            elseif size(row_vals, 1) != n
                throw(DimensionMismatch("table width mismatch"))
            end

            for j in 1:n
                board[j, i] = row_vals[j]
            end
        end
        boards = cat(boards, board, dims = 3)
    end
end

function check_win(hits)
    for x in 1:size(hits, 1)
        count = 0
        for y in 1:size(hits, 2)
            if hits[y, x] == true
                count = count + 1
            end
        end
        if (count == 5)
            return true
        end
    end
    return false
end

function score_board(board, hits)
    score = 0
    for x in 1:size(board, 1)
        for y in 1:size(board, 2)
            if hits[y, x] == false
                score += board[y, x]
            end
        end
    end
    return score
end

function part_one(infile)
    n=5
    draws = read_draws(infile)
    boards = read_boards(n, infile)
    num_boards = size(boards, 3)
    hits = zeros(Bool, n, n, num_boards)
    for draw in draws
        # Update board hits for each draw
        for i in 1:num_boards
            for x in 1:n
                for y in 1:n
                    if boards[y, x, i] == draw
                        hits[y, x, i] = true
                    end
                end
            end
            # We can check here if we've won
            if check_win(hits[:, :, i]) || check_win(permutedims(hits[:, :, i], (2,1)))
                return draw * score_board(boards[:, :, i], hits[:, :, i])
            end
        end

    end
end

function part_two(infile)
    n=5
    draws = read_draws(infile)
    boards = read_boards(n, infile)
    num_boards = size(boards, 3)
    hits = zeros(Bool, n, n, num_boards)
    winning_boards = Int[]
    score = 0
    for draw in draws
        # Update board hits for each draw
        for i in 1:num_boards
            for x in 1:n
                for y in 1:n
                    if boards[y, x, i] == draw
                        hits[y, x, i] = true
                    end
                end
            end
            # We can check here if we've won
            if isnothing(indexin(i, winning_boards)[1]) &&
               (check_win(hits[:, :, i]) || check_win(permutedims(hits[:, :, i], (2,1))))
                score = draw * score_board(boards[:, :, i], hits[:, :, i])
                push!(winning_boards, i)
                continue
            end
        end
    end
    return score
end

function run()
    infile = open("input")
    ans1 = part_one(infile)
    infile = open("input")
    ans2 = part_two(infile)
end

# @benchmark run()
