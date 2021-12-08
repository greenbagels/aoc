
using BenchmarkTools
using LinearAlgebra

function part_one()
    file = open("input")

    acceptable_lens = [2, 3, 4, 7]
    count = 0
    for line in readlines(file)
        len = 15 # 15 tokens including the | delimiter
        for str in split(line)[12:15]
            keyidx = findfirst((x) -> x == length(str), acceptable_lens)
            if !isnothing(keyidx)
                count += 1
            end
        end
    end

    return count
end

function bit_encode(str)
    byte::UInt8 = 0
    for ch in str
        byte |= 1 << (UInt(ch) - 97)
    end
    return byte
end

function count_bits(byte::UInt8)
    count = 0
    for i in 0:7
        mask::UInt8 = (1 << i)
        if (byte & mask) != 0
            count += 1
        end
    end
    return count
end

# part two, the C way
function part_two_bitpacked()
    file = open("input")

    dict = Array{Int8}(undef, 256)
    for (n,i) in zip([119, 36, 93, 109, 46, 107, 123, 37, 127, 111], 0:9)
        dict[n] = i
    end

    total = 0
    for line in readlines(file)
        # perms[i] are rows of a permutation matrix, which we assemble via bitwise
        # operations. we could (but won't) use the matrix representation of this
        # to make it easy to understand at a high level (i.e. using this matrix
        # and its inverse to permute/unpermute vectors of characters).

        # the i-th element of this array holds the signal that corresponds to
        # the "true" i-th signal
        perms = zeros(UInt8, 8)
        signals = [bit_encode(str) for str in split(line)[1:10]]
        clock = split(line)[12:15]

        signal = signals[findfirst(sig -> count_bits(sig) == 2, signals)]
        perms[3] = signal
        perms[6] = signal

        signal = signals[findfirst(sig -> count_bits(sig) == 3, signals)]
        perms[1] = signal ⊻ perms[3]

        signal = signals[findfirst(sig -> count_bits(sig) == 4, signals)]
        perms[2] = perms[3] ⊻ signal
        perms[4] = perms[2]

        signal = foldl(&, filter(sig -> count_bits(sig) == 6, signals))
        perms[2] &= signal
        perms[4] ⊻= perms[2]
        perms[6] &= signal
        perms[3] ⊻= perms[6]

        signal = foldl(&, filter(sig -> count_bits(sig) == 5, signals))
        perms[7] = signal ⊻ (perms[1] | perms[4])

        signal = foldl(|, perms[1:7])
        perms[5] = ~signal - 128

        # Make the inverse map
        arr = zeros(UInt8, 7)
        for i in 1:7
            arr[Int(log2(perms[i]))+1] = 1 << (i-1)
        end

        # Now parse digits and add to total
        for i in 1:4
            order = 10 ^ (4-i)
            digit::UInt8 = 0
            for j = 1:length(clock[i])
                digit |= arr[Int(clock[i][j]) - 96]
            end
            total += dict[digit] * order
        end
    end

    # next to godliness
    close(file)
    return total
end

function vec_encode(str)
    vec = falses(7)
    for ch in str
        vec[Int(ch) - 96] = true
    end
    return vec
end

function char_to_digit(vec)
    mat = falses(7, 10)
    mat[:, 1]  = [1, 1, 1, 0, 1, 1, 1]
    mat[:, 2]  = [0, 0, 1, 0, 0, 1, 0]
    mat[:, 3]  = [1, 0, 1, 1, 1, 0, 1]
    mat[:, 4]  = [1, 0, 1, 1, 0, 1, 1]
    mat[:, 5]  = [0, 1, 1, 1, 0, 1, 0]
    mat[:, 6]  = [1, 1, 0, 1, 0, 1, 1]
    mat[:, 7]  = [1, 1, 0, 1, 1, 1, 1]
    mat[:, 8]  = [1, 0, 1, 0, 0, 1, 0]
    mat[:, 9]  = [1, 1, 1, 1, 1, 1, 1]
    mat[:, 10] = [1, 1, 1, 1, 0, 1, 1]

    for i in 0:9
        if vec == mat[:, i + 1]
            return i
        end
    end
    throw(DomainError(vec, "Bad vector passed!"))
end

# Part 2, the fun way
function part_two_math_edition()
    file = open("input")

    total = 0
    for line in readlines(file)
        perm = falses(7, 7)
        signals = [vec_encode(str) for str in split(line)[1:10]]

        signal = signals[findfirst(vec -> norm(vec, 1) == 2, signals)]
        perm[:, 3] = signal
        perm[:, 6] = signal

        signal = signals[findfirst(vec -> norm(vec, 1) == 3, signals)]
        perm[:, 1] = signal .⊻ perm[:, 3]

        signal = signals[findfirst(vec -> norm(vec, 1) == 4, signals)]
        perm[:, 2] = signal .⊻ perm[:, 3]
        perm[:, 4] = perm[:, 2]

        signal = foldl((a,b) -> a .& b, filter(vec -> norm(vec, 1) == 6, signals))
        perm[:, 2] = perm[:, 2] .& signal
        perm[:, 4] = perm[:, 4] .⊻ perm[:, 2]
        perm[:, 6] = perm[:, 6] .& signal
        perm[:, 3] = perm[:, 3] .⊻ perm[:, 6]

        signal = foldl((a,b) -> a .& b, filter(vec -> norm(vec, 1) == 5, signals))
        perm[:, 7] = signal .⊻ (perm[:, 1] .| perm[:, 4])

        signal = foldl((a,b) -> a .| b, [perm[:,x] for x in 1:7])
        perm[:, 5] = [~bit for bit in signal]

        digits = [vec_encode(str) for str in split(line)[12:15]]

        # display(perm)

        for (digit, i) in zip(digits, 3:-1:0)
            total += 10 ^ i * char_to_digit([Bool(x) for x in inv(perm) * digit])
        end
    end

    close(file)
    return total
end

function run()
    println(part_one())
    println(part_two_bitpacked())
    @btime part_two_bitpacked()
    @btime part_two_math_edition()
end

run()

