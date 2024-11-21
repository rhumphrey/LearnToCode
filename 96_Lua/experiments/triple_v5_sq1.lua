local function main()
    local MIN_HYPOTENUSE = 5
    local MAX_HYPOTENUSE = 40

    local function measureExecutionTime(func)
        return function(...)
            local startTime = os.clock()
            local result = {func(...)}
            local endTime = os.clock()
            print(string.format("Execution time: %.4f seconds", endTime - startTime))
            return table.unpack(result)
        end
    end

    local function isPythagoreanTriple(a, b, c)
        return a * a + b * b == c * c
    end

    local function generatePythagoreanTriples()
        local triples = {}
        for hypotenuse = MIN_HYPOTENUSE, MAX_HYPOTENUSE do
            for leg2 = 4, hypotenuse - 1 do
                for leg1 = 3, leg2 - 1 do
                    if isPythagoreanTriple(leg1, leg2, hypotenuse) then
                        table.insert(triples, {leg1, leg2, hypotenuse})
                    end
                end
            end
        end
        return triples
    end

    local function printPythagoreanTriples(triples)
        for _, triple in ipairs(triples) do
            print(string.format("%d, %d, %d", triple[1], triple[2], triple[3]))
        end
    end

    local timedGenerateTriples = measureExecutionTime(generatePythagoreanTriples)
    
    -- Run multiple iterations to accumulate execution time ** Side Quest
    local cumulativeTime = 0
    for i = 1, 1000 do
        local startTime = os.clock()
        local triples = generatePythagoreanTriples()
        local endTime = os.clock()
        cumulativeTime = cumulativeTime + (endTime - startTime)
    end
    
    print(string.format("Cumulative execution time over 1000 iterations: %.4f seconds", cumulativeTime))
    printPythagoreanTriples(generatePythagoreanTriples())
end

main()
