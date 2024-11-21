-- Clean Code version

-- Main program wrapped in a local scope to avoid global pollution
local function main()
    -- Constants for clarity
    local MIN_HYPOTENUSE = 5
    local MAX_HYPOTENUSE = 40

    -- Function to measure execution time of another function
    local function measureExecutionTime(func)
        return function(...)
            local startTime = os.clock()  -- Record the start time
            local result = {func(...)}  -- Execute the function and capture the results
            local endTime = os.clock()  -- Record the end time
            print(string.format("Execution time: %.4f seconds", endTime - startTime))  -- Print the execution time
            return table.unpack(result)  -- Return the results of the function
        end
    end

    -- Function to check if three sides form a Pythagorean triple
    local function isPythagoreanTriple(a, b, c)
        return a * a + b * b == c * c
    end

    -- Function to generate Pythagorean triples
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

    -- Function to print Pythagorean triples
    local function printPythagoreanTriples(triples)
        for _, triple in ipairs(triples) do
            print(string.format("%d, %d, %d", triple[1], triple[2], triple[3]))
        end
    end

    -- Measure execution time of the generatePythagoreanTriples function
    local timedGenerateTriples = measureExecutionTime(generatePythagoreanTriples)
    local triples = timedGenerateTriples()  -- Call the wrapped function to generate triples
    printPythagoreanTriples(triples)        -- Print the generated triples
end

-- Run the main program
main()
