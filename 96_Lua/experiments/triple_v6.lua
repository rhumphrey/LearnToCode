-- Programmers Brain version

-- Main program encapsulated in a module for clarity and cognitive ease
local function main()
    -- Constants for clear, descriptive, and separate data
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
    local function isPythagoreanTriple(leg1, leg2, hypotenuse)
        return leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse
    end

    -- Function to generate Pythagorean triples
    local function generatePythagoreanTriples(minHypotenuse, maxHypotenuse)
        local triples = {}
        for hypotenuse = minHypotenuse, maxHypotenuse do
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
    local timedGenerateTriples = measureExecutionTime(function()
        return generatePythagoreanTriples(MIN_HYPOTENUSE, MAX_HYPOTENUSE)
    end)
    
    -- Generate and print Pythagorean triples
    local triples = timedGenerateTriples()
    printPythagoreanTriples(triples)
end

-- Run the main program
main()
