-- Main program wrapped in a local scope to avoid global pollution
local function main()
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

    -- Function to generate Pythagorean triples
    local function generatePythagoreanTriples()
        local triples = {}
        for hypotenuse = 5, 40 do  -- Iterate over possible hypotenuse values
            for leg2 = 4, hypotenuse - 1 do  -- Iterate over possible leg2 values
                for leg1 = 3, leg2 - 1 do  -- Iterate over possible leg1 values
                    if leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse then  -- Check if the sides form a Pythagorean triple
                        table.insert(triples, {leg1, leg2, hypotenuse})  -- Store the Pythagorean triple
                    end
                end
            end
        end
        -- Print all triples
        for _, triple in ipairs(triples) do
            print(string.format("%d, %d, %d", triple[1], triple[2], triple[3]))
        end
    end

    -- Measure execution time of the generatePythagoreanTriples function
    generatePythagoreanTriples = measureExecutionTime(generatePythagoreanTriples)
    generatePythagoreanTriples()  -- Call the wrapped function
end

-- Run the main program
main()
