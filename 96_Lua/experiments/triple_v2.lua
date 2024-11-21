--[[
Function: measureExecutionTime
Purpose: Wraps a function to measure its execution time.
Parameters:
    func (function) - The function to be measured.
Returns:
    A new function that, when called, executes the original function and prints its execution time.
--]]
local function measureExecutionTime(func)
    return function(...)
        local startTime = os.clock()  -- Record the start time
        local result = {func(...)}  -- Execute the function and capture the results
        local endTime = os.clock()  -- Record the end time
        print(string.format("Execution time: %.4f seconds", endTime - startTime))  -- Print the execution time
        return table.unpack(result)  -- Return the results of the function
    end
end

--[[
Function: generatePythagoreanTriples
Purpose: Generates and prints all Pythagorean triples with a hypotenuse between 5 and 40.
--]]
local function generatePythagoreanTriples()
    for hypotenuse = 5, 40 do  -- Iterate over possible hypotenuse values
        for leg2 = 4, hypotenuse - 1 do  -- Iterate over possible leg2 values
            for leg1 = 3, leg2 - 1 do  -- Iterate over possible leg1 values
                if leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse then  -- Check if the sides form a Pythagorean triple
                    print(string.format("%d, %d, %d", leg1, leg2, hypotenuse))  -- Print the Pythagorean triple
                end
            end
        end
    end
end

-- Measure execution time of the generatePythagoreanTriples function
generatePythagoreanTriples = measureExecutionTime(generatePythagoreanTriples)
generatePythagoreanTriples()  -- Call the wrapped function
