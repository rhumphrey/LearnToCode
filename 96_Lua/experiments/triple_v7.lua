-- Design Patterns version

-- Main program encapsulated in a module for clarity and cognitive ease
local function main()
    -- Constants for clear, descriptive, and separate data
    local MIN_HYPOTENUSE = 5
    local MAX_HYPOTENUSE = 40

    -- Abstract class for TripleChecker (Strategy Pattern)
    local function TripleChecker()
        local self = {}
        function self:isTriple(leg1, leg2, hypotenuse)
            error("isTriple method not implemented")
        end
        return self
    end

    -- Concrete class for PythagoreanTripleChecker (Strategy Pattern)
    local function PythagoreanTripleChecker()
        local self = TripleChecker()
        function self:isTriple(leg1, leg2, hypotenuse)
            return leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse
        end
        return self
    end

    -- Factory Method Pattern to create a TripleChecker
    local function createTripleChecker()
        return PythagoreanTripleChecker()
    end

    -- Template Method Pattern for measuring execution time
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
    local function generatePythagoreanTriples(minHypotenuse, maxHypotenuse, checker)
        local triples = {}
        for hypotenuse = minHypotenuse, maxHypotenuse do
            for leg2 = 4, hypotenuse - 1 do
                for leg1 = 3, leg2 - 1 do
                    if checker:isTriple(leg1, leg2, hypotenuse) then
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

    -- Create a TripleChecker using the Factory Method Pattern
    local tripleChecker = createTripleChecker()

    -- Measure execution time of the generatePythagoreanTriples function
    local timedGenerateTriples = measureExecutionTime(function()
        return generatePythagoreanTriples(MIN_HYPOTENUSE, MAX_HYPOTENUSE, tripleChecker)
    end)
    
    -- Generate and print Pythagorean triples
    local triples = timedGenerateTriples()
    printPythagoreanTriples(triples)
end

-- Run the main program
main()

