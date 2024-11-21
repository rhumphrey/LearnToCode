-- Constants for default range
local DEFAULT_MIN_HYPOTENUSE = 5
local DEFAULT_MAX_HYPOTENUSE = 40

--[[
    Function: isPythagoreanTriple
    Purpose: Check if three sides form a Pythagorean triple.
    Parameters:
        leg1 (number) - The first leg of the triangle.
        leg2 (number) - The second leg of the triangle.
        hypotenuse (number) - The hypotenuse of the triangle.
    Returns:
        boolean - True if the sides form a Pythagorean triple, false otherwise.
--]]
local function isPythagoreanTriple(leg1, leg2, hypotenuse)
    return leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse
end

--[[
    Function: generatePythagoreanTriples
    Purpose: Generate Pythagorean triples within a specified range.
    Parameters:
        minHypotenuse (number) - The minimum value for the hypotenuse.
        maxHypotenuse (number) - The maximum value for the hypotenuse.
    Returns:
        table - A list of Pythagorean triples.
--]]
local function generatePythagoreanTriples(minHypotenuse, maxHypotenuse)
    local triples = {}  -- Store results
    for hypotenuse = minHypotenuse, maxHypotenuse do  -- Iterate over possible hypotenuse values
        for leg2 = 4, hypotenuse - 1 do  -- Iterate over possible leg2 values
            for leg1 = 3, leg2 - 1 do  -- Iterate over possible leg1 values
                if isPythagoreanTriple(leg1, leg2, hypotenuse) then  -- Check if sides form a Pythagorean triple
                    table.insert(triples, {leg1, leg2, hypotenuse})  -- Add the triple to the list
                end
            end
        end
    end
    return triples
end

--[[
    Function: sortTriples
    Purpose: Sort Pythagorean triples by a given criterion.
    Parameters:
        triples (table) - The list of Pythagorean triples to sort.
        criterion (string) - The criterion to sort by ("hypotenuse" or "sum").
    Returns:
        table - The sorted list of Pythagorean triples.
--]]
local function sortTriples(triples, criterion)
    table.sort(triples, function(a, b)
        if criterion == "hypotenuse" then
            return a[3] < b[3]
        elseif criterion == "sum" then
            return (a[1] + a[2] + a[3]) < (b[1] + b[2] + b[3])
        end
    end)
    return triples
end

--[[
    Function: printPythagoreanTriples
    Purpose: Print a list of Pythagorean triples.
    Parameters:
        triples (table) - The list of Pythagorean triples to print.
--]]
local function printPythagoreanTriples(triples)
    print("Pythagorean Triples:")
    for _, triple in ipairs(triples) do
        print(string.format("Leg1: %d, Leg2: %d, Hypotenuse: %d", triple[1], triple[2], triple[3]))  -- Print each triple with formatted output
    end
    print(string.format("Total Triples Found: %d", #triples))  -- Print the total number of triples
end

--[[
    Function: main
    Purpose: Main function to run the program.
--]]
local function main()
    -- Allow user to specify the range of hypotenuses
    print("Enter the minimum value for the hypotenuse (default is 5):")
    local minHypotenuse = tonumber(io.read()) or DEFAULT_MIN_HYPOTENUSE
    
    print("Enter the maximum value for the hypotenuse (default is 40):")
    local maxHypotenuse = tonumber(io.read()) or DEFAULT_MAX_HYPOTENUSE
    
    -- Generate Pythagorean triples within the specified range
    local triples = generatePythagoreanTriples(minHypotenuse, maxHypotenuse)
    
    -- Sort the triples by hypotenuse
    triples = sortTriples(triples, "hypotenuse")
    
    -- Print the generated Pythagorean triples
    printPythagoreanTriples(triples)
end

-- Run the main program
main()

--[[ 
Key Principles Incorporated:
1. Meaningful Names: Clear, descriptive names for variables and functions to improve readability and understanding.
2. Constants for Readability: Constants like DEFAULT_MIN_HYPOTENUSE and DEFAULT_MAX_HYPOTENUSE provide context and are easy to modify.
3. Single Responsibility Functions: Each function has a single responsibility, making the code easier to manage and test.
4. Separation of Concerns: Logic for checking triples, generating triples, sorting triples, and printing triples is separated into distinct functions.
5. Modularization: Main program logic is encapsulated in a main function to keep the global namespace clean.
6. Commenting and Documentation: Added detailed documentation for each function and inline comments to explain the logic, enhancing understandability.
]]
