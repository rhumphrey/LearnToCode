-- Constants for clear, descriptive, and separate data
local MIN_HYPOTENUSE = 5
local MAX_HYPOTENUSE = 40

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
    Function: printPythagoreanTriples
    Purpose: Print a list of Pythagorean triples.
    Parameters:
        triples (table) - The list of Pythagorean triples to print.
--]]
local function printPythagoreanTriples(triples)
    for _, triple in ipairs(triples) do
        print(string.format("%d, %d, %d", triple[1], triple[2], triple[3]))  -- Print each triple
    end
end

--[[
    Function: main
    Purpose: Main function to run the program.
--]]
local function main()
    -- Generate Pythagorean triples within the specified range
    local triples = generatePythagoreanTriples(MIN_HYPOTENUSE, MAX_HYPOTENUSE)
    
    -- Print the generated Pythagorean triples
    printPythagoreanTriples(triples)
end

-- Run the main program
main()

--[[ 
Key Principles Incorporated:
1. Meaningful Names: Clear, descriptive names for variables and functions to improve readability and understanding.
2. Constants for Readability: Constants like MIN_HYPOTENUSE and MAX_HYPOTENUSE provide context and are easy to modify.
3. Single Responsibility Functions: Each function has a single responsibility, making the code easier to manage and test.
4. Separation of Concerns: Logic for checking triples, generating triples, and printing triples is separated into distinct functions.
5. Modularization: Main program logic is encapsulated in a main function to keep the global namespace clean.
6. Commenting and Documentation: Added detailed documentation for each function and inline comments to explain the logic, enhancing understandability.
]]
