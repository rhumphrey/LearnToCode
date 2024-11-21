local startTime1 = os.clock()

-- Generates Pythagorean triples
for c = 1, 40 do
    for b = 1, c-1 do
        for a = 1, b-1 do
            if a * a + b * b == c * c then
                print(string.format("%d, %d, %d", a, b, c))
            end
        end
    end
end

local endTime1 = os.clock() 
print(string.format("Execution time: %.4f seconds", endTime1 - startTime1))

local startTime2 = os.clock()

-- Generates Pythagorean triples - First Refactor for readability and efficiency
for hypotenuse = 5, 40 do            -- Start from 5 as the smallest hypotenuse in a Pythagorean triple is 5 (3,4,5)
    for leg2 = 4, hypotenuse - 1 do  -- Start from 4 as the smallest leg in a Pythagorean triple is 4 (3,4,5)
        for leg1 = 3, leg2 - 1 do    -- Start from 3 as the smallest leg in a Pythagorean triple is 3 (3,4,5)
            if leg1 * leg1 + leg2 * leg2 == hypotenuse * hypotenuse then
                print(string.format("%d, %d, %d", leg1, leg2, hypotenuse))
            end
        end
    end
end

local endTime2 = os.clock() 
print(string.format("Execution time: %.4f seconds", endTime2 - startTime2))