-- Idiomatic Lua Script

-- 1. Table construction and access
local person = {
    name = "Mav",                       -- Name field in the table
    age = 3,                            -- Age field in the table
    greet = function(self)              -- Method to greet
        return "Hello, my name is " .. self.name
    end
}

print(person:greet())                   -- Output: Hello, my name is Mav

-- 2. Iterating over arrays
local fruits = {"apple", "banana", "cherry"}
for i, fruit in ipairs(fruits) do       -- Using ipairs to iterate over the array
    print(i, fruit)
end

-- 3. Conditional statements
local temp = 25
if temp > 30 then                       -- If temperature is greater than 30
    print("It's hot outside.")
else                                    -- If temperature is 30 or below
    print("It's cool outside.")
end

-- 4. Loops
for i = 1, 5 do                         -- Loop from 1 to 5
    print("Number " .. i)
end

-- 5. Functions
function add(a, b)                      -- Function to add two numbers
    return a + b
end
print(add(5, 10))                       -- Output: 15

-- 6. Metatables and metamethods
local t = {1, 2, 3}
setmetatable(t, {
    __add = function(t1, t2)            -- Define __add metamethod for adding tables
        local result = {}
        for i = 1, #t1 do
            result[i] = t1[i] + t2[i]
        end
        return result
    end
})
local t2 = {4, 5, 6}
local sum = t + t2                      -- Adding two tables
for _, v in ipairs(sum) do
    print(v)                            -- Output: 5, 7, 9
end

-- 7. Using the __index metamethod
local default = {color = "blue"}
local t = setmetatable({}, {__index = default})  -- Set default table to use if key is missing

print(t.color)                          -- Output: blue
t.color = "red"
print(t.color)                          -- Output: red

-- 8. Coroutines
function foo()
    print("foo 1")
    coroutine.yield()                   -- Yield execution
    print("foo 2")
end

co = coroutine.create(foo)
print(coroutine.status(co))             -- Print coroutine status (suspended)
coroutine.resume(co)                    -- Resume coroutine
print(coroutine.status(co))             -- Print coroutine status (suspended again)
coroutine.resume(co)                    -- Resume coroutine
print(coroutine.status(co))             -- Print coroutine status (dead)
