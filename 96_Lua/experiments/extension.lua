-- Base table
Rectangle = {length = 0, width = 0}

-- Method to calculate area
function Rectangle:area()
    return self.length * self.width
end

-- Creating a new table for Square
Square = {side = 0}

-- Set metatable for Square to inherit from Rectangle
setmetatable(Square, {__index = Rectangle})

-- Extend Square with new method
function Square:new(side)
    local o = {}
    setmetatable(o, self)
    self.__index = self
    self.length = side
    self.width = side
    o.side = side  -- note that this line sets the 'side'
    return o
end

-- Create a square object
local sq = Square:new(5)

-- Access the inherited method
print("Square side: ", sq.side) -- Output: Square side: 5
print("Square area: ", sq:area()) -- Output: Square area: 25