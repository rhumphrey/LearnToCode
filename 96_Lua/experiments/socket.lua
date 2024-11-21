-- Install luasocket if you haven't: `luarocks install luasocket`

-- Server code
local socket = require("socket")

local server = assert(socket.bind("localhost", 12345))
local ip, port = server:getsockname()

print("Server listening on " .. ip .. ":" .. port)

while true do
    local client = server:accept()
    client:settimeout(10)
    local message, err = client:receive()
    if not err then
        print("Received from client: " .. message)
        client:send("Hello, client!\n")
    end
    client:close()
end

-- Client code (run in a separate Lua instance)
local socket = require("socket")

local client = assert(socket.connect("localhost", 12345))
client:settimeout(10)

client:send("Hello, server!\n")
local response, err = client:receive()
if not err then
    print("Received from server: " .. response)
end

client:close()
