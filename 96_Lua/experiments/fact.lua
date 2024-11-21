-- defines a factorial function
function fact(n)
    if n < 0 then
        return "Factorial is not defined for negative numbers"
    elseif n == 0 then
        return 1
    else
        return n * fact(n - 1)
    end
end

print("Enter a number:")
a = io.read("*n")
print(fact(a))
