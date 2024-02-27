def euclid(a, b)
  while a != b
    if a > b
      a = a - b
    else 
      b = b - a
    end
  end
  return a
end

puts euclid(34, 16)