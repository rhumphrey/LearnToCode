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

puts euclid(7, 16)

def euclid_mod(a, b)
  while b != 0
    a, b = b, a % b
  end
  return a.abs
end

puts euclid_mod(7, 16)