def bernoulli(n)
  a = Array.new(n + 1, 0)
  (0..n).each do |m|
    a[m] = Rational(1, m + 1)
    (m.downto(1)).each do |j|
      a[j - 1] = j * (a[j - 1] - a[j])
    end
  end
  a[0] # This is Bn
end
  
# Calculate and print the first 10 Bernoulli numbers
(0..9).each do |i|
  puts "B#{i} = #{bernoulli(i)}"
end
  