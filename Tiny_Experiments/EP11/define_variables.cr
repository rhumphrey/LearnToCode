name = "Crystal" # Type inferred as String
version = 1.0 # Type inferred as Float64
is_compiled : Bool = true # Explicit type annotation

puts "Language: #{name} (Type: #{typeof(name)})"
puts "Version: #{version} (Type: #{typeof(version)})"
puts "Compiled: #{is_compiled} (Type: #{typeof(is_compiled)})"