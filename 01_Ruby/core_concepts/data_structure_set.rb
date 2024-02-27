# Set: A set is a collection of unordered, unique elements that can be of any type. 
# You can use sets to store sets of items, check for membership, perform set operations, and eliminate duplicates.

# Create a set of colors
colors = Set.new(["red", "green", "blue"])
puts colors

# Check if an element is in the set
puts colors.include?("yellow") # => false

# Add a new element to the set
colors.add("yellow")
puts colors

# Remove an element from the set
colors.delete("red")
puts colors

# Perform set operations
puts colors.union(Set.new(["pink", "purple"]))           # => Set: {"green", "blue", "yellow", "pink", "purple"}>
puts colors
puts colors.intersection(Set.new(["green", "orange"]))   # => Set: {"green"}>
puts colors
puts colors.difference(Set.new(["blue", "yellow"]))      # => Set: {"green"}>
puts colors