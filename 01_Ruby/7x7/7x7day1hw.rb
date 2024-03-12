# day 1 Self-Study (Homework)
# Find:
#   1. The Ruby API - https://rubyapi.org/
#   2. The free online version of Programming Ruby: The Pragmatic Programmer's Guide - https://ruby-doc.com/docs/ProgrammingRuby/
#   3. A method that substitutes part of a string - https://ruby-doc.org/3.3.0/Kernel.html#method-i-sub
#   4. Information about Ruby's regular expressions - https://ruby-doc.org/3.3.0/Regexp.html
#   5. Information about Ruby's ranges - https://ruby-doc.org/3.3.0/Range.html
# Do:
#   1. Print the string "Hello, world"
#   2. For the string "Hello, Ruby", find the index of the word "Ruby"
#   3. Print your name ten times
#   4. Print the string "This is sentence number 1" where the number 1 changes from 1 to 10
#   5. Run a Ruby program from a file
#   6. Bonus problem - Write a program that picks a random number. Let a player guess the number, telling the player whether the guess is too low or too high


# 1. Print the string "Hello, world"
puts 'Hello, world'


# 2. For the string "Hello, Ruby", find the index of the word "Ruby"
str = "Hello, Ruby"
word = "Ruby"
puts str.index(word)


# 3. Print your name ten times
# using .times
name = "Teddy McTedface"
10.times { puts "#{name}"}

# using for
for i in 1..10
    puts name
end

# using while
count = 0
while count < 10
    puts name
    count += 1
end

# using until
count = 0
until count == 10
    puts name
    count += 1
end


# 4. Print the string "This is sentence number 1" where the number 1 changes from 1 to 10
# using for
string = "This is sentence number"
for i in 1..10
    puts string + ' ' + i.to_s
end

# using while
count = 1
while count < 11
    puts string + ' ' + count.to_s
    count += 1
end

# using until
count = 1
until count == 11
    puts string + ' ' + count.to_s
    count += 1
end

# using .times, do and interpolation
10.times do |i|
    puts "This is sentence number #{i + 1}"
end


# 5. Run a Ruby program from a file
# >> ruby day1hw.rb 


# 6. Bonus problem - Write a program that picks a random number. Let a player guess the number, telling the player whether the guess is too low or too high
# simple version
number = rand(1..10)
puts "Enter your guess (1-10):"
guess = gets.to_i
if guess == number
    puts 'You guessed correct'
elsif guess < number
    puts 'You guessed too low'
else
    puts 'You guessed too high'
end

# improved version that allows guesses until correct (keeping track of tries up to a maximum of 10)
number = rand(1..100)
tries = 0
guessed = false
while tries < 10 && !guessed
    puts "Enter your guess (1-100):"
    guess = gets.to_i
    tries += 1
    puts 'try ' + tries.to_s

    if guess == number
        guessed = true
        puts "You guessed the number in #{tries} tries!"
    elsif guess < number
        puts "Your guess is too low."
    elsif guess > number
        puts "Your guess is too low."
    else 
        puts "condition check error (==, < or > not found)!"
    end
end
if !guessed
    puts "You have had #{tries} tries - you have run out of tries!"   
end
