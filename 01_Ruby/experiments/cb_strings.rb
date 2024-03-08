# Working through some examples in the O'Reilly Ruby Cookbook and changing any as appropriate

# 1.1 Bulding a string from a hash data structure
# You want to iterate over a data structure, building a string from it as you do.
books = { fiction: 12, nonfiction: 8, poetry: 4 }
output = ""
# Iterate over each key-value pair in the hash
books.each_pair do |genre, count|
  output << "#{genre.capitalize} books: #{count}\n"   # Append the genre and count to the output string
end
puts output
puts

# 1.2 Substituting Variables into Strings
# Note: When you define a string by putting it in double quotes, Ruby scans it for special substitution codes.
number = 10
puts "The number is #{number}."                       
puts "The number is #{5}."                            
puts "The number after #{number} is #{number.next}."  
puts "The number prior to #{number} is #{number-1}."  
puts "We're ##{number}!"  
puts                            

# 1.3 Substituting Variables into an Existing String
# You want to create a string that contains Rubyexpressions or variable substitutions, 
# without actually performing the substitutions.
template = 'My favourite programming language today is %s.'
puts template % 'Ruby'                     
puts template % 'Python'                    
puts 'To 2 decimal places: %.2f' % Math::PI   
puts 'Zero-padded: %.5d' % Math::PI           # => "Zero-padded: 00003"
puts

# 1.4 Reversing a String by Words or Characters
# The letters (or words) of your string are in the wrong order.
s = ".sdrawkcab si gnirts sihT"
puts s.reverse                                
puts s                                        
s.reverse!                                    
puts s           
puts                             

# 1.5 Representing Unprintable Characters
# You need to make reference to a control character, a strange UTF-8 character, 
# or some other character that’s not on your keyboard.
octal = "\000\001\010\020"
octal.each_byte { |x| puts x }
hexadecimal = "\x00\x01\x10\x20"
hexadecimal.each_byte { |x| puts x }
open('smiley.html', 'wb') do |f|
  f << '<meta http-equiv="Content-Type" content="text/html;charset=UTF-8">'
  f << "\xe2\x98\xBA"
end
puts "\a" == "\x07"           # => true # ASCII 0x07 = BEL (Sound system bell)
puts "\b" == "\x08"           # => true # ASCII 0x08 = BS (Backspace)
puts "\e" == "\x1b"           # => true # ASCII 0x1B = ESC (Escape)
puts "\f" == "\x0c"           # => true # ASCII 0x0C = FF (Form feed)
puts "\n" == "\x0a"           # => true # ASCII 0x0A = LF (Newline/line feed)
puts "\r" == "\x0d"           # => true # ASCII 0x0D = CR (Carriage return)
puts "\t" == "\x09"           # => true # ASCII 0x09 = HT (Tab/horizontal tab)
puts "\v" == "\x0b"           # => true # ASCII 0x0B = VT (Vertical tab)
puts

# 1.6 You want to see the ASCII code for a character, or transform an ASCII code into a string
# To see the ASCII code for a specific character as an integer, use the #ord method
# In the first examples I'm also using the ? operator as shorthand notation for a single-character string literal
puts ?a.ord               # => 97
puts ?!.ord               # => 33
puts ?\n.ord              # => 10
# To see the integer value of a particular in a string, access it as though it were an element of an array:
puts 'a'[0].ord           # => 97
puts 'bad sound'[1].ord   # => 97
# To see the ASCII character corresponding to a given number, call its .chr method.
# This returns a string containing only one character (notice the use of inspect for characters which are not 'visible')
puts 97.chr               # => "a"
puts 33.chr               # => "!"
puts 10.chr.inspect       # => "\n"
puts 0.chr.inspect        # => "\000"
begin
  puts 256.chr            # RangeError: 256 out of char range
rescue RangeError => e
  puts "Invalid value: #{e.message}"
end
puts

# 1.7 Converting Between Strings and Symbols
# You want to get a string containing the label of a Ruby symbol, 
# or get the Ruby symbolthat corresponds to a given string
# To turn a symbol into a string, use .to_s, or .id2name, for which to_s is an alias
puts :a_symbol.to_s                         # => "a_symbol"
puts :AnotherSymbol.id2name                 # => "AnotherSymbol"
puts :"Yet another symbol!".to_s            # => "Yet another symbol!"
# You usually reference a symbol by just typing its name. If you’re given a string in code and need 
# to get the corresponding symbol, you can use .intern:
puts :dodecahedron.object_id                # ==> this should match the next example of a .object_id method call
symbol_name = "dodecahedron"
puts symbol_name.intern                     # => :dodecahedron
puts symbol_name.intern.object_id           # => this should match previous .object_id method call
puts

# 1.8 Processing a String One Character at a Time
# If you’re processing an ASCII document, then each byte corresponds to one character
# Use .each_byte to yield each byte of a string as a number, which you can turn into a one-character string
'Mr Fussysocks'.each_byte { |x| puts "#{x} = #{x.chr}" }
puts 
# Use .scan to yield each character of a string as a new one-character string
'Mr Fussysocks'.scan( /./ ) { |c| puts c }
puts

# 1.9 Processing a String One Word at a Time
# You want to split a piece of text into words, and operate on each word
# Below a method for the String class has been created
class String
  def word_count
    frequencies = Hash.new(0)
    downcase.scan(/\w+/) { |word| frequencies[word] += 1 }
    return frequencies
  end
end
# Notice that %{...} is a shorthand notation for creating a string literal I could just use '....'
puts %{Cats cats cat cat cats.}.word_count      
puts 'Cats cats cat cat cats.'.word_count      
puts %{To be, or not to be, that is the question}.word_count   
puts
# This simpler class method would also work by using the tally method
class String
  def word_count
    downcase.scan(/\w+/).tally
  end
end
puts %{Cats cats cat cat cats.}.word_count      
puts 'Cats cats cat cat cats.'.word_count      
puts %{To be, or not to be, that is the question}.word_count  
puts

# 1.10 Changing the Case of a String
# Your string is in the wrong case, or no particular case at all
# The String class provides a variety of case-shifting methods
s = 'The ROAD goes ever oN and oN, Down fRoM tHE door WHERE it BEGAN.'
puts s
puts s.upcase 
puts s.downcase 
puts s.swapcase 
puts s.capitalize 
puts
# All four methods have corresponding methods that modifya string in place rather
# than creating a new one: upcase!, downcase!, swapcase!, and capitalize!

# 1.11 Managing Whitespace
# Your string contains too much whitespace, not enough whitespace, or the wrong kind of whitespace
# Use strip to remove whitespace from the beginning and end of a string
s = " \tWhitespace at beginning and end. \t\n\n"
puts s
puts s.strip
# Add whitespace to one or both ends of a string with ljust, center and rjust
s = "Ever on."
puts s
puts s.ljust(15)
puts s.center(15)
puts s.rjust(15)
puts
# Use the gsub method with a string or regular expression to make more complex changes, 
# such as to replace one type of whitespace with another. For example 
# Normalize Ruby source code by replacing tabs with spaces
# rubyCode.gsub("\t", " ")
# Transform Windows-style newlines to Unix-style newlines
# "Line one\n\rLine two\n\r".gsub(\n\r", "\n")
# Transform all runs of whitespace into a single space character
# "\n\rThis string\t\t\tuses\n all\tsorts\nof whitespace.".gsub(/\s+/," ")

# 1.12 Testing Whether an Object Is String-Like
# You want to see whether you can treat an object as a string
# Check whether the object defines the .to_str method
puts 'A string'.respond_to? :to_str           # => true
puts Exception.new.respond_to? :to_str        # => true
puts 4.respond_to? :to_str                    # => false
# More generally, check whether the object defines the specific method of String you’re thinking about calling
def join_to_successor(s)
  begin
    raise ArgumentError, 'No successor method!' unless s.respond_to? :succ
    "#{s}#{s.succ}"
  rescue ArgumentError => e
    "An error occurred: #{e.message}"    
  end
end
puts join_to_successor('a')                       # => "ab"
puts join_to_successor(4)                         # => "45"
puts join_to_successor(4.01)                      # ArgumentError: No successor method!
puts

# 1.13 Getting the Parts of a String You Want
# You want only certain pieces of a string
# To get a substring of a string, call its slice method, or use the arrayindex operator (that is, call the [] method)
s = "Be not afraid of greatness. Some are born great, some achieve greatness, and some have greatness thrust upon 'em."
puts s.slice(17,9)
puts s[3,3] 
puts s[0,2] 
puts s[37, 4] 
puts s[14, 12] 
# To get the first portion of a string that matches a regular expression, pass the regular expression into slice or []
puts s[/.*aid/]
puts s[/up.*/]
puts

# 1.14 Handling International Encodings
# Note: Not necessary in 1.9 and later 
# You need to handle strings that contain nonASCII characters: probably Unicode characters encoded in UTF-8
# To use Unicode in Ruby, simply add the following to the beginning of code. 
# $KCODE='u'
# require 'jcode'

# 1.15 Word-Wrapping Lines of Text
# You want to turn a string full of miscellaneous whitespace into a string formatted
# with linebreaks at appropriate intervals, so that the text can be displayed in a window
# or sent as an email.
# use a regular expression like the following
def wrap(s, width=78)
  s.gsub(/(.{1,#{width}})(\s+|\Z)/, "\\1\n")
end
puts wrap("This text is too short to be wrapped.")
puts wrap("This text is not too short to be wrapped.", 20)
puts wrap("These ten-character columns are maybe a little to short for my liking!", 10)
puts

# 1.16 Generating a Succession of Strings
# You want to iterate over a series of alphabetically-increasing strings as you would over a series of numbers
# If you know both the start and end points of your succession, you can simply create a range and use .each, as you would for numbers:
('aa'..'ag').each { |x| puts x }
# The method that generates the successor of a given string is .succ. If you don’t
# know the end point of your succession, you can define a generator that uses succ,
# and break from the generator when you’re done.
def endless_string_succession(start)
  while true
    yield start
    start = start.succ
  end
end
# This code iterates over an endless succession of strings, stopping when the last two letters are the same
endless_string_succession('fol') do |x|
  puts x
  break if x[-1] == x[-2]
end
puts

# 1.17 Matching Strings with Regular Expressions
# You want to know whether or not a string matches a certain pattern.
# You can usually describe the pattern as a regular expression. The =~ operator tests a
# string against a regular expression
string = 'This is a 30-character string.'
if string =~ /([0-9]+)-character/ and $1.to_i == string.length
  puts "Yes, there are #$1 characters in that string."
end
# You can also use .match:
match = Regexp.compile('([0-9]+)-character').match(string)
if match && match[1].to_i == string.length
  puts "Yes, there are #{match[1]} characters in that string."
end
# You can check a string against a series of regular expressions with a case statement:
string = "123"
case string
when /^[a-zA-Z]+$/
  puts "Letters"
when /^[0-9]+$/
  puts "Numbers"
else
  puts "Mixed"
end
puts

# 1.18 Replacing Multiple Patterns in a Single Pass
# You want to perform multiple, simultaneous search-and-replace operations on a string.
# Use the Regexp.union method to aggregate the regular expressions you want to match 
# into one big regular expression that matches anyof them.
class String
  def mgsub(key_value_pairs=[].freeze)
    regexp_fragments = key_value_pairs.collect { |k,v| k }
    gsub(Regexp.union(*regexp_fragments)) do |match|
      key_value_pairs.detect{|k,v| k =~ match}[1]
    end
  end
end
puts "GO HOME!".mgsub([[/.*GO/i, 'Home'], [/home/i, 'is where the heart is']])
puts "Here is number #123".mgsub([[/[a-z]/i, '#'], [/#/, 'P']])

# 1.19 Validating an Email Address
# You need to see whether an email address is valid
test_addresses = [                              # The following are valid addresses according to RFC822.
'joe@example.com', 'joe.bloggs@mail.example.com',
'joe+ruby-mail@example.com', 'joe(and-mary)@example.museum',
'joe@localhost',
'joe', 'joe@', '@example.com',                  # These and the ones following are invalid
'joe@example@example.com',
'joe and mary@example.com' ]
valid = '[^ @]+'                                 # Exclude characters always invalid in email addresses
username_and_machine = /^#{valid}@#{valid}$/
print test_addresses.collect { |i| i =~ username_and_machine }   # checking for ill-formed addresses
puts
username_and_machine_with_tld = /^#{valid}@#{valid}\.#{valid}$/
print test_addresses.collect { |i| i =~ username_and_machine_with_tld } # + checking the use of local-network addresses
puts
puts

# 1.20 Classifying Text with a Bayesian Analyzer
# You want to classifychunks of text byexample: an email message is either spam or not spam, 
# a joke is either funny or not funny, and so on.
# Use Lucas Carlson’s Classifier library, available as the classifier gem.
# remember to run - gem install classifier at a command prompt if you dont have the gem
# you will also probably need to do this - gem install fast-stemmer
require 'rubygems'
require 'classifier'
classifier = Classifier::Bayes.new('Spam', 'Not spam')
classifier.train_spam 'are you in the market for viagra? we sell viagra'
classifier.train_not_spam 'hi there, are we still on for lunch?'
puts classifier.classify "we sell the cheapest viagra on the market"
puts classifier.classify "lunch sounds great"
puts