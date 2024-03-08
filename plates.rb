require 'set'

def main
  print "Plate: "
  plate = gets.chomp
  if is_valid(plate)
    puts "Valid"
  else
    puts "Invalid"
  end
end

def is_valid(s)
  # vanity plates may contain a maximum of 6 characters (letters or numbers) and a minimum of 2 characters.”
  return false if s.length < 2 or s.length > 6

  # All vanity plates must start with at least two letters.
  s[0..1].each_char do |c|
    return false unless c.is_a? String and c =~ /[A-Za-z]/
  end

  # “No periods, spaces, or punctuation marks are allowed.”
  punctuation = Set.new [".", ",", "!", "?", ";", ":", "'", '"', "-", "_", "(", ")", "[", "]", "{", "}", "+", "=", "*", "/", "\\", "|", "&", "%", "$", "#", "@", "^", "~", "`"]
  s.each_char do |c|
    return false if c == " " or punctuation.include? c
  end

  # “Numbers cannot be used in the middle of a plate; they must come at the end.
  # For example, AAA222 would be an acceptable … vanity plate; AAA22A would not be acceptable. The first number used cannot be a ‘0’.”
  # Note we have alredy checked that the first two characters are letters earlier so we can start from i + 2
  (2...s.length).each do |i|
    if s[i] =~ /[0-9]/
      return false unless s[i..-1] =~ /^[1-9][0-9]*$/
      return true
    end
  end

  return true
end

main if __FILE__ == $0