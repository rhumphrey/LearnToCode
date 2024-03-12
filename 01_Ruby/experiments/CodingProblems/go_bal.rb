# This problem was asked by Google.
# You're given a string consisting solely of (, ), and *. * can represent either a (, ), or an empty string. Determine whether the parentheses are balanced.
# For example, (()* and (*) are balanced. )*( is not balanced.

# This method checks if a string s contains balanced parentheses. It uses two variables low and high to keep track of the 
# minimum and maximum possible number of open parentheses at any point in the string. 
# It iterates over each character of the string and updates the values of low and high according to the following rules:
# If the character is an open parenthesis (, then both low and high are incremented by 1, since it adds one more open parenthesis to the string.
# If the character is a close parenthesis ), then low is decremented by 1 if it is positive, since it can match with a previous open parenthesis. 
# high is also decremented by 1, since it reduces the number of open parentheses by 1.
# If the character is any other symbol, then low is decremented by 1 if it is positive, since it can be considered as a close parenthesis that 
# matches with a previous open parenthesis. high is incremented by 1, since it can be considered as an open parenthesis that adds more flexibility to the string.
# The method returns false if high becomes negative at any point, since that means there are more close parentheses than open parentheses in the string. 
# It returns true if low becomes zero at the end, since that means the string is balanced. Otherwise, it returns false.

def balanced?(s)
  low = 0
  high = 0
  s.each_char do |char|
    if char == '('
      low += 1
      high += 1
    elsif char == ')'
      if low > 0
        low -= 1
      end
      high -= 1
    else
      if low > 0
        low -= 1
      end
      high += 1
    end
    if high < 0
      return false
    end
  end
  return low == 0
end

# Test some examples
puts balanced?("()*")     # true
puts balanced?("(*)")     # true
puts balanced?(")*(")     # false
puts balanced?("*()")     # true 
puts balanced?("(*))")    # true
puts balanced?("*(*")     # true  
puts balanced?("(()*")    # true 
puts balanced?("())*")    # false 