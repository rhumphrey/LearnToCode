### WIP ###
# This problem was asked by Google.
# Given the head of a singly linked list, swap every two nodes and return its head.
# For example, given 1 -> 2 -> 3 -> 4, return 2 -> 1 -> 4 -> 3.

class ListNode
  attr_accessor :val, :next
  def initialize(val = 0, _next = nil)
    @val = val
    @next = _next
  end
end
  
def swap_pairs(head)
  return head if head.nil? || head.next.nil?

  # Initialize the new head to the second node
  new_head = head.next

  # Swap the first two nodes
  head.next = swap_pairs(head.next.next)
  new_head.next = head
  
  new_head
end
  
# Helper method to create a linked list from an array of values
def create_linked_list(arr)
  return nil if arr.empty?
  head = ListNode.new(arr.first)
  current = head
  arr[1..].each do |val|
    current.next = ListNode.new(val)
    current = current.next
  end
  head
 end
  
# Helper method to print the linked list
def print_linked_list(head)
  current = head
  while current
    print "#{current.val} -> "
    current = current.next
  end
  print "nil\n"
end
  
# Example usage:
head = create_linked_list([1, 2, 3, 4])
print_linked_list(head) # Before: 1 -> 2 -> 3 -> 4 -> nil
head = swap_pairs(head)
print_linked_list(head) # After: 2 -> 1 -> 4 -> 3 -> nil
  
