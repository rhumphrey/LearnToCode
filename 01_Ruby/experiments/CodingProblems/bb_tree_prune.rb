# This question was asked by BufferBox.
# Given a binary tree where all nodes are either 0 or 1, prune the tree so that subtrees containing 
# all 0s are removed.
# To solve this problem, we can use a post-order traversal of the binary tree. 
# Recursively check each node and if a node is a leaf with a value of 0, remove it. 

class TreeNode
  attr_accessor :val, :left, :right
  def initialize(val = 0, left = nil, right = nil)
    @val = val
    @left = left
    @right = right
  end
end
  
def prune_tree(root)
  return nil if root.nil?
  root.left = prune_tree(root.left)
  root.right = prune_tree(root.right)
  return nil if root.val == 0 && root.left.nil? && root.right.nil?
  root
end
  
# Helper method to print the tree - for testing purposes
def print_tree(node, prefix = '', is_left = true)
  print_tree(node.right, "#{prefix}#{is_left ? '│   ' : '    '}", false) if node.right
  puts "#{prefix}#{is_left ? '└── ' : '┌── '}#{node.val}"
  print_tree(node.left, "#{prefix}#{is_left ? '    ' : '│   '}", true) if node.left
end
  
# Example usage:
# Construct the initial tree
root = TreeNode.new(0)
root.left = TreeNode.new(1)
root.right = TreeNode.new(0)
root.right.left = TreeNode.new(1)
root.right.right = TreeNode.new(0)
root.right.left.left = TreeNode.new(0)
root.right.left.right = TreeNode.new(0)
  
# Prune the tree
pruned_root = prune_tree(root)
  
# Print the pruned tree
print_tree(pruned_root)  