# frozen_string_literal: true

# Not known who this was asked by
# Given a 2-D matrix representing an image, a location of a pixel in the screen and a color C,
# replace the color of the given pixel and all adjacent same colored pixels with C.
# For example, given the following matrix, and location pixel of (2, 2), and 'G' for green:
# B B W
# W W W
# W W W
# B B B
# Becomes
# B B G
# G G G
# G G G
# B B B

# The FloodFill class provides a method to perform the flood fill algorithm on a matrix.
# It replaces all occurrences of a target color in a contiguous zone with a new color,
# starting from a given position (x_pos, y_pos).
#
# Example:
#   flood_fill = FloodFill.new(matrix, new_color)
#   flood_fill.call(x_pos, y_pos)
class FloodFill
  def initialize(matrix, new_color)
    @matrix = matrix
    @new_color = new_color
    @rows = matrix.length
    @cols = matrix[0].length
  end

  def call(x_pos, y_pos)
    @target_color = @matrix[x_pos][y_pos]
    fill(x_pos, y_pos)
    @matrix
  end

  private

  def fill(x_loc, y_loc)
    # Return if the pixel is out of bounds or is not the target color
    if x_loc.negative? || x_loc >= @rows || y_loc.negative? || y_loc >= @cols || @matrix[x_loc][y_loc] != @target_color
      return
    end

    # Fill the surrounding pixels
    @matrix[x_loc][y_loc] = @new_color
    fill(x_loc + 1, y_loc)
    fill(x_loc - 1, y_loc)
    fill(x_loc, y_loc + 1)
    fill(x_loc, y_loc - 1)
  end
end

# Example usage:
image_matrix = [
  %w[B B W],
  %w[W W W],
  %w[W W W],
  %w[B B B]
]

# show original matrix
print 'OG : '
print image_matrix
puts

# Usage of class and method
flood_fill = FloodFill.new(image_matrix, 'G')
flood_fill.call(1, 1)

# show matrix after flood fill
print 'FF : '
print image_matrix
puts