# This problem was asked by LinkedIn.
# Given a list of points, a central point, and an integer k, find the nearest k points 
# from the central point.
# For example, given the list of points [(0, 0), (5, 4), (3, 1)], the central point (1, 2), 
# and k = 2, return [(0, 0), (3, 1)].
def nearest_k_points(points, central, k)
  # Calculate the Euclidean distance between two points
  distance = ->(point1, point2) { Math.sqrt((point1[0] - point2[0])**2 + (point1[1] - point2[1])**2) }
    # Sort points by distance from the central point
    sorted_points = points.sort_by { |point| distance.call(point, central) }
    # Return the first k points from the sorted array
    sorted_points.first(k)
end
  
# Example usage:
points = [[0,0],[5,4],[3,1],[1,3],[5,1],[2,2]]
central_point = [1, 2]
k = 2
puts nearest_k_points(points, central_point, k).inspect