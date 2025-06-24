# graph_analyzer.py
from collections import defaultdict
from typing import List, Dict, Set, Any, Tuple

# --- Qodo Experiment 5: Code Explanation ---
# Objective: Evaluate Qodo's ability to understand and articulate complex code logic.
# Steps:
# 1. Open this file in your IDE.
# 2. Select the entire 'find_shortest_path_bfs' function (from its 'def' line to its 'return' line).
# 3. Right-click the selected code and look for a Qodo option like "Explain Code",
#    "Code Analysis", or "Ask Qodo about selection". Click it.
# 4. A Qodo panel or chat window should open with a detailed explanation of the function's
#    purpose, parameters, internal logic flow (like BFS traversal), and return value.
# 5. (Optional) In the Qodo chat, ask a follow-up question, e.g., "What is the time complexity
#    of this algorithm?" to see if it can provide algorithmic analysis.
# -------------------------------------------

def find_shortest_path_bfs(graph: Dict[Any, List[Any]], start: Any, end: Any) -> List[Any]:
    """
    Finds the shortest path between two nodes in an unweighted graph using Breadth-First Search (BFS).

    Args:
        graph: A dictionary representing the graph (adjacency list).
        start: The starting node.
        end: The target node.

    Returns:
        A list representing the shortest path from start to end, or an empty list if no path exists.
    """
    queue = [(start, [start])]
    visited: Set[Any] = set()

    while queue:
        current_node, path = queue.pop(0) # Pop from left for BFS

        if current_node == end:
            return path

        if current_node not in visited:
            visited.add(current_node)
            for neighbor in graph.get(current_node, []):
                if neighbor not in visited:
                    new_path = list(path)
                    new_path.append(neighbor)
                    queue.append((neighbor, new_path))
    return []

# Example Graph:
# A --- B
# |     |
# C --- D
#     /
#    E
example_graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D'],
    'C': ['A', 'D'],
    'D': ['B', 'C', 'E'],
    'E': ['D']
}

print(find_shortest_path_bfs(example_graph, 'A', 'E'))