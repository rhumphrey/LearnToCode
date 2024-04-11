/*
This problem was asked by Facebook. [Medium]
A graph is minimally-connected if it is connected and there is no edge that can be removed while still leaving the graph connected. 
For example, any binary tree is minimally-connected.
Given an undirected graph, check if the graph is minimally-connected. 
You can choose to represent the graph as either an adjacency matrix or adjacency list.
*/

// Define a prototype for a Graph
Graph := Object clone do(
    vertices := Map clone
    addVertex := method(vertex, vertices atPut(vertex, List clone))
    addEdge := method(v1, v2,
        if(vertices at(v1) isNil, addVertex(v1))
        if(vertices at(v2) isNil, addVertex(v2))
        vertices at(v1) append(v2)
        vertices at(v2) append(v1)
    )
    isConnected := method(
        // Implement a Depth-First Search (DFS) to check connectivity
        visited := Map clone
        stack := List clone append(vertices keys first)

        while(stack size > 0,
            current := stack pop
            if(visited at(current) isNil,
                visited atPut(current, true)
                vertices at(current) foreach(adjacent,
                    if(visited at(adjacent) isNil, stack append(adjacent))
                )
            )
        )
        return visited size == vertices size
    )
    isMinimallyConnected := method(
        // Check if the graph is connected
        if(self isConnected not, return false)

        // Count the edges
        edgeCount := 0
        vertices foreach(node, adjacents,
            edgeCount = edgeCount + adjacents size
        )

        // Since each edge is counted twice, divide by 2
        edgeCount = edgeCount / 2

        // Check if the number of edges is equal to the number of nodes minus 1
        return edgeCount == (vertices size) - 1
    )
)


/*
Testing the isMinimallyConnected method
*/

// Testing a graph that is not minimally connected
// Create a new graph instance
myGraph := Graph clone

// Add vertices
myGraph addVertex("A")
myGraph addVertex("B")
myGraph addVertex("C")
myGraph addVertex("D")

// Add edges
myGraph addEdge("A", "B")
myGraph addEdge("B", "C")
myGraph addEdge("C", "D")
myGraph addEdge("D", "A")
myGraph addEdge("A", "C") // Adding this edge will make the graph not minimally connected

// Check if the graph is minimally connected
myGraph isMinimallyConnected println

// Testing a graph that is minimally connected
// Create a new graph instance
myGraph2 := Graph clone

// Add vertices
myGraph2 addVertex("A")
myGraph2 addVertex("B")
myGraph2 addVertex("C")
myGraph2 addVertex("D")

// Add edges to form a tree structure
myGraph2 addEdge("A", "B")
myGraph2 addEdge("B", "C")
myGraph2 addEdge("C", "D")
// Do not add any edges that would form a cycle

// Check if the graph is minimally connected
myGraph2 isMinimallyConnected println
