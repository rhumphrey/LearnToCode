/*
This code snippet creates a list of numbers and sorts them using a custom sorting object. 
The wait function simulates a delay, and the sorted numbers are printed to the console one by one.
The main idea behind the following code is to send an asynchronous message to an object. 
So, we define the Sorter type that has the sort method. 
To send the message (sort in our case) asynchronously, prefix it with @@. 
*/

// Initialize a list of numbers to be sorted
dataList := list(10, 4, 6, 78, 12, 72, 34, 94, 35, 2, 1, 9, 16)

// Create a prototype object 'Sorter' with a 'sort' method
Sorter := Object clone do(
    // Define the 'sort' method which takes a number as an argument
    sort := method(number, 
        // Simulate a delay based on the number (divided by 10)
        wait(number/10);
        // Print the number to the console
        number println
    )
)

// Define a method 'sortThis' to sort individual numbers
sortThis := method(number,
    // Clone the 'Sorter' object to create a new sorter instance
    aSorter := Sorter clone;
    // Call the 'sort' method on the cloned sorter with the current number
    aSorter @@sort(number)
)

// Iterate over each number in 'dataList' and sort them using 'sortThis'
dataList foreach(number, sortThis(number))

wait(2)
