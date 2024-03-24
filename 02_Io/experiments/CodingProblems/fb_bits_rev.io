// This problem was asked by Facebook.
// Given a 32-bit integer, return the number with its bits reversed.
// For example, given the binary number 1111 0000 1111 0000 1111 0000 1111 0000, return 0000 1111 0000 1111 0000 1111 0000 1111.


reverseBits := method(n,
  // Convert the integer to a binary string
  binaryString := n asBinary

  // Ensure the binary string is 32 bits long
  binaryString = binaryString padStart(32 - binaryString size, "0")

  // Reverse the binary string
  reversedBinaryString := binaryString reverse

  // Convert the reversed binary string back to an integer
  result := Integer fromBinary(reversedBinaryString)
  result
)

// Example usage:
n := 0xF0F0F0F0  // This is the hexadecimal representation of 1111 0000 1111 0000 1111 0000 1111 0000
reversed := reverseBits(n)
