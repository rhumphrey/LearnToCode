// =============================================================================
// C++23 Language Features - Self-Contained Examples
// Compile with: g++ -std=c++23 -Wall -Wextra -O2 cpp23_examples.cpp -o cpp23_examples.exe
// Run with: ./cpp23_examples.exe
// =============================================================================

#include <iostream>
#include <string>
#include <vector>
#include <array>
#include <type_traits>
#include <utility>
#include <stdfloat>


// =============================================================================
// 1. if consteval - Conditional compilation based on constant evaluation
// =============================================================================

constexpr int get_value() {
    // Different behavior at compile-time vs runtime
    if consteval {
        // This branch executes during constant evaluation (compile-time)
        return 42;
    } else {
        // This branch executes at runtime
        return 24;
    }
}

void demo_if_consteval() {
    std::cout << "\n=== if consteval Demo ===\n";
    
    // At compile-time, this will be 42
    constexpr int compile_time = get_value();
    
    // At runtime, this will be 24
    int runtime = get_value();
    
    std::cout << "Compile-time value: " << compile_time << "\n";
    std::cout << "Runtime value: " << runtime << "\n";
}

// =============================================================================
// 2. Multidimensional subscript operator
// =============================================================================

template<typename T, size_t Rows, size_t Cols>
class Matrix {
private:
    std::array<std::array<T, Cols>, Rows> data{};

public:
    // C++23: Can now use multiple arguments in operator[]
    constexpr T& operator[](size_t row, size_t col) {
        return data[row][col];
    }
    
    constexpr const T& operator[](size_t row, size_t col) const {
        return data[row][col];
    }
    
    // Traditional single-argument version still works
    constexpr auto& operator[](size_t row) {
        return data[row];
    }
};

void demo_multidimensional_subscript() {
    std::cout << "\n=== Multidimensional Subscript Demo ===\n";
    
    Matrix<int, 3, 3> matrix;
    
    // C++23: Clean syntax with multiple indices
    matrix[1, 2] = 42;
    matrix[0, 0] = 10;
    matrix[2, 1] = 99;
    
    std::cout << "matrix[1, 2] = " << matrix[1, 2] << "\n";
    std::cout << "matrix[0, 0] = " << matrix[0, 0] << "\n";
    std::cout << "matrix[2, 1] = " << matrix[2, 1] << "\n";
    
    // Traditional syntax still works
    std::cout << "matrix[1][2] = " << matrix[1][2] << "\n";
}

// =============================================================================
// 3. auto(x): decay-copy in the language
// =============================================================================

template<typename T>
void process_value(T&& value) {
    std::cout << "\n=== auto(x) decay-copy Demo ===\n";
    
    // Before C++23: std::decay_t<T> copy = std::forward<T>(value);
    // C++23: Much cleaner syntax for decay-copy
    auto copy = auto(value);  // Forces copy and decay
    
    std::cout << "Original type: " << typeid(T).name() << "\n";
    std::cout << "Decayed copy type: " << typeid(decltype(copy)).name() << "\n";
    
    // Modify the copy (won't affect original)
    if constexpr (std::is_same_v<std::decay_t<T>, std::string>) {
        copy += " (modified)";
        std::cout << "Modified copy: " << copy << "\n";
    }
}

void demo_auto_decay_copy() {
    std::string original = "Hello, World!";
    const std::string& ref = original;
    
    process_value(ref);  // auto(ref) creates a non-const copy
}

// =============================================================================
// 4. [[assume]] attribute for optimization hints
// =============================================================================

int optimized_division(int a, int b) {
    // Tell the compiler that b is never zero
    // This allows for better optimization without undefined behavior
    [[assume(b != 0)]];
    
    // The compiler can now optimize this division
    return a / b;
}

void demo_assume_attribute() {
    std::cout << "\n=== [[assume]] Attribute Demo ===\n";
    
    int x = 100;
    int y = 5;
    
    // We know y is not zero, so we can use the optimized version
    int result = optimized_division(x, y);
    std::cout << x << " / " << y << " = " << result << "\n";
    
    std::cout << "Note: The [[assume]] attribute helps the compiler optimize\n";
    std::cout << "without causing undefined behavior if the assumption is wrong.\n";
}

// =============================================================================
// 5. size_t literal suffixes
// =============================================================================

void demo_size_t_literals() {
    std::cout << "\n=== size_t Literal Suffixes Demo ===\n";
    
    // C++23: Direct size_t literals
    auto size1 = 42uz;   // unsigned size_t
    auto size2 = 100z;   // signed size_t (ptrdiff_t)
    
    std::vector<int> vec(size1);  // No need for casting
    
    std::cout << "size1 type: " << typeid(size1).name() << ", value: " << size1 << "\n";
    std::cout << "size2 type: " << typeid(size2).name() << ", value: " << size2 << "\n";
    std::cout << "Vector size: " << vec.size() << "\n";
    
    // Useful in template contexts where size_t is expected
    static_assert(std::is_same_v<decltype(42uz), std::size_t>);
}

// =============================================================================
// 6. Static operator() and operator[]
// =============================================================================

struct MathOperations {
    // C++23: Static call operator - no object state needed
    static int operator()(int a, int b) {
        return a + b;
    }
    
    // C++23: Static subscript operator
    static const char* operator[](int index) {
        static const char* names[] = {"zero", "one", "two", "three"};
        return (index >= 0 && index < 4) ? names[index] : "unknown";
    }
};

void demo_static_operators() {
    std::cout << "\n=== Static Operators Demo ===\n";
    
    MathOperations ops;  // Object created but operators don't need it
    
    // Static operators can be called on objects
    std::cout << "ops(5, 3) = " << ops(5, 3) << "\n";
    std::cout << "ops[2] = " << ops[2] << "\n";
    
    // Or even on temporary objects (since they're static)
    std::cout << "MathOperations{}(10, 20) = " << MathOperations{}(10, 20) << "\n";
    std::cout << "MathOperations{}[1] = " << MathOperations{}[1] << "\n";
}

// =============================================================================
// 7. Deducing this - Explicit object parameter
// =============================================================================

class FlexibleContainer {
private:
    std::vector<int> data;

public:
    FlexibleContainer(std::initializer_list<int> init) : data(init) {}
    
    // C++23: Deducing this eliminates the need for separate const/non-const overloads
    template<typename Self>
    auto&& get_data(this Self&& self) {
        // The compiler automatically deduces the correct return type
        // and const-ness based on how this function is called
        return std::forward<Self>(self).data;
    }
    
    // Another example: automatic forwarding
    template<typename Self>
    auto&& front(this Self&& self) {
        return std::forward<Self>(self).data.front();
    }
};

void demo_deducing_this() {
    std::cout << "\n=== Deducing this Demo ===\n";
    
    FlexibleContainer container{1, 2, 3, 4, 5};
    const FlexibleContainer const_container{10, 20, 30};
    
    // Same function works for both const and non-const objects
    auto& data1 = container.get_data();       // Returns vector<int>&
    const auto& data2 = const_container.get_data(); // Returns const vector<int>&
    
    std::cout << "Mutable container size: " << data1.size() << "\n";
    std::cout << "Const container size: " << data2.size() << "\n";
    
    // Modify through non-const reference
    container.front() = 99;
    std::cout << "Modified first element: " << container.front() << "\n";
    
    std::cout << "Note: One template function handles all reference types!\n";
}

// =============================================================================
// 8. Extended floating-point types (if supported)
// =============================================================================

void demo_extended_float_types() {
    std::cout << "\n=== Extended Floating-Point Types Demo ===\n";
    
    // Note: These types may not be available on all implementations
    #ifdef __STDCPP_FLOAT16_T__
    std::float16_t half_precision = 3.14f16;
    std::cout << "float16_t value: " << static_cast<float>(half_precision) << "\n";
    #else
    std::cout << "float16_t not available on this implementation\n";
    #endif
    
    #ifdef __STDCPP_BFLOAT16_T__
    std::bfloat16_t brain_float = 2.718bf16;
    std::cout << "bfloat16_t value: " << static_cast<float>(brain_float) << "\n";
    #else
    std::cout << "bfloat16_t not available on this implementation\n";
    #endif
    
    std::cout << "These types are primarily useful for AI/ML workloads\n";
}

// =============================================================================
// 9. Relaxed range-for loop (demonstration concept)
// =============================================================================

// Simple example of different begin/end types
template<typename T>
struct CountingRange {
    T start, end_value;
    
    struct Iterator {
        T value;
        T operator*() const { return value; }
        Iterator& operator++() { ++value; return *this; }
        bool operator!=(const Iterator& other) const { return value != other.value; }
    };
    
    struct Sentinel {
        T end_value;
        bool operator==(const Iterator& it) const { return it.value == end_value; }
    };
    
    Iterator begin() const { return {start}; }
    Sentinel end() const { return {end_value}; }  // Different type from begin()
};

void demo_relaxed_range_for() {
    std::cout << "\n=== Relaxed Range-for Loop Demo ===\n";
    
    CountingRange<int> range{1, 6};
    
    std::cout << "Range values: ";
    for (auto value : range) {
        std::cout << value << " ";
    }
    std::cout << "\n";
    
    std::cout << "Note: begin() returns Iterator, end() returns Sentinel (different types)\n";
}

// =============================================================================
// 10. Named universal character escapes (demonstration)
// =============================================================================

void demo_named_unicode_escapes() {
    std::cout << "\n=== Named Unicode Escapes Demo ===\n";
    
    // C++23: Can use Unicode character names in string literals
    // Note: Support varies by compiler
    std::string greeting = "Hello \u0001F44B World!";  // Waving hand emoji
    std::cout << "Greeting with emoji: " << greeting << "\n";
    
    // Mathematical symbols
    std::string math = "The answer is \u03C0 \u2248 3.14159"; // π ≈
    std::cout << "Math expression: " << math << "\n";
    
    std::cout << "Note: Named escapes like \\N{WAVING HAND} may not be fully supported yet\n";
}

// =============================================================================
// Main function - Run all demonstrations
// =============================================================================

int main() {
    std::cout << "C++23 Language Features Demonstration\n";
    std::cout << "====================================\n";
    
    try {
        demo_if_consteval();
        demo_multidimensional_subscript();
        demo_auto_decay_copy();
        demo_assume_attribute();
        demo_size_t_literals();
        demo_static_operators();
        demo_deducing_this();
        demo_extended_float_types();
        demo_relaxed_range_for();
        demo_named_unicode_escapes();
        
        std::cout << "\n=== All demonstrations completed successfully! ===\n";
        std::cout << "\nNote: Some features may require specific compiler versions or flags.\n";
        std::cout << "Try compiling with: g++ -std=c++23 -Wall -Wextra -O2 examples.cpp\n";
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
    
    return 0;
}

