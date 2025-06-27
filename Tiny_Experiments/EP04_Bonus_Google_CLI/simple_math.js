function calculateArea(radius) {
    const PI = 3.14159;
    return PI * radius * radius;
}

let circleRadius = 5;
let area = calculateArea(circleRadius);
console.log(`The area of the circle with radius ${circleRadius} is ${area}`);