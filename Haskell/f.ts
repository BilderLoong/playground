function fibonacci(x: number): number {
  if (x <= 1) {
    return x;
  }
  
  return fibonacci(x - 2) + fibonacci(x - 1);
}

console.log(fibonacci(30))
