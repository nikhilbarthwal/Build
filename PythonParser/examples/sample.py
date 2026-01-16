def greet(name):
    """Say hello to someone."""
    print(f"Hello, {name}!")

def fibonacci(n):
    """Calculate the nth Fibonacci number."""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

class Calculator:
    """A simple calculator class."""
    
    def __init__(self):
        self.result = 0
    
    def add(self, x, y):
        self.result = x + y
        return self.result
    
    def subtract(self, x, y):
        self.result = x - y
        return self.result

if __name__ == "__main__":
    greet("World")
    
    for i in range(10):
        print(f"fib({i}) = {fibonacci(i)}")
    
    calc = Calculator()
    print(f"5 + 3 = {calc.add(5, 3)}")
    print(f"10 - 4 = {calc.subtract(10, 4)}")
