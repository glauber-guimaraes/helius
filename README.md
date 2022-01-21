# Helius Language
**WARNING!** This language lacks most useful features like `if` and `while` statements and is a work in progress. 

## Expected features
```lua
# Single line comments

# Variables
variable = 3

# loops
while variable < 3 do
    print(variable)
    variable = variable - 1
end

# conditionals
if variable > 0 do
    print("Hello world")
end

# functions
function doStuff(argA, argB) do
    print(argA + " " + argB)
end

# arrays
myArray = []
myArray[0] = 5
myArray[5] = 0

# structure or map
myStruct = {}
myStruct.a = 3
```