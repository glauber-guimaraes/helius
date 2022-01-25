# Helius Language
**WARNING!** This language is a learning experience in Rust and should not be used. 

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

## Implemented so far
```
program ::= (<statement> | <newline>)* EOF
statement ::= (<assignment>  |
               <funccall>    |
               <loop>        |
               <conditional> |
               <return>      |
               "continue"    |
               "break") "\n"
loop ::= while <expression> do ((<statement> | <newline>)* | end)
conditional ::= if <expression> then (<statement> | <newline>)* (else <conditional>)? (else (<statement> | <newline>)*)?
assignment ::= <identifier> "=" <expression>
funccall ::= <identifier> (<string_literal> | "(" <expression> {"," <expression>} ")")
funcdefinition ::= function "(" (<identifier> (, <identifier>)*)? ")" (<statement> | <newline>)*
expression ::= <unary_op> <expression> | <funcdefinition> |
                <identifier> | <number> | <string> | <boolean> | <none> |
                <funccall> | <expression> <binary_op> <expression>
return ::= return (<expression> (, <expression>)*)?
unary_op ::= "-"
binary_op ::= "+" | "-" | "*" | "/" | ">" | "<" | ">=" | "<=" | "==" | "!=" 
```