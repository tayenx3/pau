# `while` Loops
`while` loop bodies are defined with `do` and `end`
```pau
while <condition> do <body> end
```
Like [conditionals](conditionals.md), `while` loop conditions expect `bool`

## Example
```pau
var counter = 0;
let start_value = counter;

while counter < 10 do
    counter := counter + 1;
end

let end_value = counter;

start_value != end_value /: true!
start_value == 0 /: true!
end_value == 10 /: true!
start_value == end_value /: nuh uh
```