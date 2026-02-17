# Conditionals
You can make a condition in Pau with the `if`, `then`, `else` and `end` keywords.
```pau
if <condition> then <then body> (else <else body>) end
```
Unlike C or C++, the condition must be a [boolean](booleans.md).
The inner of the bodies are scoped, meaning that local variables defined in the bodies will be invalid by the end.

## Examples
```
var x = if true then 5 else 6 end; /: guarenteed to be 5
let z = 15;

if x == 5 then
    let y = 10;
    x := y;
else /: y is invalid here
    x := z;
end /: z is still visible after this

let r = y; /: uhh, never heard of this guy
let r = z; /: OK!