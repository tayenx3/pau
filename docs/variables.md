# Variables
Variables in Pau are defined using either the `let` keyword for immutability or `var` keyword for mutability. Followed by an optional type and initial value.
```pau
let x = 12;
var y = 15;
```

Variable types must be definitive at declaration (either explicitly annotated or infered through the initial value)
```
let x; /: ERROR! what's x???
```

## Mutation
You can mutate variables in Pau with the walrus operator (`:=`)
```pau
let x = 12;
var y = 15;
x := 16; /: ERROR! x is immutable
y := 10;
```

## Multiple Declarations
You can declare multiple variables and assign them all to one value by chaining declarations and `:=` operations.
```pau
let a = let b = let c: i32; /: all are i32s

var d = var e = var f = 10; /: all are 10
d := e := f := 15; /: all are 15
```

## Examples
```pau
let myVar = 12;
var mutableVar = 15;
let totallyMutableVar = 15;

mutableVar := 16;
totallyMutableVar := 20; /: LIAR!
iDontExist := 15; /: who?

let me = let mom = let dad = Person();
let cat: Pet = me; /: type mismatch!