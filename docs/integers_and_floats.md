# Integers and Floats

## Integers
Pau has 10 main integer types. But you mostly only ever need `int` or `uint`
```pau
int /: platform signed integer
uint /: platform unsigned integer
i8 i16 i32 i64
u8 u16 u32 u64
```

To write an integer, first, write the literal, then the type suffix
```pau
10
/: explicit int
10i
/: u8
10u8
```

Here's a table of the suffixes:
| Type   | Suffix    | Is Optional? |
| ------ | --------- | ------------ |
| `int`  | +`i`      | ✅          |
| `uint` | +`u`      | ❌          |
| `i8`   | +`i8`     | ❌          |
| `i16`  | +`i16`    | ❌          |
| `i32`  | +`i32`    | ❌          |
| `i64`  | +`i64`    | ❌          |
| `u8`   | +`u8`     | ❌          |
| `u16`  | +`u16`    | ❌          |
| `u32`  | +`u32`    | ❌          |
| `u64`  | +`u64`    | ❌          |

## Floats
Pau has 3 main float types
```pau
float /: platform float
f32
f64
```

Similarly to integers, when writing a float, first write the literal, then the type suffix
```pau
10.5
/: f32
10.5f32
```

Here's a table of the suffixes:
| Type    | Suffix    | Is Optional? |
| ------- | --------- | ------------ |
| `float` | (none)    | ✅          |
| `f32`   | +`f32`    | ❌          |
| `f64`   | +`f64`    | ❌          |

## Examples
```pau
let a = 10;           /: int (default)
let b = 10u;          /: uint
let c = 255u8;        /: u8 (max value)
let d = 3.14;         /: float (default)
let e = 3.14f32;      /: f32
let f = 1000000i64;   /: i64 for big numbers

def addI8(x: i8, y: i8): i8
    x + y
end

addI8(100i8, 27i8);