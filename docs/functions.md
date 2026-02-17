# Functions
Functions in Pau are defined with `def`.
```pau
def <name>(arg0: ty0, arg1: ty1, arg2: ty2, ...): <return type>
    <body>
end
```

Function parameters in Pau must have an explicit type annotation, unlike in Python or OCaml where parameter types are infered
```pau
def add(a, b): int /: ERROR! what's a and what's b? int? float? bool?
    a + b
end
```

Unlike Rust, and like C, functions are global and root-level. Meaning that functions can not be defined in functions.
```pau
def main(): int
    def aux(x: float): float /: ERROR!
        x + 0.5f32
    end

    aux(1.53) /: who tf you talm about?
end
```

Unlike Rust, and like C (again), the `main` function must return `int`. But unlike C, the `main` function does not take any arguments
```pau
def main(): float /: ERROR!
    0.6
end
```

## Examples
```pau
def add(a: int, b: int): int
    a + b
end

def main(): int
    let result = add(5, 3); /: should be 8

    result
end
```