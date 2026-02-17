# Constants
Constants in Pau are defined with the `const` keyword.
```pau
const <name>(: <type>) = <value>
```

Unlike Rust or Zig, constants in Pau are not evaluated at compile-time. Instead, they act somewhat like macros, where the identifier is replaced with the expression.

Like this:
```pau
const MYCONSTANT: int = 10;

def main(): int
    MYCONSTANT + 8
end
```

Gets replaced with this:
```pau
def main(): int
    10 + 8
end
```