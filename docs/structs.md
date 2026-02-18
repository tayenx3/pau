# Structs
Structs are defined with the `def` keyword. But you might be wondering... aren't functions `def` too?
You're right! But Pau can differentiate between them.
```pau
def <name> /: no '(' = struct! not function definition!
    (: fields :)
end
```

Like Rust and C, fields must have explicit types
```pau
/: nuh uh bro, gimme those types
def Vec3
    x,
    y,
    z
end

/: that's what i'm talking about
def Vec3
    x: float,
    y: float,
    z: float
end
```

Like [functions](functions.md), structs are global. So they can't be defined in a function
```pau
def main(): int
    def Vec3 /: ERROR!
        x: float,
        y: float,
        z: float
    end

    0
end
```

Structs are constructed with `<name>(<fields>)` like a function call
```pau
def Vec3
    x: float,
    y: float,
    z: float
end

let myVec3 = Vec3(1.0, 2.0, -5.0);
```

## Circular Types
Unlike Rust, you can create self-referential types and circular types,
because structs and arrays are stored in a separate stack slot, and variables hold a pointer to them
```pau
/: automatic indirection
def Node
    value: int,   /: physically, this field is a platform integer
    next: Node,   /: physically, this field is a pointer
end

def A
    b: B,
end

def B
    c: C,
end

def C
    a: A, /: OK!
end
```

## Examples
```pau
def Vec3
    x: float,
    y: float,
    z: float
end

def Vec4
    x: float,
    y: float,
    z: float,
    w: float
end

def Transform
    pos: Vec3,
    quat: Vec4,
    scale: Vec3
end

let transform = Transform(
    Vec3(0.0, 0.0, 0.0),
    Vec4(0.0, 0.0, 0.0, 1.0),
    Vec3(1.0, 1.0, 1.0)
);