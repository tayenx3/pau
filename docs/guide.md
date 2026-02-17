# The Pau Guide
This is a guide to learn Pau

## Important Note
Pau is experimental, so production uses are not recommended.
Pau is **NOT**:
- A systems programming language
- A super performant language
- A language for everyone

It's supposed to be:
- Deliberately simple
- Experimental
- Open for everyone to use and contribute to

## Prelude
### Comments
```pau
/: this is a single-line comment
(: this is a block comment :)
```

### "To semicolon, or not to semicolon?"
Semicolons in Pau are optional separators. Although they are optional, they are considered good practice to help disambiguate when parsing.
```pau
let x = 10; /: semicolon, OK!
let x = 10  /: no semicolon, OK!
```

### Are newlines or spaces significant
Like OCaml, Pau does not account for newlines or spaces. Instead, Pau uses separators and terminators like `;`, `,`, `end`
```pau
if x == y then
    /: body
/: <- expected 'end'!
```

## Index
- [Integers and Floats](integers_and_floats.md)
- [Operators](operators.md)
- [Variables](variables.md)
- [Booleans](booleans.md)
- [`if`-`else` conditionals](conditionals.md)
- [`while` loops](while_loops.md)
- [Functions](functions.md)
- [Arrays](arrays.md)
- [Structs](structs.md)
- [Constants](constants.md)
- [Naming Conventions](naming_conventions.md)