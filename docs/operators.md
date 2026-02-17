# Operators
Pau has 12 operators:
| Operator | Purpose                                  | Infix/Prefix |
| -------- | ---------------------------------------- | ------------ |
| `+`      | Numeric Addition and Numeric Affirmation | Both         |
| `-`      | Numeric Subtraction and Numeric Negation | Both         |
| `*`      | Numeric Multiplication                   | Infix        |
| `/`      | Numeric Division                         | Infix        |
| `%`      | Numeric Remainder                        | Infix        |
| `:=`     | Mutation                                 | Infix        |
| `==`     | Equality                                 | Infix        |
| `!=`     | Inequality                               | Infix        |
| `>`      | Numeric Greater-than                     | Infix        |
| `<`      | Numeric Less-than                        | Infix        |
| `>=`     | Numeric Greater-or-equal                 | Infix        |
| `<=`     | Numeric Less-or-equal                    | Infix        |
| `\|`     | Bitwise OR                               | Infix        |
| `&`      | Bitwise AND                              | Infix        |
| `\|\|`   | Logical OR                               | Infix        |
| `&&`     | Logical AND                              | Infix        |
| `^`      | Logical XOR and Bitwise XOR              | Infix        |
| `!`      | Logical NOT and Bitwise NOT              | Prefix       |

## Precedence
Here's all of the operators ranked by precedence from highest to lowest
- `!` (prefix)
- `*`, `/`, `%`
- `+`, `-`
- `|`, `&`, `^`
- `>`, `<`, `>=`, `<=`
- `==`, `!=`
- `||`, `&&`
- `:=`