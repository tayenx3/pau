# Booleans
Booleans in Pau have type `bool`, and is defined with the `true` or `false` keywords
```pau
true /: represents "truth"
false /: represents the opposite of true
```

Comparative operations also return `bool`
```pau
5 == 5 /: true
5 != 5 /: false
5 == 6 /: false
5 != 6 /: true
```

## Examples
```pau
var x = if true then 5 else 6 end; /: guarenteed to be 5
if x == 5 then
    let y = 10;
    x := y;
else
    let z = 15;
    x := z;
end