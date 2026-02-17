# Arrays
Arrays in Pau are annotated using `[<inner type>(; <size>)]` and defined with square brackets.
```pau
let x: [int; 5] = [0, 67, -24, 69420];
let unsizedArray: [int]; /: huh? what's the size???
```

## Indices
Arrays are indexed using `uint`, so you have to be explicit with the `u` suffix. This might be a footgun (sorry).
```pau
let array = [0, 10, 12];
array[2] /: ERROR!
array[2u] /: OK!
```

## Examples
```pau
def gimmeAGlowUp(x: [int; 5]): [int; 5]
    var result = [0, 0, 0, 0, 0];

    var idx = 0u;
    while idx < 5u do
        let value = x[idx];

        if value >= 0 then
            result[idx] := value;
        else
            result[idx] := -value;
        end

        idx := idx + 1u;
    end

    result
end

let before = [0, 5, 24, -55, 98];

let after = gimmeAGlowUp(before);