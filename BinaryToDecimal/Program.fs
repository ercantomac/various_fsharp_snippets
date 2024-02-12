let rec power (number: int) (pow: int) =
    match pow with
    | pow when pow < 0 -> 0
    | 0 -> 1
    | 1 -> number
    | _ -> number * power number (pow - 1)

let getDecimalValue (input: (int * char)) =
    let (index, bit) = input

    match bit with
    | '1' -> power 2 index
    | _ -> 0

let binToDec (binaryString: string) =
    let reversed = binaryString.ToCharArray() |> Array.rev
    let reversedList = Seq.toList reversed |> List.indexed

    reversedList
    |> List.fold (fun accumulation element -> accumulation + getDecimalValue element) 0

let a = binToDec "00000000"
printfn $"{a}"
let b = binToDec "00000001"
printfn $"{b}"
let c = binToDec "00000010"
printfn $"{c}"
let d = binToDec "00000011"
printfn $"{d}"
let e = binToDec "00000100"
printfn $"{e}"
let f = binToDec "00000101"
printfn $"{f}"
let g = binToDec "00000110"
printfn $"{g}"
