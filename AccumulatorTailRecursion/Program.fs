module AccumulatorTailRecursion

// IN THIS FUNCTION, THE LAST OPERATION IS MULTIPLICATION, NOT RECURSIVE CALL
let rec factorial (x: int) : int =
    match x with
    | x when x < 2 -> 1
    | _ -> (factorial (x - 1)) * x

// WHY DO WE WANT THE LAST ACTION TO BE RECURSIVE CALL?
// BECAUSE THAT WAY THE COMPILER CAN PERFORM "TAIL RECURSIVE" OPTIMISATION


// IN THIS FUNCTION, THE LAST OPERATION IS RECURSIVE CALL
let rec factorialWithAccumulator (x: int) (accumulator: int) : int =
    match x with
    | x when x < 2 -> accumulator
    | _ -> factorialWithAccumulator (x - 1) (x * accumulator)

// factorialWithAccumulator 4 (1 * 5)
// factorialWithAccumulator 3 (1 * 5 * 4)
// factorialWithAccumulator 2 (1 * 5 * 4 * 3)
// factorialWithAccumulator 1 (1 * 5 * 4 * 3 * 2)
// 120

// `accumulator` IS THE RESULT OF THE OPERATION(S) THAT ARE MADE UNTIL THAT POINT
// LIKE EVERY CHEF IN THE KITCHEN ADDS THEIR THING ONTO THE PLATE, AND THEN PASSES THE PLATE TO THE NEXT CHEF
// AND WHEN FINALLY THE LAST CHEF (BASE CASE) RECEIVES THE PLATE, THEY SAY "OK THIS IS DONE" AND DELIVERS THE PLATE


let fact (number: int) : int = factorialWithAccumulator number 1

printfn $"{fact 5}"
