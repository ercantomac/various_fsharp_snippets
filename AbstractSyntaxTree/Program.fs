type ASTNode =
    | Binary of BinaryOperation
    | Constant of int

and BinaryOperation =
    | Add of (ASTNode * ASTNode)
    | Subtract of (ASTNode * ASTNode)
    | Multiply of (ASTNode * ASTNode)
    | Divide of (ASTNode * ASTNode)


let Evaluate (node: ASTNode) : int =
    match node with
    | Binary operation -> EvaluateBinaryOperation operation
    | Constant c -> c

and EvaluateBinaryOperation (operation: BinaryOperation) : int =
    match operation with
    | Add (left, right) -> Evaluate left + Evaluate right
    | Subtract (left, right) -> Evaluate left - Evaluate right
    | Multiply (left, right) -> Evaluate left * Evaluate right
    | Divide (left, right) -> Evaluate left / Evaluate right


let Sum (value: ASTNode) (add: ASTNode) : ASTNode = Binary(Add(add, value))

let Difference (value: ASTNode) (subtract: ASTNode) : ASTNode = Binary(Subtract(subtract, value))

let Product (value: ASTNode) (multiply: ASTNode) : ASTNode = Binary(Multiply(multiply, value))

let Quotient (value: ASTNode) (divide: ASTNode) : ASTNode = Binary(Divide(divide, value))

let equation =
    Constant 2
    |> Sum(Constant 5)
    |> Product(Constant 12)
    |> Quotient(Constant 6)
    |> Sum(Constant 17)

let result = Evaluate equation

printfn $"{result} :d"
