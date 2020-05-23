
type Stack = StackContents of float list

let newStack = StackContents [1.0;2.0;3.0]

let (StackContents contents)= newStack

let push x (StackContents contents)=
    StackContents (x::contents)

let pop (StackContents contents)=
    match contents with
    | top::rest ->
        let newStack = StackContents rest
        (top,newStack)
    | [] ->
        failwith "Stack underflow"


let binary mathFn stack =
    let y,stack' = pop stack
    let x,stack'' = pop stack'
    let z = mathFn x y
    push z stack''

let unary f stack =
    let x,stack' = pop stack
    push (f x) stack'


// partial application
let ADD = binary (+)
let MUL = binary (*)
let SUB = binary (-)
let DIV = binary (/)
let NEG = unary (fun x -> -x)
let SQUARE = unary (fun x -> x * x)
let SHOW stack =
    let x,_ = pop stack
    printfn "The answer is %f" x
    stack

let DUP stack =
    let x,_ = pop stack
    push x stack

let SWAP stack =
    let x,s = pop stack
    let y,s' = pop s
    push y (push x s')

let EMPTY = StackContents []
let START = EMPTY

let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0


let div2by3 = EMPTY |> THREE |> TWO |> DIV
let sub2from5 = EMPTY |> TWO |> FIVE |> SUB
let add1and2thenSub3 = EMPTY |> ONE |> TWO |> ADD |> THREE |> SUB
let neg3 = EMPTY |> THREE |> NEG
let square2 = EMPTY |> TWO |> SQUARE

printfn "%A" div2by3
printfn "%A" neg3
printfn "%A" square2
