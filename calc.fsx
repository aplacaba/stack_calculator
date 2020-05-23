
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

let ADD stack =
    let x,s = pop stack
    let y,s2 = pop s
    let result = x + y
    push result s2


let MUL stack =
    let x,s = pop stack
    let y,s2 = pop s
    let result = x * y
    push result s2
    
    
let EMPTY = StackContents []
let ONE = push 1.0
let TWO = push 2.0
let THREE = push 3.0
let FOUR = push 4.0
let FIVE = push 5.0


let add1and2 = EMPTY |> ONE |> TWO |> ADD
let add2and3 = EMPTY |> TWO |> THREE |> ADD
let mult2and3 = EMPTY |> TWO |> THREE |> MUL
