
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

    
let EMPTY = StackContents []

let ONE = push 1.0 
let TWO = push 2.0 
let initialStack = EMPTY |> ONE |> TWO
let popped1, poppedStack = pop initialStack
let popped2, poppedStack2 = pop poppedStack

let _ = pop EMPTY
