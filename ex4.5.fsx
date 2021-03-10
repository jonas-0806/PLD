type E = Digit of int | Par of E | Binop of E * string * E

let rec convert_post (exp : E) =
    match exp with
        | Binop(exp1, ch, exp2) -> (convert_post exp1) + " " + (convert_post exp2) + " " + ch
        | Par(exp1) -> convert_post exp1
        | Digit(x) -> string x

let rec convert_pre (exp : E) =
    match exp with
        | Binop(exp1, ch, exp2) -> ch + " " + (convert_pre exp1) + " " + (convert_pre exp2)
        | Par(exp1) -> convert_post exp1
        | Digit(x) -> string x

let test = Binop(Binop(Digit(2), "*", Binop(Digit(3), "+", Digit(4))), "-", Digit(5))
let test2 = Binop(Binop(Digit(2), "+", Binop(Digit(3), "*", Par(Binop(Digit(4), "-", Digit(5))))), "*", Binop(Digit(6), "/", Digit(7)))

printfn "%s" (convert_post test)
printfn "%s" (convert_post test2)

printfn "%s" (convert_pre test)
printfn "%s" (convert_pre test2)




