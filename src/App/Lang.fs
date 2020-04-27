module Lang

open Parser

// Interpreter
type 'a env = (varname list * 'a) list

let rec lookup x =
    function
    | [] -> failwith ("unbound: " + x)
    | (y, w) :: env ->
        if x = y then w else lookup x env


let rec eval env =
    function
    | INT i -> i
    | ADD(e1, e2) -> eval env e1 + eval env e2
    | VAR x -> lookup x env
    | LET(x, e1, e2) ->
        let v1 = eval env e1
        eval ((x, v1) :: env) e2
    | EQ(e1, e2) ->
        if eval env e1 = eval env e2 then 1 else 0
    | SUB(_, _) -> failwith "Not Implemented"
    | NEG(_) -> failwith "Not Implemented"
    | MUL(_, _) -> failwith "Not Implemented"
    | DIV(_, _) -> failwith "Not Implemented"
    | NEQ(_, _) -> failwith "Not Implemented"
    | LT(_, _) -> failwith "Not Implemented"
    | LE(_, _) -> failwith "Not Implemented"
    | GT(_, _) -> failwith "Not Implemented"
    | GE(_, _) -> failwith "Not Implemented"
    | IF(_, _, _) -> failwith "Not Implemented"
    | AND(_, _) -> failwith "Not Implemented"
    | OR(_, _) -> failwith "Not Implemented"
    | CALL(_, _) -> failwith "Not Implemented"
    | FAIL -> failwith "Not Implemented"
    | CATCH(_, _) -> failwith "Not Implemented"
    | SEQ(_, _) -> failwith "Not Implemented"
