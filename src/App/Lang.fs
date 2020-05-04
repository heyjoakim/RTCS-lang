module Lang

open Parser

// #r "parser.dll", #r "vm.dll", open Parser, open VM, #load "lang.fs", open Lang.

// Interpreter
type 'a env = (varname list * 'a) list
// func  = funcname * (varname * exp ) // pair times a pairs

let rec lookup x =
    function
    | [] -> failwith ("unbound: " + x)
    | (y, w) :: env -> if x = y then w else lookup x env


let rec eval env =
    function
    | INT i -> i
    | ADD (e1, e2) -> eval env e1 + eval env e2
    | VAR x -> lookup x env
    | LET (x, e1, e2) ->
        let v1 = eval env e1
        eval ((x, v1) :: env) e2
    | EQ (e1, e2) -> if eval env e1 = eval env e2 then 1 else 0
    | SUB (e1, e2) -> eval env e1 - eval env e2
    | MUL (e1, e2) -> eval env e1 * eval env e2
    | DIV (e1, e2) -> eval env e1 / eval env e2
    | NEQ (e1, e2) -> if eval env e1 <> eval env e2 then 1 else 0
    | LT (e1, e2) -> if eval env e1 < eval env e2 then 1 else 0
    | LE (_, _) -> failwith "Not Implemented"
    | GT (_, _) -> failwith "Not Implemented"
    | GE (_, _) -> failwith "Not Implemented"
    | IF (_, _, _) -> failwith "Not Implemented"
    | AND (_, _) -> failwith "Not Implemented"
    | OR (_, _) -> failwith "Not Implemented"
    | CALL (f, [ e ]) -> failwith "Not Implemented"
    | FAIL -> failwith "Not Implemented"
    | CATCH (_, _) -> failwith "Not Implemented"
    | SEQ (_, _) -> failwith "Not Implemented"
    | NEG (_) -> failwith "Not Implemented"
// funcs : func list
