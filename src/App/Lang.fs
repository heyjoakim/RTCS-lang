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

let evalProg (funcs, e) =
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
        | LE (e1, e2) -> if eval env e1 <= eval env e2 then 1 else 0
        | GT (e1, e2) -> if eval env e1 > eval env e2 then 1 else 0
        | GE (e1, e2) -> if eval env e1 >= eval env e2 then 1 else 0
        | IF (e1, e2, e3) -> if eval env e1 = 1 then eval env e2 else eval env e3
        | AND (e1, e2) -> if eval env e1 = 1 && eval env e2 = 1 then 1 else 0
        | OR (e1, e2) -> if eval env e1 = 1 || eval env e2 = 1 then 1 else 0
        | CALL (f, [ e ]) ->
            let v = eval env e
            let ([ x ], body) = lookup f funcs
            eval [ (x, v) ] body
        | NEG (_) -> failwith "Not Implemented"

    eval [] e
