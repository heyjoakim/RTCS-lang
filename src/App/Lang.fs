module Lang

open Parser
open System

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
