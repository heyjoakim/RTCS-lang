# References

// #r "parser.dll", #r "vm.dll", open Parser, open VM, #load "lang.fs", open Lang.
// func = funcname _ (varname _ exp ) // pair times a pairs

## Basics

eval [](ADD 'INT 3, INT 5');;
eval [](SUB 'INT 3, INT 5');;
eval [](NEG 'INT 3, INT 5');;
eval [](EQ 'INT 3, INT 5');;
eval [](DIV 'INT 10, INT 5');;
eval [](MUL 'INT 10, INT 5');;
eval [](LT 'INT 5, INT 10');;
eval [](NEQ 'INT 5, INT 10');;
eval [](LE 'INT 5, INT 10');;
eval [](GT 'INT 5, INT 10');;
eval [](GT 'INT 5, INT 10');;
eval [](GE 'INT 5, INT 10');;

## Statements

eval [] (IF (EQ (INT 5, INT 5), INT 5, INT 99));;
eval [] (AND (GT (INT 5, INT 3), (EQ (INT 5, INT 5))));;
eval [] (OR (GT (INT 5, INT 10), (EQ (INT 5, INT 5))));;

## Assignments

eval [](LET '"x", INT 5, ADD (VAR "x", INT 42'));;
eval [("x", 5)](ADD 'VAR "x", INT 42'));;

## ParseExp

parseExpFromString "1+2";;

# ParseProg

parseProgFromString "func f(x) = x + x; f(7)";;
parseProgFromString "(if 1 then 2 else 3) +4";;

## EvalProg (Intepreter)

> evalProg (parseProgFromString "func f(x) = x + x; f(7)");;
> evalProg ([("double",(["x"], ADD (VAR "x",VAR"x")))], CALL ("double",[INT 7]));;
> > evalProg (parseProgFromString "let x = 5 in x");;  
> > evalProg ([], ADD(INT 3, INT 4));; // env and exp

## If (Intepreter)
> evalProg (parseProgFromString "(if 1 then 2 else 3) +4");;

## EQ (Intepreter)
> parseProgFromString "3==4";;
> > evalProg (parseProgFromString "3==4");;
> > evalProg ([], EQ(INT 3, INT 3));;

## comp (Compiler)

comp [] (ADD(INT 5, INT 6));;

> comp [](parseExpFromString '1+2');;
> execProg (comp [](parseExpFromString '1+2') )[];;
>
> > execProg (comp ["a"](ADD 'VAR "a", INT 5')) [1];;
> > execProg (comp ["a"](parseExpFromString 'a+2')) [1];;

## let (Compiler)
> > execProg (comp [] (parseExpFromString "let x = 5 in x")) [];;

## If (Compiler)
> parseExpFromString "(if 1 then 2 else 3) +4";;              
> execProg (comp [] (parseExpFromString "(if 1 then 2 else 3) +4")) [];;
> 
## EQ (compiler)
> execProg (comp [] (EQ(INT 5, INT 6))) [];;
> > parseExpFromString "3 == 4";;
> > execProg (comp [] (parseExpFromString "3 == 4")) [];;

## CompProg (Compiler)

compProg ([("f", ("x", ADD (VAR "x", INT 42))); ("g", ("y", CALL ("f", CALL ("f", VAR "y"))))], CALL ("g", INT 5));;

> compProg ([], ADD (INT 3, INT 5));;



// let env = ["pi"; "bigNumber"; "a"];;
// let c = comp env (ADD (VAR "a", INT 5));;
// let st = [3; 1000000; 42];;
// execProg c st;;

let foo =
    compProg
        ([ ("foo", ([ "x"; "y"; "z" ], ADD(ADD(VAR "x", VAR "y"), VAR "z"))) ], CALL("foo", [ INT 10; INT 42; INT 11 ]))

let foo2 =
    "func foo(x,y,z) = (x + y) + z; foo(10,42,11)"

evalProg (call)
        // | CALL (f, [ e ]) ->
        //     let v = eval env e
        //     let ([ x ], body) = lookup f funcs
        //     eval [ (x, v) ] body
        // | CALL (f, [ e1; e2 ]) ->
        //     let v1 = eval env e1
        //     let v2 = eval env e2
        //     let ([ x1; x2 ], body) = lookup f funcs
        //     eval [ (x1, v1); (x2, v2) ] body