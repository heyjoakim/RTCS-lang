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

## If (Intepreter)
> evalProg (parseProgFromString "(if 1 then 2 else 3) +4");;

## comp (Compiler)

comp [] (ADD(INT 5, INT 6));;

> main "1+2" [];;
> comp [](parseExpFromString '1+2');;
> execProg (comp [](parseExpFromString '1+2') )[];;
>
> > execProg (comp ["a"](ADD 'VAR "a", INT 5')) [1];;
> > execProg (comp ["a"](parseExpFromString 'a+2')) [1];;

## let (Compiler)
> > execProg (comp [] (parseExpFromString "let x = 5 in x")) [];;

## If (Compiler)
> parseProgFromString "(if 1 then 2 else 3) +4";;              
> evalProg (parseProgFromString "(if 1 then 2 else 3) +4");;