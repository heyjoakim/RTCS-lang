# References

## Basics
eval [] (ADD (INT 3, INT 5));;
eval [] (SUB (INT 3, INT 5));;
eval [] (EQ (INT 3, INT 5));;
eval [] (DIV (INT 10, INT 5));;
eval [] (MUL (INT 10, INT 5));;
eval [] (LT (INT 5, INT 10));;
eval [] (NEQ (INT 5, INT 10));;
eval [] (LE (INT 5, INT 10));;
eval [] (GT (INT 5, INT 10));;
eval [] (GT (INT 5, INT 10));;
eval [] (GE (INT 5, INT 10));;

## Statements
eval [] (IF (EQ (INT 5, INT 5), INT 5, INT 99));;
eval [] (AND (GT (INT 5, INT 3), (EQ (INT 5, INT 5))));;
eval [] (OR (GT (INT 5, INT 10), (EQ (INT 5, INT 5))));;


## Assignments
eval [] (LET ("x", INT 5, ADD (VAR "x", INT 42)));;
eval [("x", 5)] (ADD (VAR "x", INT 42)));;