##PEMDAS REPL CALCULATOR

	o Command line utility written in Clojure.
	o Converts user inputted infix mathematical expressions to reverse polish notation using the shunting yard algorithm. Then parses and solves RPN expression.
	o Supports all real numbers
	o Supports a wide variety of mathematical operators/functions

"Please enter an arithmetic expression separated by spaces.

i.e.  sin ( 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )
Supports +, -, *, /, ^, sin, cos, tan, arcsin, arccos, arctan"

Calculator also supports the use of variable storage and assignment.

Type 'assignments' to see variables stored.
Type 'exit' to exit.

There are some bugs. But for the most part it works pretty well.

To run you must have leiningen installed and use the command "lein run" @ the project path.