# A DIY functional programming language: MiniCAML
## An individual project in Canada

The goal of this project is to explore concepts such as **free variables**, **substitution**, **evaluation**, **type checking**, and **type inference**.

Three key questions in programming language design: 
1. **Grammar** (What are the syntactically legal expressions? What expressions does the parser accept?)
2. **Static semantics** (What are well-typed expression? What expressions does the type-checker accept?)
3. **Dynamic semantics** (How is an expression executed?)

This project has extended the syntactic category for expressions with **n-ary function abstractions**, **n-ary function applications**, and **recursion**. 

(1). Instead of abstracting over a single variable, the fun construct in MiniCAML now abstracts over multiple variables at once. This means we can construct functions of multiple variables:

fun (x: int, y: int) => (x * x) + (y * y) Unlike in OCaml, these functions are not implicitly curried. They need to be called with exactly the correct number of arguments. This is the same behaviour as in other languages you may be familiar with such as C, Java, or Python.
We can also define functions that take zero arguments: fun () => true

(2). Since functions now abstract over multiple variables, we also need a way to apply a function to multiple variables at once. Function application in MiniCAML has thus been extended:

let f = (fun (x: int, y: int) => (x * x) + (y * y)) in f (3, 4) We can also call a function with zero arguments.

let g = (fun () => true) in g () Unlike in OCaml, there is no implicit partial application of functions. This means that the following expression is ill-typed.

let f = (fun (x: int, y: int) => (x * x) + (y * y)) in f (3) If we wanted to be able to apply f to just one argument at a time.

let f = (fun (x: int) => (fun (y: int) => (x * x) + (y * y))) in (f (3)) (4) Note that in this case, calling f (3, 4) would result in a type error.

(3). Finally, the rec construct allows us to define recursive functions: rec (fact: int -> int) => fun (n: int) => if n = 0 then 1 else n * fact (n - 1) The rec construct binds a new variable which we may then refer to within its body.

