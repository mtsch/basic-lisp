# basic-lisp

A working lisp-like language interpreter with:

* ints, strings, bools, lists, recursive functions and variables
* global mutable refs
* some basic built-in functions including:
    * arithmetic operators
    * equality and order checking
    * head, tail, cons for lists
    * concat for lists and strings
    * throwing errors
    * printing to and reading from stdout
    * converting expressions to strings
    * evaluating strings
    * slurping files
    * setting and getting global mutable refs
    * if-then-else expressions

To see some examples, run:
```
(def file (slurp "example.bl")) (eval file)
```
