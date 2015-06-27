Programming Summer Term 2015
============================

Solutions for the [Programming class 2015](http://www.inf.tu-dresden.de/index.php?node_id=3628&ln=de)

This is not the official reference solution and may contain errors.

### How to use AM0 Interpreter.

You need to install haskell.
Open a terminal and enter:

´´´
runhaskell AMx/Interpreter.hs [file] [input]
´´´

For Example 

´´´
runhaskell AMx/Interpreter.hs E08/A1 
´´´

should give

´´´
(1, ε, [], 3, ε)
(2, ε, [1/3], ε, ε)
(3, 3, [1/3], ε, ε)
(4, 1:3, [1/3], ε, ε)
(5, 1, [1/3], ε, ε)
(6, ε, [1/3], ε, ε)
(7, 3, [1/3], ε, ε)
(8, 2:3, [1/3], ε, ε)
(9, 1, [1/3], ε, ε)
(10, ε, [1/1], ε, ε)
(11, ε, [1/1], ε, 1)
(2, ε, [1/1], ε, 1)
(3, 1, [1/1], ε, 1)
(4, 1:1, [1/1], ε, 1)
(5, 0, [1/1], ε, 1)
(12, ε, [1/1], ε, 1)
´´´
