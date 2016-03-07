# TESTOSTERONE

```lisp
(deftest "This is a demostrations")

(-- "EQL and EQUALP should work"
    (equalp '(1 2 (2 4)) '(1 2 (2 4)))
    (equal 1 1.0)
    
    (-- "Also we need to test it with strings"
	(equalp "asd" "ASd")))
```

## How can i define a test?

First define a package for your tests, for example ``my-test.a``, then in that package use ``deftest`` macro to declare that in that package you have tests.

The ``--`` macro is used to declare your tests, you can agroup and nest tests.

```lisp
(-- "Testing a web server
 (-- "Testing ajax"
   (-- "GET"
    (your tests ...))))
```

Each test has three parts ``(COMPARISON-FUNCTION VALUE-1 VALUE-2)``. So ensure that you write the test in that way, but you can also use custom macros.


### Usefull macros for test definitions

Two arguments:

* ``is``:``equalp``
* ``isnt`` : ``not equalp``

One argument:

* ``truep`` 
* `` nilp`` 

## Running 
You can run test using ``runtest`` function. With the first argument as a string representing the package name. Each package that starts with that name and has a test definitions will be effectively tested.

```lisp
(defpackage mytest-a.b (:use :cl :testosterone))
(in-package :mytest-a.b)

(-- "FOO")

(runtest "MYTEST-A.B") ;; <-- NOTE THE UPPERCASE
```