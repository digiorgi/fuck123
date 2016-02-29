# TESTOSTERONE

```lisp
(def-test "This is a demostrations")

(-- "EQL and EQUALP should work"
    (equalp '(1 2 (2 4)) '(1 2 (2 4)))
    (equal 1 1.0)
    
    (-- "Also we need to test it with strings"
	(equalp "asd" "ASd")))
```