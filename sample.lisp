(defpackage :testosterone.sample
  (:use :cl :testosterone))

(in-package :testosterone.sample)

(def-test "This is a demostrations")

(-- "EQL and EQUALP should work"
    (equalp '(1 2 (2 4)) '(1 2 (2 4)))
    (equal 1 1.0)
    
    (-- "Also we need to test it with strings"
	(equalp "asd" "ASd")))


(defparameter *testosterone-test* (make-test  "This is a demostration"))

(test-add
 *testosterone-test*
 (make-set
  :description "EQL and EQUALP should work"
  :functions
  (list (make-function :comp #'equalp
		       :comp-des "equalp"
		       :fun-a (lambda () '(1 2 (2 4)))
		       :fun-a-des "'(1 2 (2 4))"
		       :fun-b (lambda () '(1 2 (2 4)))
		       :fun-b-des "'(1 2 (2 4))")
	(make-function :comp #'equal
		       :comp-des "equal"
		       :fun-a (lambda () 1)
		       :fun-a-des "1"
		       :fun-b (lambda () 1)
		       :fun-b-des "1.0"))
  :childs
  (list
   (make-set
    :description "Also we need to test it with strings"
    :functions
    (list (make-function :comp #'equalp
			 :comp-des "equalp"
			 :fun-a (lambda () "ASdsdasdadadasdasdasasdasadasdas")
			 :fun-a-des "ASdsdasdadadasdasdasasdasadasdas"
			 :fun-b (lambda () "ASd")
			 :fun-b-des "Asd"))
    :childs nil))))

(let* ((f (make-function :comp #'equalp
			       :comp-des "equalp"
			       :fun-a (lambda () '(1 2 (2)))
			       :fun-a-des "'(1 2 (2 4))"
			       :fun-b (lambda () '(1 2 (2 4)))
			       :fun-b-des "'(1 2 (2 4))"))
       (r (testosterone::function-run f)))
  (when (testosterone::function-errorp r)
    (print (testosterone::function-error-msg r))))


(testosterone::test-run *testosterone-test*)
