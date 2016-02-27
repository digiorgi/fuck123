(defpackage :testosterone.sample
  (:use :cl :testosterone))

(in-package :testosterone.sample)

(tests "This is a demostrations")

(-- "EQL and EQUALP should work"
    (equalp '(1 2 (2 4)) '(1 2 (2 4)))
    (equal 1 1.0)
    
    (-- "Also we need to test it with strings"
	(equalp "asd" "ASd")))


(defparameter *testosterone-test* (make-test  "This is a demostration"))

(test-add
 *testosterone-test*
 (let ((parent-set-0))
   (setf parent-set-0
	 (make-set
	  :description "EQL and EQUALP should work"
	  :parent nil
	  :functions
	  (list (make-function :comp #'equalp
			       :comp-des "equalp"
			       :fun-a (lambda () '(1 2 (2 4)))
			       :fun-a-des "'(1 2 (2 4))"
			       :fun-b (lambda () '(1 2 (2 4)))
			       :fun-b-des "'(1 2 (2 4))")
		(make-function :comp #'equal
			       :comp-des "equalp"
			       :fun-a (lambda () 1)
			       :fun-a-des "1"
			       :fun-b (lambda () 1.0)
			       :fun-b-des "1.0"))
	  :childs
	  (list
	   (let ((parent-set-1))
	     (setf parent-set-1
		   (make-set
		    :description "Also we need to test it with strings"
		    :parent parent-set-0
		    :functions
		    (list (make-function :comp #'equalp
					 :comp-des "equalp"
					 :fun-a (lambda () "asd")
					 :fun-a-des "asd"
					 :fun-b (lambda () "ASd")
					 :fun-b-des "Asd"))
		    :childs nil))
	     parent-set-1))))
   parent-set-0))

(let* ((f (make-function :comp #'equalp
			       :comp-des "equalp"
			       :fun-a (lambda () '(1 2 (2 4)))
			       :fun-a-des "'(1 2 (2 4))"
			       :fun-b (lambda () '(1 2 (2 4)))
			       :fun-b-des "'(1 2 (2 4))"))
       (r (testosterone::function-run f)))
  (testosterone::function-error-msg r))
