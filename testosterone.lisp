;;;; testoterone.lisp
(in-package :testosterone)

(defclass <function> ()
  ((comp :reader fun-comp :initarg :comp)
   (comp-des :reader fun-comp-des :initarg :comp-des)
   (fun-a :reader fun-a :initarg :fun-a)
   (fun-a-des :reader fun-a-des :initarg :fun-a-des)
   (fun-b :reader fun-b :initarg :fun-b)
   (fun-b-des :reader fun-b-des :initarg :fun-b-des)))

(defun make-function (&key comp comp-des fun-a fun-a-des fun-b fun-b-des)
  (make-instance '<function>
		 :comp comp
		 :comp-des comp-des
		 :fun-a fun-a
		 :fun-a-des fun-a-des
		 :fun-b fun-b
		 :fun-b-des fun-b-des))

(defclass <set> ()
  ((parent-set :reader set-parent :initarg :parent :initform nil)
   (child-set :reader fact-childs :initarg :childs :initform nil)
   (description :reader fact-description :initarg :description)
   (functions :documentation "list of <function>"
	      :reader functions :initarg :functions)))

(defun make-set (&key parent childs description functions)
  (make-instance '<set> :parent parent :childs childs :description description
		 :functions functions))

(defclass <test> ()
  ((description :reader test-description :initarg :description)
   (sets :documentation "List of <set>s"
	 :accessor test-sests :initform nil)))

(defun make-test (description)
  (make-instance '<test> :description description))

(defgeneric test-add (test set))

(defmethod test-add ((test <test>) (set <set>))
  (push set (test-tests test)))

(defun is (x y) "equalp" (equalp x y))
(defun isnt (x y) "not equalp" (not (equalp x y)))




