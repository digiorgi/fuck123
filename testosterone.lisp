(in-package :testosterone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION-RESULT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <function-result> () ())
(defgeneric function-successp (function))
(defgeneric function-error-msg (function))
(defgeneric function-result-value (function))
(defgeneric function-errorp (functionp))
(defclass <function-error> (<function-result>)
  ((error-msg :accessor function-error-msg
	      :initarg :msg :initform (error "An error msg is needed"))))
(defun make-function-error (error-msg)
  (make-instance '<function-error> :msg error-msg))
(defmethod function-successp ((f <function-error>)) nil)
(defmethod function-errorp ((f <function-error>)) f)
(defclass <function-success> (<function-result>)
  ((result-value :initarg :result :initform (error "Need a result value"))))
(defun make-function-success (result)
  (make-instance '<function-success> :result result))
(defmethod function-successp ((f <function-success>)) f)
(defmethod function-errorp ((f <function-success>)) nil)
(defmethod function-result-value ((f <function-result>))
  (slot-value f 'result-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun secure-lambda-call (f f-description)
  (handler-case	
    (make-function-success (funcall f))
    (condition (my-condition)
      (make-function-error
       (format nil "~a: ~a" f-description my-condition)))))
(defun secure-comp-call (comp a-result b-result comp-des a-des b-des)
  (if (and (function-successp a-result)
	   (function-successp b-result))
      (let* ((a (function-result-value a-result))
	     (b (function-result-value b-result))
	     (comp-r
	      (secure-lambda-call (lambda () (funcall comp a b)) comp-des)))
	(if (function-errorp comp-r) comp-r
	    (let ((comp-r (function-result-value comp-r)))
	      (if (eql comp-r t)
		  (make-function-success t)
		  (make-function-error
		   (format nil "(~a ~a ~a) =>~%~a"
			   comp-des
			   a-des
			   b-des
			   comp-r))))))
      a-result))
(defgeneric function-run (function))
(defmethod function-run ((function <function>))
  (with-slots (comp comp-des fun-a fun-a-des fun-b fun-b-des) function
    (let* ((a (secure-lambda-call fun-a fun-a-des))
	   (b (secure-lambda-call fun-b fun-b-des))
	   (r (secure-comp-call comp a b comp-des fun-a-des fun-b-des))
	   (some-error (some #'function-errorp (list a b r))))
      (or some-error
	  r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <set> ()
  ((parent-set :reader set-parent :initarg :parent :initform nil)
   (childs-set :reader set-childs :initarg :childs :initform nil)
   (description :reader set-description :initarg :description)
   (functions :documentation "list of <function>"
	      :reader set-functions :initarg :functions)))
(defun make-set (&key childs description functions)
  (make-instance '<set> :childs childs :description description
		 :functions functions))
(defgeneric set-test (set))
(defgeneric set-add-backtrace-to-error (set error))
(defgeneric set-test-all-childs (set))
(defmethod initialize-instance :after ((this <set>) &key)
  (loop for child in (slot-value this 'childs-set)
     do (setf (slot-value child 'parent-set)
	      this)))
(defun string+ (&rest args)
  (apply #'concatenate 'string args))
(defmethod set-add-backtrace-to-error ((this <set>)
				       (function-error <function-error>))
  (let* ((parents (nreverse
		   (loop for set = this then (set-parent set)
		      while set collecting set)))
	 (trace-msg-list (loop for set in parents
			    collecting
			      (string+ (set-description set) "~% ")))
	 (trace-msg (reduce #'string+ trace-msg-list)))
    (make-function-error
     (string+ trace-msg (function-error-msg function-error)))))
(defmethod set-test-all-functions ((this <set>))
  (let ((functions (set-functions this)))
    (if (not functions)
	(make-function-success nil)
	(loop for function in functions
	   for function-result = (function-run function)
	   until (function-errorp function-result)
	   finally (return function-result)))))
(defmethod set-test ((this <set>))
  (let ((function-result (set-test-all-functions this)))
    (if (function-successp function-result)
	(set-test-all-childs this)
	(set-add-backtrace-to-error this function-result))))
(defmethod set-test-all-childs ((set <set>))
  (or (loop for child in (set-childs set)
	 for function-result = (set-test child)
	 until (function-errorp function-result)
	 finally (return function-result))
      (make-function-success t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <test> ()
  ((description :reader test-description :initarg :description)
   (sets :documentation "List of <set>s"
	 :accessor test-sets :initform nil)))
(defun make-test (description)
  (make-instance '<test> :description description))
(defgeneric test-add (test set))
(defgeneric test-run (test))
(defmethod test-add ((test <test>) (set <set>))
  (push set (test-sets test)))
(defmethod test-run ((test <test>))
  (loop for set in (test-sets test)
     for function-result = (set-test set)
     until (function-errorp function-result)
     finally (when (function-errorp function-result)
	       (format t
		       (string+ (function-error-msg function-result) "~%")))
       (return function-result)))
(defun test-package (package)
  (let* ((symbol (find-symbol "*TESTOSTERONE-TEST*" package) )
	 (test (and (boundp symbol) (symbol-value symbol))))
    (if test
	(test-run test)
	(warn "No test found in package ~a" (package-name package)))))
(defun test-packages-starting-with (package-name-string)
  (let* ((package-pattern package-name-string)
	 (pattern-size (1- (length package-pattern)))
	 (is-valid-package (lambda (package-name)(search package-pattern package-name
						    :start2 0
						    :start2 pattern-size)))
	 (all-packages (mapcar #'package-name (list-all-packages)))
	 (packages-to-test
	  (remove-if-not is-valid-package all-packages)))
    (loop for i in packages-to-test do
	 (test-package (find-package i)))
    packages-to-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mdefun (f-name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,f-name ,args ,@body)))
(mdefun to-string (x)
  (format nil "~a" x))
(mdefun expand-to-make-function (statement)
  (let ((statement (macroexpand statement)))
    (destructuring-bind (comparison a-value b-value) statement
      `(make-function :comp       (function ,comparison)
		      :comp-des  ,(to-string comparison)
		      :fun-a      (lambda () ,a-value)
		      :fun-a-des ,(to-string a-value)
		      :fun-b      (lambda () ,b-value)
		      :fun-b-des ,(to-string b-value)))))
(mdefun set-definitionp (statment)
  (equalp '-- (car statment)))
(mdefun make-function-definitionp (statment)
  (not (set-definitionp statment)))
(mdefun expand-set-definition (in-statments &key (top-level nil))
  (destructuring-bind (_ description &rest statments) in-statments
    (declare (ignore _))
    (let* ((make-function-definitions
	    (remove-if-not #'make-function-definitionp statments))
	   (set-definitions
	    (remove-if-not #'set-definitionp statments))
	   (functions (mapcar #'expand-to-make-function
			      make-function-definitions))
	   (childs (mapcar #'expand-set-definition
			   set-definitions))
	   (make-set
	    `(make-set
	      :description ,description
	      :functions  (list ,@functions)
	      :childs     (list ,@childs))))
      (if top-level
	  `(test-add ,(intern "*TESTOSTERONE-TEST*" *package*) ,make-set)
	  make-set))))

(defmacro is (x y) `(equalp ,x ,y))
(defmacro isnt (x y) `(equalp nil (equalp ,x ,y)))
(defmacro truep (x) `(equalp t (and ,x t)))
(defmacro nilp  (x) `(equalp nil (and ,x nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL INTERFACE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro -- (&body body)
  (expand-set-definition (cons '-- body) :top-level t))
(defmacro deftest (test-description)
  `(defparameter ,(intern "*TESTOSTERONE-TEST*" *package*)
     (make-test ,test-description)))
(defun runtest (package-string)
  (test-packages-starting-with package-string))
