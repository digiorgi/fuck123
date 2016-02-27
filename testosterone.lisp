(in-package :testosterone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION-RESULT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <function-result> () ())
(defgeneric function-successp (function))
(defgeneric function-error-msg (function))
(defgeneric function-result-value (function))
(defgeneric function-errorp (functionp))
(defclass <function-error> (<function-result>)s
  ((error-msg :accessor function-error-msg
	      :initarg :msg :initform (error "An error msg is needed"))))
(defun make-function-error (error-msg)
  (make-instance '<function-error> :msg error-msg))
(defmethod function-successp ((f <function-error>)) nil)
(defmethod function-errorp ((f <function-error>)) f)
(defmethod function-error-msg ((f <function-error>))
    (slot-value f 'error-msg))
(defclass <function-success> (<function-result>)
  ((result-value :initarg :result :initform (error "Need a result value"))))
(defun make-function-sucess (result)
  (make-instance '<function-success> :result result))
(defmethod function-successp ((f <function-success>)) f)
(defmethod function-errorp ((f <function-success>)) nil)
(defmethod function-result-value ((f <function-result>))
  (slot-value f 'result-value))
(defmacro if-function-success (functions body-t body-nil)
  `(if (and ,@(loop for f in functions collecting `(function-successp ,f)))
     (let ,(loop for f in functions collecting `(,f (function-value ,f)))
       ,@body-t)
     (progn ,@body-nil)))

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
    (make-function-sucess (funcall f))
    (condition (my-condition)
      (make-function-error
       (format nil "~a: ~a" f-description my-condition)))))
(defun secure-comp-call (comp a-result b-result comp-des a-des b-des)
  (if (and (function-successp a-result)
	   (function-successp b-result))
      (let* ((a (function-result-value a-result))
	     (b (function-result-value b-result))
	     (comp-r
	      (secure-lambda-call (lambda () (funcall comp () a b)) comp-des)))
	(if (function-errorp comp-r) comp-r
	    (let ((comp-r (function-result-value comp-r)))
	      (if (eql comp-r t)
		  (make-function-sucess t)
		  (make-function-error
		   (format nil "(~a ~a ~a)=>~%~a"
			   comp-des
			   a-des
			   b-des
			   (function-result-value b)))))))
      a-result))
(defgeneric function-run (function))
(defmethod function-run ((function <function>))
  (with-slots (comp comp-des fun-a fun-a-des fun-b fun-b-des) function
    (let* ((a (secure-lambda-call fun-a fun-a-des))
	   (b (secure-lambda-call fun-b fun-b-des))
	   (r (secure-comp-call comp a b comp-des fun-a-des fun-b-des))
	   (some-error (some #'function-errorp (list a b r))))
      (or some-error
	  t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass <set> ()
  ((parent-set :reader set-parent :initarg :parent :initform nil)
   (child-set :reader fact-childs :initarg :childs :initform nil)
   (description :reader fact-description :initarg :description)
   (functions :documentation "list of <function>"
	      :reader functions :initarg :functions)))
(defun make-set (&key parent childs description functions)
  (make-instance '<set> :parent parent :childs childs :description description
		 :functions functions))
(defgeneric set-test (set)
  (:documentation "Returns T if all is ok, otherwise print a description of 
                   the error"))
(defmethod set-test ((set <set>))
  )

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
  (loop for set in (test-sets test) while (set-test)))
(defun is (x y) "equalp" (equalp x y))
(defun isnt (x y) "not equalp" (not (equalp x y)))
(defun test-package (package)
  (let ((test (symbol-value (find-symbol "*TESTOSTERONE-TEST*" package))))
    (if test
	(test-run test)
	(warn "No test found in package ~a" (package-name package)))))
(defun test-packages-starting-with (package-symbol)
  (let* ((package-pattern (package-name (find-package package-symbol)))
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
