;;;; package.lisp

(defpackage #:testosterone
  (:use #:cl)
  (:export :--
	   :deftest
	   :runtest
	   :is
	   :isnt
	   :truep
	   :nilp))

