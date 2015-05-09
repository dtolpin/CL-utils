; $Id: memoize.lisp,v 1.3 2005/05/10 11:28:47 dvd Exp $

(defpackage "MEMOIZE"
	(:documentation "utilities to create memoized functions")
	(:use "COMMON-LISP")
	(:export "MEMO" "DEFMEMO" "CLRMEMO"))
(in-package "MEMOIZE")

(defun MEMO (fn &key (key #'values) (test #'eql))
	"computes a memoized function; returns the function and the memo table for it"
	(let ((memotab (make-hash-table :test test)))
		(values
			#'(lambda (&rest args)
				(let ((k (apply key args)))
					(multiple-value-bind (value found) (gethash k memotab)
						(if found value
							(setf (gethash k memotab) (apply fn args))))))
			memotab)))

(defmacro DEFMEMO (name args memoargs &body body)
	"defines a memoized function"
	(let ((memotab (gensym)) (key (gensym)) (k (gensym)))
		`(let (
				(,memotab (make-hash-table 
					:test ,(or (cadr (member :test memoargs)) '#'eql)))
				(,key ,(or (cadr (member :key memoargs)) '#'values)))
			(defun ,name ,args
				(let ((,k (funcall ,key ,@args)))
					(multiple-value-bind (v found) (gethash ,k ,memotab)
						(if found v
							(setf (gethash ,k ,memotab) (progn ,@body))))))
			(setf (get ',name 'memotab) ,memotab))))

(defun CLRMEMO (name)
	"clears function's memory"
	(assert (symbol-function name))
	(clrhash (get name 'memotab)))