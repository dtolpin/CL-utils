(defpackage "PROMISE"
	(:documentation "Scheme-like promise facility")
	(:use "COMMON-LISP")
	(:export "PROMISE-P" "DELAY" "FORCE"))
(in-package "PROMISE")

(defstruct PROMISE "structure to hold promise"
	lambda result ready)

(defmacro DELAY (&body body)
	"create a promise"
	`(make-promise 
		:lambda
			#'(lambda ()
				,@body)))

(defun FORCE (p)
	"force the promise"
	(assert (promise-p p))
	(if (promise-ready p)
		(promise-result p)
		(let ((result (funcall (promise-lambda p))))
			(setf (promise-ready p) t)
			(setf (promise-result p) result))))
			
