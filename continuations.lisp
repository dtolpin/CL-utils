; $Id: continuations.lisp,v 1.7 2005/06/07 13:01:20 dvd Exp $
;
; continuation macros from "on lisp" by paul graham
;
; i removed top-level continuation and added =toplevel macro
; so that it works with multi-parameter and parameterless
; functions, and with CMUCL

(defpackage "CONTINUATIONS"
  (:documentation "Paul Graham's continuation macros, slightly edited 
 		for portability. The macros allows to use implicit continuation 
 		passing in programs in continuation-passing style.
 		
 		The macros bind locally a variable called =cont=, and add symbols
 		starting with = for each function bindings, beware of conflicts.")
  (:nicknames "CONT")
  (:use "COMMON-LISP")
  (:export "=LAMBDA" "=DEFUN" "=BIND" "=WITH-CONT=" 
  	"=VALUES" "=FUNCALL" "=APPLY"))
(in-package "CONTINUATIONS")

(defmacro =WITH-CONT= (&body body)
	"establishes initial binding for =cont=;
	must wrap code with calls to continuation-passing macros"
	`(let ((=cont= #'values))
		,@body))
		
(defmacro =LAMBDA (parms &body body)
	"defun with implicit continuation"
	`#'(lambda (=cont= ,@parms) ,@body))

(defmacro =DEFUN (name parms &body body)
	"defun with implicit continuation; binds =name" 
	(let ((f (intern (concatenate 'string "=" (symbol-name name)))))
		`(progn
			 (defmacro ,name ,parms
				 `(,',f =cont= ,,@parms))
			 (defun ,f (=cont= ,@parms) ,@body))))
 
(defmacro =BIND (parms expr &body body)
	"Makes body continuation of the expression; the expression
	must be continuation-passing, while the body may or may not.
	
	(=bind (a) (=values 'done) (print a) t) -> T
	DONE
	
	The body is bound to =cont= before calling expr, thus the body 
	will be invoked again if a =closure from expr is called.
	
	(setq i 0 later nil)
	(=bind () (progn (setq later (lambda () (=values))) (=values))
  	(format t \"i=~A~%\" (incf i)))
  
  will increment and print i on each (funcall later)."

	`(let ((=cont= #'(lambda ,parms ,@body)))
		,expr))

(defmacro =VALUES (&rest retvals)
	"returns retvals (passing them through the continuation chain)"
	`(funcall =cont= ,@retvals))

(defmacro =FUNCALL (fn &rest args)
	"funcall for functions defined with =defun or =lambda"
	`(funcall ,fn =cont= ,@args))

(defmacro =APPLY (fn &rest args)
	"apply for functions defined with =defun or =lambda"
	`(apply ,fn =cont= ,@args))

; testing

(defvar *saved* nil)

(=defun again () (declare (ignore =cont=))
	(if *saved* (funcall (pop *saved*)) nil))
	
(=defun node (tree)
	(etypecase tree
		(null (again))
		(cons (push #'(lambda () (node (car tree))) *saved*)
			(node (cdr tree)))
		(atom (=values tree))))
		
(defun flatten-tree (tree)
	(let ((*saved* nil) (nodes nil))
		(=with-cont=
			(=bind (node) (node tree)
				(when node (push node nodes) (again))))
	nodes))

(defun test ()
	(assert (equal (flatten-tree nil) nil))
	(assert (equal (flatten-tree '(a b)) '(a b)))
	(assert (equal (flatten-tree '((a (b c) ((d e) f)))) '(a b c d e f))))

(eval-when (:load-toplevel :execute)
	(test))

