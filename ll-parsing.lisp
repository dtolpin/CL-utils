; $Id: ll-parsing.lisp,v 1.17 2006/05/03 18:06:58 dvd Exp $

(defpackage "LL-PARSING"
	(:nicknames "LL")
	(:documentation "LL parser combinators")
	(:use "COMMON-LISP")
	(:export
		"SUCCESSP" "RESULT" "FAILURE" "REST" "DEFPARSER"
		"PSELECT" "RETURN-SUCCESS" "RETURN-FAILURE"
		"PARSE-GROUP" "PARSE-OR"
		"PARSE-SOME" "PARSE-MAYBE" "PARSE-MANY" "PARSE-EMPTY"
		"TRANSFORM-RESULT"
		"P-ANY" "P-ALL" "P-EQL" "P-IF"
		"P-ALPHA-CHAR" "P-DIGIT-CHAR" "P-OTHER-CHAR"
		"P-CHAR" "P-NOTCH" "P-CHAR-CLASS" "P-CHAR-NCLAS"))
(in-package "LL-PARSING")

(defmacro SUCCESSP (r)
	"returns true if the parsing is successful"
	`(eq (caar ,r) 'result))

(defmacro RESULT (r)
	"retrieves result"
	`(cdar ,r))

(defmacro FAILURE (r)
	"retrieves failure"
	`(cdar ,r))

; rest is rest

(defmacro DEFPARSER (parser args &body body)
	"defines a functional parser"
	`(defun ,parser ,(cdr args)
		#'(lambda (,(car args))
			(if (null ,(car args))
				(return-failure "EOF" () ())
				,@body))))

(defmacro PSELECT (m p &rest ps)
	"wraps ps into descending applications of m"
	(if (null ps) p
		(let ((q (car ps)) (ps (cdr ps)))
			`(,m ,p (pselect ,m ,q ,@ps)))))

(defmacro RETURN-SUCCESS (r l)
	"returns result as ((RESULT . r) . l)"
	`(cons (cons 'result ,r) ,l))

(defmacro RETURN-FAILURE (reason r l)
	"returns failed parsing as (FAILURE reason extra)"
	`(cons (list* 'failure ,reason ,r) ,l))

(defmacro PARSE-GROUP2 (p q)
	"p then q"
	`#'(lambda (#0=#:l)
		(let ((#1=#:r1 (funcall ,p #0#)))
			(if (successp #1#)
				(let ((#2=#:r2  (funcall ,q (rest #1#))))
					(if (successp #2#)
						(return-success (nconc (copy-list (result #1#)) (result #2#)) (rest #2#))
						(return-failure (car (failure #2#)) 
							(nconc (copy-list (result #1#)) (cdr (failure #2#))) (rest #2#))))
				#1#))))

(defmacro PARSE-GROUP (p &rest ps)
	"arguments in sequence"
	`(pselect parse-group2 ,p ,@ps))

(defmacro PARSE-OR2 (p q)
	"p or q"
	`#'(lambda (#0=#:l)
		(let ((#1=#:r (funcall ,p #0#)))
			(if (successp #1#) #1# (funcall ,q #0#)))))

(defmacro PARSE-OR (p &rest ps)
	"one of arguments"
	`(pselect parse-or2 ,p ,@ps))

(defmacro PARSE-EMPTY ()
	"empty string"
	`#'(lambda (#0=#:l) (return-success () #0#)))

(defmacro PARSE-MAYBE (p)
	"at most one occurence of p"
	`(parse-or ,p (parse-empty)))

(defmacro PARSE-SOME (p)
	"zero or more occurences of p"
	`#'(lambda (#0=#:l)
		(labels (
				(#1=#:step (#0# #2=#:rl)
				  (let ((#3=#:r (funcall ,p #0#)))
				    (if (successp #3#)
				      (#1# (rest #3#) (nconc #2# (copy-list (result #3#))))
				      (return-success #2# #0#)))))
			(#1# #0# ()))))

(defmacro PARSE-MANY (p)
  "one or more occurences of p"
	`(parse-group ,p (parse-some ,p)))

(defmacro TRANSFORM-RESULT (x p)
	"passes the result of parser p through transformer x"
	`#'(lambda (#0=#:l)
		(let ((#1=#:r (funcall ,p #0#)))
			(if (successp #1#)
				(return-success (funcall ,x (result #1#)) (rest #1#))
				#1#))))

; parser aid
(defmacro P-ALL () "swallow the rest"
	'#'(lambda (l) (return-success l ())))
	
(defparser P-ANY (l)
	(return-success (list (car l)) (cdr l)))

(defparser P-EQL (l a)
	(if (eql (car l) a)
		(return-success (list (car l)) (cdr l))
		(return-failure "eql" () l)))

(defparser P-IF (l pred)
	(if (funcall pred (car l))
		(return-success (list (car l)) (cdr l))
		(return-failure "if" () l)))

; useful in scanners
(defparser P-ALPHA-CHAR (l)
	(if (alpha-char-p (car l))
		(return-success (list (car l)) (cdr l))
		(return-failure "alpha" () l)))

(defparser P-DIGIT-CHAR (l)
	(if (digit-char-p (car l))
		(return-success (list (car l)) (cdr l))
		(return-failure "digit" () l)))

(defparser P-OTHER-CHAR (l)
	(if (<= 128 (char-code (car l)))
		(return-success (list (car l)) (cdr l))
		(return-failure "other" () l)))

(defparser P-CHAR (l c)
	(if (char= (car l) c)
		(return-success (list (car l)) (cdr l))
		(return-failure (format nil "~S" c) () l)))

(defparser P-NOTCH (l c)
	(if (char= (car l) c)
		(return-failure (format nil "not ~S" c) () l)
		(return-success (list (car l)) (cdr l))))

(defparser P-CHAR-CLASS (l cl)
	(if (member (car l) cl :test #'char=)
		(return-success (list (car l)) (cdr l))
		(return-failure (format nil "in ~S" cl) () l)))

(defparser P-CHAR-NCLAS (l cl)
	(if (member (car l) cl :test #'char=)
		(return-failure (format nil "not in ~S" cl) () l)
		(return-success (list (car l)) (cdr l))))
		
(defun TEST ()
	(assert (equal
		(funcall
			(parse-group (p-digit-char) (p-digit-char) (p-alpha-char))
			(coerce "12a" 'list))
		'((RESULT #\1 #\2 #\a))))
	(assert (equal (funcall (parse-maybe (p-digit-char)) ()) '((RESULT))))
	(assert (equal
		(funcall
			(parse-many (parse-or (p-digit-char) (p-alpha-char)))
			(coerce "12a45bc6" 'list))
		`((RESULT ,@(coerce "12a45bc6" 'list)))))
	(assert (equal
		(funcall
			(parse-group (p-digit-char) (p-digit-char))
			'(#\1 #\a))
		`((FAILURE "digit" #\1) #\a)))
	(assert (equal
		(funcall
			(transform-result #'(lambda (r) (remove-if #'alpha-char-p r))
				(parse-many (parse-or (p-digit-char) (p-alpha-char))))
				(coerce "12a45bc6" 'list))
		`((RESULT ,@(coerce "12456" 'list)))))
	(assert (equal
		(funcall
			(transform-result #'(lambda (r) (list (coerce r 'string)))
				(parse-many (parse-or (p-digit-char) (p-alpha-char))))
				(coerce "12a45bc6" 'list))
		`((RESULT "12a45bc6"))))
	t)

(eval-when (:load-toplevel :execute) (test))
