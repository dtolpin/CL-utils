(defpackage "CALCULATOR"
	(:documentation "simple calculator on top of LL-PARSING")
	(:export "CALCULATE")
	(:use "COMMON-LISP" "LL-PARSING"))
(in-package "CALCULATOR")

(defmacro scanner ()
	`(parse-many (parse-or	
		(transform-result #'(lambda (l) `(,(parse-integer (coerce l 'string))))
			(parse-many (p-digit-char)))
		(p-char-class '(#\+ #\- #\* #\/ #\( #\)))
		(transform-result #'(lambda (l) (declare (ignore l))) 
			(p-char-class '(#\Space #\Tab #\Newline))))))

(defun factor (l)
	(funcall 
		(parse-or (p-if #'numberp)
			(transform-result #'(lambda (l) `(,(cadr l)))
				(parse-group (p-char #\() #'expr (p-char #\)))))
		l))

(defun term (l)
	(funcall
		(transform-result #'(lambda (l) `(,(reduce #'(lambda (a b) (* a b)) l)))
			(parse-group #'factor
				(parse-some (parse-or
					(transform-result #'(lambda (l) `(,(cadr l)))
						(parse-group (p-char #\*) #'factor))
					(transform-result #'(lambda (l) `(,(/ 1 (cadr l))))
						(parse-group (p-char #\/) #'factor))))))
		l))

(defun expr (l)
	(funcall
		(transform-result #'(lambda (l) `(,(reduce #'(lambda (a b) (+ a b)) l)))
			(parse-group #'term
				(parse-some (parse-or
					(transform-result #'(lambda (l) `(,(cadr l)))
						(parse-group (p-char #\+) #'term))
					(transform-result #'(lambda (l) `(,(- (cadr l))))
						(parse-group (p-char #\-) #'term))))))
		l))

(defun calculate (s)
	(let ((tokens (funcall (scanner) (coerce s 'list))))
		(if (and (successp tokens) (null (rest tokens)))
			(let ((r (expr (result tokens))))
				(if (and (successp r) (null (rest r)))
					(format t "~A -> ~A" s (float (car (result r))))
					r))
			tokens)))
