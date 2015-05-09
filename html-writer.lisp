; $Id: html-writer.lisp,v 1.13 2005/09/15 20:45:45 dvd Exp $

(defpackage "HTML-WRITER"
	(:nicknames "HTML")
	(:documentation "HTML serialization utilities. The basic structure is a list
	with 
	  - element's head, 
	    null for spliced contest, 
	    or function for external serializer as car, 
	  - lists or strings in cdr.
	The serializer prints the structure as well-formed XHTML, indenting the content.")
	(:use "COMMON-LISP")
	(:export 
		"SERIALIZE"
		"ESCAPE-AVALUE" "ESCAPE-CDATA"
		"*INLINE-TAGS*" "*VERBATIM-TAGS*" "*ROW-CDATA-TAGS*" "*COLLAPSE-TAGS*"
		"LEVEL"))
(in-package "HTML-WRITER")

(defparameter *inline-tags* '("a" "input" "span")
	"list of tags formatted inline")
(defparameter *verbatim-tags* '("textarea" "pre" "verbatim" "script")
	"list of tags which contents should not be indented")
(defparameter *collapse-tags* '("br" "hr" "link" "input" "option" "textarea"))
(defparameter *raw-cdata-tags* '("script")
	"tags where cdata is not escaped") 

(defvar *level* 0 "current indent level, for external serializers")

(defun LEVEL ()
	"returns current indent level,
	for use in external serializers"
	*level*)

(defun SERIALIZE (output html
		&key (level 0) (indent-text t) (escape-cdata t) (collapse 'some))
	"writes html to output stream;
	- level sets initial indentation level
	- indent-text controls whether characted data is indented
	- escape-cdata triggers escaping of cdata (useful for JavaScript)
	- collapse allows short notation for empty elements (<a/>), if it is a list,
	  then the tag should belong to the list, otherwise tags are collapsed
	  if collapse is a true value.
	
	The HTML tree is a list with nil, string or function 
	as the first elements, list of nodes as rest. 
	* If the head is null, the rest is serialized at the current level. 
	* If the head is a string, it is the element's name followed by whitespace,
	  followed by attributes, the rest is serialized as children of the element.
	* If the head is a cons, it is in the form (name (attr-name attr-value)*).
	* If the head is a function, it is invoked as (funcall function output rest);
	  the indent level on entry to the function is available through 
	  a call to (LEVEL).
	  
	There are two ways to define elements with attribute:
		((\"a\" (\"href\" \"http://davidashen.net/\")) \"Things from David Tolpin\")
	or just
		(\"a href=\\\"http://davidashen.net/\\\"\" \"Things from David Tolpin\")
	the latter form is often more convenient, especially when attribute names
	are generated using format."
	(declare (special indent-text escape-cdata))
	(when (eq output t)
		(setf output *standard-output*))
	(when (eq collapse 'some)
		(setf collapse *collapse-tags*))
	(labels (
			(INDENT (level) (dotimes (i level) (princ #\Tab output)))
			(SERIALIZE (html level)
				"performs actual recursive serialization."
				(etypecase html
					(null)
					(string
						(let ((text (if escape-cdata (escape-cdata html) html)))
							(cond 
								(indent-text
									(do ((i 0 (1+ i)) (i0 0))
										((= i (length text)) (princ (subseq text i0) output))
										(when (char= (char text i) #\Newline)
											(princ (subseq text i0 i) output)
											(terpri output) (indent level) 
											(setf i0 (1+ i)))))
								(t (princ text output)))))
					(cons
						(let ((head (car html)) (tail (cdr html)))
							(flet (
									(NODE (name head)
										(let ((inline (member name *inline-tags* :test #'equal)))
											(unless inline
												(fresh-line output) (indent level))
											(cond 
												(tail
													(format output "<~A>" head)
													(let ((sublevel (1+ level))
															(indent-text
																(and indent-text 
																	(not (member name *verbatim-tags*
																		:test #'equal))))
															(escape-cdata
																(and escape-cdata
																	(not (member name *raw-cdata-tags*
																	 	:test #'equal)))))
														(declare (special indent-text escape-cdata))
														(mapc
															#'(lambda (tree) (serialize tree sublevel))
															tail))
													(format output "</~A>" name))
												(t
													(if (or (eq t collapse) (member head collapse :test #'equal))
														(format output "<~A/>" head)
														(format output "<~A></~A>" head name)))))))
							(etypecase head
								(null
									(mapc
										#'(lambda (tree) (serialize tree level))
										tail))
								(string 
									(node (subseq head 0 (position #\Space head)) head))
								(cons
									(node (car head) 
										(format nil "~A~{ ~A~}" (car head)
											(mapcar
												#'(lambda (attr)
													(format nil "~A=\"~A\""
														(car attr) (escape-avalue (cadr attr))))
												(cdr head)))))
								(function (let ((*level* level)) (apply head output tail))))))))))
		(serialize html level)))

(defmacro ESCAPE-STRING (raw &rest escape-cases)
	"wrapper around escape-string-if, escape-cases are case clauses
	with special character in car, escaped representation in cadr"
	`(escape-string-if ,raw #'(lambda (c) (case c ,@escape-cases))))
	
(defun ESCAPE-STRING-IF (raw escaped-char)
	"escape special characters, escaped-char returns escaped
	representation if special, nil otherwise"
	(let* (
			(virgin t)
			(escaped-length
				(do ((i 0 (1+ i)) (length 0))
					((= i (length raw)) length)
					(incf length
						(let ((esc (funcall escaped-char (char raw i))))
							(cond
								(esc (setf virgin nil) (length esc))
								(t 1)))))))
		(if virgin raw
			(let ((escaped (make-string escaped-length)))
				(do ((i 0 (1+ i)) (j 0))
					((= i (length raw)) escaped)
					(let* ((c (char raw i)) (esc (funcall escaped-char c)))
						(cond
							(esc (replace escaped esc :start1 j) (incf j (length esc)))
							(t (setf (char escaped j) c) (incf j)))))))))

(defun ESCAPE-AVALUE (value)
	"escape special characters in attribute value, returns value"
	(escape-string value 
		(#\< "&lt;")
		(#\& "&amp;")
		(#\> "&gt;")
		(#\' "&apos;")
		(#\" "&quot;")))

(defun ESCAPE-CDATA (data)
  "escapes special characters in character data, returns data"
	(escape-string data
		(#\< "&lt;")
		(#\& "&amp;")
		(#\> "&gt;")))

(defun test ()
	(assert (string= (escape-avalue "") ""))
	(assert (string= (escape-avalue "abc") "abc"))
	(assert (string= (escape-avalue "\"a\"b") "&quot;a&quot;b"))
	(assert (string= (escape-avalue "<>") "&lt;&gt;"))
	(assert (string= (escape-cdata "") ""))
	(assert (string= (escape-cdata "&amp;") "&amp;amp;"))
	(assert (string= (escape-cdata ">abc<def>g") "&gt;abc&lt;def&gt;g"))
	
	(flet (
			(EXPECT (doc html)
				(with-output-to-string (outp)
					(serialize outp doc :collapse t)
					(let ((got (get-output-stream-string outp)))
						(assert (string= got html)
							() "~S expands to ~S and not to ~S" doc got html)))))
		(expect '("x") "<x/>")
		(expect '(("x" ("a" "b")  ("c" "d"))) "<x a=\"b\" c=\"d\"/>")
		(expect '("x" nil) "<x></x>")
		(expect '("x" ("y"))
"<x>
	<y/></x>")
		(expect '("x" (nil "y")) "<x>y</x>")
		(expect '("x" (nil "y
z")) 
"<x>y
	z</x>")
	(expect '("x a='b'") "<x a='b'/>")
	(expect
		(list #'(lambda (outp &rest x) (princ (apply #'concatenate 'string x) outp))
			"<" "a" "/>")
		"<a/>")
	(expect
		(list "x" (list #'serialize '("y")))
"<x>
<y></y></x>")
	(expect
		(list "x"
			(list
				(lambda (outp &rest x)
					(apply #'serialize outp (append x (list :level (level) :collapse t))))
					'("y")))
"<x>
	<y/></x>")
	(expect
		(list #'(lambda (outp &rest x) (princ (apply #'concatenate 'string x) outp))
			"<" "a" "/>")
		"<a/>")))

(eval-when (:execute :load-toplevel)
	(test))

