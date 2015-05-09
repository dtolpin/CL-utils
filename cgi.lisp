; $Id: cgi.lisp,v 1.20 2005/06/21 15:58:24 dvd Exp $

(defpackage "CGI"
	(:documentation "Utilities for use by CGI scripts; intended to be simple.
		See RFC 3875 for the authoritative description of CGI 1.1, RFC 2388 for
		returning values from forms in multipart/form-data responses.
		
		A new query is created with a call to (query :input input :getenv getenv).
		input is *standard-input* if omitted, if nil, an empty query is created.
		Getenv should be able to retrieve environment variables. The query holds
		an association list of parameters in query-parameters, path-info in
		query-path-info.
		
		During the parsing, condition of type cgi-error signals non-conforming input
		or environment.

		To retrieve values for a particular name, use (get-values name query). If a
		value in the list is a MIME, then the data from it can be read with 
		(make-mime-input-stream value), or (with-input-from-mime (input value) ... ).
		Other available MIME fields are mime-type and mime-filename. Polymorphic 
		get-value-string converts any value to a string.
		
		Example:
		
		(let* ((query (query))
					(fo-source (car (get-values \"fo-source\" query))))
				(cond
					((mime-p fo-source)
						(with-input-from-mime (input fo-source)
							(handler-case
								(loop
									(let ((line (read-line input)))
										(princ line)
										(terpri)))
								(end-of-file ()))))
					((stringp fo-source)
						(princ fo-source))
					(t)))
		")
	(:use "COMMON-LISP")
	(:export 
		"*CONTENT-LENGTH-LIMIT*"
		"CGI-ERROR"
		"QUERY" "QUERY-P" "QUERY-PATH-INFO" "QUERY-PARAMETERS"
		"GET-VALUES" "GET-VALUE-STRING"
		"MIME" "MIME-P" "MIME-TYPE" "MIME-FILENAME" "WITH-INPUT-FROM-MIME"))
(in-package "CGI")

(defparameter *content-length-limit* nil
	"maximum Content-Length for post requests; nil is no limit.
	if the request exceeds the maximum length, an error is reported.")

(define-condition cgi-error (simple-error) ()
	(:documentation "condition signalled by the query parser"))

(defstruct QUERY
	"CGI query data. Parameters is an alist of parameters, 
	car is the name (string), cdr is a list of values.
	Values are either strings or MIMEs."
	(path-info nil :type (or string null))
	(parameters nil :type list))

(defstruct MIME
	"Uploaded file."
	(type "text/plain" :type string :read-only t)
	(filename nil :type (or null string) :read-only t)
  (data nil :type (or null string pathname)))

(defun MAKE-MIME-INPUT-STREAM (mime &rest modifiers)
	"returns a stream to read mime data from"
	(etypecase (mime-data mime)
		(string (make-string-input-stream (mime-data mime)))
		(pathname (apply #'open (mime-data mime) :direction :input modifiers))
		(null)))

(defmacro WITH-INPUT-FROM-MIME ((inp mime &rest modifiers) &body body)
	"executes body in environment where inp is bound to input stream on MIME data,
	then closes the stream. Similar to with-input-from-string."
	`(let ((,inp (make-mime-input-stream ,mime ,@modifiers)))
		(unwind-protect (progn ,@body)
			(close ,inp)
			(when (pathnamep (mime-data ,mime))
				(delete-file (mime-data ,mime))
				(setf (mime-data ,mime) nil)))))

(defun GET-VALUES (name query)
	"returns a list of values for name"
	(cdr (assoc name (query-parameters query) :test #'equal)))

(defun GET-VALUE-STRING (value)
	"retrieves string from a parameter's value"
	(etypecase value
		((or string null) value)
		(mime
			(etypecase (mime-data value) 
				((or string null)	(mime-data value))
				(pathname
					(setf (mime-data value)
						(with-input-from-mime (inp value) (with-output-to-string (outp)
							(let ((buffer (make-array 512 :element-type 'character)))
								(loop (let ((end (read-sequence buffer inp)))
									(when (zerop end) (return))
									(write-sequence buffer outp :end end))))))))))))

(defun (SETF GET-VALUES) (values name query)
	"assings a new list of values to the named slot, returns the list of values"
	(let ((entry (assoc name (query-parameters query) :test #'equal)))
		(if entry
			(setf (cdr entry) values)
			(push (cons name values) (query-parameters query))))
	values) 

(defmacro CGI-ERROR (format-control &rest format-arguments)
	`(error 'cgi-error
		:format-control ,format-control 
		:format-arguments (list ,@format-arguments)))

(defun READ-URLENCODED (input content-length)
	"reads x-www-form-urlencoded query and returns as string"
	(unless content-length
		(cgi-error "CONTENT_LENGTH must be in environment"))
	(when (and *content-length-limit* (> content-length *content-length-limit*))
		(cgi-error "Content-Length ~D exceeds the limit of ~D"
			content-length *content-length-limit*))
	(let ((query-string (make-array content-length :element-type 'character)))
		(read-sequence query-string input :start 0 :end content-length)
		query-string))

(defun URLDECODE (encoded start end)
	"decodes URLencoded strings: #\+ -> #\Space, \"%XY\" -> (code->char X*16+y)"
	(let* (
			(length
				(do ((i start (1+ i)) (j 0 (1+ j)))
					((>= i end)
						(when (/= i end) (cgi-error "~S is not properly urlencoded" encoded))
						j)
					(when (char= (char encoded i) #\%) (incf i 2))))
			(decoded (make-array length :element-type 'character)))
		(do ((i start (1+ i)) (j 0 (1+ j)))
			((= j length) decoded)
			(case (char encoded i)
				(#\+ (setf (char decoded j) #\Space))
				(#\% 
					(setf (char decoded j)
						(handler-case
								(code-char
									(parse-integer encoded :start (+ i 1) :end (+ i 3) :radix 16))
							(error () (cgi-error "~S is not properly urlencoded"))))
					(incf i 2))
				(t (setf (char decoded j) (char encoded i)))))))

(defun PARSE-MEDIA-TYPE (unparsed) 
	"parses media-type; returns string."
	(when unparsed
		(string-downcase
			(remove-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return)))
				(let ((options-offset (position #\; unparsed)))
					(if options-offset (subseq unparsed 0 options-offset) unparsed))))))
 
(defun PARSE-CONTENT-LENGTH (unparsed)
	"parses content-length as read from environment.
	Ensures that content-length is non-null and integer.
	Returns integer."
	(unless unparsed (cgi-error "CONTENT_LENGTH must be in the environment"))
	(handler-case
			(parse-integer unparsed)
		(error () (cgi-error "CONTENT_LENGTH must be an integer, not ~S" unparsed))))

(defun PARSE-QUERY-STRING (query-string &optional (parameters nil) (field-offset 0))
	"Parses HTTP query string, returns alist."
  (check-type query-string (or null string))
  (let (
  		(field-length
  			(or (position-if (lambda (c) (member c '(#\& #\;)))
  					query-string :start field-offset)
  				(length query-string))))
		(let ((= (position #\= query-string :start field-offset :end field-length)))
			(when =
				(let ((name (urldecode query-string field-offset =)) 
							(value (urldecode query-string (1+ =) field-length)))
					(let ((value (if (> (length value) 0) value nil))
								(entry (assoc name parameters :test #'equal)))
						(if entry (push value (cdr entry))
							(push (list name value) parameters))))))
		(if (= (length query-string) field-length) parameters
  		(parse-query-string query-string parameters (1+ field-length)))))

(defun PARSE-MULTIPART (input content-type parameters)
	"Parses headers and body of a multipars query, returns parameters;
	content-type should hold value of Content-Type: header, normally passed
	in CONTENT_TYPE environment variable."
	(let ((boundary (cdr (rfc2388:find-parameter "BOUNDARY"
					(rfc2388:header-parameters (rfc2388:parse-header content-type :value))))))
		(unless boundary
			(cgi-error "missing boundary in multipart/form-data header"))
		(dolist (part (rfc2388:parse-mime input boundary) parameters)
			(let ((content-disposition
							(rfc2388:header-parameters
								(rfc2388:find-content-disposition-header
									(rfc2388:mime-part-headers part)))))
				(when content-disposition
					(let (
							(name (cdr (rfc2388:find-parameter "NAME" content-disposition)))
							(type (rfc2388:content-type part :as-string t))
							(filename (cdr (rfc2388:find-parameter "FILENAME" content-disposition)))
							(contents (rfc2388:mime-part-contents part)))
						(let (
								(entry (assoc name parameters :test #'equal))
								(value (if filename
									(make-mime :type type :filename filename :data contents)
									contents)))
							(if entry (push value (cdr entry))
								(push (list name value) parameters)))))))))

(defun QUERY (&key (input nil input-provided) (getenv #'port:getenv))
	"Parses CGI query from the input stream; or, if nil is
	passed explicitly, creates an empty query -- good for 
	testing and placeholders. getenv is a parameter to facilitate
	passing in a non-default system environment, such as passed
	in from FastCGI."
	(check-type input (or null stream))
	(unless input-provided
		(setf input *standard-input*))
	(cond
		(input
			(make-query
				:path-info (funcall getenv "PATH_INFO")
				:parameters
					(let (
							(request-method (funcall getenv "REQUEST_METHOD"))
							(content-type (funcall getenv "CONTENT_TYPE"))
							(parameters (parse-query-string (funcall getenv "QUERY_STRING"))))
						(cond
							((null request-method)
								(cgi-error "REQUEST_METHOD must be in the environment"))
							((string= request-method "GET")
								parameters)
							((string= request-method "POST")
								(let ((media-type (parse-media-type content-type)))
									(cond
										((null media-type)
											(cgi-error "missing CONTENT_TYPE in POST request"))
										((string= media-type "application/x-www-form-urlencoded")
											(parse-query-string
												(read-urlencoded input (parse-content-length
													(funcall getenv "CONTENT_LENGTH")))
												parameters))
										((string= media-type "multipart/form-data")
											(parse-multipart input content-type parameters))
										(t
											(cgi-error "illegal CONTENT_TYPE ~S in POST request"
												media-type)))))
							(t (cgi-error "illegal REQUEST_METHOD ~S" request-method))))))
				(t (make-query))))

(defmacro ASSERT-CGI-ERROR (&body body)
	"asserts that body signals cgi-error"
	`(handler-case (progn ,@body (assert nil () "~A: ~{~S ~}" test-group ',body)) 
		(cgi-error ())))

(defmacro ASSERT-SUCCESS (&body body)
	"asserts that body returns non-nil value"
	`(assert (progn ,@body) () "~A: ~{~S ~}" test-group ',body))

(defun TEST ()
	"unit tests for query parser"
	(let ((test-group "parse-content-length"))
		(assert-cgi-error (parse-content-length nil))
		(assert-cgi-error (parse-content-length "abc"))
		(assert-success (= (parse-content-length "0") 0))
		(assert-success (= (parse-content-length "12345") 12345)))

	(let ((test-group "parse-query-string"))
		(assert-success (equal (parse-query-string "a=b") '(("a" "b"))))
		(assert-success (equal (parse-query-string "a=b+c") '(("a" "b c"))))
		(assert-success (equal (parse-query-string "a=b&a=c") '(("a" "c" "b"))))
		(assert-success (equal (parse-query-string "abcd&efgh") '()))
		(assert-success (equal (parse-query-string "%3a=%2A") '((":" "*"))))
		(assert-success (equal (parse-query-string "name%3aspace=%2Av+a+l+u+e%2a")
			'(("name:space" "*v a l u e*"))))
		(assert-success (equal (parse-query-string "ab=cd&ef=gh")
			'(("ef" "gh") ("ab" "cd"))))
		(assert-cgi-error (parse-query-string "%AG=de"))
		(assert-cgi-error (parse-query-string "a=%A")))
	
	(let ((test-group "parse-header-value"))
		(assert-success (equal (parse-media-type nil) '()))
		(assert-success (equal (parse-media-type "x/Y") "x/y"))
		(assert-success (equal (parse-media-type " x /  Y  ; x=y") "x/y")))
	
	(let ((test-group "query")
			(env (make-hash-table :test #'equal)))
		(flet ((getenv (name) (gethash name env)))
			(assert-success
				(setf (gethash "QUERY_STRING" env) "a=b&a=c&c=d")
				(setf (gethash "REQUEST_METHOD" env) "GET")
				(equalp 
					(query :getenv #'getenv) 
					(make-query :path-info nil :parameters '(("c" "d") ("a" "c" "b")))))
			(assert-success
				(setf (gethash "QUERY_STRING" env) "a=b")
				(setf (gethash "REQUEST_METHOD" env) "POST")
				(setf (gethash "CONTENT_LENGTH" env) "7")
				(setf (gethash "CONTENT_TYPE" env) "application/x-www-form-urlencoded")
				(with-input-from-string (inp "a=c&c=d")
					(equalp
						(query :input inp :getenv #'getenv) 
						(make-query :path-info nil :parameters '(("c" "d") ("a" "c" "b"))))))
			t))
	t)

(eval-when (:load-toplevel :execute)
	(test))
