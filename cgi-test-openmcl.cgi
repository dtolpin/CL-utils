#!/bin/sh

/usr/local/bin/openmcl -norc -I /Users/dvd/Workplace/Davidashen/CL-UTILS/cgi.openmcl -b -e '

(progn (setf rfc2388:*make-tmp-file-name* 
	(let ((counter 0))
		(lambda () (format nil "/tmp/cgi-test-~S.tmp" (incf counter)))))

(let ((query (cgi:query)))
	(cond
		((equal (cgi:query-path-info query) "/image")
			(format t "Content-Type: ~A~%~%" 
				(cgi:mime-type (car (cgi:get-values "image" query))))
			(cgi:with-input-from-mime (inp (car (cgi:get-values "image" query)))
				(let ((buffer  (make-array 512 :element-type (quote character)))
					  (output (open "/tmp/image" :direction :output :if-exists :supersede)))
					(loop
						(let ((end (read-sequence buffer inp)))
							(when (zerop end) (return))
							(write-sequence buffer *standard-output* :end end))))))
		(t
			(format t "Content-Type: text/plain~%~%")
			(dolist (param (cgi:query-parameters query))
				(format t "~%name: ~A values:~{ ~S~}~%" (car param) 
					(mapcar (function cgi:get-value-string) (cdr param)))))))
(finish-output *standard-output*)
(quit))
'