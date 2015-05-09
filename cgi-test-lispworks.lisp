(load "/Users/dvd/.lispworks")
(mk:oos :cgi :Load)

(defun cgi-test ()
	(setf rfc2388:*make-tmp-file-name* 
		(let ((counter 0))
			(lambda () (format nil "/tmp/cgi-test-~A.tmp" (incf counter)))))

	(let ((query (cgi:query)))
		(cond
			((equal (cgi:query-path-info query) "/image")
				(format *standard-output* "Content-Type: ~A~%~%" 
					(cgi:mime-type (car (cgi:get-values "image" query))))
				(cgi:with-input-from-mime (mime-input (car (cgi:get-values "image" query))
						:external-format '(:latin-1 :eol-style :lf))
					(let ((buffer  (make-array 512 :element-type 'character)))
						(loop
							(let ((end (read-sequence buffer mime-input)))
								(when (zerop end) (return))
								(write-sequence buffer *standard-output* :end end))))))
			(t
				(format *standard-output* "Content-Type: text/plain~%~%")
				(dolist (param (cgi:query-parameters query))
					(format t "~%name: ~A values:~{ ~S~}~%" (car param) 
						(mapcar #'cgi:get-value-string (cdr param)))))))
		(finish-output *standard-output*))

(compile 'cgi-test)
(deliver 'cgi-test "cgi-test-lw.cgi" 1)
