; $Id: urx-all.lisp,v 1.2 2005/03/08 23:43:56 dvd Exp $

(defconstant urx-files '(
		"promise" 
		"ucs-package" "ucs" "ucs-ranges"
		"urx-package" "urx" "urx-xml-ranges"))

(let ((*default-pathname-defaults*
			(make-pathname :type nil :defaults *load-pathname*)))
	(defun LOAD-URX ()
		(mapc #'load urx-files))
	
	(defun COMPILE-URX ()
		(load-urx)
		(mapc #'compile-file urx-files)))

(load-urx)
