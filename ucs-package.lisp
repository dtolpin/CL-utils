; $Id: ucs-package.lisp,v 1.5 2005/06/08 08:50:00 dvd Exp $

(defpackage "UCS"
	(:documentation "Unicode utilities")
	(:use "COMMON-LISP")
	(:export "RUNE" "UTF-TO-UCS" "UCS-LENGTH" "UCS-TO-UTF" "UTF-LENGTH"
		"RUNE-IN-RANGES" "UCS-RANGES"))
