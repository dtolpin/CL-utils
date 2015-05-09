; $Id: urx-package.lisp,v 1.1 2005/03/08 23:43:56 dvd Exp $

(defpackage "URX"
	(:documentation "Unicode regular expressions")
	(:use "COMMON-LISP" "UCS")
	(:export "COMPILE-REGULAR-EXPRESSION" "STRING-MATCHES-P"))
