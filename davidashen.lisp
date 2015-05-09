; $Id: davidashen.lisp,v 1.3 2005/06/08 08:50:00 dvd Exp $

(defpackage "DAVIDASHEN"
	(:documentation "umbrella package for my utilities.")
	(:nicknames "DVD")
	(:use "CONTINUATIONS" "PROMISE" "MEMOIZE" "UCS")
	(:export
		"PROMISE-P" "DELAY" "FORCE"
		"=LAMBDA" "=DEFUN" "=BIND" "=WITH-CONT=" 
  	"=VALUES" "=FUNCALL" "=APPLY"
  	"MEMO" "DEFMEMO" "CLRMEMO"
  	"RUNE" "UTF-TO-UCS" "UCS-LENGTH" "UCS-TO-UTF" "UTF-LENGTH"
  	"RUNE-IN-RANGES" "UCS-RANGES"))