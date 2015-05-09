;; -*- encoding: utf-8; -*- ; for Lispworks - change to :latin-1 when :UNICODE is off
; $Id: ucs.lisp,v 1.18 2005/11/09 09:02:45 dvd Exp $

(in-package "UCS")

(deftype RUNE (&optional (width 16))
	"unicode codepoint"
	`(unsigned-byte ,width))

#- :UNICODE
(progn ; macros and functions to parse and write UTF-8
	(defmacro UX-DISPATCH (&rest args)
		"the UTF-8 FSA, used by both UTF-TO-UCS and UCS-LENGTH"
		`(macrolet (
				(UX-MASK (args mask length)
					`(ux ,@args (rem c ,mask) i ,length)))
			(let ((c (char-code (char s i))) (i (1+ i)))
				(cond
					((< c #x80) (ux ,@args c i 0))
					((< c #xC0) (error "invalid UTF8 byte ~D" c))
					((< c #xE0) (ux-mask ,args #x20 1))
					((< c #xF0) (ux-mask ,args #x10 2))
					((< c #xF8) (ux-mask ,args #x08 3))
					((< c #xFC) (ux-mask ,args #x04 4))
					((< c #xFE) (ux-mask ,args #x02 5))
					(t (error "invalid UTF8 byte ~D" c))))))
	
	(defmacro UX-ADVANCE (&rest args)
		`(ux ,@args (+ (* c 64) (rem (char-code (char s i)) 64)) 
			(1+ i) (1- n)))

	(defun VX-MASK (c i)
		(declare (type rune c) (type fixnum i))
		"mask for trailing bits in UTF-8" 
		(code-char (logior #x80 (logand #x3F (ash c (* -6 i))))))
	
	(defun VX-LENGTH (c)
		(declare (type rune c))
		(cond
			((< c #x80) 0)
			((< c #x800) 1)
			((< c #x10000) 2)
			((< c #x200000) 3)
			((< c #x4000000) 4)
			((< c #x80000000) 5)
			(t (error "invalid unicode ~D" c)))))

#- :UNICODE
(defun UTF-TO-UCS (s &key (start 0) end)
	(declare (type string s) (type fixnum start) (type (or null fixnum) end))
  "converts a UTF8-encoded string to a vector of UCS codes"
  (when (null end) (setf end (length s)))
	(labels (
			(UX (v c i n)
				(declare (type (vector rune) v) (type rune c) (type fixnum i n))
				(if (zerop n) (left (progn (vector-push-extend c v) v) i)
					(ux-advance v)))
			(LEFT (v i)
				(declare (type (vector rune) v) (type fixnum i))
				(if (>= i end) v
					(ux-dispatch v))))
		(left 
			(make-array (list (- end start)) :element-type 'rune 
				:fill-pointer 0 :adjustable t)
			start)))

#+ :UNICODE
(defun UTF-TO-UCS (s &key (start 0) end)
	(declare (type string s) (type fixnum start) (type (or null fixnum) end))
  "converts a UTF8-encoded string to a vector of UCS codes;
  for Unicode-enabled implementations, assumes that characters
  are already in Unicode and just copies them one-by-one to (array rune)"
  (when (null end) (setf end (length s)))
	(let ((v (make-array (list (- end start)) :element-type 'rune)))
		(dotimes (i (- end start) v)
			(setf (aref v i) (char-code (char s (+ i start)))))))

#- :UNICODE
(defun UCS-LENGTH (s &key (start 0) end)
	(declare (type string s) (type fixnum start) (type (or null fixnum) end))
	"computes the number of Unicode characters in a UTF-8-encoded string"
  (when (null end) (setf end (length s)))
	(labels (
			(UX (len c i n)
				(declare (type rune c) (type fixnum len i n))
				(if (zerop n) (left (1+ len) i)
					(ux-advance len)))
			(LEFT (len i)
				(if (>= i end) len
					(ux-dispatch len))))
		(left 0 start)))

#+ :UNICODE
(defun UCS-LENGTH (s &key (start 0) end)
	(declare (type string s) (type fixnum start) (type (or null fixnum) end))
	"computes the number of Unicode characters in a UTF-8-encoded string"
  (when (null end) (setf end (length s)))
  (- end start))
  
#- :UNICODE
(defun UCS-TO-UTF (v &key (start 0) end)
	(declare (type vector v) (type fixnum start) (type (or fixnum null) end))
	"encodes a rune array in UTF-8"
	(when (null end) (setf end (length v)))
	(let (
			(s (make-array (- end start) :element-type 'character 
				:fill-pointer 0 :adjustable t)))
		(do ((i start (1+ i)))
			((= i end) s)
			(let* ((c (aref v i)) (n (vx-length c)))
				(vector-push-extend (code-char
					(ecase n
						(0 c)
						(1 (logior #xC0 (ash c -6)))
						(2 (logior #xE0 (ash c -12)))
						(3 (logior #xF0 (ash c -18)))
						(4 (logior #xF8 (ash c -24)))
						(5 (logior #xFC (ash c -30)))))
					s)
				(dotimes (i n) (vector-push-extend (vx-mask c (- n i 1)) s))))))

#+ :UNICODE
(defun UCS-TO-UTF (v &key (start 0) end)
	(declare (type vector v) (type fixnum start) (type (or fixnum null) end))
	"encodes a rune array in UTF; for UNICODE-enabled implementation, 
	just copies runes to characters"
	(when (null end) (setf end (length v)))
	(let ((s (make-array (- end start) :element-type 'character)))
		(do ((i start (1+ i)))
			((= i end) s)
			(setf (char s (- i start)) (code-char (aref v i))))))

#- :UNICODE
(defun UTF-LENGTH (v &key (start 0) end)
	(declare (type vector v) (type fixnum start) (type (or fixnum null) end))
	"determines UTF length of a rune vector"
	(when (null end) (setf end (length v)))
	(do ((i start (1+ i)) (length 0 (1+ length)))
		((= i end) length)
		(incf length (vx-length (aref v i)))))

#+ :UNICODE
(defun UTF-LENGTH (v &key (start 0) end)
	(declare (type vector v) (type fixnum start) (type (or fixnum null) end))
	"determines UTF length of a rune vector;
	for UNICODE-enabled implementations, returns the number of runes"
	(when (null end) (setf end (length v)))
	(- end start))

(defun RUNE-IN-RANGES (u rs)
	(declare (type rune u) (type simple-vector rs))
  "searches for the unicode value in ordered vector of ranges and points"
	(labels (
			(BETWEEN (n m)
				(declare (type fixnum n m))
				(if (> n m) nil
					(let* ((i (floor (+ n m) 2)) (r (svref rs i)))
						(declare (type (or rune (cons rune rune)) r))
						(cond
							((atom r) 
								(cond
									((< u r) (between n (1- i)))
									((> u r) (between (1+ i) m))
									(t r)))
							((< u (car r)) (between n (1- i)))
							((> u (cdr r)) (between (1+ i) m))
							(t r))))))
		(between 0 (1- (length rs)))))

(defun TEST ()
  "self-test"			
  (assert (equalp (utf-to-ucs "") #()))
  (assert (equalp (utf-to-ucs "Давид") #(1044 1072 1074 1080 1076)))
  (assert (equalp (utf-to-ucs "David") #(68 97 118 105 100)))
  
  (assert (= (ucs-length "") 0))
  (assert (= (ucs-length "Դավիդ") 5))
  (assert (= (ucs-length "David") 5))

	(assert (equalp (ucs-to-utf (utf-to-ucs "Դավիդ")) "Դավիդ"))
	(assert (equalp (ucs-to-utf (utf-to-ucs "David")) "David"))
	(assert
		(equalp (ucs-to-utf (utf-to-ucs "Dавиd" :start 1 :end (1- (length "Dавиd"))))
		"ави"))
	(assert
		(equalp (ucs-to-utf (utf-to-ucs "Dавиd") :start 1 :end (1- (ucs-length "Dавиd")))
		"ави"))
	
	(assert (= (utf-length (utf-to-ucs "David")) (length "David")))
	(assert (= (utf-length (utf-to-ucs "Давид")) (length "Давид")))
  

	(mapc (lambda (x) (assert (rune-in-ranges 1 x)))
		'(#(1) #(0 1) #(0 1 2) #(1 2) #((0 . 1)) #((0 . 2)) #((1 . 2))))
	(mapc (lambda (x) (assert (not (rune-in-ranges 1 x))))
		'(#() #(0) #(2) #(0 2) #((0 . 0)) #((2 . 2))))
	t)

(eval-when (:execute :load-toplevel)
	(test))
