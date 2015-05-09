; $Id: mandel.lisp,v 1.5 2005/05/01 19:56:07 dvd Exp $

(defpackage "MANDELBROT"
	(:documentation "Plots fractals in PostScript")
	(:use "COMMON-LISP")
	(:export "FIELD" "PLOT"))
(in-package "MANDELBROT")

(defun POINT (z c n) 
	"computes mandelbrot sets"
	(declare (type (complex single-float) z c) (type fixnum n))
	(cond
		((< 2.0 (abs z)) n)
		((zerop n) nil)
		(t (point (+ (* z z) c) c (1- n)))))

(defun FIELD (lb rt res n paint)
	"draws a field between lb and rt, calling paint c color on non-mandelbrot points"
	(let ((le (realpart lb)) (bo (imagpart lb)) (ri (realpart rt)) (to (imagpart rt)))
		(let ((sh (float (/ (- ri le) res))) (sv (float (/ (- to bo) res))))
			(do ((re le (+ re sh)))
				((<= ri re) nil)
				(do ((im bo (+ im sv)))
					((<= to im) nil)
					(let* ((c (complex re im)) (color (point #C(0e0 0e0) c n)))
						(when color (funcall paint c color))))))))

(defun PLOT (niter lb rt res outp)
	 (let (
			(colcomp (expt 2 (/ (floor (log niter 2)) 3)))
			(colscale (float (/ (- (realpart rt) (realpart lb)) res)))
			(dimscale (/ 600 (- (realpart rt) (realpart lb)))))
		(flet (
				(colcomp (color m)
					(do ((i 0 (1+ i)) (color (1- color) (/ color colcomp)))
						((= i m) (float (/ (mod color colcomp) colcomp))))))
			(format outp "~
	~A ~:*~A scale
	~A ~A translate 
	~A setlinewidth~%"
				dimscale
				(- (realpart lb)) (-  (imagpart lb))
				(/ (- (realpart rt) (realpart lb)) res))
			(field lb rt res niter
				#'(lambda (c color) 
					(format outp
						"~A ~A ~A setrgbcolor ~A ~A moveto~%~A ~:*~A rlineto stroke~%" 
						(colcomp color 0) (colcomp color 1) (colcomp color 2)
						(realpart c) (imagpart c)
						(* colscale (+ 0.5 (expt (/ color niter) 4))))))
			(format outp "showpage~%"))))