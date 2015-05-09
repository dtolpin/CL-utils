; $Id: realmandel.lisp,v 1.1 2005/05/01 19:56:07 dvd Exp $

(defpackage "MANDELBROT"
	(:documentation "Plots fractals in PostScript")
	(:use "COMMON-LISP")
	(:export "FIELD" "PLOT"))
(in-package "MANDELBROT")

(defun POINT (re im cre cim n) 
	"computes mandelbrot sets"
	(declare (type single-float re im cre cim) (type fixnum n))
	(let ((imim (* im im)) (rere (* re re)) (reim (* re im)))
		(declare (type (single-float) imim rere reim))
		(cond
			((< 4.0 (+ imim rere)) n)
			((= n 0) nil)
			(t (point (+ (- rere imim) cre) (+ reim reim cim) cre cim (1- n))))))

(defun FIELD (lb rt res n paint)
	"draws a field between lb and rt, calling paint c color on non-mandelbrot points"
	(let ((le (realpart lb)) (bo (imagpart lb)) (ri (realpart rt)) (to (imagpart rt)))
		(let ((sh (float (/ (- ri le) res))) (sv (float (/ (- to bo) res))))
			(do ((re le (+ re sh)))
				((<= ri re) nil)
				(do ((im bo (+ im sv)))
					((<= to im) nil)
					(let* ((color (point 0e0 0e0 re im n)))
						(when color (funcall paint re im color))))))))

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
				#'(lambda (re im color) 
					(format outp
						"~A ~A ~A setrgbcolor ~A ~A moveto~%~A ~:*~A rlineto stroke~%" 
						(colcomp color 0) (colcomp color 1) (colcomp color 2)
						re im
						(* colscale (+ 0.5 (expt (/ color niter) 4))))))
			(format outp "showpage~%"))))