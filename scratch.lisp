(mk:oos :ll-parsing :load)
(mk:oos :continuations :load)
(mk:oos :html-writer :load)

(cd "/Users/dvd/Workplace/Davidashen/CL-UTILS")

(progn (compile-file "mandel.lisp") (load "mandel") (use-package "MANDELBROT"))
(progn (compile-file "realmandel.lisp") (load "mandel") (use-package "MANDELBROT")) ; does not use complex numbers in POINT

(time (let* (
		(niter 999)
		(lb #C(0.1343500e0 0.6331000e0))
		(rt #C(0.1344125e0 0.6331625e0))
		(res 200))
	(with-open-file (outp "fractal.ps" :if-exists :supersede :direction :output)
		(plot niter lb rt res outp))))
		
(time (let* (
		(niter 1024)
		(lb #C(-2.2e0 -1.5e0))
		(rt #C(0.8e0 1.5e0))
		(res 400))
	(with-open-file (outp "fractal.ps" :if-exists :supersede :direction :output)
		(plot niter lb rt res outp))))

;;; MEMOIZE

(progn (compile-file "memoize.lisp") (load "memoize") (use-package "MEMOIZE"))

(defun fib (n)
	(declare (notinline fib))
	(check-type n (integer 1 *) "fib's argument must be a positive integer")
	(cond
		((= n 1) 1)
		((= n 2) 1)
		(t (+ (fib (- n 1)) (fib (- n 2))))))

(defmemo fib (n) ()
	(check-type n (integer 1 *) "fib's argument must be a positive integer")
	(cond
		((= n 1) 1)
		((= n 2) 1)
		(t (+ (fib (- n 1)) (fib (- n 2))))))
