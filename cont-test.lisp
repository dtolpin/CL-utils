(load "continuations.lisp")

(use-package "CONTINUATIONS")

(defun tree-iterator (tree)
	(let ((saved nil) (nodes nil))
		(=with-cont=
			(=defun node (tree)
				(etypecase tree
					(null (again))
					(cons (push #'(lambda () (node (car tree))) saved)
						(node (cdr tree)))
					(atom (=values tree))))
			(=defun again ()
				(if saved (funcall (pop saved)) nil))
			(=bind (node) (node tree)
				(when node (push node nodes) (again))))
	nodes))
