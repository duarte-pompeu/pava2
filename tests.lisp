(load "lisp-unit.lisp")
(use-package :lisp-unit)
(load "main.lisp")

(setq *p1* (make-pessoa "joão" 18))

(define-test class
	(assert-equal "pessoa" (get-obj-class *p1*))
)

(define-test getters
	(let* ((p1 (make-pessoa "joao" 18)))
	(assert-equal "joão" (pessoa-nome *p1*))
	(assert-equal 18 (pessoa-idade *p1*))
))

(setq *print-failures* t)
(run-tests :all)
		
