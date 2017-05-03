; To run the tests: $ clisp tests.lisp

(load "lisp-unit.lisp")
(use-package :lisp-unit)
(load "main.lisp")

(def-class pessoa nome idade)
(setq *p1* (make-pessoa :nome "joão" :idade 18))

(define-test class
	(assert-equal "pessoa" (get-obj-class *p1*))
)

(define-test getters
	(assert-equal "joão" (pessoa-nome *p1*))
	(assert-equal 18 (pessoa-idade *p1*))
)

(define-test setters
	(let ((p2 (make-pessoa :nome "maria" :idade 18)))
	(set-pessoa-nome p2 "ana")
	(set-pessoa-idade p2 25)
	
	(assert-equal "ana" (pessoa-nome p2))
	(assert-equal 25 (pessoa-idade p2))
	
))

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
