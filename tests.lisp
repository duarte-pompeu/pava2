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
	(let ((p (make-pessoa :nome "maria" :idade 18)))
	(set-pessoa-nome p "ana")
	(set-pessoa-idade p 25)
	
	(assert-equal "ana" (pessoa-nome p))
	(assert-equal 25 (pessoa-idade p))
))

(define-test herança
	(def-class (estudante pessoa) curso)
	(make-estudante :nome "abc" :idade 25 :curso "leic")
)

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
