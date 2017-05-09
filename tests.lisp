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
	(let ((e (make-estudante :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (estudante-nome e))
	(assert-equal 25 (estudante-idade e))
	(assert-equal "leic" (estudante-curso e))
))

(define-test herança-duplicados
	(def-class (aluno pessoa) nome curso)
	(let ((a (make-aluno :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (aluno-nome a))
	(assert-equal 25 (aluno-idade a))
	(assert-equal "leic" (aluno-curso a))
))

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
