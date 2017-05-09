; To run the tests: $ clisp tests.lisp

(load "lisp-unit.lisp")
(use-package :lisp-unit)
(load "main.lisp")

(def-class pessoa nome idade)
(setq *p1* (make-pessoa :nome "joão" :idade 18))

(define-test class
	"classes"

	(assert-equal "pessoa" (get-obj-class *p1*))
)

(define-test getters
	"getters"

	(assert-equal "joão" (pessoa-nome *p1*))
	(assert-equal 18 (pessoa-idade *p1*))
)

(define-test setters
	"setters"

	(let ((p (make-pessoa :nome "maria" :idade 18)))
	(set-pessoa-nome p "ana")
	(set-pessoa-idade p 25)
	
	(assert-equal "ana" (pessoa-nome p))
	(assert-equal 25 (pessoa-idade p))
))

(define-test herança
	"herança simples"

	(def-class (estudante pessoa) curso)
	(let ((e (make-estudante :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (estudante-nome e))
	(assert-equal 25 (estudante-idade e))
	(assert-equal "leic" (estudante-curso e))
))

(define-test herança-duplicados
	"teste para herança simples, atributos duplicados"

	(def-class (aluno pessoa) nome curso)
	(let ((a (make-aluno :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (aluno-nome a))
	(assert-equal 25 (aluno-idade a))
	(assert-equal "leic" (aluno-curso a))
))

(def-class a x)
(def-class b y)
(define-test herança-multipla
	"teste para herança multipla, atributos distintos"

	(def-class (c a b) z)
	(let ((i (make-c :x 1 :y 2 :z 3)))

	(assert-equal 1 (c-x i))
	(assert-equal 2 (c-y i))
	(assert-equal 3 (c-z i))
))

(def-class aa x y)
(def-class bb x y)
(define-test herança-multipla-2
	"teste para herança multipla, atributos com mesmo nome nas superclasses"

	(def-class (cc aa bb) z)
	(let ((i (make-cc :x 1 :y 2 :z 3)))

	(assert-equal 1 (cc-x i))
	(assert-equal 2 (cc-y i))
	(assert-equal 3 (cc-z i))
))

(def-class aaa x y)
(def-class bbb y z)
(define-test herança-multipla-3
	"teste para herança multipla, atributos com mesmo nome entre sub e superclasses"

	(def-class (ccc aaa bbb) z w)
	(let ((i (make-ccc :x 1 :y 2 :z 3 :w 4)))

	(assert-equal 1 (ccc-x i))
	(assert-equal 2 (ccc-y i))
	(assert-equal 3 (ccc-z i))
	(assert-equal 4 (ccc-w i))
))


(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
