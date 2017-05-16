; To run the tests: $ clisp tests.lisp

(load "lisp-unit.lisp")
(use-package :lisp-unit)
(load "main.lisp")

(def-class pessoa nome idade)
(setq *p1* (make-pessoa :nome "joão" :idade 18))
(def-class (estudante pessoa) curso)
(def-class (aluno pessoa) nome curso)
(def-class (investigador aluno) tese)
(setq *a1* (make-aluno :nome "joão" :idade 18 :curso "leic"))

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
	
	(let ((e (make-estudante :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (estudante-nome e))
	(assert-equal 25 (estudante-idade e))
	(assert-equal "leic" (estudante-curso e))
))

(define-test herança-duplicados
	"teste para herança simples, atributos duplicados"

	
	(let ((a (make-aluno :nome "abc" :idade 25 :curso "leic")))

	(assert-equal "abc" (aluno-nome a))
	(assert-equal 25 (aluno-idade a))
	(assert-equal "leic" (aluno-curso a))
))

(def-class aa x)
(def-class bb y)
(define-test herança-multipla
	"teste para herança multipla, atributos distintos"

	(def-class (cc aa bb) z)
	(let ((i (make-cc :x 1 :y 2 :z 3)))

	(assert-equal 1 (cc-x i))
	(assert-equal 2 (cc-y i))
	(assert-equal 3 (cc-z i))
))

(def-class aaa x y)
(def-class bba x y)
(define-test herança-multipla-2
	"teste para herança multipla, atributos com mesmo nome nas superclasses"

	(def-class (ccc aaa bbb) z)
	(let ((i (make-ccc :x 1 :y 2 :z 3)))

	(assert-equal 1 (ccc-x i))
	(assert-equal 2 (ccc-y i))
	(assert-equal 3 (ccc-z i))
))

(def-class aaaa x y)
(def-class bbbb y z)
(define-test herança-multipla-3
	"teste para herança multipla, atributos com mesmo nome entre sub e superclasses"

	(def-class (cccc aaaa bbbb) z w)
	(let ((i (make-cccc :x 1 :y 2 :z 3 :w 4)))

	(assert-equal 1 (cccc-x i))
	(assert-equal 2 (cccc-y i))
	(assert-equal 3 (cccc-z i))
	(assert-equal 4 (cccc-w i))
))

(define-test recognizer-simples
	"assegurar que (pessoa? [instancia de pessoa] é verdade"
	(assert-true (pessoa? *p1*))
)

(define-test recognizer-simples-falso
	"assegurar que (aluno? [instancia de pessoa] é falso"
	(assert-false (aluno? *p1*))
)

(define-test recognizer-not-objects
	"make sure recognizers for non-objects return false gracely, without crashes"
	(assert-false (pessoa? 30))
	(assert-false (aluno? '()))
	(assert-false (investigador? "abc"))
	(assert-false (estudante? '((123) (456))))
)

(define-test recognizer-multiple
	"recognizer para polimorfismo"
	(let ((x (make-investigador :nome "joao" :idade 30 :curso "DEIC" :tese "optimizacao de sintaxe lisp")))
	
	(assert-true (investigador? x))
	(assert-true (aluno? x))
	(assert-true (pessoa? x))
))

(define-test recognizer-multiple-falso
	"recognizer false: assegurar que (investigador? [instancia de aluno]) é falso"
	(let ((x (make-aluno :nome "joao" :idade 30 :curso "MEIC")))
	
	(assert-false (investigador? x))
	(assert-true (aluno? x))
	(assert-true (pessoa? x))
	(assert-equal "joao" (pessoa-nome x))
	(assert-equal "joao" (aluno-nome x))
	(assert-equal 30 (pessoa-idade x))
	(assert-equal "MEIC" (aluno-curso x))
))


(def-class (a pessoa))
(def-class (b a))
(def-class (c b))
(def-class (d b))
(def-class (e c d))
(define-test recognizer-loop
	"tentar gerar hierarquia problematica"

	(let ((x (make-e :nome "qwerty" :idade 4321)))
	(assert-true (pessoa? x))
	(assert-equal "qwerty" (pessoa-nome x))
	(assert-equal 4321 (pessoa-idade x))
))


(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
