; To run the tests: $ clisp pava-testes.lisp

(load "lisp-unit.lisp")
(use-package :lisp-unit)
(load "main.lisp")

(def-class person
  name
  age)

(def-class researcher
  group)

(def-class (student person)
  course)

(def-class sportsman
  activity
  schedule)

(def-class (ist-student student sportsman))

(def-class (phd-student ist-student researcher)
    thesis)


; testes
(define-test pava-1
	(let ((s (make-student :name "Paul" :age 21 :course "Informatics")))
		(assert-equal "Paul" (person-name s))
		(assert-equal "Informatics" (student-course s))
))

(define-test pava-2
	(let ((m (make-ist-student :name "Maria" :course "IA" :activity "Tennis")))
		(assert-true (ist-student? m))
		(assert-true (student? m))
		(assert-true (sportsman? m))

		(assert-equal "Maria" (ist-student-name m))
		(assert-equal "Maria" (person-name m))
		(assert-equal "Tennis" (sportsman-activity m))
		(assert-equal "Tennis" (ist-student-activity m))
))

(define-test pava-3
	(let ((b (make-phd-student :name "Brian" :age 28 :course "Informatics" :activity "Soccer" :group "ESW" :thesis "Code Migration")))
		(assert-true (researcher? b))
		(assert-true (person? b))
		(assert-true (student? b))
		(assert-true (sportsman? b))
		(assert-true (phd-student? b))
		
		(assert-equal "Code Migration" (phd-student-thesis b))
		(assert-equal "Brian" (student-name b))
		(assert-equal "ESW" (phd-student-group b))
		(assert-equal "Brian" (phd-student-name b))
))

(setq *print-failures* t)
(setq *print-errors* t)
(setq *print-summary* t)
(run-tests :all)
		
