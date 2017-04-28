(defun rl()
	(load "01.lisp")
)

(defmacro def-class (class-name &rest body)
	; class name as a string	
	;~ (format t  "~S~%" (string-downcase (string class-name)))
	
	; loop each attrib
	(loop for att in body
		;~ do (format t "attribute: ~S~%" att)
	)
	
	; splice all values into containing expression - this seems useful to define make-person (arg1 arg2 ... argn)
	;~ (format t "~S~%" `(,@body))
	(setq constructor-name (concatenate 'string "make-" (string-downcase (string class-name))))
	;~ (format t "~S~%" constructor-name)
	`(progn
		; definir construtor
		(defun ,(read-from-string constructor-name)	
			(,@body) ; args
			(format t "~S: ~S ~%" ,(string-downcase (string class-name)) (list ,@body)))
		
		; definir simbolo com meta informação da classe, por exemplo:
		; >(def-class pessoa nome idade)
		; ("pessoa" '(NOME IDADE))
		; >pessoa
		; ("pessoa" '(NOME IDADE))
		(setq ,class-name (cons ,(string-downcase (string class-name)) '('(,@body))))
))


(def-class pessoa nome idade)
(def-class estudante)

(make-pessoa "duarte" 23)
;(make-pessoa :nome "pompeu" :idade 24) <- devia ser assim!
(make-estudante)
