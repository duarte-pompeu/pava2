(defun rl()
	(load "main.lisp")
)

(setq *class-hashmap* (make-hash-table :test #'equal))

(defun get-class (class-name)
	(gethash class-name *class-hashmap*)
)

(defun set-class (class-name class)
	(setf (gethash class-name *class-hashmap*) class)
)

(defun get-class-name (class)
	(first class)
)

(defun get-class-attributes (class)
	(second class)
)

(defun get-obj-class (obj)
	(first obj)
)

(defun get-attribs-hash (obj)
	(second obj)
)

(defmacro def-class (class-name &rest body)
	; splice all values into containing expression - this seems useful to define make-person (arg1 arg2 ... argn)
	(setq constructor-name (concatenate 'string "make-" (string-downcase (string class-name))))
	;~ (format t "~S~%" constructor-name)
	`(progn
		; definir simbolo com meta informação da classe, por exemplo:
		; >(def-class pessoa nome idade)
		; ("pessoa" '(NOME IDADE))
		; >pessoa
		; ("pessoa" '(NOME IDADE))
		(set-class ,(string-downcase (string class-name))  
			(list ,(string-downcase (string class-name)) '(,@body)))
		
		; definir construtor
		(defun ,(read-from-string constructor-name)	
			(,@body) ; args
			(list ,(string-downcase (string class-name)) ; FIXME: use symbol
				(make-hash-table :test #'equal)))
		
		,@(let ((result nil))
			(dolist (attrib body result)
				;(format t "~S ~%" (concatenate 'string (string-downcase (string class-name)) "-" (string-downcase (string attrib))))
				(push
					`(defun ,(read-from-string (concatenate 'string (string-downcase (string class-name)) "-" (string-downcase (string attrib))))
						(object)
						(gethash ,(string-downcase (string attrib)) (get-attribs-hash object))) ; TODO: check if working
					result)
			))
	)
)


(def-class pessoa nome idade)
(def-class estudante)

(make-pessoa "duarte" 23)
;(make-pessoa :nome "pompeu" :idade 24) <- devia ser assim!
(make-estudante)
