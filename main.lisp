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

(defmacro def-class (first-arg &rest body)	
	(let* ((constructor-name nil)
		(classname nil)
		(existe-herança nil)
		(class-fields nil))
	
	(if (listp first-arg)
		(progn
			(format t "herança~%")
			(setq classname (first first-arg))
			(setq existe-herança t)
		)
		(progn 
			(format t "simples~%")
			(setq classname first-arg)
			(setq existe-herança nil)
	))
	
	; splice all values into containing expression - this seems useful to define make-person (arg1 arg2 ... argn)
	(setq constructor-name (concatenate 'string "make-" (string-downcase (string classname))))
	
	; check for inheritance
	(if (not existe-herança)
		(setq class-fields body) ; no inherited fields
		(let ((superclass-attribs (get-class-attributes (get-class (string-downcase (string (second first-arg))))))
			 (result nil))
			 (format t "body ~S ~%" body)
			 (format t "body fields ~S ~%" class-fields)
			(format t "~S ~%" superclass-attribs)
			(setq class-fields body)
			(dolist (attrib superclass-attribs result)
				(format t "attrib ~S ~%" attrib)
				(push attrib result)
			)
			(format t "result ~S ~%" result)
			(setq class-fields (concatenate 'list class-fields result))
			(format t "all class fields ~S ~%" class-fields)
		)		
	)	
	(format t "all class fields ~S ~%" class-fields)
	
	`(progn
		; definir simbolo com meta informação da classe, por exemplo:
		; >(def-class pessoa nome idade)
		; ("pessoa" '(NOME IDADE))
		; >pessoa
		; ("pessoa" '(NOME IDADE))
		(set-class ,(string-downcase (string classname))  
			(list ,(string-downcase (string classname)) '(,@class-fields)))
		
		; definir construtor
		(defun ,(read-from-string constructor-name)	
			(&key ,@class-fields) ; args
			(let ((hashmap (make-hash-table :test #'equal)))
				
				; set fields
				,@(mapcar #'(lambda (attrib) `(setf (gethash ,(string-downcase (string attrib)) hashmap) ,attrib)) class-fields)
								
				;~ ,@(let ((result nil))
					;~ (dolist (attrib class-fields result)
						;~ (push `(setf (gethash ,(string-downcase (string attrib)) hashmap) ,attrib)
							;~ result)))
				
				(list ,(string-downcase (string classname)) ; FIXME: use symbol
					hashmap)))
		
		
		
		; generate getters
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object)
				(gethash ,(string-downcase (string attrib)) (get-attribs-hash object))))
			class-fields)
		; old code for getters
		;~ ,@(let ((result nil))
			;~ (dolist (attrib class-fields result)
				;~ ;(format t "~S ~%" (concatenate 'string (string-downcase (string classname)) "-" (string-downcase (string attrib))))
				;~ (push
					;~ `(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "-" (string-downcase (string attrib))))
						;~ (object)
						;~ (gethash ,(string-downcase (string attrib)) (get-attribs-hash object))) ; TODO: check if working
					;~ result)
			;~ ))
		
		; generate setters
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string "set-" (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object new-value)
				(setf (gethash ,(string-downcase (string attrib)) (get-attribs-hash object)) new-value)))
			class-fields)
	)
))
