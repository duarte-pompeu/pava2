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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-class-name (class)
	(first class)
)

(defun get-class-attributes (class)
	(second class)
)

(defun get-superclasses (class)
	(third class)
)

;~ (defun get-super-from-hierarchy (class)
	;~ (get-super-aux (get-superclasses class) '())
;~ )

;~ (defun get-super-aux (lst result)
	;~ (if (equal lst nil)
		;~ result
		;~ (progn
			;~ (get-super-aux '(get-superclasses(first)) result)
			;~ (get-super-aux (rest lst) (concatenate 'list result (get-superclasses (first lst)))))
;~ )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
			;~ (format t "com herança~%")
			(setq classname (first first-arg))
			(setq existe-herança t)
		)
		(progn 
			;~ (format t "sem herança~%")
			(setq classname first-arg)
			(setq existe-herança nil)
	))
	
	; splice all values into containing expression - this seems useful to define make-person (arg1 arg2 ... argn)
	(setq constructor-name (concatenate 'string "make-" (string-downcase (string classname))))
	
	; check for inheritance
	(if (not existe-herança)
		(setq class-fields body) ; no inherited fields
		(let ((superclass-attribs nil)
			 (result nil))
			(setq class-fields body)
			(dolist (superclass (rest first-arg) result)
				;~ (format t "processing superclass ~S ~%" superclass)
				(dolist (attrib (get-class-attributes (get-class (string-downcase (string superclass)))) result)
					(if (not (member attrib class-fields)) (push attrib result))
				)
				;~ (format t "current result ~S ~%" result)
			)
			;~ (format t "result ~S ~%" result)
			(setq class-fields (concatenate 'list class-fields result))
			;~ (format t "all class fields ~S ~%" class-fields)
	))
	
	; create class and put it in classes map
	; >(def-class pessoa nome idade)
	; ("pessoa" '(NOME IDADE))
	(set-class (string-downcase (string classname))  
			(list
				(string-downcase (string classname)) 
				class-fields
				(if existe-herança
					(progn (format t "rest: ~S~%" (rest first-arg))
					(rest first-arg))
					'()
	)))
	`(progn
		
		; generate constructor
		(defun ,(read-from-string constructor-name)	
			(&key ,@class-fields) ; args
			(let ((hashmap (make-hash-table :test #'equal)))
				
				; set fields
				,@(mapcar #'(lambda (attrib) `(setf (gethash ,(string-downcase (string attrib)) hashmap) ,attrib)) class-fields)
				; old code with do list (check deb5431a35121c3972a44d380e1a218b48ece3bb or previous)
				
				(list ,(string-downcase (string classname)) ; FIXME: use symbol
					hashmap)))
		
		; generate getters
		; usar recognizer para verificar se pode ser aplicada ao objecto
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object)
				(gethash ,(string-downcase (string attrib)) (get-attribs-hash object))))
			class-fields)
		; old code for getters (check deb5431a35121c3972a44d380e1a218b48ece3bb or previous)
		
		; generate setters
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string "set-" (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object new-value)
				(setf (gethash ,(string-downcase (string attrib)) (get-attribs-hash object)) new-value)))
			class-fields)
		
		; generate recognizer
		(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "?"))
			(object)
			;~ (format t "obj: ~S~%" object)
			;~ (format t "superclasses: ~S~%" (get-superclasses (get-class (get-obj-class object))))
			;~ (format t "class: ~S~%" (read-from-string (get-obj-class object)))
			(cond ((equal (get-obj-class object) ,(string-downcase (string classname))) T)
				;~ ((member-if #'(lambda (x) (equal x (read-from-string (get-obj-class object))) (get-superclasses object)))
				((not (equal nil (member ',(read-from-string (string-downcase (string classname))) (get-superclasses (get-class (get-obj-class object)))))) T)
			))
	)
))
