(defun rl()
	(load "main.lisp"))

(setq *class-hashmap* (make-hash-table :test #'equal))

(defun get-class (class-name)
	"recebe o nome de uma classe, procura-a no hashmap e retorna a estrutura de dados correspondente, caso exista"
	(gethash class-name *class-hashmap*))

(defun set-class (class-name class)
	"guarda uma classe no mapa de classes"
	(setf (gethash class-name *class-hashmap*) class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-class-name (class)
	"recebe uma classe, retorna o seu nome"
	(first class))

(defun get-class-attributes (class)
	"recebe uma classe, retorna uma lista com os seus atributos"
	(second class))

(defun get-superclasses (class)
	"recebe uma classe, retorna uma lista com o nome das suas superclasses directas"
	(third class))

(defun get-all-superclasses (class)
	"recebe o nome de uma classe, retorna uma lista com todas as suas superclasses"
	(let ((supers '())
		(to-process (get-superclasses (get-class class)))
		(current-class nil))

		(loop while (not (null (first to-process)))
		do (progn
			(setq current-class (first to-process))
			(setq to-process (rest to-process))
			(push current-class supers)
			(setq to-process (concatenate 'list to-process (get-superclasses (get-class (string-downcase (string current-class))))))))
		supers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-obj-class (obj)
	"recebe um objecto, retorna o nome da sua classe"
	(first obj))

(defun get-attribs-hash (obj)
	"recebe um objecto, retorna o seu hashmap de atributos e valores"
	(second obj))

(defmacro def-class (first-arg &rest body)

	(let* ((constructor-name nil)
		(classname nil)
		(existe-herança nil)
		(class-fields nil)
		(attributes '()))

	(if (listp first-arg)
		(progn
			(setq classname (first first-arg))
			(setq existe-herança t))
		(progn
			(setq classname first-arg)
			(setq existe-herança nil)))

	(loop for i in body
	do (if (listp i)
		(setf attributes (append attributes (list (first i))))
		(setf attributes (append attributes (list i)))))

	; splice all values into containing expression - this seems useful to define make-person (arg1 arg2 ... argn)
	(setq constructor-name (concatenate 'string "make-" (string-downcase (string classname))))

	; check for inheritance
	(if (not existe-herança)
		(setq class-fields attributes) ; no inherited fields
		(let ((superclass-attribs nil)
			 (result nil))
			(setq class-fields attributes) ; FIXME: do always
			(dolist (superclass (rest first-arg) result)
				(dolist (attrib (get-class-attributes (get-class (string-downcase (string superclass)))) result)
					(if (not (member attrib class-fields)) (push attrib result))))

			; get attributes from hashmap, which can have attributes or a lists with (attribute default-value)
			(loop for field in result
			do (if (listp field)
				(setq class-fields (concatenate 'list class-fields (list (first field))))
				(setq class-fields (concatenate 'list class-fields (list field)))))

			(setq body (concatenate 'list body result))))

	; create class and put it in classes map
	; >(def-class pessoa nome idade)
	; ("pessoa" '(NOME IDADE))
	(set-class (string-downcase (string classname))
			(list
				(string-downcase (string classname))
				body
				(if existe-herança
					(progn
					(mapcar (lambda (x) (string-downcase (string x))) (rest first-arg)))
					'())))

	`(progn

		; generate constructor
		(defun ,(read-from-string constructor-name)
			(&key ,@body) ; args
			(let ((hashmap (make-hash-table :test #'equal)))

				; set fields
				,@(mapcar #'(lambda (attrib) `(setf (gethash ,(string-downcase (string attrib)) hashmap) ,attrib)) class-fields)

				(list ,(string-downcase (string classname))
					hashmap)))

		; generate getters
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object)
				(if (,(read-from-string (concatenate 'string (string-downcase (string classname)) "?")) object)
					(gethash ,(string-downcase (string attrib)) (get-attribs-hash object))
					(error "getter is from an incompatible class"))))
			class-fields)

		; generate setters
		,@(mapcar #'(lambda (attrib)
			`(defun ,(read-from-string (concatenate 'string "set-" (string-downcase (string classname)) "-" (string-downcase (string attrib))))
					(object new-value)
				(if (,(read-from-string (concatenate 'string (string-downcase (string classname)) "?")) object)
				(setf (gethash ,(string-downcase (string attrib)) (get-attribs-hash object)) new-value)
				(error "setter is from an incompatible class"))))
			class-fields)

		; generate recognizer
		(defun ,(read-from-string (concatenate 'string (string-downcase (string classname)) "?"))
			(object)

			(cond
			; casos em que estutura de dados não corresponde aos nossos objectos
			((not (listp object)) nil)
			((not (equal 2 (length object))) nil)

			; simplest case: macro class == obj class
			((equal (get-obj-class object) ,(string-downcase (string classname))) T)

			; harder case: macro class == 1 of object superclasses
			((member-if #'(lambda (x) (equal x ,(string-downcase (string classname)))) (get-all-superclasses (get-class-name object))) T)
			))
	)
))

