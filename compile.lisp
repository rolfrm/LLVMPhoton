(defpackage :photon-compiler
  (:use :common-lisp :photon-types :utils)
  (:export add-global expected-type add-local scope
	   get-variable compile-assert compile-value compile-ast
	   photon-variable-name photon-variable-type
	   primitive-type-name make-photon-variable
	   )
  )

(in-package :photon-compiler)

(defun compile-ast (ast) (declare (ignore ast)))

(defstruct photon-variable (name) (type) (data) (dependencies))
(defvar expected-type nil)
(defvar add-global nil)
(defvar add-local nil)
(defvar scope nil)

(defun get-variable (name)
  (labels
      ((+get-variable (scope)
	 (if scope
	     (let ((found (gethash name (car scope))))
	       (if found
		   found
		   (+get-variable (cdr scope))))
	     nil)))
    (+get-variable scope)))


(defun compile-assert (condition &optional (error-string "Compile error"))
  (unless condition
    (error error-string)))

(defvar dependent-variables nil)

(defun compile-value (value)
  (cond
   ((integerp value)
    (when expected-type
      (compile-assert (or (type-is-integer expected-type)
			  (type-is-float expected-type))
		      "Type does not match expected type"))
    (let* ((used-type (if expected-type expected-type
			i64-type))
	   (type-name (primitive-type-name used-type)))
      (let ((var (funcall add-local (format nil "alloca ~a" type-name) expected-type)))
	(funcall add-local (format nil "store ~a ~a, ~a* ~a"
			   type-name value type-name (photon-variable-name var)) nil)
	(let* ((load-val (format nil "load ~a, ~a* ~a" type-name type-name (photon-variable-name var)))
	       (var2 (funcall add-local load-val used-type)))
	  var2))))
   ((stringp value) (funcall add-local (format nil "~a" value)))
   ((symbolp value)
    (let ((var (get-variable value)))
      (when var
	(when (photon-variable-data var)
	  ;; If its not a stack variable
	  ;; currently only stack variables does not have data
	  (setf (gethash (photon-variable-name var) dependent-variables) var))
	var)))
   (t (error "Should not happen"))))


(defun compile-with-function(fcn-def args)
  (let ((compiled-args (mapcar (lambda (sub) (compile-ast sub)) args)))
    
    (let* ((fcn-type (photon-variable-type fcn-def))
	   (args (mapcar (lambda (compiled-arg)
			   (format nil "~a %~a"
				   (primitive-type-name (photon-variable-type compiled-arg))
				   (photon-variable-name
				    compiled-arg)))
			 compiled-args))
	   (string-args (join-strings args #\,))
	   (string-arg-types (join-strings (mapcar (lambda
						    (arg)(primitive-type-name
							  (photon-variable-type
							   arg)))
						   compiled-args)
	     #\, )))
      (let ((ret (primitive-type-name (function-type-return-type fcn-type)))
	    (fcn-name (photon-variable-name fcn-def)))
	(unless (gethash (photon-variable-name fcn-def) dependent-variables)
	  (setf (gethash (photon-variable-name fcn-def) dependent-variables) fcn-def)
	  (funcall add-global (format nil "declare ~a @~a(~a)" ret fcn-name string-arg-types))
	  )
	(funcall add-local (format nil "call ~a (~a)@~a(~a)"
			   ret
			   string-arg-types fcn-name
			   string-args)
	(function-type-return-type fcn-type))
      ))))

(defun is-function(variable)
  (function-type-p (photon-variable-type variable)))

(defun is-builtin-macro(variable)
  (eq (photon-variable-type variable) :builtin-macro))

(defun compile-with-builtin-macro(macro-def args)
  (let ((macro-fcn (photon-variable-data macro-def)))
    (funcall macro-fcn macro-def args)))
    
(defun compile-ast(ast)
  (if (listp ast)
      (let ((first (first ast)))
	(let ((fcn-def
	       (if (listp first)
		   (compile-value ast)
		   (get-variable first))))
	  (unless fcn-def
	    (error (format nil "No variable named '~a'" fcn-def)))
	  (cond ((is-function fcn-def)
		 (compile-with-function fcn-def (rest ast) ))
		((is-builtin-macro fcn-def)
		 (compile-with-builtin-macro fcn-def (rest ast) ))
		(t (error "WHY?")))))
    (compile-value ast)))
