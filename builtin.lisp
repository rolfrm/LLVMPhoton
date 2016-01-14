(defpackage :photon-builtin 
  (:use :common-lisp :photon-compiler :photon-types)
  (:export the-macro defun-macro make-operator-macro code))
(in-package :photon-builtin)

(defun make-operator-macro (opcode)
  (flet ((operator-compile
	     (op-def args)
	   (declare (ignore op-def))
	   (compile-assert (eq (length args) 2))
	   (let ((compiled-args
		  (mapcar (lambda (arg) (compile-ast arg))
			  args)))
	     (format t "Compiled args: ~a~%" compiled-args)
	     (let ((arg-names (mapcar #'photon-variable-name compiled-args)))
	       (format t "Compiled arg: ~a~%" (first compiled-args))
	       (let ((used-type (if expected-type
				    expected-type
				    (photon-variable-type (first compiled-args)))))
		 (funcall add-local (apply #'format nil "~a ~a %~a, %~a" opcode (primitive-type-name used-type) arg-names) used-type))))))
    #'operator-compile))


(defun the-macro (mac-def ast)
  (declare (ignore mac-def))
  (let ((expected-type (first ast)))
    (compile-ast (second ast))))

(defvar code '())
(defvar global-code '())
(defun defun-macro (mac-def ast)
  (declare (ignore mac-def))
  (compile-assert (> (length ast) 2) "Arg cnt wrong")
  (let ((fun-name (first ast))
	(args (second ast))
	(body (cddr ast)))
    (compile-assert (symbolp fun-name) "function name must be a symbol")
    (compile-assert (listp args) "args must be a list")
    (mapcar
     (lambda (arg)
       (progn
	 (compile-assert (eq (length arg) 2) "arg must be cons of two")
	 (compile-assert (symbolp (first arg)) "arg name must be a symbol")))
     args)
    (let ((fcn-scope (make-hash-table))
	  (arg-vars nil))
      (dolist (arg args)
	(destructuring-bind (name type) arg
	    (let ((variable (make-photon-variable :name (format nil "%~a" name) :type type)))
	      (setf (gethash name fcn-scope) variable)
	      (setf arg-vars (cons variable arg-vars))
	      )))
      (setf arg-vars (reverse arg-vars))
      (let* ((scope (cons fcn-scope scope))
	     (last-var nil)
	     (body-code
	      (let ((code nil))
		(dolist (expr body)
		  (format t "Compiling ~a~%" expr)
		  (setf last-var (compile-ast expr))
		  (format t "last var ~a~%" last-var)
		 )
		code))
	     (ret-type (if last-var (photon-variable-type last-var) void-type)))
	(let (( arg-string
	       (utils:join-strings #\, (mapcar (lambda (variable) (format nil "~a ~a"
								 (primitive-type-name (photon-variable-type variable))
								 (photon-variable-name variable)
								 
								 )) arg-vars ))))
	  (funcall add-local (format nil "define ~a @~a(~a){" (primitive-type-name ret-type) fun-name arg-string)))
					;(setf code (cons body-code code))
	(dolist (code-part (reverse body-code))
	  (setf code (cons code-part code)))
	(if last-var
	    (funcall add-local (format nil "ret ~a ~a" (primitive-type-name ret-type) (photon-variable-name last-var)))
	  (funcall add-local (format nil "ret ~a" (primitive-type-name ret-type))))
	(funcall add-local (format nil "}"))
	))))

(defvar nameid 0)
(defun add-local (cmd &optional (type nil))
  (if (and type (not (eq type void-type)))
      (let ((var (make-photon-variable :name (format nil "%tmp~a" (incf nameid)) :type type)))
	(setf code (cons (format nil "~a = ~a" (photon-variable-name var) cmd) code))
	var)
      (progn (setf code (cons cmd code)) nil)))


(defun add-global (cmd &optional (type nil))
  (if (and type (not (eq type void-type)))
      (let ((var (make-photon-variable :name (format nil "%tmp~a" (incf nameid)) :type type)))
	(setf global-code (cons (format nil "~a = ~a" (photon-variable-name var) cmd) global-code))
	var)
      (progn (setf global-code (cons cmd global-code)) nil)))

(defun add-variable(variable)
  (setf (gethash (photon-variable-name variable) (first scope) )
	variable))
