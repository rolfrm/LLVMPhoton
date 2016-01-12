(proclaim '(optimize (debug 3) (speed 0)))
(print "Hej")

(quicklisp:quickload 'cffi)

(defpackage :cffi-user
  (:use :common-lisp :cffi))
   
(in-package :cffi-user)

(define-foreign-library libdl (t "/usr/lib/x86_64-linux-gnu/libdl.so"))
(use-foreign-library libdl)

(defcfun "dlopen" :int64 (path :string) (flag :int))
(defcfun "dlsym" :int64 (lib :int64) (symbol :string))
(defcfun "dlclose" :int64 (lib :int64))
(defvar randid 0)

(defun compile-il (il-code &optional (so-file nil))
  (let ((id (incf randid)))
    (let ((temp-il-file (format nil "./tmp~a.il" id))
	  (temp-so-file (or so-file (format nil "./tmp~a.so" id)))
	  (temp-asm-file (format nil "./tmp~a.il.s" id)))
      (with-open-file (stream temp-il-file :direction :output :if-exists :supersede)
	 (format stream "~a" il-code))
      (sb-ext:run-program "llc-3.7" (list "-relocation-model=pic" temp-il-file)
			  :search :wait :output t)
      (sb-ext:run-program "gcc" (list temp-asm-file "-o" temp-so-file "-fPIC" "-shared" )
			  :search :wait :output t)
      (format t "~a~%" temp-so-file)
      (dlopen temp-so-file 1);257);1)
      )))

(defun concat-lines (list-of-strings)
  (with-output-to-string (s)
      (dolist (str list-of-strings)
	(write-string str s)
	(write-char #\newline s))))

(defun join-strings-stream (s strings seperator)
  (if (cdr strings)
      (progn
	(write-string (car strings) s)
	(write-char seperator s)
	(join-strings-stream s (cdr strings) seperator))
    (write-string (car strings) s)))

(defun join-strings(strings seperator)
  (if strings
      (with-output-to-string (s)
	(join-strings-stream s strings seperator))
      ""))

(defstruct photon-variable (name) (type) (data))
(defstruct primitive-type (name) (size))
(defstruct struct-type (name) (members nil))
(defstruct struct-member (name) (photon-type))
(defstruct pointer-type (inner-type))
(defstruct function-type (return-type) (arg-types nil))

(defvar i64-type (make-primitive-type :name "i64" :size 8))
(defvar i32-type (make-primitive-type :name "i32" :size 4))
(defvar i16-type (make-primitive-type :name "i64" :size 8))
(defvar i8-type (make-primitive-type :name "i32" :size 4))
(defvar f32-type (make-primitive-type :name "f32" :size 4))
(defvar f64-type (make-primitive-type :name "f64" :size 8))
(defvar void-type (make-primitive-type :name "void" :size 0))
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

(defun type-is-float (type)
  (or (eq f32-type type)
      (eq f64-type type)))

(defun type-is-integer(type)
  (or (eq i8-type type)
      (eq i16-type type)
      (eq i32-type type)
      (eq i64-type type)))

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
      (let ((var (add-local (format nil "alloca ~a" type-name) expected-type)))
	(add-local (format nil "store ~a ~a, ~a* ~a"
			   type-name value type-name (photon-variable-name var)) nil)
	(let* ((load-val (format nil "load ~a, ~a* ~a" type-name type-name (photon-variable-name var)))
	       (var2 (add-local load-val used-type)))
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
  (let ((compiled-args
	 (mapcar (lambda (sub) (compile-ast sub)) args)))
    (unless (gethash (photon-variable-name fcn-def) dependent-variables)
      (setf (gethash (photon-variable-name fcn-def) dependent-variables) fcn-def))
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
	(add-local (format nil "call ~a (~a)@~a(~a)"
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

(defun make-operator-macro (name opcode)
  (flet ((operator-compile
	  (op-def args)
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

(defun the-macro (mac-def ast)
  (let ((expected-type (first ast)))
    (compile-ast (second ast))))
(defvar code '())

(defun defun-macro (mac-def ast)
  (compile-assert (> (length ast) 2) "Arg cnt wrong")
  (let ((fun-name (first ast))
	(args (second ast))
	(body (cddr ast)))
    (compile-assert (symbolp fun-name) "function name must be a symbol")
    (compile-assert (listp args) "args must be a list")
    (let ((_args (mapcar
		 (lambda (arg)
		   (progn
		     (compile-assert (eq (length arg) 2) "arg must be cons of two")
		     (compile-assert (symbolp (first arg)) "arg name must be a symbol")))
		 args))
	  (fcn-scope (make-hash-table))
	  (arg-vars nil))
      (dolist (arg args)
	(destructuring-bind (name type) arg
	    (let ((variable (make-photon-variable :name (format nil "%~a" name) :type type)))
	      (setf (gethash name fcn-scope) variable)
	      (setf arg-vars (cons variable arg-vars))
	      )))
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
		(join-strings (mapcar (lambda (variable) (format nil "~a ~a"
								 (primitive-type-name (photon-variable-type variable))
								 (photon-variable-name variable)
								 
								 )) arg-vars)
			      #\,)))
	  
	  (funcall add-local (format nil "define ~a @~a(~a){" (primitive-type-name ret-type) fun-name arg-string)))
					;(setf code (cons body-code code))
	(dolist (code-part (reverse body-code))
	  (setf code (cons code-part code)))
	(if last-var
	    (funcall add-local (format nil "ret ~a ~a" (primitive-type-name ret-type) (photon-variable-name last-var)))
	  (funcall add-local (format nil "ret ~a" (primitive-type-name ret-type))))
	(funcall add-local (format nil "}"))
	))))
  
(defvar run-fcn
  "
define void @run_fcn(void ()* %f){
  call void %f()
  ret void
}

define void @run_fcn2(void (i8*)* %f2, i8* %v2){
  call void %f2(i8* %v2)
  ret void
}

define i8* @deref(i8** %v2){
  %v = load i8*, i8** %v2
  ret i8* %v
}

define void @set_ptr(i8** %dst, i8* %src){
  store i8* %src, i8** %dst
  ret void
}

")

(defvar run-fcn-dll (compile-il run-fcn ))
(format t "fcn dll:~a~%" run-fcn-dll)
(dlclose run-fcn-dll)
(define-foreign-library librun (t "./tmp1.so"))
(use-foreign-library librun)
(defcfun ("run_fcn" runfcn) :void (fcn :int64))
(defcfun ("run_fcn2" runfcn2) :void (fcn :int64) (arg :int64))
(defcfun ("deref" deref) :int64 (arg :int64))
(defcfun ("set_ptr" set-ptr) :void (dst :int64) (src :int64))

(defvar test1-fcn
  "
@.str = private constant [13 x i8] c\"hello world\\0A\\00\"
@helloworld = global [13 x i8]* @.str
declare i32 @printf(i8* noalias nocapture, ...)
define void @run_test(){
 %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0
 call i32 (i8*,...) @printf(i8* %cast210)
 ret void
}

define void @run_test2(i8* %str){
 call i32 (i8*,...) @printf(i8* %str)
 ret void
}")

(defvar test1 (compile-il test1-fcn))
(defvar run-test (dlsym test1 "run_test"))
(runfcn run-test)
(runfcn2 (dlsym test1 "run_test2") (deref (dlsym test1 "helloworld")))
(format t "NNN ~a~%" (dlsym test1 "helloworld"))
;; The following test code

(defvar testcode
  '(+ 1 2)
  ;'(progn
  ;   (defvar x 4)
  ;   (defvar y 5)   
  ; )
  )
;; could generate (optimally)
(defvar r1 "

@x = global i64 4
@xptr = global i64* null
@y = global i64 5

define void @eval(){

 %v = inttoptr i64 444444444444 to i64*
 store i64* %v, i64** @xptr
 store i64 314, i64* @x 
 store i64 5, i64* @y
 ret void
}
")

(defvar r2 "
@.str = private constant [13 x i8] c\"hello wor%i\\0A\\00\"
declare i32 @printf(i8* noalias nocapture, ...)
@.run_test = global void () * null
@xptr = global i64* null

define void @eval(){
 %run_test = load void () *, void () ** @.run_test
 call void () %run_test()
 %v = inttoptr i64 ~a to i64*
 %v2 = load i64, i64* %v
 %v3 = add i64 %v2, %v2

 %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0
 call i32 (i8*, ...)@printf(i8* %cast210, i64 %v3)
 store i64 %v3, i64* %v
 ret void
}
")

(defvar r1dl (compile-il r1))
(defvar eval2 (dlsym r1dl "eval"))
(defvar xdl (dlsym r1dl "x"))
(defvar ydl (dlsym r1dl "y"))

(runfcn eval2)

(print (deref xdl))
(format t "~%~%")
(defvar r2dl (compile-il (format nil r2 xdl)))
(defvar eval3 (dlsym r2dl "eval"))
(set-ptr (dlsym r2dl ".run_test") (dlsym test1 "run_test"))
(runfcn eval3)
(runfcn eval3)
(runfcn eval3)
(runfcn eval3)
(runfcn eval3)
(runfcn eval3)


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
	(setf code (cons (format nil "~a = ~a" (photon-variable-name var) cmd) code))
	var)
      (progn (setf code (cons cmd code)) nil)))

(defun add-variable(variable)
  (setf (gethash (photon-variable-name variable) (first scope) )
	variable))
(let ((scope (list (make-hash-table)))
      (add-local #'add-local)
      (dependent-variables (make-hash-table))
      (code nil))
  (add-variable (make-photon-variable :name '+ :type :builtin-macro :data
				      (make-operator-macro '+ "add")))
  (add-variable (make-photon-variable :name 'the :type :builtin-macro :data
				      #'the-macro))
  (add-variable (make-photon-variable :name 'defun :type :builtin-macro :data
				      #'defun-macro))
  (add-variable (make-photon-variable
		 :name '|testrun|
		 :type (make-function-type :return-type void-type :arg-types nil)
		 :data run-test))
  
  (compile-ast `(defun |testhello1| ((x ,i32-type)) (testrun)))
  (format t "Dependent Variables:~%~a~%" dependent-variables)
  (let ((compile-out (concat-lines (reverse code))))
    
    (format t "Compile Out:~%~a~%" compile-out)

    
    (compile-il compile-out)))
