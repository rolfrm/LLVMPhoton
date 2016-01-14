(defpackage :photon-test
  (:use :common-lisp :photon-compiler :llvm :photon-builtin :photon-types :utils :cffi))
(in-package :photon-test)

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
(il-lib-close run-fcn-dll)
(define-foreign-library librun (il-lib-filename run-fcn-dll))
(use-foreign-library librun)
(defcfun ("run_fcn" runfcn) :void (fcn :int64))
(defcfun ("run_fcn2" runfcn2) :void (fcn :int64) (arg :int64))
(defcfun ("deref" deref) :int64 (arg :int64))
(defcfun ("set_ptr" set-ptr) :void (dst :int64) (src :int64))

(defvar test1-fcn
  "
@.str = private constant [13 x i8] c\"hello worlE\\0A\\00\"
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
;(runfcn2 (dlsym test1 "run_test2") (deref (dlsym test1 "helloworld")))
;(format t "NNN ~a~%" (dlsym test1 "helloworld"))
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
(defvar global-code nil)
(defun add-global (cmd &optional (type nil))
  (if (and type (not (eq type void-type)))
      (let ((var (make-photon-variable :name (format nil "%tmp~a" (incf nameid)) :type type)))
	(setf global-code (cons (format nil "~a = ~a" (photon-variable-name var) cmd) global-code))
	var)
      (progn (setf global-code (cons cmd global-code)) nil)))

(defun add-variable(variable)
  (setf (gethash (photon-variable-name variable) (first scope) )
	variable))
(let ((scope (list (make-hash-table)))
      (add-local #'add-local)
      (dependent-variables (make-hash-table))
      (add-global #'add-global)
      (global-code nil)
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
  
  (compile-ast `(defun |testhello1| ((x ,i32-type)) (|testrun|)))
  (format t "Dependent Variables:~%~a~%" dependent-variables)
  (let ((compile-out (concat-lines (reverse (concatenate 'list code global-code)))))
    
    (format t "Compile Out:~%~a~%" compile-out)

    
    (compile-il compile-out)
    ))



(defparameter test2-fcn
  "
@.str = private constant [13 x i8] c\"hello worl_\\0A\\00\"
declare i32 @printf(i8* noalias nocapture, ...)
define void @run_test3(){
 %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0
 call i32 (i8*,...) @printf(i8* %cast210)
 ret void
}
")
(defparameter test2-lib (compile-il test2-fcn))
(defvar r3 "
declare void @run_test3()
define void @eval(){
 call void () @run_test3()
 ret void
}
")

(defparameter r3-lib (compile-il r3))

(let ((evalfcn (dlsym r3-lib "eval"))) (runfcn evalfcn))
(format t "R3lib: ~a~%" r3-lib)
(print (dlclose r3-lib))
(print (dlclose test2-lib))


(defparameter test2-fcn
  "
@.str = private constant [13 x i8] c\"hello worlD\\0A\\00\"
declare i32 @printf(i8* noalias nocapture, ...)
define void @run_test3(){
 %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0
 call i32 (i8*,...) @printf(i8* %cast210)
 ret void
}
")
(print test2-fcn)
(defparameter test2-lib (compile-il test2-fcn))
(defparameter r3-lib (compile-il r3))

(let ((evalfcn (dlsym r3-lib "eval"))) (runfcn evalfcn))
(format t "R3lib: ~a~%" r3-lib)
