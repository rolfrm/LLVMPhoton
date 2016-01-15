(proclaim '(optimize (debug 3) (speed 0)))
(quicklisp:quickload 'cffi)

(defpackage :llvm
  (:use :common-lisp :cffi)
  (:export compile-il il-lib-get-sym il-lib-close il-lib-open il-lib-filename))
   
(in-package :llvm)

(define-foreign-library libdl (t "/usr/lib/x86_64-linux-gnu/libdl.so"))
(use-foreign-library libdl)

(defcfun "dlopen" :int64 (path :string) (flag :int))
(defcfun "dlsym" :int64 (lib :int64) (symbol :string))
(defcfun "dlclose" :int64 (lib :int64))

(defstruct il-lib (filename) (handle))

(defun il-lib-get-sym (lib sym)
  (let ((sym (dlsym (il-lib-handle lib) sym)))
    (if (eq sym 0) nil sym)))
(defun il-lib-close(lib)
  (dlclose (il-lib-handle lib)))
(defun il-lib-open(filepath)
  (let ((out-lib (make-il-lib :handle (dlopen filepath 257)
			      :filename filepath)))
    (when (eq (il-lib-handle out-lib) 0)
      (error "Could not load ~a" filepath))
    out-lib))

(defun run-program (program &rest args)
  (sb-ext:process-exit-code (sb-ext:run-program program args :search t :wait t :output t)))

(defvar randid 0)
(defun compile-il (il-code &optional (so-file nil))
  (let ((id (incf randid)))
    (let ((temp-il-file (format nil "./tmp~a.il" id))
	  (temp-so-file (or so-file (format nil "./tmp~a.so" id)))
	  (temp-asm-file (format nil "./tmp~a.il.s" id)))
      (with-open-file (stream temp-il-file :direction :output :if-exists :supersede)
	(format stream "~a" il-code))
      (let ((compile-exit-code (run-program "llc-3.7" "-relocation-model=pic" temp-il-file)))
	(unless (eq compile-exit-code 0)
	  (error "LLC exited with code ~a" compile-exit-code )))
      (let ((compile-exit-code 
	     (run-program "gcc" temp-asm-file "-o" temp-so-file "-fPIC" "-shared")))
	(unless (eq compile-exit-code 0)
	  (error "GCC exited with code ~a" compile-exit-code )))
      (il-lib-open temp-so-file)
      )))


(defvar run-fcn-dll nil)
(unless run-fcn-dll
  (let (( run-fcn
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

"))
    (defparameter run-fcn-dll (compile-il run-fcn ))))



