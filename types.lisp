(defpackage :photon-types
  (:use :common-lisp)
  (:export type-is-float type-is-integer
	   i64-type i32-type i16-type i8-type
	   f32-type f64-type
	   primitive-type-name primitive-type-p
	   struct-type-name struct-type-p struct-type-members
	   struct-member-name struct-member-photon-type
	   pointer-type-inner-type pointer-type-p
	   function-type-p function-type-return-type function-type-arg-types
	   void-type-p void-type
	   dynamic-type-p
	   )
  )
(in-package :photon-types)

(defstruct primitive-type (name) (size))
(defstruct struct-type (name) (members nil))
(defstruct struct-member (name) (photon-type))
(defstruct pointer-type (inner-type))
(defstruct function-type (return-type) (arg-types nil))
(defstruct _void-type)
(defstruct dynamic-type)
(defvar i64-type (make-primitive-type :name "i64" :size 8))
(defvar i32-type (make-primitive-type :name "i32" :size 4))
(defvar i16-type (make-primitive-type :name "i64" :size 8))
(defvar i8-type (make-primitive-type :name "i32" :size 4))
(defvar f32-type (make-primitive-type :name "f32" :size 4))
(defvar f64-type (make-primitive-type :name "f64" :size 8))
(defvar void-type (make-_void-type))

(defun type-is-float (type)
  (or (eq f32-type type)
      (eq f64-type type)))

(defun type-is-integer(type)
  (or (eq i8-type type)
      (eq i16-type type)
      (eq i32-type type)
      (eq i64-type type)))



					;(defvar void-type (make-void-type :name "void"))
;;(defun parse-type (type-graph)
;;  (let ((ptr #'make-pointer-type))

;;(defun serialize-type (type))

;; Examples of types
;; i32, i64, f64, (ptr f64), (fcn (ptr f64) (a f64) (b f64))
;; (struct vec2 (x f64) (y f64))
;; (fcn vec2 (a vec2) (b vec2))


    
