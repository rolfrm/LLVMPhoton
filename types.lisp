(defpackage :photon-types
  (:use :common-lisp))

(defstruct photon-variable (name) (type) (data))
(defstruct primitive-type (name) (size))
(defstruct struct-type (name) (members nil))
(defstruct struct-member (name) (photon-type))
(defstruct pointer-type (inner-type))
(defstruct function-type (return-type) (arg-types nil))
(defstruct void-type)
(defstruct dynamic-type)
(defvar i64-type (make-primitive-type :name "i64" :size 8))
(defvar i32-type (make-primitive-type :name "i32" :size 4))
(defvar i16-type (make-primitive-type :name "i64" :size 8))
(defvar i8-type (make-primitive-type :name "i32" :size 4))
(defvar f32-type (make-primitive-type :name "f32" :size 4))
(defvar f64-type (make-primitive-type :name "f64" :size 8))
					;(defvar void-type (make-void-type :name "void"))
;;(defun parse-type (type-graph)
;;  (let ((ptr #'make-pointer-type))

;;(defun serialize-type (type))

;; Examples of types
;; i32, i64, f64, (ptr f64), (fcn (ptr f64) (a f64) (b f64))
;; (struct vec2 (x f64) (y f64))
;; (fcn vec2 (a vec2) (b vec2))


    
