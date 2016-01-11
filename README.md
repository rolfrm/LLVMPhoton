# LLVM Photon
Experimental LISP compiler using LLVM as a backend.

Its still in the very early phase. The compiler is written in SBCL (Common Lisp) and requires that specific implementation to run. In the future I will built the required parts to run the compiler from Photon itself, so this requires building a small subset of common lisp to support the compiler. 

Benefits of using LLVM instead of Tiny C Compiler (tcc) is hopefully that better performance can be achieved and that it will support more platforms (x86 32/64, ARM 32/64).
LLVM IR code is also a better format than C because it is more rigid

Current status:
  ```lisp
  (compile-ast `(defun |hello-world| ((x ,i32-type)) (the ,i32-type (+ x 3))))
  ```
Which is essencially equal to ```(defun hello-world ((x i32)) (the i32 (+ x 3)))```
Generates the following LLVM IR code:
```c
define i32 @hello-world(i32 %X){
%tmp1 = alloca i32
store i32 3, i32* %tmp1
%tmp2 = load i32, i32* %tmp1
%tmp3 = add i32 %X, %tmp2
ret i32 %tmp3
}
```
Which compiles and works.
