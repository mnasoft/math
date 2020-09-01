;;;; appr-func-temptate.lisp

(in-package #:math.appr)

(export '*apr-args-1*)
(defparameter *apr-args-1* '(x1 yy)
   "Аргументы для функции одного параметра.
@begin[lang=lisp](code) '(x1 yy) @end(code) ")

(export '*apr-args-2*)
(defparameter *apr-args-2* '(x1 x2 yy)
  "Аргументы для функции двух параметров.
@begin[lang=lisp](code) '(x1 x2 yy) @end(code) ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*apr-func-1-2*)
(defparameter *apr-func-1-2* '((x1) (1.0) (yy))
   "Шаблон для построения линейной функции одного параметра: x1 с двумя коэффициентами.
@begin[lang=lisp](code) '((x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)+b @end(code) ")

(export '*apr-func-1-3*)
(defparameter *apr-func-1-3* '((x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c тремя коэффициентами.
@begin[lang=lisp](code) '((x1 x1) (x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(2)+b·x@sub(1)+c @end(code)")

(export '*apr-func-1-4*)
(defparameter *apr-func-1-4* '((x1 x1 x1) (x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c четырьмя коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x1) (x1 x1) (x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(3)+b·x@sub(1)@sup(2)+c·x@sub(1)+d @end(code)
")

(export '*apr-func-1-5*)
(defparameter *apr-func-1-5* '((x1 x1 x1 x1) (x1 x1 x1) (x1 x1) (x1) (1.0) (yy))
  "Шаблон для построения квадратичной функции одного параметра: x1 c пятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x1 x1) (x1 x1 x1) (x1 x1) (x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(4)+b·x@sub(1)@sup(3)+c·x@sub(1)@sup(2)+d·x@sub(1)+e @end(code)
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*apr-func-2-4*)
(defparameter *apr-func-2-4* '((x1 x2) (x1) (x2) (1.0) (yy))
  "@b(Описание:) *apr-func-2-4* шаблон для построения функции двух параметров: 
x1 и x2 c четырьмя коэффициентами.
@begin[lang=lisp](code) '((x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)·x@sub(2)+a@sub(2)·x@sub(1)+a@sub(3)·x@sub(2)+a@sub(4) @end(code)")

(export '*apr-func-2-5*)
(defparameter *apr-func-2-5* '((x1 x1) (x2 x2)         (x1) (x2) (1.0) (yy))
  "@b(Описание:) *apr-func-2-5* шаблон для построения функции двух параметров: 
x1 и x2 c пятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1) (x2 x2)         (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)+a@sub(2)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)+a@sub(4)·x@sub(2)+a@sub(5) @end(code)")

(export '*apr-func-2-6*)
(defparameter *apr-func-2-6* '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy))
  "@b(Описание:) *apr-func-2-6* шаблон для построения функции двух параметров: x1 и x2 c шестью коэффициентами.
@begin[lang=lisp](code) '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)+a@sub(2)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)·x@sub(2)+a@sub(5)·x@sub(1)+a@sub(6)·x@sub(2)+a@sub(7) @end(code)")

(export '*apr-func-2-7*)
(defparameter *apr-func-2-7* '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1) (x2) (1.0) (yy))
  "@b(Описание:) *apr-func-2-7* шаблон для построения функции двух параметров: x1 и x2 c семью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)+a@sub(2)·x@sub(1)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)@sup(2)+a@sub(4)·x@sub(2)@sup(2)+a@sub(5)·x@sub(1)+a@sub(6)·x@sub(2)+a@sub(7)@end(code)")

(export '*apr-func-2-8*)
(defparameter *apr-func-2-8* '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy))
  "@b(Описание:) *apr-func-2-8* шаблон для построения функции двух параметров: x1 и x2 c восемью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)+a@sub(2)·x@sub(1)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)@sup(2)+a@sub(4)·x@sub(2)@sup(2)+a@sub(5)·x@sub(1)·x@sub(2)+a@sub(6)·x@sub(1)+a@sub(7)·x@sub(2)+a@sub(8)@end(code)")

(export '*apr-func-2-9*)
(defparameter *apr-func-2-9* '((x1 x1 x2 x2) (x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy))
  "Шаблон для построения функции двух параметров: x1 и x2 c девятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x2 x2) (x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)@sup(2)+ a@sub(2)·x@sub(1)@sup(2)·x@sub(2)+ a@sub(3)·x@sub(1)·x@sub(2)@sup(2)+ a@sub(4)·x@sub(1)@sup(2)+ a@sub(5)·x@sub(2)@sup(2)+ a@sub(6)·x@sub(1)·x@sub(2)+ a@sub(7)·x@sub(1)+ a@sub(8)·x@sub(2)+ a@sub(9)
@end(code)")
