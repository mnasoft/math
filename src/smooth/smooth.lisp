;;;; smoothing.lisp
;;;; ./src/smooth/smooth.lisp

(defpackage :math/smooth
  (:use #:cl)
  (:export gauss-smoothing
	   exp-smoothing
	   cauchy-smoothing
	   hann-smoothing)
  (:export weight-func-list
	   weight-func-p)
  (:documentation
   "@b(Описание:) Пакет @b(:math/smooth) содержит весовые функции
для методов сглаживания данных.

@begin(section) @title(Доступные весовые функции)

@begin(list)
 @item(@b(gauss-smoothing) — весовая функция Гаусса.)
 @item(@b(exp-smoothing) — экспоненциальная весовая функция.)
 @item(@b(cauchy-smoothing) — весовая функция Коши.)
 @item(@b(hann-smoothing) — весовая функция Ханна.)
 @item(@b(weight-func-list) — список весовых функций; @b(weight-func-p) — предикат проверки.)
@end(list)

@end(section)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (ql:quickload :math)
 (loop :for d :from 0.0 :to 1.0 :by 0.25
       :collect (math/smooth:gauss-smoothing d))
 => (1.0 0.9394131 0.7788008 0.5698403 0.36787944)
@end(code)"))

(in-package :math/smooth)

(defun gauss-smoothing (d)
  "@b(Описание:) функция @b(gauss-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
@end(code)"  
  (exp (* -1 d d)))

(defun exp-smoothing (d)
  "@b(Описание:) функция @b(exp-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
@end(code)"  
  (exp (* -1 d)))

(defun cauchy-smoothing (d)
  "@b(Описание:) функция @b(cauchy-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
@end(code)"  
  (/ 1 (+ 1 (* d d))))

(defun hann-smoothing (d)
  "@b(Описание:) функция @b(hann-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
  (loop :for d :from 0 :to 4 :by 0.1 :do
     (format t \"~{~5F~^ ~}~%\" 
             (list d 
               (gauss-smoothing  d)
               (exp-smoothing    d)
               (cauchy-smoothing d)
               (hann-smoothing   d))))
@end(code)"  
  (if (< d 1) (* 1/2 ( + 1 ( cos (* pi d)))) 0))

(defun weight-func-list ()
  (list #'gauss-smoothing
	#'exp-smoothing
	#'cauchy-smoothing
	#'hann-smoothing))

(defun weight-func-p (func)
  (if (member func (weight-func-list)) t nil))
