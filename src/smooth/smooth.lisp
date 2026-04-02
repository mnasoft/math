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
  "@b(Описание:) функция @b(gauss-smoothing) возвращает значение
гауссовой весовой функции для расстояния @b(d), базируется на exp(-d^2).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gauss-smoothing 0.0) => 1.0
 (gauss-smoothing 1.0) => 0.36787944
@end(code)"
  (exp (* -1 d d)))

(defun exp-smoothing (d)
  "@b(Описание:) функция @b(exp-smoothing) возвращает экспоненциальную
весовую функцию exp(-d).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (exp-smoothing 0.0) => 1.0
 (exp-smoothing 1.0) => 0.36787944
@end(code)"
  (exp (* -1 d)))

(defun cauchy-smoothing (d)
  "@b(Описание:) функция @b(cauchy-smoothing) возвращает вес по формуле
1/(1+d^2) (Коши).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cauchy-smoothing 0.0) => 1.0
 (cauchy-smoothing 1.0) => 0.5
@end(code)"
  (/ 1 (+ 1 (* d d))))

(defun hann-smoothing (d)
  "@b(Описание:) функция @b(hann-smoothing) возвращает весовую функцию
Ханна: 0.5*(1+cos(pi*d)) для d<1 иначе 0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (hann-smoothing 0.0) => 1.0
 (hann-smoothing 0.5) => 0.5
 (hann-smoothing 1.0) => 0.0
@end(code)"
  (if (< d 1) (* 1/2 (+ 1 (cos (* pi d)))) 0))

(defun weight-func-list ()
  (list #'gauss-smoothing
	#'exp-smoothing
	#'cauchy-smoothing
	#'hann-smoothing))

(defun weight-func-p (func)
  "@b(Описание:) предикат @b(weight-func-p) возвращает t, если @b(func)
является одной из поддерживаемых весовых функций.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (weight-func-p #'gauss-smoothing) => t
 (weight-func-p #'list) => nil
@end(code)"
  (not (null (member func (weight-func-list) :test #'eq))))
