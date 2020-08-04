;;;; smoothing.lisp

(in-package #:math)

(annot:enable-annot-syntax)

@export
@doc
"@b(Описание:) функция @b(gauss-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
@end(code)
"
(defun gauss-smoothing (d)
  (exp (* -1 d d)))

@export
@doc
"@b(Описание:) функция @b(exp-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
@end(code)
"
(defun exp-smoothing (d)
  (exp (* -1 d)))

@export
@doc
"@b(Описание:) функция @b(cauchy-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
@end(code)
"
(defun cauchy-smoothing (d)
  (/ 1 (+ 1 (* d d))))

@export
@doc
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
@end(code)
"
(defun hann-smoothing (d)
  (if (< d 1) (* 1/2 ( + 1 ( cos (* pi d)))) 0))
