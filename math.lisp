;;;; package.lisp

(defpackage #:math 
  (:use #:cl #:math/core #:math/arr-matr)
  (:export mult-matr-vect )
  (:export split-range
	   split-range-by-func)
  (:export row  col
	   rows cols)
  (:export round-to-significant-digits))

(in-package :math)

;;;; (use-package (find-package :math/arr-matr) (find-package :math))
;;;; (use-package (find-package :math/core) (find-package :math))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (asdf:system-description (asdf:find-system :math))
      (uiop:read-file-form 
       (make-pathname
        :directory (pathname-directory
                    (asdf:system-definition-pathname :math))
        :name "description")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +significant-digits+ 4)

(export '(round-to-significant-digits))

(defun round-to-significant-digits (val &optional (significant-digits +significant-digits+) (base-val val))
  "@b(Описание:) функция @b(round-to-significant-digits) округляет значение
val до количества значащих цифр, задаваемых аргументом significant-digits.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (round-to-significant-digits 456.32738915923           ) => 456.3
 (round-to-significant-digits 456.32738915923 6         ) => 456.327
 (round-to-significant-digits 456.32738915923 6 53562.23) => 456.3
@end(code)
"
  (labels ((find-divisor (val)
	     "@b(Описание:) функция @b(find-divisor)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-divisor 10.964739714723287d0)
@end(code)
"
	     (do ((divisor 1))
		 ((<= 1 (abs (* val divisor)) 10) divisor)
	       (cond
		 ((< (abs (* val divisor)) 1) (setf divisor (* divisor 10)))
		 ((< 1 (abs (* val divisor))) (setf divisor (/ divisor 10))))))

	   (my-round (val &optional (sb-kernel::divisor 1))
	     "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (my-round 10.964739714723287d0 1/100) => 10.96
@end(code)
"
	     (coerce (* (round val sb-kernel::divisor)
			sb-kernel::divisor)
		     'single-float)))
    (my-round val
              (/ (expt 10 (* -1 (- significant-digits 1)))
		 (find-divisor base-val)))))


(my-round-n 10.964739714723287d0 8)
significant digits

0.1565d0

(format t "~F" 15/100) ; 0.15 => NIL

(my-round-n 0.0010964739714723287d0)

(my-round 10.964739714723287d0 1/100)

(math:
