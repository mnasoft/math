(in-package :math/smooth)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  #'MATH/SMOOTH:HANN-SMOOTHING 'function
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
")

(make-doc
  #'MATH/SMOOTH:GAUSS-SMOOTHING 'function
  "@b(Описание:) функция @b(gauss-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (gauss-smoothing d)))
@end(code)
")

(make-doc
  #'MATH/SMOOTH:CAUCHY-SMOOTHING 'function
  "@b(Описание:) функция @b(cauchy-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (cauchy-smoothing d)))
@end(code)
")

(make-doc
  #'MATH/SMOOTH:WEIGHT-FUNC-P 'function
  NIL)

(make-doc
  #'MATH/SMOOTH:WEIGHT-FUNC-LIST 'function
  NIL)

(make-doc
  #'MATH/SMOOTH:EXP-SMOOTHING 'function
  "@b(Описание:) функция @b(exp-smoothing)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (loop :for d :from 0 :to 4 :by 1/10 :collect
     (list d (exp-smoothing d)))
@end(code)
")
