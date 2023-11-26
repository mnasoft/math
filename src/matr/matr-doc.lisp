
(in-package :MATH/MATR)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (closer-mop:class-direct-slots  (find-class class))
        :key #'closer-mop:slot-definition-name))

(make-doc
  (find-method #'MATH/MATR:TRANSPOSE NIL '(CONS))
  t
  "Выполняет транспонирование")

(make-doc
  (find-method #'MATH/MATR:ROWS NIL '(ARRAY))
  t
  "@b(Описание:) метод @b(rows) возвращает количество строк в массиве @b(a).")


