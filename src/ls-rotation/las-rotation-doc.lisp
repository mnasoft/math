(in-package :math/ls-rotation)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MATH/LS-ROTATION:SOLVE-LINEAR-SYSTEM-ROTATION 'function
  "@b(Описание:) функция @b(solve-linear-system-rotation) решает систему линейных
алгебраических уравнений (СЛАУ) методом вращения, состоящего из:
@begin(list)
 @item(сведения СЛАУ к треугольной системе;)
 @item(нахождение корней методом обратного хода метода Гаусса. )
@end(list)


 @b(Переменые:)
@begin(list)
@item(matr - массив, у которого количество строк (первая размерность)
должно быть на единицу меньше количества столбцов (вторая размерность).
Данная матрица меняется в процессе в процессе вычисления функции)
@end(list)

  @b(Возвращает:) вектор с количеством элементов равным количеству строк в СЛАУ.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (require :cl-utilities)
 (let* ((data '((0.0d0 0.0d0 1.0d0 3.0d0)
                (0.0d0 1.0d0 0.0d0 2.0d0)
                (1.0d0 0.0d0 1.0d0 4.0d0)))
       (mtr (make-array '(3 4) :initial-contents data))
       (m (cl-utilities:copy-array mtr)))
  (values (math:solve-linear-system-rotation mtr)))
 => #(1.0d0 2.0d0 3.0d0)

 (let* ((data '((1.0d0 0.0d0 1.0d0 4.0d0)
	       (0.0d0 0.0d0 1.0d0 3.0d0)
	       (0.0d0 1.0d0 0.0d0 2.0d0)))
       (mtr (make-array '(3 4) :initial-contents data))
       (m (cl-utilities:copy-array mtr)))
  (values (math:solve-linear-system-rotation mtr)))
 => #(1.0d0 2.0d0 3.0d0)

 (let ((m-test (make-array '(3 4)
			  :initial-contents
			  '((10.0d0 11.0d0  12.0d0  4.0d0)
			    (15.0d0 17.0d0  21.0d0  2.0d0)
			    (70.0 8.0  10.0 3.0)))))
  (solve-linear-system-rotation (cl-utilities:copy-array m-test)))
 =>#(0.03588235294117642d0 2.182352941176469d0 -1.6970588235294102d0)
@end(code)

 Есть необходимость доработки с точки зрения решения разреженной СЛАУ!
")

