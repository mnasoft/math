(in-package :math/core)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))


(make-doc
 #'square
 'function 
 "@b(Описание:) функция @b(square) возвращает квадрат значения.

 @b(Переменые:)
@begin(list)
 @item(x - число.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (square 5) => 25 
 (square -4) => 16 
@end(code)")

(make-doc
 #'exclude-nil-from-list
 'function 
 "@b(Описание:) функция @b(exclude-nil-from-list) возвращает список в
 котором нет nil-элементов (они исключаются).

 @b(Пример использования:) @begin[lang=lisp](code)
 (exclude-nil-from-list '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) 
 => (1.1 1.0 0.9 1.2 0.8)
@end(code)")

(make-doc
 #'e-value
 'function
 "Возвращает приближенное значение числа e
Пример использования:
  (coerce (e-value      1) 'double-float) 2.0d0
  (coerce (e-value      2) 'double-float) 2.5d0
  (coerce (e-value      4) 'double-float) 2.7083333333333335d0
  (coerce (e-value      6) 'double-float) 2.7180555555555554d0
  (coerce (e-value      8) 'double-float) 2.71827876984127d0
  (coerce (e-value     10) 'double-float) 2.7182818011463845d0
  (coerce (e-value     15) 'double-float) 2.7182818284589945d0
  (coerce (e-value     16) 'double-float) 2.7182818284590424d0
  (coerce (e-value     17) 'double-float) 2.718281828459045d0
  (coerce (e-value  10000) 'double-float) 2.718281828459045d0
  (exp 1.0d0)                             2.718281828459045d0
"   

 )

(make-doc
 #'split-range
 'function
 "@b(Описание:) split-range

 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-range 10 20 5)  => (10.0 12.0 14.0 16.0 18.0 20.0)
@end(code)
 "
 )

(make-doc
 #'split-range-by-func
 'function
 "@b(Описание:) split-range-by-func

 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-range-by-func 1 10 5) => (1.0 1.5848932 2.5118864 3.981072 6.3095737 10.0)
 (split-range-by-func 1 10 10) =>
 (1.0 1.2589254 1.5848932 1.9952624 2.5118864 3.1622777 3.981072 5.0118723  6.3095737 7.943282 10.0)
@end(code)
"
 )

(make-doc
 #'depth-sphere-along-cone
 'function
 "@b(Описание:) функция @b(depth-sphere-along-cone) возвращает 
заглубление сферы с радиусом R в конуc с углом при вершине 
равным alpha от линии пересечения конуса с цилиндром."
 )

(make-doc
 #'round-to-significant-digits
 'function
 "@b(Описание:) функция @b(round-to-significant-digits) округляет значение
val до количества значащих цифр, задаваемых аргументом significant-digits.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (round-to-significant-digits 456.32738915923           ) => 456.3
 (round-to-significant-digits 456.32738915923 6         ) => 456.327
 (round-to-significant-digits 456.32738915923 6 53562.23) => 456.3
@end(code)
"
 )

(make-doc '+significant-digits+ 'variable
          "Определяет количество значащих цифр при округлении по умолчанию.")

(make-doc
 #'summ-distance 'function
 "@b(Описание:) обобщенная функция @b(summ-distance) 
возвращает сумму расстояний по каждому направлению."
 )

(make-doc
 #'summ-distance 'function
 "@b(Описание:) обобщенная функция @b(summ-distance) 
возвращает сумму расстояний по каждому направлению."
 )

(make-doc
 #'distance 'function
 "@b(Описание:) обобщенная функция @b(distance)
возвращает расстояние между x1 и x2. Как корень квадратный из 
сумм квадратов расстояний по каждому направлению.")

(make-doc
 #'distance-relative 'function
 "@b(Описание:) обобщенная функция @b(distance-relative)
возвращает относительную длину между x и xi, длина приведения dx.
Корень квадратный из сумм квадратов расстояний по каждому направлению
отнесенному к длине приведения.")

(make-doc
 #'matr-name-* 'function
 "Matr")

(make-doc
 #'dimensions 'function 
 "@b(Описание:) обобщенная_функция @b(dimensions) возвращает список,
содержащий размерности матрицы @b(matrix).")

(make-doc
 #'rows 'function
 "@b(Описание:) обобщенная_функция @b(rows) возврвщает количество строк
матрицы @b(matrix).")

(make-doc
 #'cols 'function
 "@b(Описание:) обобщенная_функция @b(cols) возврвщает количество столбцов
матрицы @b(matrix).")

(make-doc
 #'equivalent 'function
 "@b(Описание:) обобщенная_функция @b(equivalent) возвращает T,
если матирицы @b(matrix-1) и @b(matrix-2) имеют одинаковые размерности и их 
соответствующие элементы равны (для них функция @b(test) возвращает T ).")

(make-doc
 #'main-diagonal 'function
 "@b(Описание:) обобщенная_функция @b(main-diagonal) извлекает главную
 диагональ матрицы.

Элементы возвращаются в порядке возрастания строк.")

(make-doc
 #'row 'function
 "@b(Описание:) обобщенная_функция @b(row) 
возвращает строку @b(row) матрицы @b(matrix).")

(make-doc
 #'col 'function
 "@b(Описание:) обобщенная_функция @b(col) 
возвращает строку @b(col) матрицы @b(matrix).")


(make-doc
 #'anti-diagonal 'function
 "@b(Описание:) обобщенная_функция @b(anti-diagonal)
возвращает список элементов, находящихся на побочной диагонали матрицы.

 В результирующем списке элементы следуют по строкам.

 Д.б опредена только для квадратной матрицы.")

(make-doc
 #'squarep 'function
 "@b(Описание:) обобщенная_функция @b(squarep) возвращает T, 
если матрица @b(matrix) является квадратной.")

(make-doc
 #'mref 'function
 "@b(Описание:) обобщенная_функция @b(mref) возвращает элемент матрицы,
находяшийся в строке @b(row) и столбце @b(col).")

(make-doc
 #'copy 'function
 "@b(Описание:) обобщенная_функция @b(copy) возвращает ссылку на новый объект,
созданный на основе @b(obj).")

(make-doc
 #'add 'function
 "@b(Описание:) обобщенная_функция @b(multiply)
выполняет сложение аргументов @b(a) и @b(b).")

(make-doc
 #'multiply 'function
 "@b(Описание:) обобщенная_функция @b(multiply) 
выполняет перемножение аргументов @b(a) и @b(b).")

(make-doc
 #'transpose 'function
 "@b(Описание:) обобщенная_функция @b(transpose) 
возвращает транспонированную матрицу.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(make-doc
  (find-method #'DISTANCE NIL '(NUMBER NUMBER))
  t
  "@b(Описание:) 

@b(Пример использования:)
 (distance 1 0 ) => 1
 (distance 2 0 ) => 2
 (distance 2 1 ) => 1
")

(make-doc
  (find-method #'DISTANCE NIL '(CONS CONS))
  t
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance '(1 1 1) '(0 0 0)) => 1.7320508 = (sqrt (+ 1 1 1))
 (distance '(2 2 2) '(0 0 0)) => 3.4641016 = (sqrt (+ 4 4 4))
 (distance '(2 1 2) '(0 0 0)) => 3.0 =       (sqrt (+ 4 1 4))
@end(code)
")

(make-doc
  (find-method #'DISTANCE NIL '(VECTOR VECTOR))
  t
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance #(1 1 1) #(0 0 0)) => 1.7320508
 (distance #(2 2 2) #(0 0 0)) => 3.4641016
 (distance #(2 1 2) #(0 0 0)) => 3.0
@end(code)
")

(make-doc
  (find-method #'SUMM-DISTANCE NIL '(VECTOR VECTOR))
  t
  "@b(Описание:) функция @b(summ-distance) возвращает сумму 
расстояний по каждому направлению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (summ-distance #(1 2 3) #(3 2 1)) => 4 = (+ 2 0 2)
@end(code)
")

(make-doc
  (find-method #'SUMM-DISTANCE NIL '(CONS CONS))
  t
  "@b(Описание:) функция @b(summ-distance) возвращает сумму 
расстояний по каждому направлению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (summ-distance '(1 2 3) '(3 2 1)) => 4 = (+ 2 0 2)
@end(code)
")

(make-doc
  (find-method #'DISTANCE-RELATIVE NIL '(NUMBER NUMBER NUMBER))
  t
  "Пример использования:
 (distance-relative 1 0 1) 1.0
 (distance-relative 2 0 1) 2.0
 (distance-relative 2 0 2) 1.0
")

(make-doc
  (find-method #'DISTANCE-RELATIVE NIL '(CONS CONS CONS))
  t
  "Пример использования:
 (distance-relative '(1 1 1) '(0 0 0) '(1 1 1)) 1.7320508
 (distance-relative '(2 2 2) '(0 0 0) '(1 1 1)) 3.4641016
 (distance-relative '(2 2 2) '(0 0 0) '(1 2 2)) 2.4494898
")

(make-doc
  (find-method #'DISTANCE-RELATIVE NIL '(VECTOR VECTOR VECTOR))
  t
  "@b(Описание:) метод @b(distance-relative) возвращает относительное 
расстояние от точки @b(x) до точки @b(xi) по отношению к базовым длинам,
находящимся в @b(dx).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance-relative #(1 1 1) #(0 0 0) #(1 1 1)) => 1.7320508 
 (distance-relative #(1 1 1) #(0 0 0) #(2 2 2)) => 0.8660254 
 (distance-relative #(1 2 3) #(0 0 0) #(3 2 1)) => 3.1797974 = (sqrt (+ (* 1/3 1/3) (* 2/2 2/2) (* 3/1 3/1)))
@end(code)
")
