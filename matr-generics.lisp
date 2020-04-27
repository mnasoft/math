;;;; matr-generics.lisp

(in-package #:math)
(annot:enable-annot-syntax)

@export
(defgeneric matr-name-* (matrix) (:documentation "Matr"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
(defgeneric dimensions (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(dimensions) возвращает список,
содержащий размерности матрицы @b(matrix)."))

@export
(defgeneric rows (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(rows) возврвщает количество строк
матрицы @b(matrix)."))

@export
(defgeneric cols (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(cols) возврвщает количество столбцов
матрицы @b(matrix)."))

@export
(defgeneric equivalent (matrix-1 matrix-2 &key test) 
    (:documentation "@b(Описание:) обобщенная_функция @b(equivalent) возвращает T,
если матирицы @b(matrix-1) и @b(matrix-2) имеют одинаковые размерности и их 
соответствующие элементы равны (для них функция @b(test) возвращает T )."))

@export
(defgeneric row (matrix row)
    (:documentation "@b(Описание:) обобщенная_функция @b(row) 
возвращает строку @b(row) матрицы @b(matrix)."))

@export
(defgeneric (setf row) (values matrix row)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf row))
заменяет строку @b(row) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

@export
(defgeneric col (matrix col)
  (:documentation "@b(Описание:) обобщенная_функция @b(col) 
возвращает строку @b(col) матрицы @b(matrix)."))

@export
(defgeneric (setf col) (values matrix col)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf col))
заменяет столбец @b(col) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@export
(defgeneric main-diagonal (matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b(main-diagonal) извлекает главную диагональ матрицы.

 Элементы возвращаются в порядке возрастания строк."))

@export
(defgeneric (setf main-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf main-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), находящимся на главной диагонали.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))


@export
(defgeneric anti-diagonal (matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b(anti-diagonal)
возвращает список элементов, находящихся на побочной диагонали матрицы.

 В результирующем списке элементы следуют по строкам.

 Д.б опредена только для квадратной матрицы."))

@export
(defgeneric (setf anti-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf anti-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), на побочной диагонали матрицы.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

@export
(defgeneric squarep (matrix) 
  (:documentation
   "@b(Описание:) обобщенная_функция @b(squarep) возвращает T, 
если матрица @b(matrix) является квадратной."))

@export
(defgeneric mref (matrix row col)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(mref) возвращает элемент матрицы,
находяшийся в строке @b(row) и столбце @b(col)."))

@export
(defgeneric (setf mref) (value matrix row col)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(mref) устанавливает
значение @b(value) элементу матрицы, находящемуся в 
строке @b(row) и столбце @b(col) ."))

@export
(defgeneric copy (obj)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(copy) возвращает ссылку на новый объект,
созданный на основе @b(obj)."))

@export
(defgeneric convert-to-triangular (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(convert-to-triangular) 
выполняет приведение  матрицы @b(matrix) к треугольному виду,
для решения системы ЛУ методом Гаусса."))


@export
(defgeneric solve-linear-system-gauss (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(solve-linear-system-gauss)
возвращает матрицу, содержащую корни решения системы линейных уравений.

 Решение системы линейных уравнений выполняется методом Гаусса.
"))

@export
(defgeneric add (a b)
  (:documentation "@b(Описание:) обобщенная_функция @b(multiply)
выполняет сложение аргументов @b(a) и @b(b)."))

@export
(defgeneric multiply (a b)
    (:documentation "@b(Описание:) обобщенная_функция @b(multiply) 
выполняет выполняет перемножение аргументов @b(a) и @b(b)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix) (:documentation "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) (:documentation "Проверка матриц на равенство"))

;;;; (setf *print-case* :downcase)
;;;; (setf *print-case* :upcase)

