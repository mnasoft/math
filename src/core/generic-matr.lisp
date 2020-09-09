;;;; /src/core/generic-matr.lisp

(in-package :math/core)

(export 'matr-name-*)

(defgeneric matr-name-* (matrix) (:documentation "Matr"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'dimensions)

(defgeneric dimensions (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(dimensions) возвращает список,
содержащий размерности матрицы @b(matrix)."))

(export 'rows)

(defgeneric rows (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(rows) возврвщает количество строк
матрицы @b(matrix)."))

(export 'cols)

(defgeneric cols (matrix)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(cols) возврвщает количество столбцов
матрицы @b(matrix)."))

(export 'equivalent)

(defgeneric equivalent (matrix-1 matrix-2 &key test) 
    (:documentation "@b(Описание:) обобщенная_функция @b(equivalent) возвращает T,
если матирицы @b(matrix-1) и @b(matrix-2) имеют одинаковые размерности и их 
соответствующие элементы равны (для них функция @b(test) возвращает T )."))

(export 'row)

(defgeneric row (matrix row)
    (:documentation "@b(Описание:) обобщенная_функция @b(row) 
возвращает строку @b(row) матрицы @b(matrix)."))

(export 'row)

(defgeneric (setf row) (values matrix row)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf row))
заменяет строку @b(row) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

(export 'col)

(defgeneric col (matrix col)
  (:documentation "@b(Описание:) обобщенная_функция @b(col) 
возвращает строку @b(col) матрицы @b(matrix)."))

(export 'col)

(defgeneric (setf col) (values matrix col)
    (:documentation "@b(Описание:) обобщенная_функция @b((setf col))
заменяет столбец @b(col) матрицы @b(matrix) элементами, находящимися в списке @b(values)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'main-diagonal)

(defgeneric main-diagonal (matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b(main-diagonal) извлекает главную диагональ матрицы.

 Элементы возвращаются в порядке возрастания строк."))

(export 'main-diagonal)

(defgeneric (setf main-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf main-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), находящимся на главной диагонали.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(export 'anti-diagonal)

(defgeneric anti-diagonal (matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b(anti-diagonal)
возвращает список элементов, находящихся на побочной диагонали матрицы.

 В результирующем списке элементы следуют по строкам.

 Д.б опредена только для квадратной матрицы."))

(export 'anti-diagonal)

(defgeneric (setf anti-diagonal) (elements matrix)
  (:documentation
"@b(Описание:) обобщенная_функция @b((setf anti-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), на побочной диагонали матрицы.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(export 'squarep)

(defgeneric squarep (matrix) 
  (:documentation
   "@b(Описание:) обобщенная_функция @b(squarep) возвращает T, 
если матрица @b(matrix) является квадратной."))

(export 'mref)

(defgeneric mref (matrix row col)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(mref) возвращает элемент матрицы,
находяшийся в строке @b(row) и столбце @b(col)."))

(export 'mref)

(defgeneric (setf mref) (value matrix row col)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(mref) устанавливает
значение @b(value) элементу матрицы, находящемуся в 
строке @b(row) и столбце @b(col) ."))

(export 'copy)

(defgeneric copy (obj)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(copy) возвращает ссылку на новый объект,
созданный на основе @b(obj)."))

(export 'add)

(defgeneric add (a b)
  (:documentation "@b(Описание:) обобщенная_функция @b(multiply)
выполняет сложение аргументов @b(a) и @b(b)."))

(export 'multiply)

(defgeneric multiply (a b)
    (:documentation "@b(Описание:) обобщенная_функция @b(multiply) 
выполняет перемножение аргументов @b(a) и @b(b)."))

(export 'transpose)

(defgeneric transpose (matrix)
  (:documentation "@b(Описание:) обобщенная_функция @b(transpose) 
возвращает транспонированную матрицу."))
