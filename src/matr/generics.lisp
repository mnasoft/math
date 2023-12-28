;;;; ./src/matr/defgeneric.lisp
(in-package :math/matr)

(defgeneric mref (matrix row col)
  (:documentation
   "@b(Описание:) обобщенная функция @b(mref) возвращает элемент матрицы,
находяшийся в строке @b(row) и столбце @b(col). Нумерация строк и
столбцов начинается с нуля."))

(defgeneric (setf mref) (value matrix row col)
  (:documentation
   "@b(Описание:) обобщенная функция @b((setf mref)) устанавливает
значение @b(value) элементу матрицы, находящемуся в строке @b(row) и
столбце @b(col). Нумерация строк и столбцов начинается с нуля."))

(defgeneric transpose (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(transpose) возвращает матрицу того
же типа, что и @b(matrix) являющуютя результатом ее транспонирования."))

(defgeneric rows (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(rows) возвращает количество строк
матрицы @b(matrix)."))

(defgeneric cols (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(cols) возврвщает количество столбцов
матрицы @b(matrix)."))

(defgeneric row (matrix row)
  (:documentation
   "@b(Описание:) обобщенная функция @b(row) возвращает строку @b(row)
матрицы @b(matrix)."))

(defgeneric col (matrix col)
  (:documentation
   "@b(Описание:) обобщенная функция @b(col) возвращает строку @b(col)
 матрицы @b(matrix)."))

(defgeneric (setf row) (values matrix row)
  (:documentation
   "@b(Описание:) обобщенная функция @b((setf row)) заменяет строку
 @b(row) матрицы @b(matrix) элементами, находящимися в списке
 @b(values)."))

(defgeneric (setf col) (values matrix col)
  (:documentation
   "@b(Описание:) обобщенная функция @b((setf col)) заменяет столбец
 @b(col) матрицы @b(matrix) элементами, находящимися в списке
 @b(values)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix)
  (:documentation
   "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) )

(defgeneric matr-name-* (matrix))

(defgeneric copy (obj)
  (:documentation
    "@b(Описание:) обобщенная функция @b(copy) возвращает ссылку на новый объект,
созданный на основе @b(obj)."))

(defgeneric dimensions (matrix)
  (:documentation
    "@b(Описание:) обобщенная функция @b(dimensions) возвращает список,
 содержащий размерности матрицы @b(matrix)."))

(defgeneric equivalent (matrix-1 matrix-2 &key test)
  (:documentation
     "@b(Описание:) обобщенная функция @b(equivalent) возвращает T,
если матирицы @b(matrix-1) и @b(matrix-2) имеют одинаковые размерности и их 
соответствующие элементы равны (для них функция @b(test) возвращает T )."))

(defgeneric main-diagonal (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(main-diagonal) извлекает главную
 диагональ матрицы.

Элементы возвращаются в порядке возрастания строк."))

(defgeneric (setf main-diagonal) (elements matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b((setf main-diagonal))
 устанавливает новые значения элементам матрицы @b(matrix),
 находящимся на главной диагонали.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(defgeneric squarep (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(squarep) возвращает T, 
если матрица @b(matrix) является квадратной."))

(defgeneric anti-diagonal (matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(anti-diagonal)
возвращает список элементов, находящихся на побочной диагонали матрицы.

 В результирующем списке элементы следуют по строкам.

 Д.б опредена только для квадратной матрицы."))

(defgeneric (setf anti-diagonal) (elements matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b((setf anti-diagonal)) устанавливет 
новые значения элементам матрицы @b(matrix), на побочной диагонали матрицы.

 Элементы @b(elements) устанавливаются в порядке возрастания строк."))

(defgeneric add (a b)
  (:documentation
   "@b(Описание:) обобщенная функция @b(add) выполняет сложение
 аргументов @b(a) и @b(b)."))

(defgeneric multiply (a b)
  (:documentation
   "@b(Описание:) обобщенная функция @b(multiply) выполняет перемножение
 аргументов @b(a) и @b(b)."))

(defgeneric rotate-x (α)
  (:documentation
     "@b(Описание:) обобщенная функция @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол α вокруг
  оси x."))

(defgeneric transform (point matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(transform) возвращает координаты точки
  @b(point), преобразованные с помощью матрицы @b(matrix)."))

(defgeneric swap-rows (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-rows) возвращает новый
объект, совпадающий по типу с @b(matrix), у которого строки @b(i) и
@b(j) меняются местами."))

(defgeneric swap-rows* (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-rows*) меняет местами строки
@b(i) и @b(j) в матрице @b(matrix) и возвращает его в качестве
результата."))
