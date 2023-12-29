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

(defgeneric transform (point matrix)
  (:documentation
   "@b(Описание:) обобщенная функция @b(transform) возвращает координаты точки
  @b(point), преобразованные с помощью матрицы @b(matrix)."))

(defgeneric swap-rows* (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-rows*) меняет местами строки
@b(i) и @b(j) в матрице @b(matrix) и возвращает его в качестве
результата."))

(defgeneric swap-rows (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-rows) возвращает новый
объект, совпадающий по типу с @b(matrix), у которого строки @b(i) и
@b(j) меняются местами."))

(defgeneric swap-cols* (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-cols*) меняет местами столбцы
@b(i) и @b(j) в матрице @b(matrix) и возвращает его в качестве
результата."))

(defgeneric swap-cols (matrix i j)
  (:documentation
   "@b(Описание:) обобщенная функция @b(swap-cols) возвращает новый
объект, совпадающий по типу с @b(matrix), у которого столбцы @b(i) и
@b(j) меняются местами."))


(defgeneric unite-cols (matrix)
  (:documentation  
  "@b(Описание:) обобщенная функция @b(unite-cols) объединяет столбцы
матрицы (список списков) в вектор (список)."))

(defgeneric unite-rows (matrix)
  (:documentation
   "@b(Описание:) обобщенная @b(unite-rows) объединяет строки матрицы
 (список списков) в вектор (список)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric average-value (matrix)
  (:documentation 
   "@b(Описание:) функция @b(average-value) вычисляет среднее значение по
 элементам матрицы (списка списков)."))

(defgeneric average-not-nil-value (matrix)
  (:documentation
  "@b(Описание:) функция @b(average-not-nil-value) вычисляет среднее
значение по элементам матрицы (списка списков) с исключением
nil-элементов."))

;;;;

(defgeneric average-row-value (matrix)
  (:documentation  
  "@b(Описание:) функция @b(average-row-value) вычисляет 
средние значения в строках матрицы (списка списков)."))

(defgeneric average-row-not-nil-value (matrix)
  (:documentation  
  "@b(Описание:) функция @b(average-row-not-nil-value) вычисляет среднее
 значение по элементам матрицы (списка списков)."))

;;;;

(defgeneric average-col-value (matrix)
  (:documentation 
   "@b(Описание:) функция @b(average-col-value) вычисляет среднее
значение по столбцам матрицы (списка списков)."))

(defgeneric average-col-not-nil-value (matrix)
  (:documentation  
  "@b(Описание:) функция @b(average-col-not-nil-value) вычисляет среднее 
значение по элементам матрицы (списка списков)."))

(defgeneric append-col (col matrix)
  (:documentation    
  "@b(Описание:) функция @b(append-col) добавляет столбец @b(col) к
матрице @b(matrix)."))

(defgeneric append-row (row matrix)
  (:documentation      
  "@b(Описание:) функция @b(append-row) возвращает новую матрицу,
формируемую путем добавления строки @b(row) к матрице @b(matrix)."))

(defgeneric prepend-col (col matrix)
  (:documentation
   "@b(Описание:) функция @b(prepend-col) возвращает новую матрицу,
формируемую путем добавления столбца @b(col) к матрице @b(matrix)
перед ее первым столбцом (имеющим индекс 0)."))

(defgeneric prepend-row (row matrix)
  (:documentation
   "@b(Описание:) функция @b(prepend-row) возвращает новую матрицу,
формируемую путем добавления строки @b(row) к матрице @b(matrix) перед
ее первой строкой (имеющей индекс 0)."))

(defgeneric prepend-rows (rows matrix)
  (:documentation
   "@b(Описание:) функция @b(prepend-rows) возвращает новую матрицу,
формируемую путем добавления строк @b(rows) к матрице @b(matrix) перед
ее первой строкой (имеющей индекс 0)."))

(defgeneric rotate-x (angle)
  (:documentation
   "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на выраженный в
радианах угол @b(angle) вокруг оси X."))

(defgeneric rotate-y (angle)
  (:documentation
   "@b(Описание:) метод @b(rotate-y) возвращает однородную матрицу
преобразования, которая вращает систему координат на выраженный в
радианах угол @b(angle) вокруг оси Y."))

(defgeneric rotate-z (angle)
  (:documentation
   "@b(Описание:) метод @b(rotate-z) возвращает однородную матрицу
преобразования, которая вращает систему координат на выраженный в
радианах угол @b(angle) вокруг оси Z."))

(defgeneric rotate-v (angle vector)
  (:documentation
   "@b(Описание:) метод @b(rotate-v) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол @b(angle)
вокруг оси, заданной вектором @b(v). Вектор @b(vector) должен быть
нормализованным (иметь единичную длину)."))

(defgeneric rotate-around (p1 p2 angle)
  (:documentation
  "@b(Описание:) метод @b(rotate-around) возвращает однородную матрицу
преобразования, которая вращает протсранство вокруг оси, заданной
точками @b(p1) и @b(p2), на угол @b(angle)."))
