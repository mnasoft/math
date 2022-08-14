
(in-package #:MATH/STAT)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  'MATH/STAT::*G-T* 'variable
  "Критические значения Gt для критерия Граббса.
|----+-------------------------------------|
| n  | Одно наибольшее или одно наименьшее |
|    | значение при уровне значимости q    |
|----+------------------+------------------|
|    | сыше 1%          | свыше 5%         |
|----+-------------------------------------|
| 3  | 1.155            | 1.155            |
............................................
............................................
............................................
| 40 | 3.381            | 3.036            |
|----+------------------+------------------|
   см. ГОСТ Р 8.736-2011
")

(make-doc
  #'MATH/STAT:VARIATION-COEFFICIENT 'function
  "@b(Описание:) возвращает 
@link[uri=\"https://ru.wikipedia.org/wiki/Коэффициент_вариации\"](коэффициент вариации)
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (variation-coefficient '(1.1 1.0 0.9 1.2 0.8))
@end(code)
")

(make-doc
  #'MATH/STAT:GRUBBS-MIN 'function
  "@b(Описание:) функция grubbs-min возврвщает значения критерия Граббса 
для минимального значения списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.7 9.9 10.1)))
   (grubbs-min lst)) => 1.2863455 
@end(code)
")

(make-doc
  #'MATH/STAT:MAX-NOT-NIL-VALUE 'function
  "@b(Описание:) функция max-not-nil-value возвращает максимальное значение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (max-not-nil-value '(nil 20 nil 5 nil 10)) => 20
@end(code)
")

(make-doc
  #'MATH/STAT:STANDARD-DEVIATION 'function
  "@b(Описание:) функция standard-deviation возвращает среднеквадратичное 
(стандартное) отклонение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (standard-deviation '(1.1 1.0 0.9 1.2 0.8)) => 0.1581139
@end(code)
")

(make-doc
  #'MATH/STAT:AVERAGE 'function
  "@b(Описание:) функция @b(average) возврвщает среднее значение для перечня величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average 1.1 1.0 0.9 1.2 0.8) => 1.0 
@end(code)
")

(make-doc
  #'MATH/STAT:DELTA-MAX-VALUE 'function
  "Возвращает отклонение максимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-max-value '(1.1 1.0 0.9 1.2 0.8)) => 0.20000005
@end(code)
")

(make-doc
  #'MATH/STAT:AVER-MAX-MIN 'function
  "@b(Описание:) функция @b(aver-max-min) возвращает список,  состоящий из:
@begin(list)
 @item(среднего значения величины;)
 @item(максимального значения величины;)
 @item( минимального значения величины.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (aver-max-min '(7.3869333 9.938901 8.541331 10.828626 9.348187 11.323172))
 => (9.561192 11.323172 7.3869333)
@end(code)

")

(make-doc
  #'MATH/STAT:MIN-NOT-NIL-VALUE 'function
  "@b(Описание:) функция min-not-nil-value возвращает минимальное значение для списка величин.

 @b(Переменые:)
@begin(list)
 @item(x - список, содержащий числовые значения или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (min-not-nil-value '(nil 20 nil 5 nil 10)) => 5 
@end(code)
")

(make-doc
  #'MATH/STAT:AVERAGE-VALUE 'function
  "@b(Описание:) функция @b(average-value) возвращает среднее значение для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
(average-value '(1.1 1.0 0.9 1.2 0.8)) => 1.0
@end(code)
")

(make-doc
  #'MATH/STAT:MAX-VALUE 'function
  "@b(Описание:) функция max-value возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(max-value '(1.1 1.0 0.9 1.2 0.8)) => 1.2 
@end(code)
")

(make-doc
  #'MATH/STAT:GRUBBS 'function
  "@b(Описание:) функция grubbs вычисляет значение критерия Граббса (см. п. 6.1 см. ГОСТ Р 8.736-2011).

 @b(Переменые:)
@begin(list)
@item(n - количество повторяющихся измерений величины.)
@item(q - уровень значимости в доях.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.85 9.9 10.1)))
   (grubbs (length lst))) => 2.215 
@end(code)
")

(make-doc
  #'MATH/STAT:DISPERSION 'function
  "@b(Описание:) функция dispersion возвращает дисперсию для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (dispersion '(1.1 1.0 0.9 1.2 0.8)) => 0.025000006
@end(code)
")

(make-doc
  #'MATH/STAT:AVER-DMAX-DMIN 'function
  "@b(Описание:) функция @b(aver-max-min) возвращает список, состоящий из:
@begin(list)
 @item(из среднего значения величины;)
 @item(отклонения максимального занчения в выборке от среднего;)
 @item(отклонения минимального занчения в выборке от среднего.)
@end(list)

 Входящие в список величины округляются до количества значащих цифр равных 
@b(significant-digits).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (aver-dmax-dmin '(17.3869333 19.938901 12.41331 11.828626 10.348187 12.323172)) 
 => (14.04 5.9 -3.69)
@end(code)
")

(make-doc
  #'MATH/STAT:GRUBBS-MAX 'function
  "@b(Описание:) функция grubbs-max возврвщает значения критерия Граббса 
для максимального значения списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числовые значения.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 10.8 9.9 9.85 9.9 10.1)))
   (grubbs-max lst)) => 2.4095862
@end(code)
")

(make-doc
  #'MATH/STAT:AVERAGE-NOT-NIL-VALUE 'function
  "@b(Описание:) функция @b(average-not-nil-value) возвращает среднее значение
для списка величин.

 @b(Переменые:)
@begin(list)
@item(x - список, содержащий числа или nil.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (average-not-nil-value '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) => 1.0 
@end(code)
")

(make-doc
  #'MATH/STAT:DELTA-MIN-VALUE 'function
  "@b(Описание:) функция @b(delta-min-value)
возвращает отклонение минимальной величины от среднего значения
для списка величин.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (delta-min-value '(1.1 1.0 0.9 1.2 0.8)) -0.19999999 
@end(code)
")

(make-doc
  #'MATH/STAT:CLEAN-FLAGRANT-ERROR 'function
  "@b(Описание:) функция @b(clean-flagrant-error) удаляет из статистики грубые промахи.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((lst '(10.0 10.1 10.15 10.2 12.0 9.9 5.0 9.9 10.1)))
   (clean-flagrant-error lst)) (9.9 9.9 10.0 10.1 10.1 10.15 10.2)
@end(code)")

(make-doc
  #'MATH/STAT:CLEAN-MAX-FLAGRANT-ERROR 'function
  "Удаляет из статистики грубые промахи")

(make-doc
  #'MATH/STAT:CLEAN-MIN-FLAGRANT-ERROR 'function
  "Удаляет из статистики грубые промахи")

(make-doc
  #'MATH/STAT:MIN-VALUE 'function
  "@b(Описание:) функция @b(min-value) возвращает максимальное значение для списка величин

 @b(Пример использования:)
@begin[lang=lisp](code)
(min-value '(1.1 1.0 0.9 1.2 0.8)) => 0.8 
@end(code)
")

(make-doc
  #'MATH/STAT:MAKE-RANDOM-VALUE-LIST 'function
  "Создает список случайных величин:

 @b(Переменые:)
@begin(list)
 @item(mid-value     - среднее значение; )
 @item(std-deviation - стандартное отклонение;)
 @item(n             - количество точек; )
 @item(top-level     - дискретизация точек)
@end(list)
")

