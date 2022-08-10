;;;; ./src/series/series.lisp

(in-package :math/series)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  #'MATH/SERIES:<GEOMETRIC>-Q 'function
  NIL)

(make-doc
  #'MATH/SERIES:<ARITHMETIC>-D 'function
  NIL)

(make-doc
  #'MATH/SERIES:<ARITHMETIC>-A 'function
  NIL)

(make-doc
  #'MATH/SERIES:SCALE-BY-NUMBER&SUMM 'function
  "@b(Описание:) обобщенная_функция @b(scale-by-number&summ) возвращает
 масштаб (разность для арифмеической прогрессии или степень для
 геометрической прогрессии) такую, чтобы сумма @b(n) членов
 прогрессии равнялась @b(S).")

(make-doc
  #'MATH/SERIES:FIRST-BY-NUMBER&SUMM 'function
  "@b(Описание:) обобщенная функция @b(first-by-number&summ)
 возвращает первый член прогрессии, такой чтобы сумма n членов
 равнялась @b(S). При этом прогрессия сама поргрессия @b(series)
 изменяется.")

(make-doc
  #'MATH/SERIES:<GEOMETRIC>-B 'function
  NIL)

(make-doc
  #'MATH/SERIES:SUMM 'function
  "@b(Описание:) обобщенная_функция @b(summ) возвращает сумму @b(n)
 членов прогрессии.")

(make-doc
  #'MATH/SERIES:ITEM 'function
  "@b(Описание:) обобщенная_функция @b(item) возвращает значение
 @b(i)-того члена прогрессии.")

(make-doc
  #'MATH/SERIES::ITEMS-BY-SUMM 'function
  "@b(Описание:) обобщенная функция @b(items-by-summ) возвращает
 количество членов прогрессии, чтобы их сумма равнялась @b(summ).")

(make-doc
  #'(setf MATH/SERIES:<GEOMETRIC>-Q)
  'function
  NIL)

(make-doc
  #'(setf MATH/SERIES:<ARITHMETIC>-D)
  'function
  NIL)

(make-doc
  #'(setf MATH/SERIES:<ARITHMETIC>-A)
  'function
  NIL)

(make-doc
  #'(setf MATH/SERIES:<GEOMETRIC>-B)
  'function
  NIL)

(make-doc
  (find-class 'MATH/SERIES:<ARITHMETIC>) t
  NIL)

(make-doc
  (find-class 'MATH/SERIES:<GEOMETRIC>) t
  "@b(Описание:) класс @b(<geometric>) представляет геометрическую
   прогрессию.")

(make-doc
  (find-class 'MATH/SERIES::<SERIES>) t
  NIL)

(make-doc
 (find-slot 'MATH/SERIES::A 'MATH/SERIES:<ARITHMETIC>)
 t
 "Первый член арифметической прогрессии (нумерация
      начинается с нуля).")

(make-doc
 (find-slot 'MATH/SERIES::D 'MATH/SERIES:<ARITHMETIC>)
 t
 "Разность арифметической прогрессии.")

(make-doc
 (find-slot 'MATH/SERIES::B 'MATH/SERIES:<GEOMETRIC>)
 t
 "Первый член геометрической прогрессии (нумерация
      начинается с нуля).")

(make-doc
 (find-slot 'MATH/SERIES::Q 'MATH/SERIES:<GEOMETRIC>)
 t
 "Знаменатель геометрической прогрессии.")

(make-doc
  (find-method #'MATH/SERIES::ITEMS-BY-SUMM NIL '(MATH/SERIES:<GEOMETRIC> T))
  t
  "@b(Описание:) функция @b(n) количество членов геометрической
прогрессии, чтобы их сумма равнялась @b(S).")

(make-doc
  (find-method #'MATH/SERIES::ITEMS-BY-SUMM NIL '(MATH/SERIES:<ARITHMETIC> T))
  t
  "@b(Описание:) функция @b(n) количество членов геометрической
прогрессии, чтобы их сумма равнялась @b(S).")

(make-doc
  (find-method #'MATH/SERIES:ITEM NIL '(MATH/SERIES:<GEOMETRIC> INTEGER))
  t
  NIL)

(make-doc
  (find-method #'MATH/SERIES:ITEM NIL '(MATH/SERIES:<ARITHMETIC> INTEGER))
  t
  NIL)

(make-doc
  (find-method #'MATH/SERIES:SUMM NIL '(MATH/SERIES:<GEOMETRIC> INTEGER))
  t
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (summ (make-instance '<geometric> :b 2 :q 1.2) 5) => 14.883203
@end(code)")

(make-doc
  (find-method #'MATH/SERIES:SUMM NIL '(MATH/SERIES:<ARITHMETIC> INTEGER))
  t
  "@b(Пример использования:)
@begin[lang=lisp](code)
  (summ (make-instance '<arithmetic> :a 2 :d 1.2) 5)  ; => 22.0
@end(code)")

(make-doc
  (find-method #'MATH/SERIES:FIRST-BY-NUMBER&SUMM NIL '(MATH/SERIES:<GEOMETRIC>
                                                        T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/SERIES:FIRST-BY-NUMBER&SUMM NIL '(MATH/SERIES:<ARITHMETIC>
                                                        T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/SERIES:SCALE-BY-NUMBER&SUMM NIL '(MATH/SERIES:<GEOMETRIC>
                                                        T T))
  t
  NIL)

(make-doc
  (find-method #'MATH/SERIES:SCALE-BY-NUMBER&SUMM NIL '(MATH/SERIES:<ARITHMETIC>
                                                        T T))
  t
  NIL)
