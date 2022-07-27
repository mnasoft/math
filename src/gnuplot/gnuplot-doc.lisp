;;;; ./src/gnuplot/gnuplot-doc.lisp
(in-package :math/gnuplot)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  'MATH/GNUPLOT:*TERM-PNGCAIRO* 'variable
  NIL)

(make-doc
  'MATH/GNUPLOT:*TERM-PDFCAIRO* 'variable
  NIL)

(make-doc
  'MATH/GNUPLOT:*PM3D-MAP* 'variable
  "STUB")

(make-doc
  'MATH/GNUPLOT:*PALETTE-DEFINED-01* 'variable
  "STUB")

(make-doc
  'MATH/GNUPLOT:*DEFAULT-GNUPLOT-DIRECROTY* 'variable
  "Каталог для вывода по умолчанию.")

(make-doc
  'MATH/GNUPLOT:*PALETTE-DEFINED* 'variable
  "STUB")

(make-doc
  #'MATH/GNUPLOT:GNUPLOT-SPLOT 'function
  "Осуществляет подготовку данных, содержащихся в файле f-name с расширением data.
Данные в файле должны иметь формат gp-list
")

(make-doc
  #'MATH/GNUPLOT:GNUPLOT-PLOT 'function
  "STUB")

(make-doc
  #'MATH/GNUPLOT:MAKE-TABLE 'function
  "@b(Описание:) make-table выполняет формирование списка точек, разделенного на группы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (make-table (split-range 1.0 0.0 2) (split-range -3 0 3))
  =>  (((1.0 -3.0) (1.0 -2.0) (1.0 -1.0) (1.0 0.0))
       ((0.5 -3.0) (0.5 -2.0) (0.5 -1.0) (0.5 0.0))
       ((0.0 -3.0) (0.0 -2.0) (0.0 -1.0) (0.0 0.0)))
@end(code)
")

(make-doc
  #'MATH/GNUPLOT:MAKE-PLOT-DATA-FILE 'function
  "@b(Описание:) функция @b(make-plot-data-file) выполняет вывод
данных @b(data) в файл с именем f-name и расширением data.

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Пример 1
 (make-plot-data-file
  \"plot2\"
  (mapcar
   #'(lambda (x)
       (list x (sin x) (sqrt x)))
   (math:split-range 0.0 10 1000)))
@end(code)
")

(make-doc
  #'MATH/GNUPLOT:RGB 'function
  "@b(Описание:) функция @b(rgb) возвращает строковое представление цвета.

 @b(Переменые:)
@begin(list)
@iterm(aa = 0..255 яркость;) 
@iterm(rr = 0..255 насыщенность красного;) 
@iterm(gg = 0..255 насыщенность зеленого;) 
@iterm(bb = 0..255 насыщенность синего.) 
@end(list)
")

(make-doc
  #'MATH/GNUPLOT:GNUPLOT-DATA-SPLOT 'function
  "STUB")

(make-doc
  #'MATH/GNUPLOT:TABLE-APPLY 'function
  "@b(Описание:) функция @b(table-apply)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-table (split-range 1 4 3) (split-range 5 7 2)) 
 => 
 (((1.0 5.0) (1.0 6.0) (1.0 7.0)) 
  ((2.0 5.0) (2.0 6.0) (2.0 7.0))
  ((3.0 5.0) (3.0 6.0) (3.0 7.0)) 
  ((4.0 5.0) (4.0 6.0) (4.0 7.0)))

 (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'* 10.) 
 =>
 (((1.0 5.0  50.0) (1.0 6.0  60.0) (1.0 7.0  70.0))
  ((2.0 5.0 100.0) (2.0 6.0 120.0) (2.0 7.0 140.0))
  ((3.0 5.0 150.0) (3.0 6.0 180.0) (3.0 7.0 210.0))
  ((4.0 5.0 200.0) (4.0 6.0 240.0) (4.0 7.0 280.0)))

 (table-apply (make-table (split-range 1 4 3) (split-range 5 7 2))  #'vector)
 =>
 (((1.0 5.0 #(1.0 5.0)) (1.0 6.0 #(1.0 6.0)) (1.0 7.0 #(1.0 7.0)))
  ((2.0 5.0 #(2.0 5.0)) (2.0 6.0 #(2.0 6.0)) (2.0 7.0 #(2.0 7.0)))
  ((3.0 5.0 #(3.0 5.0)) (3.0 6.0 #(3.0 6.0)) (3.0 7.0 #(3.0 7.0)))
  ((4.0 5.0 #(4.0 5.0)) (4.0 6.0 #(4.0 6.0)) (4.0 7.0 #(4.0 7.0))))
@end(code)
")

(make-doc
  #'MATH/GNUPLOT:GNUPLOT-DATA-PLOT 'function
  "@b(Описание:) функция @b(gnuplot-data-plot)

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Пример 1
 (math:gnuplot-data-plot
 \"plot2\"
 (mapcar #'(lambda (x) (list x (sin x)(sqrt x)) )
	 (math:split-range 0.0 10 1000) )
 :plot \"plot 'plot2.data' u 1:2 with lines lt 1, 'plot2.data' u 1:3 with lines lt 2 \")
@end(code)
")

(make-doc
  #'MATH/GNUPLOT::FIND-FONT-FAMILY 'function
  "@b(Описание:) функция @b(find-font-family) возвращает имя шрифта,
установленного в систему похожего на тот, что указан.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-font-family :family \"Arial\")
 (find-font-family :family \"Courier New\")
 (find-font-family :family \"Times New Roman\")
@end(code)
")

(make-doc
  #'MATH/GNUPLOT::MAKE-2D-LIST-BY-FUNC 'function
  NIL)

(make-doc
  #'MATH/GNUPLOT::FILE-NAME 'function
  "Определяет имя файла в каталоге поумолчанию.")

(make-doc
  #'MATH/GNUPLOT::TABLE-APPLY-0 'function
  "Пример использования:
   (table-apply-0 (make-table (split-range 1 4 3) (split-range 5 7 2))  #'vector) =>
   ((#(1.0 5.0) #(1.0 6.0) #(1.0 7.0))
    (#(2.0 5.0) #(2.0 6.0) #(2.0 7.0))
    (#(3.0 5.0) #(3.0 6.0) #(3.0 7.0))
    (#(4.0 5.0) #(4.0 6.0) #(4.0 7.0))) ")

(make-doc
  #'MATH/GNUPLOT::TABLE-APPLY-1 'function
  "Пример использования:
  ;;(table-apply-1 (make-table (split-range 1 4 3) (split-range 5 7 2))  #'*  10.) 

=>
 (((1.0 5.0 50.0) (1.0 6.0 60.0) (1.0 7.0 70.0))
  ((2.0 5.0 100.0) (2.0 6.0 120.0) (2.0 7.0 140.0))
  ((3.0 5.0 150.0) (3.0 6.0 180.0) (3.0 7.0 210.0))
  ((4.0 5.0 200.0) (4.0 6.0 240.0) (4.0 7.0 280.0)))")

(make-doc
  #'MATH/GNUPLOT:GNUPLOT-DATA-TO-FILE 'function
  "@b(Описание:) обобщенная функция @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*).")

(make-doc
  #'MATH/GNUPLOT::OUTPUT 'function
  NIL)

(make-doc
  #'MATH/GNUPLOT::MOVE 'function
  NIL)

(make-doc
  #'MATH/GNUPLOT::PLOT 'function
  NIL)

(make-doc
  #'MATH/GNUPLOT::GNUPLOT-VECTOR-DIRECTION 'function
  NIL)

(make-doc
  #'MATH/GNUPLOT::GNUPLOT-VECTOR-ORIGIN 'function
  NIL)

(make-doc
  #'(setf MATH/GNUPLOT::GNUPLOT-VECTOR-DIRECTION)
  'function
  NIL)

(make-doc
  #'(setf MATH/GNUPLOT::GNUPLOT-VECTOR-ORIGIN)
  'function
  NIL)

(make-doc
  (find-class 'MATH/GNUPLOT::TERM-PDFCAIRO) t
  "
 @b(Пример использования:)
@begin[lang=gnuplot](code)
 set term pdfcairo
             {{no}enhanced} {mono|color}
             {font <font>} {fontscale <scale>}
             {linewidth <lw>} {rounded|butt|square} {dashlength <dl>}
             {background <rgbcolor>}
             {size <XX>{unit},<YY>{unit}}
@end(code)
")

(make-doc
  (find-class 'MATH/GNUPLOT::TERM-PNGCAIRO) t
  "
 @b(Пример использования:)
@begin[lang=gnuplot](code)
 set term pngcairo
             {{no}enhanced} {mono|color}
             {{no}transparent} {{no}crop} {background <rgbcolor>}
             {font <font>} {fontscale <scale>}
             {linewidth <lw>} {rounded|butt|square} {dashlength <dl>}
             {pointscale <ps>}
             {size <XX>{unit},<YY>{unit}}
@end(code)
")

(make-doc
  (find-class 'MATH/GNUPLOT::GNUPLOT-VECTOR) t
  NIL)

(make-doc
  (find-class 'MATH/GNUPLOT::<TERM>) t
  NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::NO-TRANSPARENT 'MATH/GNUPLOT::TERM-PNGCAIRO)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::NO-CROP 'MATH/GNUPLOT::TERM-PNGCAIRO)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::POINTSCALE 'MATH/GNUPLOT::TERM-PNGCAIRO)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::ORIGN 'MATH/GNUPLOT::GNUPLOT-VECTOR)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::DIRECTION 'MATH/GNUPLOT::GNUPLOT-VECTOR)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::NO-ENHANCED 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::MONO/COLOR 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::FONT 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::FONTSCALE 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::LINEWIDTH 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::ROUNDED/BUTT/SQUARE 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::DASHLENGTH 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::BACKGROUND 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::SIZE 'MATH/GNUPLOT::<TERM>)
 t
 NIL)

(make-doc
 (find-slot 'MATH/GNUPLOT::SIZE-UNITS 'MATH/GNUPLOT::<TERM>)
 t
 "По умолчанию - пиксели|пиксели=72*in|пиксели=72/25.4*cm")

(make-doc
  (find-method #'MATH/GNUPLOT::PLOT NIL '(STRING MATH/GNUPLOT::<TERM> CONS))
  t
  NIL)

(make-doc
  (find-method #'MATH/GNUPLOT::MOVE NIL '(MATH/GNUPLOT::GNUPLOT-VECTOR VECTOR))
  t
  NIL)

(make-doc
  (find-method #'MATH/GNUPLOT::OUTPUT NIL '(MATH/GNUPLOT::TERM-PDFCAIRO STRING
                                            T))
  t
  NIL)

(make-doc
  (find-method #'MATH/GNUPLOT::OUTPUT NIL '(MATH/GNUPLOT::TERM-PNGCAIRO STRING
                                            T))
  t
  NIL)

(make-doc
  (find-method #'MATH/GNUPLOT:GNUPLOT-DATA-TO-FILE NIL '(T CONS))
  t
  "@b(Описание:) метод @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*). 

 Данные должны быть представлены 2d-list.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gnuplot-data-to-file \"data\" 
  (loop :for i :from 0 :to 4 :by 1/10 :collect (list i (* i i))))
@end(code)
")

(make-doc
  (find-method #'MATH/GNUPLOT:GNUPLOT-DATA-TO-FILE NIL '(T ARRAY))
  t
  "@b(Описание:) метод @b(gnuplot-data-to-file) выводит данные
@b(data) в файл с именем @b(f-name), расположенный в каталоге поумолчанию
(см. переменную *default-gnuplot-direcroty*). 

 Данные должны быть представлены 2d-array.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (gnuplot-data-to-file \"data\" 
  (loop :for i :from 0 :to 4 :by 1/10 :collect (list i (* i i))))
@end(code)

 (gnuplot-data-to-file \"data\" (make-array '(5 2) :initial-contents '((0 0)(1 1)(2 4)(3 9)(4 16))))
")
