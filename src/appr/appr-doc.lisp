(in-package #:math/appr)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))

(make-doc
  'MATH/APPR:*APR-FUNC-2-6* 'variable
  "@b(Описание:) *apr-func-2-6* шаблон для построения функции двух
параметров: x1 и x2 c шестью коэффициентами.

@begin[lang=lisp](code) '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)+a@sub(2)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)·x@sub(2)+a@sub(5)·x@sub(1)+a@sub(6)·x@sub(2)+a@sub(7) @end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-2-5* 'variable
  "@b(Описание:) *apr-func-2-5* шаблон для построения функции двух параметров: 
x1 и x2 c пятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1) (x2 x2)         (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)+a@sub(2)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)+a@sub(4)·x@sub(2)+a@sub(5) @end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-1-2* 'variable
  "Шаблон для построения линейной функции одного параметра: x1 с двумя коэффициентами.
@begin[lang=lisp](code) '((x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)+b @end(code) ")

(make-doc
  'MATH/APPR:*APR-FUNC-1-4* 'variable
  "Шаблон для построения квадратичной функции одного параметра: x1 c
четырьмя коэффициентами.

@begin[lang=lisp](code) '((x1 x1 x1) (x1 x1) (x1) (1.0) (yy)) @end(code)

@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(3)+b·x@sub(1)@sup(2)+c·x@sub(1)+d @end(code)
")

(make-doc
  'MATH/APPR:*APR-FUNC-2-7* 'variable
  "@b(Описание:) *apr-func-2-7* шаблон для построения функции двух
параметров: x1 и x2 c семью коэффициентами.

@begin[lang=lisp](code) '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)+a@sub(2)·x@sub(1)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)@sup(2)+a@sub(4)·x@sub(2)@sup(2)+a@sub(5)·x@sub(1)+a@sub(6)·x@sub(2)+a@sub(7)@end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-1-5* 'variable
  "Шаблон для построения квадратичной функции одного параметра: x1 c пятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x1 x1) (x1 x1 x1) (x1 x1) (x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(4)+b·x@sub(1)@sup(3)+c·x@sub(1)@sup(2)+d·x@sub(1)+e @end(code)
")

(make-doc
  'MATH/APPR:*APR-ARGS-1* 'variable
  "Аргументы для функции одного параметра.
@begin[lang=lisp](code) '(x1 yy) @end(code) ")

(make-doc
  'MATH/APPR:*APR-FUNC-1-3* 'variable
  "Шаблон для построения квадратичной функции одного параметра: x1 c тремя коэффициентами.
@begin[lang=lisp](code) '((x1 x1) (x1) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) yy(x@sub(1))=a·x@sub(1)@sup(2)+b·x@sub(1)+c @end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-2-8* 'variable
  "@b(Описание:) *apr-func-2-8* шаблон для построения функции двух параметров: x1 и x2 c восемью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)+a@sub(2)·x@sub(1)·x@sub(2)@sup(2)+a@sub(3)·x@sub(1)@sup(2)+a@sub(4)·x@sub(2)@sup(2)+a@sub(5)·x@sub(1)·x@sub(2)+a@sub(6)·x@sub(1)+a@sub(7)·x@sub(2)+a@sub(8)@end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-2-4* 'variable
  "@b(Описание:) *apr-func-2-4* шаблон для построения функции двух параметров: 
x1 и x2 c четырьмя коэффициентами.
@begin[lang=lisp](code) '((x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)·x@sub(2)+a@sub(2)·x@sub(1)+a@sub(3)·x@sub(2)+a@sub(4) @end(code)")

(make-doc
  'MATH/APPR:*APR-FUNC-2-9* 'variable
  "Шаблон для построения функции двух параметров: x1 и x2 c девятью коэффициентами.
@begin[lang=lisp](code) '((x1 x1 x2 x2) (x1 x1 x2) (x1 x2 x2) (x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) @end(code)
@begin[lang=scribe](code) 
yy(x@sub(1))=a@sub(1)·x@sub(1)@sup(2)·x@sub(2)@sup(2)+ a@sub(2)·x@sub(1)@sup(2)·x@sub(2)+ a@sub(3)·x@sub(1)·x@sub(2)@sup(2)+ a@sub(4)·x@sub(1)@sup(2)+ a@sub(5)·x@sub(2)@sup(2)+ a@sub(6)·x@sub(1)·x@sub(2)+ a@sub(7)·x@sub(1)+ a@sub(8)·x@sub(2)+ a@sub(9)
@end(code)")

(make-doc
  'MATH/APPR:*APR-ARGS-2* 'variable
  "Аргументы для функции двух параметров.
@begin[lang=lisp](code) '(x1 x2 yy) @end(code) ")

(make-doc
  (macro-function 'MATH/APPR:MAKE-APPROXIMATION-LAMBDA) t
  "@b(Описание:) макрос @b(make-approximation-lambda) определяет
 аппроксимиующую lambda-функцию, построенную на основании списка,
 каждый элемент которого является списком, содержащим значения 
 аргументов функции и значение функции, соответствующее этим аргументам.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((func 
	 (make-approximation-lambda (xx yy) 
				    ((xx xx) (xx) (1.0) (yy)) 
				    ((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0)))))
   (funcall func 1.0)  ;=>  1.0d0 
   (funcall func 3.0)  ;=>  9.0d0
   (funcall func 5.0)) ;=> 25.0d0
@end(code)
")

(make-doc
  (macro-function 'MATH/APPR:MAKE-APPROXIMATION-DEFUN) t
  "@b(Описание:) макрос @b(make-approximation-defun) определяет
аппроксимиующую функцию с именем @b(func-name), построенной на
основании списка, каждый элемент которого является списком, содержащим значения 
 аргументов функции и значение функции, соответствующее этим аргументам.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-approximation-defun (xx yy) 
			   ((xx xx) (xx) (1.0) (yy)) 
			   ((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0))
			   square-func)
 (square-func 1.0) =>  1.0d0
 (square-func 3.0) =>  9.0d0
 (square-func 5.0) => 25.0d0
@end(code)
")

(make-doc
  #'MATH/APPR:MAKE-LEAST-SQUARES-MATRIX 'function
  "@b(Описание:) функция @b(make-least-squares-matrix) возвращает матрицу
для расчета коэффициентов полиномиальной зависимости вида @b(ff) со 
списком имен факторов влияния и имени функции отклика @b(vv),
которая приближается к экспериментальным точкам @b(ex_pts),
методом наименьших квадратов.

 @b(Переменые:)
@begin(list)
 @item(vv - список, состоящий из имен факторов влияния и имени функции отклика;)
 @item(ff - задает вид функциональной зависимости;)
 @item(ex_pts - задает значения факторов влияния и значение функции отклика;)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
;;;; Для аппроксимации экспериментальных точек полиномом второй степени функции одной переменной.
;;;; Здесь коэффициенты a1, a2, a3 можно найти решив САУ, получаемую в результате. 
;;;; (defun yy (xx) (+ (* a1 xx xx) (* a2 xx) a3))

 (make-least-squares-matrix 
  '(xx yy) 
  '((xx xx) (xx) (1.0) (yy)) 
  '((-1.0 1.0) (0.0 0.0) (2.0 4.0) (3.0 9.0)))
  => Matr 3х4
     [ 98.0d0    34.0d0    14.0d0    98.0d0   ]
     [ 34.0d0    14.0d0    4.0d0     34.0d0   ]
     [ 14.0d0    4.0d0     4.0d0     14.0d0   ]
;;;; Для аппроксимации экспериментальных точек полиномом второй степени функции двух переменных.
;;;; Здесь коэффициенты a1, a2, a3 можно найти решив САУ, получаемую в результате. 
     (defun yy (x1 x2) (+ (* 1.0 x1 x1) (* 2.0 x2 x2)  (* 3.0 x1 x2)  (* 4.0 x1) (* 5.0 x2) 6.0))
    
 (make-least-squares-matrix 
  '(x1 x2 yy) 
  '((x1 x1) (x2 x2) (x1 x2) (x1) (x2) (1.0) (yy)) 
  (let ((pts nil))
    (loop :for i :from -2 :to 2 :do
      (loop :for j :from -2 to 2 :do
	(push (list i j (yy i j)) pts)))
    pts))

;  => Matr 6х7
; [ 170.0d0   100.0d0   0.0d0     0.0d0     0.0d0     50.0d0    670.0d0  ]
; [ 100.0d0   170.0d0   0.0d0     0.0d0     0.0d0     50.0d0    740.0d0  ]
; [ 0.0d0     0.0d0     100.0d0   0.0d0     0.0d0     0.0d0     300.0d0  ]
; [ 0.0d0     0.0d0     0.0d0     50.0d0    0.0d0     0.0d0     200.0d0  ]
; [ 0.0d0     0.0d0     0.0d0     0.0d0     50.0d0    0.0d0     250.0d0  ]
; [ 50.0d0    50.0d0    0.0d0     0.0d0     0.0d0     25.0d0    300.0d0  ]

  (solve-x *)
;  => Matr 1х6
; [ 1.0d0     2.0d0     3.0d0     4.0d0     5.0d0     6.0d0    ]
@end(code)

 См. также math/ls-gauss/solve-x; math/ls-rotation/solve
  ")

(make-doc
  #'MATH/APPR:AVERAGING-FUNCTION-LAMBDA 'function
  "@b(Описание:) функция @b(averaging-function-lambda) возвращает исходный код
 аппроксимиующей lambda-функции, построенной на основании списка, 
 каждый элемент которого является списком, содержащим значения 
 аргументов функции и значение функции, соответствующее этим аргументам.

 @b(Переменые:)
 @begin(list)
 @item(args-fuc-names - список, содержащий имена аргументов и функции ;)
 @item(func-view - вид функциональной зависимости. Функциональная 
                   зависимость должна использовать имена из args-fuc-names ;)
 @item(args-results - списка, каждый элемент которого является списком,
                      содержащим значения аргументов функции и значение функции
 (соответствующее этим аргументам).)
 @end(list)

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (averaging-function-lambda '(xx yy) 
			    '((xx xx) (xx) (1.0) (yy)) 
			    '((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0)))
 => (LAMBDA (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
 (averaging-function-lambda '(xx yy) 
			    '((xx xx xx) (xx xx) (xx) (1.0) (yy)) 
			    '((-2.0 -8.0) (-1.0 -1.0) (0.0 0.0) (1.0 1.0) (2.0 8.0)))
 => (LAMBDA (XX) (+ (* 1.0d0 XX XX XX) (* 0.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
@end(code)")

(make-doc
  #'MATH/APPR:MAKE-LINEAR-INTERPOLATION 'function
  "@b(Описание:) функция @b(make-linear-interpolation) возвращает функциональной 
 зависимость, аппроксимируемую по функции одного переменного, заданной таблично.

 @b(Переменые:)
 @begin(list)
 @item(points - список, состоящий из аргумента и значения ;)
 @item(ff - вид функциональной зависимости см. *apr-func-1-2* --- *apr-func-1-5*.)
 @end(list)

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (let* ((points-01 '((0.0 0.0) (1.0 1.0) (2.0 2.0)))
	(func-1 (make-linear-interpolation points-01)))
   (loop :for x :from 0 :to 2 :by 1/10
	 :collect (mapcar #'(lambda (el) (coerce el 'single-float))
			  (list x
				(funcall func-1 x)))))
 =>  ((0.0 0.0) (0.1 0.1) (0.2 0.2) (0.3 0.3) (0.4 0.4) (0.5 0.5) (0.6 0.6)
      (0.7 0.7) (0.8 0.8) (0.9 0.9) (1.0 1.0) (1.1 1.1) (1.2 1.2) (1.3 1.3)
      (1.4 1.4) (1.5 1.5) (1.6 1.6) (1.7 1.7) (1.8 1.8) (1.9 1.9) (2.0 2.0))
 
 (let* ((points-02 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0)))
	(func-2 (make-linear-interpolation points-02 :ff *apr-func-1-3*)))
   (loop :for x :from 1 :to 3 :by 1/10
	 :collect (mapcar #'(lambda (el) (coerce el 'single-float))
			  (list x
				(funcall func-2 x)))))
 =>  ((1.0 1.0) (1.1 1.21) (1.2 1.44) (1.3 1.69) (1.4 1.96) (1.5 2.25) (1.6 2.56)
      (1.7 2.89) (1.8 3.24) (1.9 3.61) (2.0 4.0) (2.1 4.41) (2.2 4.84) (2.3 5.29)
      (2.4 5.76) (2.5 6.25) (2.6 6.76) (2.7 7.29) (2.8 7.84) (2.9 8.41) (3.0 9.0))
 @end(code)
")

(make-doc
  #'MATH/APPR:APPR-TABLE 'function
  "@b(Описание:) функция @b(appr-table) возвращает результат линейной 
интерполяции (или экстраполяции) для значения @b(x) на таблице @b(table).

 @b(Пример использования:) 
@begin[lang=lisp](code)
 (appr-table 0.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 0.5
 (appr-table 1.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 2.5
 (appr-table 3.0 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0))) => 2.0
 Тест: (test-approximation-appr-table)
@end(code)
")

(make-doc
  #'MATH/APPR:MAKE-APPR-LINEAR 'function
  "@b(Описание:) функция @b(make-appr-linear) возвращает функцию,
являющуюся линейной интерполяцией функции одного переменного.

 @b(Пример использования:)
@begin[lang=lisp](code)
(let ((f1 (math/appr:make-appr-linear
   (loop :for i :from 0 :to 10
	 :collect (list (* 1.0d0 i) (* 0.1d0 i i))))))
  (loop :for i :from 0 :to 10 :by 1/5
	:collect
	(list (* 1.0 i) (coerce (funcall f1 (* 1d0 i))'single-float ))))
  => '((0.0 0.0) (0.2 0.02) (0.4 0.04) (0.6 0.06) (0.8 0.08)
       (1.0 0.1) (1.2 0.16) (1.4 0.22) (1.6 0.28) (1.8 0.34)
       (2.0 0.4) (2.2 0.50) (2.4 0.60) (2.6 0.70) (2.8 0.80)
       (3.0 0.9) (3.2 1.04) (3.4 1.18) (3.6 1.32) (3.8 1.46)
       (4.0 1.6) (4.2 1.78) (4.4 1.96) (4.6 2.14) (4.8 2.32)
       (5.0 2.5) (5.2 2.72) (5.4 2.94) (5.6 3.16) (5.8 3.38)
       (6.0 3.6) (6.2 3.86) (6.4 4.12) (6.6 4.38) (6.8 4.64)
       (7.0 4.9) (7.2 5.20) (7.4 5.50) (7.6 5.80) (7.8 6.10)
       (8.0 6.4) (8.2 6.74) (8.4 7.08) (8.6 7.42) (8.8 7.76)
       (9.0 8.1) (9.2 8.48) (9.4 8.86) (9.6 9.24) (9.8 9.62)
       (10.0 10.0))
@end(code)
")

(make-doc
  #'MATH/APPR:AVERAGING-FUNCTION-DEFUN 'function
  "@b(Описание:) функция @b(averaging-function-defun) возвращает исходный код
 именований аппроксимиующей функции, построенной на основании списка, 
 каждый элемент которого является списком, содержащим значения 
 аргументов функции и значение функции, соответствующее этим аргументам.

 @b(Переменые:)
 @begin(list)
 @item(args-fuc-names - список, содержащий имена аргументов и функции ;)
 @item(func-view - вид функциональной зависимости. Функциональная 
 зависимость должна использовать имена из args-fuc-names ;)
 @item(args-results - списка, каждый элемент которого является списком,
 содержащим значения аргументов функции и значение функции
 (соответствующее этим аргументам).)
 @item(func-name - имя функции.)
 @end(list)

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (averaging-function-defun '(xx yy) 
			   '((xx xx) (xx) (1.0) (yy)) 
			   '((0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0))
			   'square-func)
 => (DEFUN SQUARE-FUNC (XX) (+ (* 1.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
 (averaging-function-defun '(xx yy) 
			   '((xx xx xx) (xx xx) (xx) (1.0) (yy)) 
			   '((-2.0 -8.0) (-1.0 -1.0) (0.0 0.0) (1.0 1.0) (2.0 8.0))
			   'cubic-func)
 => (DEFUN CUBIC-FUNC (XX) (+ (* 1.0d0 XX XX XX) (* 0.0d0 XX XX) (* 0.0d0 XX) (* 0.0d0 1.0)))
@end(code)")

(make-doc
  #'MATH/APPR:APPR-BILINEAR-A2D-FUNC 'function
  NIL)

(make-doc
  #'MATH/APPR:APPR-LINEAR-A1D-FUNC 'function
  NIL)

(make-doc
  #'MATH/APPR:SMOOTH-BY-POINTS 'function
  "@b(Описание:) обобщенная функция @b(smooth-by-points) возвращает 
значение в точке @b(point-s), являющееся результатом сглаживания 
зависимости заданной:
@begin(list)
 @item(таблицей узловых точек @b(nod-points);)
 @item(таблицей значений @b(nod-values);)
 @item(функцией @b(weight-func) учета веса значений от относительного 
       расстояния до аргумента (при которых эти значения определены);) 
 @item(базовой длины (базовых длинн) @b(base-dist-s), по которой (которым)
 вычисляются относительные расстояния.)
@end(list)")

(make-doc
  #'MATH/APPR:REFINE-SMOOTHING-BY-POINTS 'function
  "@b(Описание:) обобщенная функция @b(refine-smoothing-by-points) 
возвращает массив значений @b(rez-values@sub(i)) такой, что
для точек @b(nod-points@sub(i)) сумма отклонений между @b(nod-values@sub(i)) и 
@b((smooth-by-points point-s@sub(i) base-dist-s nod-points rez-values@sub(i) :weight-func weight-func))
не превысит @b(delta).

 Вторым значением возвращается:
@begin(list)
 @item(T - если такой массив удалось найти за указанной в параметре @b(iterations) количестве итераций;)
 @item(nil - если за указанной количество итераций @b(iterations) такой массив удалось найти не удалось.)
@end(list)
")

(make-doc
  #'MATH/APPR:APPROXIMATE 'function
  NIL)

(make-doc
  #'MATH/APPR:APPR-BILINEAR-X1 'function
  NIL)

(make-doc
  #'MATH/APPR:APPR-LINEAR-X1 'function
  NIL)

(make-doc
  #'MATH/APPR:MAKE-REFINE-SMOOTING 'function
  NIL)

(make-doc
  #'MATH/APPR:APPR-BILINEAR-X2 'function
  NIL)

(make-doc
  (find-class 'MATH/APPR:<APPR-LINEAR>) t
  "@b(Описание:) Класс @b(<appr-linear>) представляет линейную интерполяцию.

 @b(Пример использования:)
 @begin[lang=lisp](code)
 (let ((a1 (make-instance '<appr-linear>
			  :x1  (vector 1 2 3)
			  :a1d (vector 1 4 9)))
       (a2 (make-instance '<appr-linear>
			  :x1  (vector 2 3  4)
			  :a1d (vector 4 9 16))))
   (loop :for i :from 0 :to 5 :by 1/5 :do
     (format t \"| ~5F | ~5F | ~5F |~%\" i (approximate i a1) (approximate i a2))))
 =>
 | 0.0 |  -2.0 |  -6.0 |
 | 0.2 |  -1.4 |  -5.0 |
 | 0.4 |  -0.8 |  -4.0 |
 | 0.6 |  -0.2 |  -3.0 |
 | 0.8 |   0.4 |  -2.0 |
 | 1.0 |   1.0 |  -1.0 |
 | 1.2 |   1.6 |  -0.0 |
 | 1.4 |   2.2 |   1.0 |
 | 1.6 |   2.8 |   2.0 |
 | 1.8 |   3.4 |   3.0 |
 | 2.0 |   4.0 |   4.0 |
 | 2.2 |   5.0 |   5.0 |
 | 2.4 |   6.0 |   6.0 |
 | 2.6 |   7.0 |   7.0 |
 | 2.8 |   8.0 |   8.0 |
 | 3.0 |   9.0 |   9.0 |
 | 3.2 |  10.0 |  10.4 |
 | 3.4 |  11.0 |  11.8 |
 | 3.6 |  12.0 |  13.2 |
 | 3.8 |  13.0 |  14.6 |
 | 4.0 |  14.0 |  16.0 |
 | 4.2 |  15.0 |  17.4 |
 | 4.4 |  16.0 |  18.8 |
 | 4.6 |  17.0 |  20.2 |
 | 4.8 |  18.0 |  21.6 |
 | 5.0 |  19.0 |  23.0 |
 @end(code)
")

(make-doc
  (find-class 'MATH/APPR:<APPR-BILINEAR>) t
  "@b(Описание:) Класс @b(<appr-bilinear>) представляет билинейную интерполяцию.")

(make-doc
 (find-slot 'MATH/APPR::X1 'MATH/APPR:<APPR-LINEAR>)
 t
 "Вектор аргументов.")

(make-doc
 (find-slot 'MATH/APPR::A1D-FUNC 'MATH/APPR:<APPR-LINEAR>)
 t
 "Вектор функций.")

(make-doc
 (find-slot 'MATH/APPR::X1 'MATH/APPR:<APPR-BILINEAR>)
 t
 "Вектор реперных значений по первому направлению (измерению).")

(make-doc
 (find-slot 'MATH/APPR::X2 'MATH/APPR:<APPR-BILINEAR>)
 t
 "Вектор реперных значений по второму направлению (измерению).")

(make-doc
 (find-slot 'MATH/APPR::A2D-FUNC 'MATH/APPR:<APPR-BILINEAR>)
 t
 "Двумерный массив функций размерности которого, на единицу 
 меньше количества реперных значений по соответствующему направлению (измерению).")

(make-doc
  (find-method #'MATH/APPR:MAKE-REFINE-SMOOTING NIL '(VECTOR VECTOR NUMBER))
  t
  "@b(Описание:) метод @b(make-refine-smooting) в случе 
нахождения сглаживания для функции одного переменного.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((nod-pts #(-2.0 -1.0 -0.5 0.0 0.5  1.0 2.0))
	(nod-rez #( 4.0  1.0 0.25 0.0 0.25 1.0 4.0))
	(base-dists-1_5 1.5 )
	(base-dists-1_0 1.0 )
	(base-dists-0_6 0.6 )
	(base-dists-0_4 0.4 )
	(func (make-refine-smooting nod-pts nod-rez base-dists-1_5)))
   (loop :for i :from -2 :to 2 :by 1/10
	 :collect (list (* 1.0 i)
			(* 1.0 i i)
			(funcall func (* 1.0 i))
			(* 100 (1- (/ (* 1.0 i i)
				      (funcall func (* 1.0 i))))))))
  => '(( 0.0 0.00 0.00350)
       ( 0.1 0.01 0.01315)
       ( 0.2 0.04 0.04218)
       ( 0.3 0.09 0.09071)
       ( 0.4 0.16 0.15898)
       ( 0.5 0.25 0.24730)
       ( 0.6 0.36 0.35599)
       ( 0.7 0.49 0.48542)
       ( 0.8 0.64 0.63591)
       ( 0.9 0.81 0.80774)
       ( 1.0 1.00 1.00107)
       ( 1.1 1.21 1.21590)
       ( 1.2 1.44 1.45205)
       ( 1.3 1.69 1.70909)
       ( 1.4 1.96 1.98635)
       ( 1.5 2.25 2.28284)
       ( 1.6 2.56 2.59730)
       ( 1.7 2.89 2.92817)
       ( 1.8 3.24 3.27363)
       ( 1.9 3.61 3.63161)
       ( 2.0 4.00 3.99986))
@end(code)
")

(make-doc
  (find-method #'MATH/APPR:MAKE-REFINE-SMOOTING NIL '(ARRAY VECTOR VECTOR))
  t
  "@b(Описание:) метод @b(make-refine-smooting) в случе 
нахождения сглаживания для функции двух переменных.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((nod-lst '((-1.0 -1.0) (1.0 -1.0) (-1.0 1.0) (1.0 1.0)))
	(nod-rez  #(-10.0       0.0       0.0       10.0))
	(nod-pts (make-array '(4 2) :initial-contents nod-lst))
	(base-dists-1_5 #(1.5 1.5))
	(base-dists-1_0 #(1.0 1.0))
        (base-dists-0_6 #(0.6 0.6))
	(base-dists-0_4 #(0.4 0.4))
	(func (make-refine-smooting nod-pts nod-rez base-dists-1_5)))
   (funcall func 0.0 0.5))
@end(code)
")

(make-doc
  (find-method #'MATH/APPR:APPROXIMATE NIL '(NUMBER MATH/APPR:<APPR-LINEAR>))
  t
  "@b(Описание:) метод @b(approximate) возвращает значение функции одного переменного 
 в точке point для функции заданой таблично и аппроксимированной объектом @b(a-l).
")

(make-doc
  (find-method #'MATH/APPR:APPROXIMATE NIL '(T MATH/APPR:<APPR-BILINEAR>))
  t
  "@b(Описание:) метод @b(approximate)")

(make-doc
  (find-method #'MATH/APPR:REFINE-SMOOTHING-BY-POINTS NIL '(VECTOR VECTOR
                                                                   NUMBER))
  t
  "@b(Описание:) метод @b(refine-smoothing-by-points) в случе 
нахождения сглаживания для функции одного переменного.")

(make-doc
  (find-method #'MATH/APPR:REFINE-SMOOTHING-BY-POINTS NIL '(ARRAY VECTOR VECTOR))
  t
  "@b(Описание:) метод @b(refine-smoothing-by-points) в случе 
нахождения сглаживания для функции двух переменных.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let* ((nod-lst '((-1.0 0.0) (1.0 0.0) (-1.0 1.0) (1.0 1.0)))
	(nod-rez  #(-10.0       0.0       0.0       10.0))
	(nod-pts (make-array '(4 2) :initial-contents nod-lst))
	(base-dists-1_5 #(1.5 1.5))
	(base-dists-1_0 #(1.0 1.0))
        (base-dists-0_6 #(0.6 0.6))
	(base-dists-0_4 #(0.4 0.4)))
   (refine-smoothing-by-points nod-pts nod-rez base-dists-0_4) 
   (refine-smoothing-by-points nod-pts nod-rez base-dists-0_6)
   (refine-smoothing-by-points nod-pts nod-rez base-dists-1_0)
   (refine-smoothing-by-points nod-pts nod-rez base-dists-1_5)
 )
 => #(-10.019267 -0.019267347 0.019267347 10.019267), T,  1, 2.9726332e-4, #(-9.999926 7.424513e-5  -7.424499e-5  9.999926), #(-10.0 0.0 0.0 10.0), #(0.4 0.4)
 => #(-10.663013 -0.66271365  0.6627135 10.663013),   T,  4, 4.3922185e-4, #(-9.99989  1.0987265e-4 -1.1000411e-4 9.99989),  #(-10.0 0.0 0.0 10.0), #(0.6 0.6)
 => #(-16.005812 -5.632663    5.6326632 16.00581),    T, 15, 9.807203e-4,  #(-9.999755 2.451054e-4  -2.4542634e-4 9.999755), #(-10.0 0.0 0.0 10.0), #(1.0 1.0)
 => #(-29.902119 -15.834344  15.834344 29.902119),    T, 40, 8.0980576e-4, #(-9.999799 2.0380187e-4 -2.0355334e-4 9.999799), #(-10.0 0.0 0.0 10.0), #(1.5 1.5)
@end(code)")

(make-doc
  (find-method #'MATH/APPR:SMOOTH-BY-POINTS NIL '(NUMBER NUMBER VECTOR VECTOR))
  t
  "@b(Описание:) метод @b(smooth-by-points) возвращает значение, являющееся результатом
сглаживания зависимости заданной:
@begin(list)
 @item(аргументами @b(points);)
 @item(значениями в этих точках @b(values);)
 @item(функцией @b(weight-func) учета веса значений от относительного 
       расстояния до аргумента (при которых эти значения определены);) 
 @item(базовой длины, по которой вычисляются относительные расстояния.)
@end(list)

  Этот вариант метода примерняется для сглаживания функции одного аргумента.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (let ((args #(-2.0 -1.0 0.0 1.0 2.0))
	(vals #( 4.0  1.0 0.0 1.0 4.0))
	(dx-2_0 2.0)
	(dx-1_0 1.0)
	(dx-0_6 0.6))
    (loop :for i :from 0 :to 2 :by 1/5
	  :collect (list (* i 1.0)
			 (* i i 1.0)
			 (smooth-by-points i dx-0_6 args vals)
			 (smooth-by-points i dx-1_0 args vals)
			 (smooth-by-points i dx-2_0 args vals))))
  => '((0.0 0.0 0.11070304 0.4977933 1.3665789)
       (0.2 0.04 0.1735467 0.5375060 1.3774427)
       (0.4 0.16 0.3702085 0.6551497 1.4096088)
       (0.6 0.36 0.6500492 0.8464661 1.4618422)
       (0.8 0.64 0.8946066 1.1046556 1.5322074)
       (1.0 1.00 1.1105981 1.4196386 1.6182360)
       (1.2 1.44 1.4516152 1.7761649 1.7171193)
       (1.4 1.96 2.0848030 2.1525292 1.8259060)
       (1.6 2.56 2.9039226 2.5225418 1.9416808)
       (1.8 3.24 3.5229838 2.8611753 2.0617056)
       (2.0 4.00 3.8243356 3.1507930 2.1835241))
@end(code)
")

(make-doc
  (find-method #'MATH/APPR:SMOOTH-BY-POINTS NIL '(VECTOR VECTOR ARRAY VECTOR))
  t
  "@b(Описание:) метод @b(smooth-by-points) возвращает значение, являющееся результатом
сглаживания зависимости заданной:
@begin(list)
 @item(аргументами @b(points);)
 @item(значениями в этих точках @b(values);)
 @item(функцией @b(weight-func) учета веса значений от относительного 
       расстояния до аргумента (при которых эти значения определены);) 
 @item(базовой длины, по которой вычисляются относительные расстояния.)
@end(list)
  Этот вариант метода примерняется для сглаживания функции двух аргументов.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (smooth-by-points #(0.0 0.0) #(1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 0.53788286
 (smooth-by-points #(0.0 0.0) #(0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 0.117073804
 (smooth-by-points #(0.0 0.0) #(0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 0.0038534694
 
 (smooth-by-points #(1.0 0.0) #(1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 (smooth-by-points #(1.0 0.0) #(0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 0.9999999
 (smooth-by-points #(1.0 0.0) #(0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 
 (smooth-by-points #(0.0 1.0) #(1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 (smooth-by-points #(0.0 1.0) #(0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 (smooth-by-points #(0.0 1.0) #(0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 
 (smooth-by-points #(1.0 1.0) #(1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.4621171
 (smooth-by-points #(1.0 1.0) #(0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.8829262
 (smooth-by-points #(1.0 1.0) #(0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.9961466
 
 (smooth-by-points #(0.5 0.5) #(1.0 1.0) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 (smooth-by-points #(0.5 0.5) #(0.6 0.6) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
 (smooth-by-points #(0.5 0.5) #(0.4 0.4) (make-array '(4 2) :initial-contents '((0.0 0.0) (1.0 0.0) (0.0 1.0) (1.0 1.0))) #(0.0 1.0 1.0 2.0)) 1.0
@end(code)
")
