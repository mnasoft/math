;;;; ./src/half-div/half-div-doc.lisp
(in-package #:math/half-div)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MATH/HALF-DIV:H-DIV-LST 'function
  "@b(Описание:) функция @b(h-div-lst) возвращает результат решения
 уравнения func(X[0],X[1],...,X[n],...,X[m])=0 методом половинного
 деления.

 Поиск решения выполняется на отрезке [a,b] для аргумента с номером
 n (первый аргумет имеет номер 0).

 @b(Переменые:)
@begin(list)
 @item(a - левая граница отрезка;)
 @item(b - правая граница отрезка;)
 @item(func - вид функциональной зависимости;)
 @item(n - номер аргумента в списке аргументов функции при изменении
           которого на отрезке [a,b] выполняется поиск решения;)
 @item(p_lst - список аргуметов функции;)
 @item(eps - комплексная точность поиска решения;)
 @item(iters - максимальное количество итераций для достижения
               заданной точности.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (defun xy2(x y) (- (* x (log x)) y))
 (h-div-lst 2.0 100000.0 #'xy2 0 '(t 10000.) :iters 50)
@end(code)")

(make-doc
  #'MATH/HALF-DIV:H-DIV 'function
  "@b(Описание:) функция @b(h-div) возвращает результат решения
 уравнения func(x)=0 методом половинного деления.

 Поиск решения выполняется на отрезке [a,b].

 @b(Переменые:)
@begin(list)
 @item(a - левая граница отрезка;)
 @item(b - правая граница отрезка;)
 @item(func - вид функциональной зависимости;)
 @item(eps - комплексная точность поиска решения;)
 @item(iters - максимальное количество итераций для достижения заданной точности.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
(defun x2(x) (- (* x (log x)) 10000.))
(h-div 2.0 100000.0 #'x2 :iters 50)
@end(code)
")

(make-doc
  #'MATH/HALF-DIV::SUBST-ELEMENT-BY-NO 'function
  NIL)

(make-doc
  #'MATH/HALF-DIV::BOOLE-TO-INT 'function
  NIL)

(make-doc
  #'MATH/HALF-DIV::SAME-ZNAK 'function
  NIL)

(make-doc
  #'MATH/HALF-DIV::EPSYLON 'function
  "Функция для вычисления комплексной точности.")
