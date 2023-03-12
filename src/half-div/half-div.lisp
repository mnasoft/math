;;;; ./src/half-div/half-div.lisp

(defpackage :math/half-div
  (:use #:cl)
  (:export h-div) 
  (:export h-div-lst)
  (:documentation
   "@b(Описание:) пакет @b( half-div) реализует алгоритм половинного
 деления для выполнения поиска корня функции на отрезке."))

(in-package :math/half-div)

(defun boole-to-int (b) (if b 1 0))

(defun same-znak (a b)
  (if (zerop (logxor
	      (boole-to-int (minusp a))
	      (boole-to-int (minusp b))))
      t
      nil))

(defun epsylon (x &key (eps 1e-6))
    "Функция для вычисления комплексной точности."
  (+ (* (abs x) eps ) eps))

(defun h-div (a b func &key (eps 1e-6) (iters 1000))
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
"
  (do*
   ( (i 0 (1+ i))
     (fa (funcall func a))
     (fb (funcall func b))
     (x (/ (+ a b) 2) (/ (+ a b) 2))
     (fx (funcall func x)(funcall func x)))
   ((or
     (> i iters)
     (<= (abs(- b a))
	 (epsylon x :eps eps)))
    (values x
	    (not (> i iters))
	    (abs(- b a))
	    (epsylon x :eps eps)))
    (if (same-znak fa fx)
	(setf a x fa fx)
	(setf b x fb fx))))

(defun h-div-lst (a b func n p_lst &key (eps 1e-6) (iters 1000))
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
@end(code)"
  (labels
      ((subst-by-no (lst el position)
         (substitute el (nth position lst) lst :start position)))
    (do*
     ( (i 0 (1+ i))
       (fa (apply func (subst-by-no p_lst a n)))
       (fb (apply func (subst-by-no p_lst b n)))
       (x (/ (+ a b) 2)
	  (/ (+ a b) 2))
       (fx (apply func (subst-by-no p_lst x n))
	   (apply func (subst-by-no p_lst x n))))
     ((or
       (> i iters)
       (<= (abs(- b a))
	   (epsylon x :eps eps)))
      (values x
	      (not (> i iters))
	      (abs(- b a))
	      (epsylon x :eps eps)))
      (if (same-znak fa fx)
	  (setf a x fa fx)
	  (setf b x fb fx)))))
