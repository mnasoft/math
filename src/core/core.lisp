;;;; ./src/core/core.lisp

(defpackage :math/core
  (:use #:cl)
  (:export norma
           *semi-equal-relativ*
           *semi-equal-zero*
           semi-equal
           )
  (:export summ-distance 
	   distance               
	   exclude-nil-from-list 
	   depth-sphere-along-cone
	   distance-relative
	   square)
  (:export split-range
	   split-range-by-func )
  (:export matr-name-*
	   )
  (:export round-to-significant-digits
           +significant-digits+))

(in-package :math/core)

(defun square (x)
  "@b(Описание:) функция @b(square) возвращает квадрат значения.

 @b(Переменые:)
@begin(list)
 @item(x - число.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (square 5) => 25 
 (square -4) => 16 
@end(code)"  
  (* x x))

(defun exclude-nil-from-list (lst)
  "@b(Описание:) функция @b(exclude-nil-from-list) возвращает список в
 котором нет nil-элементов (они исключаются).

 @b(Пример использования:) @begin[lang=lisp](code)
 (exclude-nil-from-list '(1.1 1.0 nil 0.9 nil 1.2 nil 0.8)) 
 => (1.1 1.0 0.9 1.2 0.8)
@end(code)"  
  (let ((res nil))
    (dolist (el lst (reverse res) )
      (when el (push el res )))))

(defun e-value (n)
   (let ((rez 1)
	(nf 1))
    (dotimes (i n rez)
      (setf nf  (/ nf (1+ i))
	    rez (+ rez nf)))))

(defun split-range (from to steps)
  "@b(Описание:) split-range

 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-range 10 20 5)  => (10.0 12.0 14.0 16.0 18.0 20.0)
@end(code)
 "  
 (loop :for i :from 0 :to steps
	:collect (coerce (+ from (* (/ i steps ) (- to from))) 'float)))

(defun split-range-by-func (from to steps &key
					    (func #'(lambda (x) (log x 10)))
					    (anti-func #'(lambda (x) (expt 10 x))))
  "@b(Описание:) split-range-by-func

 @b(Пример использования:)
@begin[lang=lisp](code)
 (split-range-by-func 1 10 5) => (1.0 1.5848932 2.5118864 3.981072 6.3095737 10.0)
 (split-range-by-func 1 10 10) =>
 (1.0 1.2589254 1.5848932 1.9952624 2.5118864 3.1622777 3.981072 5.0118723  6.3095737 7.943282 10.0)
@end(code)
"  
 (mapcar
   #'(lambda (el)(funcall anti-func el))
   (split-range (funcall func from) (funcall func to) steps)))

(defun depth-sphere-along-cone (r alpha)
  "@b(Описание:) функция @b(depth-sphere-along-cone) возвращает 
заглубление сферы с радиусом R в конуc с углом при вершине 
равным alpha от линии пересечения конуса с цилиндром."  
   (let ((betta (- pi (/ alpha 2))))
    (- r (* r (tan (/ betta  2))))))

(defparameter +significant-digits+ 4
  "Определяет количество значащих цифр при округлении по умолчанию.")

(defun round-to-significant-digits (val &optional (significant-digits +significant-digits+) (base-val val))
  "@b(Описание:) функция @b(round-to-significant-digits) округляет значение
val до количества значащих цифр, задаваемых аргументом significant-digits.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (round-to-significant-digits 456.32738915923           ) => 456.3
 (round-to-significant-digits 456.32738915923 6         ) => 456.327
 (round-to-significant-digits 456.32738915923 6 53562.23) => 456.3
@end(code)
"  
   (labels ((find-divisor (val)
	     "@b(Описание:) функция @b(find-divisor)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-divisor 10.964739714723287d0)
@end(code)
"
	     (do ((divisor 1))
		 ((<= 1 (abs (* val divisor)) 10) divisor)
	       (cond
		 ((< (abs (* val divisor)) 1) (setf divisor (* divisor 10)))
		 ((< 1 (abs (* val divisor))) (setf divisor (/ divisor 10))))))

	   (my-round (val &optional (divisor 1))
	     "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (my-round 10.964739714723287d0 1/100) => 10.96
@end(code)
"
	     (coerce (* (round val divisor)
			divisor)
		     'single-float)))
    (my-round val
              (/ (expt 10 (* -1 (- significant-digits 1)))
		 (find-divisor base-val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/generic.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric distance (x1 x2)
  (:documentation
   "@b(Описание:) обобщенная функция @b(distance)
возвращает расстояние между x1 и x2. Как корень квадратный из 
сумм квадратов расстояний по каждому направлению."))

(defgeneric distance-relative (x0 x1 dx)
  (:documentation
   "@b(Описание:) обобщенная функция @b(distance-relative)
возвращает относительную длину между x0 и x1, длина приведения dx.
Корень квадратный из сумм квадратов расстояний по каждому направлению
отнесенному к длине приведения."))

(defgeneric summ-distance (x1 x2)
  (:documentation
   "@b(Описание:) обобщенная функция @b(summ-distance) возвращает сумму
 расстояний по каждому направлению."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/generic-matr.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric matr-name-* (matrix)
  (:documentation "Matr"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *semi-equal-relativ* 1e-6
  "@b(Описание:) переменная @b(*semi-equal-relativ*) определяет
относительную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defparameter *semi-equal-zero*    1e-6
  "@b(Описание:) переменная @b(*semi-equal-zero*) определяет
абсолютную величину, на которую могут отличаться значения 
считающиеся равными при срвнении из с помощью функции @b(semi-equal).
")

(defgeneric norma (x))

(defmethod norma ((x real))
  (abs x))

(defmethod norma ((x number))
  (sqrt (+ (* (realpart x) (realpart x))
           (* (imagpart x) (imagpart x)))))

(defmethod norma ((x cons))
  (let ((n (loop :for i :in x
                 :collect (* (norma i)))))
    (/ (apply #'+ n) (length n))))

(defmethod norma ((x vector))
  (let ((n (loop :for i :across x
                 :collect (* (norma i)))))
    (/ (apply #'+ n) (length n))))

(defgeneric semi-equal (x y &key tolerance) )

(defmethod semi-equal ((x number) (y number)
		       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x) (norma y)))))))
  "@b(Описание:) функция @b(semi-equal) возвращает T, если 
расстояние между значениями меньше tolerance. При этом 
имеется в виду, что значения примерно равны.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (semi-equal 1.0 1.000001) T
 (semi-equal 1.0 1.00001)  nil
@end(code)
"
  (< (distance x y) tolerance))

(defmethod semi-equal ((x cons) (y cons)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x)
                                                       (norma y)))))))
  (when (and (= (length x) (length y))
             (< (distance x y) tolerance))
    t))

(defmethod semi-equal ((x vector) (y vector)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x)
                                                       (norma y)))))))
  (when (and (= (length x) (length y))
             (< (distance x y) tolerance))
    t))

(defmethod semi-equal ((x1 vector) (x2 cons)
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x1)
                                                       (norma x2)))))))
  (semi-equal x1 (coerce x2 'vector) :tolerance tolerance))

(defmethod semi-equal ((x1 cons) (x2 vector) 
                       &key
                         (tolerance (+ *semi-equal-zero*
                                       (* *semi-equal-relativ*
                                          (norma (list (norma x1)
                                                       (norma x2)))))))
  (semi-equal (coerce x1 'vector) x2 :tolerance tolerance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/core/method.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance

(defmethod distance ((x1 real) (x2 real))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (distance 1 0 ) => 1
 (distance 2 0 ) => 2
 (distance 2 1 ) => 1
@end(code)"
  (let ((rez (- x1 x2)))
    (abs rez)))

(defmethod distance ((x1 number) (x2 number))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (distance (complex 1 2) 0) => 2.236068
 (distance 0 (complex 1 2)) => 2.236068
 (distance (complex 0 2) (complex 1 2)) => 1.0
@end(code)"
  (sqrt (+ (square (- (realpart x1) (realpart x2)))
           (square (- (imagpart x1) (imagpart x2))))))

(defmethod distance ((x1-lst cons) (x2-lst cons))
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance '(1 1 1) '(0 0 0)) => 1.7320508 = (sqrt (+ 1 1 1))
 (distance '(2 2 2) '(0 0 0)) => 3.4641016 = (sqrt (+ 4 4 4))
 (distance '(2 1 2) '(0 0 0)) => 3.0 =       (sqrt (+ 4 1 4))
@end(code)"  
  (assert (= (length x1-lst) (length x1-lst)))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x1 x2)
			(square (distance x1 x2)))
		    x1-lst x2-lst))))

(defmethod distance ((x1 vector) (x2 vector))
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance #(1 1 1) #(0 0 0)) => 1.7320508
 (distance #(2 2 2) #(0 0 0)) => 3.4641016
 (distance #(2 1 2) #(0 0 0)) => 3.0
@end(code)"
  (assert (= (length x1) (length x2)))
  (sqrt (loop :for i :from 0 :below (array-dimension x1 0)
	      :summing
              (square (distance (svref x1 i) (svref x2 i))))))

(defmethod distance ((x1 vector) (x2 cons))
  (distance x1 (coerce x2 'vector)))

(defmethod distance ((x1 cons) (x2 vector))
  (distance (coerce x1 'vector) x2))


;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance-relative

(defmethod distance-relative ((x number) (xi number) (dx number))
    "
 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance-relative 1 0 1) => 1.0
 (distance-relative 2 0 1) => 2.0
 (distance-relative 2 0 2) => 1.0
@end(code)"
  (let ((rez (/ (- x xi) dx)))
    (sqrt (square rez))))

(defmethod distance-relative ((x-lst cons) (xi-lst cons) (dx-lst cons))
    " @b(Пример использования:)
@begin[lang=lisp](code)
 (distance-relative '(1 1 1) '(0 0 0) '(1 1 1)) => 1.7320508
 (distance-relative '(2 2 2) '(0 0 0) '(1 1 1)) => 3.4641016
 (distance-relative '(2 2 2) '(0 0 0) '(1 2 2)) => 2.4494898
@end(code)"
  (assert (apply #'= (mapcar #'length `(,x-lst ,xi-lst ,dx-lst))))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x xi dx)
			(let ((rez (/ (- x xi) dx)))
			  (square rez)))
		    x-lst xi-lst dx-lst))))

(defmethod distance-relative ((x vector) (xi vector) (dx vector))
    " @b(Описание:) метод @b(distance-relative) возвращает относительное 
расстояние от точки @b(x) до точки @b(xi) по отношению к базовым длинам,
находящимся в @b(dx).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance-relative #(1 1 1) #(0 0 0) #(1 1 1)) => 1.7320508 
 (distance-relative #(1 1 1) #(0 0 0) #(2 2 2)) => 0.8660254 
 (distance-relative #(1 2 3) #(0 0 0) #(3 2 1)) => 3.1797974 = (sqrt (+ (* 1/3 1/3) (* 2/2 2/2) (* 3/1 3/1)))
@end(code)
"
  (assert (apply #'= (mapcar #'length `(,x ,xi ,dx))))
  (sqrt (apply #'+ (loop :for i :from 0 :below (array-dimension x 0)
			 :collect
			 (let ((rez (/ (- (svref x i) (svref xi i)) (svref dx i))))
			   (square rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; summ-distance

(defmethod summ-distance ((x1 vector) (x2 vector))
  "@b(Описание:) метод @b(summ-distance) возвращает сумму 
расстояний по каждому направлению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (summ-distance #(1 2 3) #(3 2 1)) => 4 = (+ 2 0 2)
@end(code)
"
  (assert (= (length x1) (length x2)))
  (apply #'+ (loop :for i :from 0 :below (length x1)
		   :collect (abs (- (svref x1 i) (svref x2 i))))))

(defmethod summ-distance ((x1 cons) (x2 cons))
  "@b(Описание:) функция @b(summ-distance) возвращает сумму 
расстояний по каждому направлению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (summ-distance '(1 2 3) '(3 2 1)) => 4 = (+ 2 0 2)
@end(code)
"
  (assert (= (length x1) (length x2)))
  (apply #'+
	 (mapcar
	  #'(lambda (el1 el2)
	      (abs (- el1 el2)))
	  x1 x2)))
