;;;; /src/core/method.lisp

(in-package :math/core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance

(export 'distance)

(defmethod distance ((x1 number) (x2 number) )
  "@b(Описание:) 

@b(Пример использования:)
 (distance 1 0 ) => 1
 (distance 2 0 ) => 2
 (distance 2 1 ) => 1
"
  (let ((rez (- x1 x2)))
    (abs rez)))

(export 'distance)

(defmethod distance ((x1-lst cons) (x2-lst cons))
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance '(1 1 1) '(0 0 0)) => 1.7320508 = (sqrt (+ 1 1 1))
 (distance '(2 2 2) '(0 0 0)) => 3.4641016 = (sqrt (+ 4 4 4))
 (distance '(2 1 2) '(0 0 0)) => 3.0 =       (sqrt (+ 4 1 4))
@end(code)
"
  (assert (= (length x1-lst) (length x1-lst)))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x1 x2)
			(let ((rez (- x1 x2)))
			  (square rez)))
		    x1-lst x2-lst))))

(export 'distance)

(defmethod distance ((x1 vector) (x2 vector))
  "@b(Описание:) метод @b(distance) возвращает расстояние 
между точками @b(x1-lst) и @b(x2-lst).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (distance #(1 1 1) #(0 0 0)) => 1.7320508
 (distance #(2 2 2) #(0 0 0)) => 3.4641016
 (distance #(2 1 2) #(0 0 0)) => 3.0
@end(code)
"
  (assert (= (length x1) (length x2)))
  (sqrt (apply #'+
	       (loop :for i :from 0 :below (array-dimension x1 0)
		     :collect
		     (let ((rez (- (svref x1 i) (svref x2 i)))) (* rez rez))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; distance-relative

(export 'distance-relative)

(defmethod distance-relative ((x number) (xi number) (dx number))
    "Пример использования:
 (distance-relative 1 0 1) 1.0
 (distance-relative 2 0 1) 2.0
 (distance-relative 2 0 2) 1.0
"
  (let ((rez (/ (- x xi) dx)))
    (sqrt (square rez))))

(export 'distance-relative)

(defmethod distance-relative ((x-lst cons) (xi-lst cons) (dx-lst cons))
   "Пример использования:
 (distance-relative '(1 1 1) '(0 0 0) '(1 1 1)) 1.7320508
 (distance-relative '(2 2 2) '(0 0 0) '(1 1 1)) 3.4641016
 (distance-relative '(2 2 2) '(0 0 0) '(1 2 2)) 2.4494898
"
  (assert (apply #'= (mapcar #'length `(,x-lst ,xi-lst ,dx-lst))))
  (sqrt (apply #'+ (mapcar
		    #'(lambda (x xi dx)
			(let ((rez (/ (- x xi) dx)))
			  (square rez)))
		    x-lst xi-lst dx-lst))))

(export 'distance-relative)

(defmethod distance-relative ((x vector) (xi vector) (dx vector))
  "@b(Описание:) метод @b(distance-relative) возвращает относительное 
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

(export 'summ-distance)

(defmethod summ-distance ((x1 vector) (x2 vector))
  "@b(Описание:) функция @b(summ-distance) возвращает сумму 
расстояний по каждому направлению.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (summ-distance #(1 2 3) #(3 2 1)) => 4 = (+ 2 0 2)
@end(code)
"
  (assert (= (length x1) (length x2)))
  (apply #'+ (loop :for i :from 0 :below (length x1)
		   :collect (abs (- (svref x1 i) (svref x2 i))))))

(export 'summ-distance)

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
