
;;;; ./src/series/series.lisp

(defpackage #:math/series
    (:use #:cl )
    (:export open-tin-file))

(in-package :math/series)

(defclass quadric ()
  ((a :accessor quadric-a :initform 1.0 :initarg :a :documentation "Коэффициент при степени 2.")
   (b :accessor quadric-b :initform 0.0 :initarg :b :documentation "Коэффициент при степени 1.")
   (c :accessor quadric-c :initform 1.0 :initarg :c :documentation "Коэффициент при степени 0."))
  (:documentation "Квадратное уравнение."))

(defclass cubic ()
  ((a :accessor quadric-a :initform  1.0 :initarg :a :documentation "Коэффициент при степени 3.")
   (b :accessor quadric-b :initform -1.0 :initarg :b :documentation "Коэффициент при степени 2.")
   (c :accessor quadric-c :initform  0.0 :initarg :c :documentation "Коэффициент при степени 1.")
   (d :accessor quadric-d :initform -1.0 :initarg :d :documentation "Коэффициент при степени 0."))
  (:documentation "Кубическое уравнение."))

(defclass quartic ()
  ((a :accessor quadric-a :initform 1.0 :initarg :a :documentation "Коэффициент при степени 4.")
   (b :accessor quadric-b :initform 0.0 :initarg :b :documentation "Коэффициент при степени 3.")
   (c :accessor quadric-c :initform 0.0 :initarg :c :documentation "Коэффициент при степени 2.")
   (d :accessor quadric-d :initform 0.0 :initarg :c :documentation "Коэффициент при степени 1.")
   (e :accessor quadric-e :initform 1.0 :initarg :c :documentation "Коэффициент при степени 0."))
  (:documentation "Уравнение четвертой степени."))

(defgeneric tab (eq x)
  )

(defmethod tab ((eq quadric) x)
  (let ((a (quadric-a eq))
        (b (quadric-b eq))
        (c (quadric-c eq)))
    (+ (* a x x) (* b x) c)))

(defmethod tab ((eq cubic) x)
  (let ((a (quadric-a eq))
        (b (quadric-b eq))
        (c (quadric-c eq))
        (d (quadric-d eq)))
    (+ (* a x x x) (* b x x) (* c x) d)))


*a*

(defgeneric roots (eq)
  )

(loop :for x :from -10.0 :to 10.0 :do 
  (format t "~a ~a~%" x (tab (make-instance 'cubic) x)))

(tab (make-instance 'cubic) (second (roots (make-instance 'cubic))))

(defmethod roots ((eq cubic))
  (let ((a (quadric-a eq))
        (b (quadric-b eq))
        (c (quadric-c eq))
        (d (quadric-d eq)))
    (let* ((d0 (- (* b b) (* 3d0 a c)))
           (d1 (+ (* 2d0 b b b) (* -9d0 a b c) (* 27d0 a a d)))
           (c+ (expt (/ (+ d1 (sqrt (- (* d1 d1)(* 4d0 d0 d0 d0)))) 2d0) (/ 3d0)))
           (c- (expt (/ (- d1 (sqrt (- (* d1 d1)(* 4d0 d0 d0 d0)))) 2d0) (/ 3d0)))
           (ξ (* 0.5d0 (+ -1d0 (sqrt -3d0)))))
      (list 
       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 0) c+) (/ d0 (* (expt ξ 0) c+))
             ))
       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 0) c-) (/ d0 (* (expt ξ 0) c-))))

       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 1) c+) (/ d0 (* (expt ξ 1) c+))
             ))
       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 1) c-) (/ d0 (* (expt ξ 1) c-))))

       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 2) c+) (/ d0 (* (expt ξ 2) c+))
             ))
       (* (/ -1 (* 3 a))
          (+ b (* (expt ξ 2) c-) (/ d0 (* (expt ξ 2) c-))))
       ))))

(defmethod roots ((eq quadric))
  (let ((a (quadric-a eq))
        (b (quadric-b eq))
        (c (quadric-c eq)))
    (let ((d (sqrt (- (* b b) (* a c)))))
      (list (/ (+ (- b) d) 2 a)
            (/ (+ (- b) (- d)) 2 a)))))

(defparameter *eq* (make-instance 'quadric))

(roots *eq*)

(defclass arithmetic ()
  ((a :accessor arithmetic-a :initform 0.0 :initarg :a
      :documentation "Первый член арифметической прогрессии (нумерация
      начинается с нуля).")
   (d :accessor arithmetic-d :initform 1.2 :initarg :d
      :documentation "Разность арифметической прогрессии.")))

(defclass geometric ()
  ((b :accessor geometric-b :initform 1 :initarg :b
      :documentation "Первый член геометрической прогрессии (нумерация
      начинается с нуля)." )
   (q :accessor geometric-q :initform 1.2 :initarg :q
      :documentation "Знаменатель геометрической прогрессии." ))
  (:documentation
   "@b(Описание:) класс @b(geometric) представляет геометрическую
   прогрессию."))

(defmethod print-object ((series arithmetic) s)
  (format s "~A, ~A+~A, ..., ~A+~A*i"
          (arithmetic-a series)
          (arithmetic-a series)
          (arithmetic-d series)
          (arithmetic-a series)
          (arithmetic-d series)))

(defmethod print-object ((series geometric) s)
  (format s "~A, ~A*~A, ..., ~A+~A^i"
          (geometric-b series)
          (geometric-b series)
          (geometric-q series)
          (geometric-b series)
          (geometric-q series)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric summ (series n)
  (:documentation "Вычисляет сумму @b(n) членов прогрессии."))

(defgeneric item (series i)
  (:documentation "Возвращает значение @b(i)-того члена прогрессии."))

(defmethod summ ((series geometric) (n integer))
  (let ((b (geometric-b series))
        (q (geometric-q series)))
    (/ (* b (- (expt q n) 1))(- q 1))))

#+nil
(loop :for i :from 0 :to 4
      :summing (item *g* i) :into total
      :finally (return total))

(defmethod summ ((series arithmetic) (n integer))
  (let ((a (arithmetic-a series))
        (d (arithmetic-d series)))
    (* 1/2 n (+ (* 2 a)
                (* d (- n 1))))))

(defmethod item ((series geometric) (i integer))
  (let ((b (geometric-b series))
        (q (geometric-q series)))
    (* b (expt q i))))

(defmethod item ((series arithmetic) (i integer))
  (let ((a (geometric-a series))
        (d (geometric-d series)))
    (+ a (expt d i))))

(defmethod items-by-summ ((series geometric) S)
  "@b(Описание:) функция @b(n) количество членов геометрической
прогрессии, чтобы их сумма равнялась @b(S)."
  (let ((b (geometric-b series))
        (q (geometric-q series)))
    (/ (log (+ 1 (/ (* S (- q 1)) b))) (log q))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g* (make-instance 'geometric :b 2.0))

(defparameter *a* (make-instance 'arithmetic))

*a* *g*

(summ *g* 5)

(defun S (a q n)
  "@b(Описание:) функция @b(S) возвращает сумму членов геометрической
прогрессии.

 @b(Переменые:)
@begin(list)
 @item(a - первый член геометрической прогрессии;)
 @item(q - модуль геометрической прогрессии;)
 @item(n - количество членов геометрической прогрессии.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (s-n 2 1.2 5)  ; => 14.883203
@end(code)"
  (/ (* a (- (expt q n) 1))(- q 1)))

(defun n (a q S)
  "@b(Описание:) функция @b(n) количество членов геометрической
прогрессии, чтобы их сумма равнялась @b(S)."
  (/ (log (+ 1 (/ (* S (- q 1)) a))) (log q)))

(defun a (S q n)
  "@b(Описание:) функция @b(a) возвращает первый член геометрической
  прогрессии такой, чтобы сумма @b(n) членов геометрической прогрессии
  равнялась @b(S)."
  (/ (* S (- q 1)) (- (expt q n) 1)))

(defun q (a n S)
  "@b(Описание:) функция @b(q) возвращает степень геометрической
  прогрессии такую, чтобы сумма @b(n) членов геометрической прогрессии
  равнялась @b(S).
"
  (half-div:h-div 0.01 10.0 
   #'(lambda (q) (- (/ (* a (- (expt q n) 1)) (- q 1)) S))))

"arithmetic series"
"geometric series"
