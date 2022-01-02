;;;; ./src/equation/equation.lisp

(defpackage #:math/equation
  (:use #:cl )
  (:export tab
           roots)
  (:export linear
           quadric
           cubic
           quartic)
  (:export coeff-a
           coeff-b
           coeff-c
           coeff-d
           coeff-e))

(in-package :math/equation)

(defclass linear ()
  ((a :accessor coeff-a :initform 1.0 :initarg :a :documentation "Коэффициент при степени 1.")
   (b :accessor coeff-b :initform -1.0 :initarg :b :documentation "Коэффициент при степени 0."))
  (:documentation "Линейное уравнение."))
  
(defclass quadric ()
  ((a :accessor coeff-a :initform 1.0 :initarg :a :documentation "Коэффициент при степени 2.")
   (b :accessor coeff-b :initform 0.0 :initarg :b :documentation "Коэффициент при степени 1.")
   (c :accessor coeff-c :initform -1.0 :initarg :c :documentation "Коэффициент при степени 0."))
  (:documentation "Квадратное уравнение."))

(defclass cubic ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation "Коэффициент при степени 3.")
   (b :accessor coeff-b :initform  0.0 :initarg :b :documentation "Коэффициент при степени 2.")
   (c :accessor coeff-c :initform  0.0 :initarg :c :documentation "Коэффициент при степени 1.")
   (d :accessor coeff-d :initform -1.0 :initarg :d :documentation "Коэффициент при степени 0."))
  (:documentation "Кубическое уравнение."))

(defclass quartic ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation "Коэффициент при степени 4.")
   (b :accessor coeff-b :initform  0.0 :initarg :b :documentation "Коэффициент при степени 3.")
   (c :accessor coeff-c :initform  0.0 :initarg :c :documentation "Коэффициент при степени 2.")
   (d :accessor coeff-d :initform  0.0 :initarg :c :documentation "Коэффициент при степени 1.")
   (e :accessor coeff-e :initform -1.0 :initarg :c :documentation "Коэффициент при степени 0."))
  (:documentation "Уравнение четвертой степени."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric tab (eq x)
  (:documentation 
   "@b(Описание:) обобщенная_функция @b(tab) возвращает значение фукции @b(eq) в точке @b(x)."))

(defgeneric roots (eq)
  (:documentation 
   "@b(Описание:) обобщенная_функция @b(roots) возвращает корни уравнения @b(eq)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tab ((eq linear) x)
  (let ((a (coeff-a eq)) 
        (b (coeff-b eq)))
    (+ (* a x ) b)))

(defmethod tab ((eq quadric) x)
  (let ((a (coeff-a eq)) 
        (b (coeff-b eq))
        (c (coeff-c eq)))
    (+ (* a x x) (* b x) c)))

(defmethod tab ((eq cubic) x)
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq))
        (d (coeff-d eq)))
    (+ (* a x x x) (* b x x) (* c x) d)))

(defmethod tab ((eq quartic) x)
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq))
        (d (coeff-d eq))
        (e (coeff-e eq)))
    (+ (* a x x x x) (* b x x x) (* c x x) (* d x) e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod roots ((eq linear))
  "@b(Описание:) метод @b(roots) возвращает список корней линейного уравнения @b(eq).

 @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance 'linear :a 1 :b -2)) 

@end(code)"
  (let ((a (coeff-a eq))
        (b (coeff-b eq)))
    (- (/ b a))))

(defmethod roots ((eq quadric))
  "@b(Описание:) метод @b(roots) возвращает список корней квадратного уравнения @b(eq).

 @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance 'quadric :a 1 :b -2 :c 1)) => (1.0 1.0)
(roots (make-instance 'quadric :a 1 :b -4 :c 4)) => (2.0 2.0)
@end(code)"
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq)))
    (let ((Δ (sqrt (- (* b b) (* 4 a c)))))
      (list (/ (+ (- b) Δ) 2 a)
            (/ (+ (- b) (- Δ)) 2 a)))))

(defmethod roots ((eq cubic))
  "@b(Описание:) метод @b(roots) возвращает список корней кубического уравнения @b(eq).

 @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance 'cubic :a 2 :b -3 :c -3 :d 2)) 
=> (#C(-1.0d0 -0.0d0) #C(0.5000000000000004d0 2.9605947323337506d-16) #C(1.9999999999999996d0 -2.9605947323337506d-16))
(roots (make-instance 'cubic :a 1 :b  2 :c 10 :d -20))
=> (#C(-1.6844040539106864d0 -3.4313313501976914d0) #C(1.3688081078213714d0 1.9558293600573398d-16) #C(-1.684404053910686d0 3.4313313501976905d0))
(roots (make-instance 'cubic :a 1 :b -6 :c 12  :d -8))
=> (2.0d0 2.0d0 2.0d0)
@end(code)"
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq))
        (d (coeff-d eq))
        (ξ (* 0.5d0 (+ -1.0d0 (sqrt -3.0d0)))))
    (let* ((Δ0 (- (* b b) (* 3d0 a c)))
           (Δ1 (+ (* 2d0 b b b) (* -9.0d0 a b c) (* 27.0d0 a a d)))
           (c+ (expt (/ (+ Δ1 (sqrt (- (* Δ1 Δ1) (* 4.0d0 Δ0 Δ0 Δ0)))) 2d0) (/ 3d0)))
           (c- (expt (/ (- Δ1 (sqrt (- (* Δ1 Δ1) (* 4.0d0 Δ0 Δ0 Δ0)))) 2d0) (/ 3d0)))
           (c (if (< (abs c-) (abs c+)) c+ c-)))
      (if (= Δ0 Δ1 0.0d0)
          (loop :for k :from 0 :to 2
                :collect (/ (- b) (* 3.0d0 a)))
          (loop :for k :from 0 :to 2
                :collect (* (/ -1.0d0 (* 3.0d0 a)) (+ b (* (expt ξ k) c) (/ Δ0 (* (expt ξ k) c)))))))))

(defmethod roots ((eq quartic))
  "@b(Описание:) метод @b(roots) возвращает список корней кубического уравнения @b(eq)."
  (error "Not yet defined."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *eq* (make-instance 'linear))

(roots *eq*)
