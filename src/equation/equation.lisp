;;;; ./src/equation/equation.lisp

(defpackage #:math/equation
  (:use #:cl)
  (:export tab
           func
           roots)
  (:export <linear>
           <quadric>
           <cubic>
           <quartic>)
  (:export coeff-a
           coeff-b
           coeff-c
           coeff-d
           coeff-e))

(in-package :math/equation)

(defclass <linear> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation  "Коэффициент при степени 1.")
   (b :accessor coeff-b :initform -1.0 :initarg :b :documentation  "Коэффициент при степени 0."))
  (:documentation
   "@b(Описание:) класс @b(<linear>) представляет линейное уравнение вида:

 @b(Пример использования:)
@begin[lang=c](code)
 f(x)=@b(a)*x+@b(b)
@end(code)
."))
  
(defclass <quadric> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation  "Коэффициент при степени 2.")
   (b :accessor coeff-b :initform  0.0 :initarg :b :documentation  "Коэффициент при степени 1.")
   (c :accessor coeff-c :initform -1.0 :initarg :c :documentation  "Коэффициент при степени 0."))
  (:documentation
   "@b(Описание:) класс @b(<linear>) представляет квадратное уравнение вида:

@begin[lang=c](code)
 f(x)=@b(a)*x@sup(2)+@b(b)*x+@b(c)
@end(code)
."))

(defclass <cubic> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation  "Коэффициент при степени 3.")
   (b :accessor coeff-b :initform  0.0 :initarg :b :documentation  "Коэффициент при степени 2.")
   (c :accessor coeff-c :initform  0.0 :initarg :c :documentation  "Коэффициент при степени 1.")
   (d :accessor coeff-d :initform -1.0 :initarg :d :documentation  "Коэффициент при степени 0."))
  (:documentation
   "@b(Описание:) класс @b(<linear>) представляет кубическое уравнение вида:

@begin[lang=c](code)
 f(x)=@b(a)*x@sup(3)+@b(b)*x@sup(2)+@b(c)*x+@b(d)
@end(code)
."))

(defclass <quartic> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a :documentation  "Коэффициент при степени 4.")
   (b :accessor coeff-b :initform  0.0 :initarg :b :documentation  "Коэффициент при степени 3.")
   (c :accessor coeff-c :initform  0.0 :initarg :c :documentation  "Коэффициент при степени 2.")
   (d :accessor coeff-d :initform  0.0 :initarg :d :documentation  "Коэффициент при степени 1.")
   (e :accessor coeff-e :initform -1.0 :initarg :e :documentation  "Коэффициент при степени 1."))
  (:documentation
   "@b(Описание:) класс @b(<linear>) представляет уравнение четвертой степени вида:

@begin[lang=c](code)
 f(x)=@b(a)*x@sup(4)+@b(b)*x@sup(3)+@b(c)*x@sup(2)+@b(d)*x+@b(e)
@end(code)
."))

(defmethod print-object ((eq <linear>) s)
  (format s "f(x)=(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq)))

(defmethod print-object ((eq <quadric>) s)
  (format s "f(x)=(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq)))

(defmethod print-object ((eq <cubic>) s)
  (format s "f(x)=(~S)*x^3+(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq) (coeff-d eq)))

(defmethod print-object ((eq <quartic>) s)
  (format s "f(x)=(~S)*x^4+(~S)*x^3+(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq) (coeff-d eq) (coeff-e eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric tab (eq x)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(tab) возвращает значение фукции
 @b(eq) в точке @b(x)."))

(defgeneric roots (eq)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(roots) возвращает корни
 уравнения @b(eq)."))

(defgeneric func (eq)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(func) функцию одного
переменного, представляющее полином."))

(defmethod func ((eq <linear>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (funcall (func (make-instance '<linear>)) 1) => 0.0
@end(code)
"
  (values
   #'(lambda (x) (+ (* x (coeff-a eq)) (coeff-b eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <quadric>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (funcall (func (make-instance '<quadric>)) 2) => 3.0
@end(code)
"
  (values
   #'(lambda (x)
       (+ (* x x (coeff-a eq))
          (* x (coeff-b eq))
          (coeff-c eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <cubic>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (funcall (func (make-instance '<cubic>)) 5) => 124.0
@end(code)
"
  (values
   #'(lambda (x)
       (+ (* x x x (coeff-a eq))
          (* x x (coeff-b eq))
          (* x (coeff-c eq))
          (coeff-d eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <quartic>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
 (funcall (func (make-instance '<quartic>)) 0) => -1.0
@end(code)
"
  (values
   #'(lambda (x)
       (+
        (* x x x x (coeff-a eq))
        (* x x x (coeff-b eq))
        (* x x (coeff-c eq))
        (* x (coeff-d eq))
        (coeff-e eq)))
   (format nil "~S" eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tab ((eq <linear>) x)
  (let ((a (coeff-a eq)) 
        (b (coeff-b eq)))
    (+ (* a x ) b)))

(defmethod tab ((eq <quadric>) x)
  (let ((a (coeff-a eq)) 
        (b (coeff-b eq))
        (c (coeff-c eq)))
    (+ (* a x x) (* b x) c)))

(defmethod tab ((eq <cubic>) x)
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq))
        (d (coeff-d eq)))
    (+ (* a x x x) (* b x x) (* c x) d)))

(defmethod tab ((eq <quartic>) x)
  " @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance '<linear> :a 1 :b -2)) 
@end(code)"
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq))
        (d (coeff-d eq))
        (e (coeff-e eq)))
    (+ (* a x x x x) (* b x x x) (* c x x) (* d x) e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod roots ((eq <linear>))
  (let ((a (coeff-a eq))
        (b (coeff-b eq)))
    (- (/ b a))))

(defmethod roots ((eq <quadric>))
  " @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance '<quadric> :a 1 :b -2 :c 1)) => (1.0 1.0)
(roots (make-instance '<quadric> :a 1 :b -4 :c 4)) => (2.0 2.0)
@end(code)"
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq)))
    (let ((Δ (sqrt (- (* b b) (* 4 a c)))))
      (list (/ (+ (- b) Δ) 2 a)
            (/ (+ (- b) (- Δ)) 2 a)))))

(defmethod roots ((eq <cubic>))
  " @b(Пример использования:) @begin[lang=lisp](code)
(roots (make-instance '<cubic> :a 2 :b -3 :c -3 :d 2)) 
=> (#C(-1.0d0 -0.0d0) 
    #C(0.5000000000000004d0 2.9605947323337506d-16) 
    #C(1.9999999999999996d0 -2.9605947323337506d-16))
(roots (make-instance '<cubic> :a 1 :b  2 :c 10 :d -20))
=> (#C(-1.6844040539106864d0 -3.4313313501976914d0) 
    #C(1.3688081078213714d0 1.9558293600573398d-16) 
    #C(-1.684404053910686d0 3.4313313501976905d0))
(roots (make-instance '<cubic> :a 1 :b -6 :c 12  :d -8))
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

(defmethod roots ((eq <quartic>))
    "@b(Описание:) метод @b(roots) возвращает список корней кубического
  уравнения @b(eq)."
  (error "Not yet defined."))
