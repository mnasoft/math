;;;; ./src/equation/equation.lisp

(defpackage #:math/equation
  (:use #:cl )
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
  ((a :accessor coeff-a :initform  1.0 :initarg :a)
   (b :accessor coeff-b :initform -1.0 :initarg :b)))
  
(defclass <quadric> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a)
   (b :accessor coeff-b :initform  0.0 :initarg :b)
   (c :accessor coeff-c :initform -1.0 :initarg :c)))

(defclass <cubic> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a)
   (b :accessor coeff-b :initform  0.0 :initarg :b)
   (c :accessor coeff-c :initform  0.0 :initarg :c)
   (d :accessor coeff-d :initform -1.0 :initarg :d)))

(defclass <quartic> ()
  ((a :accessor coeff-a :initform  1.0 :initarg :a)
   (b :accessor coeff-b :initform  0.0 :initarg :b)
   (c :accessor coeff-c :initform  0.0 :initarg :c)
   (d :accessor coeff-d :initform  0.0 :initarg :c)
   (e :accessor coeff-e :initform -1.0 :initarg :c)))

(defmethod print-object ((eq <linear>) s)
  (format s "f(x)=(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq)))

(defmethod print-object ((eq <quartic>) s)
  (format s "f(x)=(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq)))

(defmethod print-object ((eq <cubic>) s)
  (format s "f(x)=(~S)*x^3+(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq) (coeff-d eq)))

(defmethod print-object ((eq <quartic>) s)
  (format s "f(x)=(~S)*x^4+(~S)*x^3+(~S)*x^2+(~S)*x+(~S)"
          (coeff-a eq) (coeff-b eq) (coeff-c eq) (coeff-d eq) (coeff-e eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric tab (eq x))

(defgeneric roots (eq))

(defgeneric func (eq))

(defmethod func ((eq <linear>))
  "(funcall (func (make-instance '<linear>)) 1)"
  (values
   #'(lambda (x) (+ (* x (coeff-a eq)) (coeff-b eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <quadric>))
  "(funcall (func (make-instance '<quadric>)) 2)"
  (values
   #'(lambda (x)
       (+ (* x x (coeff-a eq))
          (* x (coeff-b eq))
          (coeff-c eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <cubic>))
  "(funcall (func (make-instance '<cubic>)) 5)"
  (values
   #'(lambda (x)
       (+ (* x x x (coeff-a eq))
          (* x x (coeff-b eq))
          (* x (coeff-c eq))
          (coeff-d eq)))
   (format nil "~S" eq)))

(defmethod func ((eq <quartic>))
  "(funcall (func (make-instance '<quartic>)) 0)"
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
  (let ((a (coeff-a eq))
        (b (coeff-b eq))
        (c (coeff-c eq)))
    (let ((Δ (sqrt (- (* b b) (* 4 a c)))))
      (list (/ (+ (- b) Δ) 2 a)
            (/ (+ (- b) (- Δ)) 2 a)))))

(defmethod roots ((eq <cubic>))
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
  (error "Not yet defined."))
