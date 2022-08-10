;;;; ./src/series/series.lisp

(defpackage #:math/series
  (:use #:cl )
  (:export <arithmetic>
           <arithmetic>-a
           <arithmetic>-d
           <geometric>
           <geometric>-b
           <geometric>-q
           summ
           item 
           first-by-number&summ
           scale-by-number&summ
           )
  (:documentation
   "@b(Описание:) пакет @b(math/series) определяет некоторые операции с
 прогрессиями."))

(in-package :math/series)

(defclass <series> ()
  ())

(defclass <arithmetic> (<series>)
  ((a :accessor <arithmetic>-a :initform 0.0 :initarg :a)
   (d :accessor <arithmetic>-d :initform 1.2 :initarg :d)))

(defclass <geometric> ()
  ((b :accessor <geometric>-b :initform 1 :initarg :b)
   (q :accessor <geometric>-q :initform 1.2 :initarg :q)))

(defmethod print-object ((series <arithmetic>) s)
  (format s "~A, ~A+~A, ..., ~A+~A*i"
          (<arithmetic>-a series)
          (<arithmetic>-a series)
          (<arithmetic>-d series)
          (<arithmetic>-a series)
          (<arithmetic>-d series)))

(defmethod print-object ((series <geometric>) s)
  (format s "~A, ~A*~A, ..., ~A+~A^i"
          (<geometric>-b series)
          (<geometric>-b series)
          (<geometric>-q series)
          (<geometric>-b series)
          (<geometric>-q series)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric summ (series n))

(defgeneric item (series i))

(defgeneric items-by-summ (series summ))

(defgeneric first-by-number&summ (series n S))

(defgeneric scale-by-number&summ (series n S &key from to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod summ ((series <geometric>) (n integer))
  (let ((b (<geometric>-b series))
        (q (<geometric>-q series)))
    (/ (* b (- (expt q n) 1))(- q 1))))

(defmethod summ ((series <arithmetic>) (n integer))
  (let ((a (<arithmetic>-a series))
        (d (<arithmetic>-d series)))
    (* 1/2 n (+ (* 2 a)
                (* d (- n 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod item ((series <geometric>) (i integer))
  (let ((b (<geometric>-b series))
        (q (<geometric>-q series)))
    (* b (expt q i))))

(defmethod item ((series <arithmetic>) (i integer))
  (let ((a (<geometric>-a series))
        (d (<geometric>-d series)))
    (+ a (expt d i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod items-by-summ ((series <geometric>) S)
  (let ((b (<geometric>-b series))
        (q (<geometric>-q series)))
    (/ (log (+ 1 (/ (* S (- q 1)) b))) (log q))))

(defmethod items-by-summ ((series <arithmetic>) S)
  (let* ((a (<arithmetic>-a series))
         (d (<arithmetic>-d series))
         (eq
           (make-instance 'math/equation:<quadric>
                          :a (/ d 2)
                          :b (- a (/ d 2) )
                          :c (- S))))
    (apply #'max (math/equation:roots eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod first-by-number&summ ((series <geometric>) n S)
  (let ((q (<geometric>-q series)))
    (setf (<geometric>-b series)
          (/ (* S (- q 1)) (- (expt q n) 1)))))

(defmethod first-by-number&summ ((series <arithmetic>) n S)
  (let ((d (<arithmetic>-d series)))
    (setf (<arithmetic>-a series)
          (+ (/ (- S (* d n n 1/2)) n) (* d 1/2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale-by-number&summ ((series <geometric>) n S &key (from 0.001) (to 1000.))
  (let ((b (<geometric>-b series)))
    (setf (<geometric>-q series) 
          (half-div:h-div
           from to
           #'(lambda (q)
               (- (/ (* b (- (expt q n) 1)) (- q 1)) S))))))

(defmethod scale-by-number&summ ((series <arithmetic>) n S &key from to)
  (declare (ignore from to))
  (let ((a (<arithmetic>-a series)))
    (setf (<arithmetic>-d series)
          (/ (- (/ (* 2 S) n) (* 2 a)) (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(loop :for i :from 0 :to 4
      :summing (item *g* i) :into total
      :finally (return total))

#+nil (defparameter *g* (make-instance '<geometric> :b 1.0 :q 1.2))
#+nil (defparameter *a* (make-instance '<arithmetic> :a 0.0 :d 1.2))

