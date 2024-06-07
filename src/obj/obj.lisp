(defpackage :math/obj 
  (:use #:cl) 
  (:export b* b/ b+ b- ;; Операции биаргументного умножения, деления, слоожения и вычитания;
           m* m/ m+ m- ;; Операции мультиаргументного умножения, деления, слоожения и вычитания;
           )
  (:export point-3d
           <point-3d>
           <point-3d>-x
           <point-3d>-y
           <point-3d>-z
           )
  (:export <vector-3d>
           <vector-3d>-x
           <vector-3d>-y
           <vector-3d>-z
           ))

(in-package :math/obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun b (i n p)
  (* (math/stat:combinations i n) (expt p i) (expt (- 1 p) (- n i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <point-3d> ()
  ((x :accessor <point-3d>-x :initform 0.0d0 :initarg :x :type 'double-float )
   (y :accessor <point-3d>-y :initform 0.0d0 :initarg :y :type 'double-float )
   (z :accessor <point-3d>-z :initform 0.0d0 :initarg :z :type 'double-float )))

(defmethod print-object ((point-3d <point-3d>) s)
  (format s "(~A ~A ~A)"
          (<point-3d>-x point-3d)
          (<point-3d>-y point-3d)
          (<point-3d>-z point-3d)))

(defclass <vector-3d> ()
  ((x :accessor <vector-3d>-x :initform 0.0d0 :initarg :x :type 'double-float )
   (y :accessor <vector-3d>-y :initform 0.0d0 :initarg :y :type 'double-float )
   (z :accessor <vector-3d>-z :initform 0.0d0 :initarg :z :type 'double-float )))

(defmethod print-object ((vector-3d <vector-3d>) s)
  (format s "(~A ~A ~A)"
          (<vector-3d>-x vector-3d)
          (<vector-3d>-y vector-3d)
          (<vector-3d>-z vector-3d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun point-3d (x y z)
  (make-instance '<point-3d>
                 :x (coerce x 'double-float)
                 :y (coerce y 'double-float) :z (coerce z 'double-float)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric b* (v1 v2)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(b*) умножение 2-х аргументов."))

(defgeneric b/ (v1 v2)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(b*) деление 2-х аргументов."))

(defgeneric b+ (v1 v2)
  (:documentation
   "@b(Описание:) обобщенная_функция @b(b*) сложение 2-х аргументов."))

(defgeneric b- (v1 v2)
    (:documentation
     "@b(Описание:) обобщенная_функция @b(b*) вычитание 2-х аргументов."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod b* ((v1 number) (v2 number))
  (* v1 v2))

(defmethod b* ((v1 number)(p <point-3d>))
  (let ((rez (make-instance '<point-3d>))
        )
    (setf (<point-3d>-x rez) (* v1 (<point-3d>-x p)))
    (setf (<point-3d>-y rez) (* v1 (<point-3d>-y p)))
    (setf (<point-3d>-z rez) (* v1 (<point-3d>-z p)))    
    rez))

(defmethod b* ((p <point-3d>) (v1 number))
  (b* v1 p))

;;;;;;;;;;

(defmethod b+ ((v1 number)(v2 number))
  (+ v1 v2))

(defmethod b+ ((p1 <point-3d>) (p2 <point-3d>))
  (let ((rez (make-instance '<point-3d>))
        )
    (setf (<point-3d>-x rez) (+ (<point-3d>-x p1) (<point-3d>-x p2)))
    (setf (<point-3d>-y rez) (+ (<point-3d>-y p1) (<point-3d>-y p2)))
    (setf (<point-3d>-z rez) (+ (<point-3d>-z p1) (<point-3d>-z p2)))    
    rez))

;;;;;;;;;;

(defmethod b- ((p1 <point-3d>) (p2 <point-3d>))
  (let ((vec (make-instance '<vector-3d>))
        )
    (setf (<vector-3d>-x vec) (- (<point-3d>-x p2) (<point-3d>-x p1)))
    (setf (<vector-3d>-y vec) (- (<point-3d>-y p2) (<point-3d>-y p1)))
    (setf (<vector-3d>-z vec) (- (<point-3d>-z p2) (<point-3d>-z p1)))    
    vec))

(defmethod b/ ((v1 number) (v2 number))
  (/ v1 v2))

(defmethod b/ ((p <point-3d>) (v number))
  (let ((rez (make-instance '<point-3d>)))
    (setf (<point-3d>-x rez) (/ (<point-3d>-x p) v))
    (setf (<point-3d>-y rez) (/ (<point-3d>-y p) v))
    (setf (<point-3d>-z rez) (/ (<point-3d>-z p) v))    
    rez))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun m* (v &rest rest)
  (reduce #'(lambda (el1 el2) (b* el1 el2)) rest :initial-value v))

(defun m/ (v &rest rest)
  (reduce #'(lambda (el1 el2) (b/ el1 el2)) rest :initial-value v))

(defun m- (v &rest rest)
  (reduce #'(lambda (el1 el2) (b- el1 el2)) rest :initial-value v))

(defun m+ (v &rest rest)
  (reduce #'(lambda (el1 el2) (b+ el1 el2)) rest :initial-value v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun B-point (prm points &key (weights (make-list (length points) :initial-element 1.0d0)))
  (let ((n (1- (length points))))
    (loop :for i :from 0 :to n
          :for w :in weights
          :collect (m* (b i n prm) (svref points i) w) :into a-first
          :collect (m* (b i n prm) w)                 :into a-second
          :finally (return (m/ (apply #'m+ a-first) (apply #'m+ a-second))))))

(progn 
  (defparameter *p*
    (vector
     (point-3d  0 -1 0)
     (point-3d  0.66 -1.0 0.0)
     (point-3d  1.0  -0.66 0.0)
     (point-3d  1.0   0.0  0.0)))
  
  (defparameter w '(1d0 1.66666666666666d0 3d0 1.66666666666666d0 1d0))

  (defparameter *pts*
    (loop :for i :from 0 :to 100
          :collect
          (let ((p (B-point (/ i 100) *p* ))) ;; :weights  w
            (list (<point-3d>-x p) (<point-3d>-y p)))))

  (vgplot:plot (map 'list #'first *pts*)
               (map 'list #'second *pts*) ""
               
               (map 'list #'(lambda (p) (<point-3d>-x p ) ) *p*)
               (map 'list #'(lambda (p) (<point-3d>-y p ) ) *p*)
               ""))
#+nil
(progn 
  (vgplot:format-plot t "unset tics")
  (vgplot:format-plot t "set label ")
  (vgplot:xlabel "X")
  (vgplot:ylabel "Y"))
