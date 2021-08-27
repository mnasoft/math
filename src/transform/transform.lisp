(defpackage #:math/transform
  (:use #:cl #:math/arr-matr #:math/coord) ;; #:math/arr-matr
  (:export normalize
           rotate-x
           rotate-y
           rotate-z
           rotate-v
           move-xyz
           move-v
           rotate-around))

(in-package #:math/transform)

(defun normalize (v)
  "@b(Описание:) функция @b(normalize) возвращает нормализованный вектор.
Длина нормализованного вектора равна 1.0.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (normalize '(1 2 3)) => (0.26726124 0.5345225 0.8017837)
 (normalize '(2 -3))  => (0.5547002 -0.8320503)
@end(code)
"
    (let ((len (sqrt (apply #'+ (mapcar #'(lambda (el) (* el el)) v)))))
      (mapcar #'(lambda (el) (/ el len)) v)))

(defmethod rotate-x ((α number))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол α вокруг
  оси x.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn (defparameter *p* (make-instance 'math/arr-matr:<matrix> :dimensions '(1 4)))
        (setf (math/arr-matr:row *p* 0) '(10.0 20.0 30.0 1.0))
        (math/arr-matr:multiply *p* (rotate-y (dtr 90.0))))
@end(code)
"
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(1.0     0.0         0.0   0.0))
      (setf (math/arr-matr:row matrix 1) `(0.0 ,(cos α) ,(- (sin α)) 0.0))
      (setf (math/arr-matr:row matrix 2) `(0.0 ,(sin α)    ,(cos α)  0.0))
      (setf (math/arr-matr:row matrix 3) `(0.0     0.0         0.0   1.0))
      matrix))

(defmethod rotate-y ((β number))
    "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
  преобразования, которая вращает систему координат на угол β вокруг
  оси y."

    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(   ,(cos β)  0.0 ,(sin β) 0.0))
      (setf (math/arr-matr:row matrix 1) `(       0.0   1.0     0.0  0.0))
      (setf (math/arr-matr:row matrix 2) `(,(- (sin β)) 0.0 ,(cos β) 0.0))
      (setf (math/arr-matr:row matrix 3) `(       0.0   0.0     0.0  1.0))
      matrix))

(defmethod rotate-z ((γ number))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол γ вокруг оси
z."
  (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
    (setf (math/arr-matr:row matrix 0) `(,(cos γ) ,(- (sin γ)) 0.0 0.0))
    (setf (math/arr-matr:row matrix 1) `(,(sin γ)    ,(cos γ)  0.0 0.0))
    (setf (math/arr-matr:row matrix 2) `(    0.0         0.0   1.0 0.0))
    (setf (math/arr-matr:row matrix 3) `(    0.0         0.0   0.0 1.0))
    matrix))

(defmethod rotate-v ((θ number) (v cons))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает систему координат на угол α вокруг
оси, заданной вектором v. Вектор v должен быть нормализованным (иметь
единичную длину)."
  (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4)))
        (x (first  v))
        (y (second v))
        (z (third  v)))
    (setf (math/arr-matr:row matrix 0) `(,(+       (cos θ) (* (- 1 (cos θ)) x x)) ,(- (* (- 1 (cos θ)) x y) (* (sin θ) z)) ,(+ (* (- 1 (cos θ)) x z) (* (sin θ) y)) 0.0))
    (setf (math/arr-matr:row matrix 1) `(,(+ (* (- 1 (cos θ)) y x) (* (sin θ) z))       ,(+ (cos θ) (* (- 1 (cos θ)) y y)) ,(- (* (- 1 (cos θ)) y z) (* (sin θ) x)) 0.0))
    (setf (math/arr-matr:row matrix 2) `(,(- (* (- 1 (cos θ)) z x) (* (sin θ) y)) ,(+ (* (- 1 (cos θ)) z y) (* (sin θ) x))       ,(+ (cos θ) (* (- 1 (cos θ)) z z)) 0.0))
    (setf (math/arr-matr:row matrix 3) `(                                     0.0                                      0.0                                      0.0 1.0))
    matrix))

(defmethod move-xyz ((dx number) (dy number) (dz number))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая перемещает систему координат на (dx dy dz).
"
  (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
    (setf (math/arr-matr:row matrix 0) `(1.0 0.0 0.0 0.0))
    (setf (math/arr-matr:row matrix 1) `(0.0 1.0 0.0 0.0))
    (setf (math/arr-matr:row matrix 2) `(0.0 0.0 1.0 0.0))
    (setf (math/arr-matr:row matrix 3) `(,dx ,dy ,dz 1.0))
    matrix))

(defmethod move-v ((v cons))
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая перемещает систему координат в направлении
вектора v."
  (move-xyz (first v) (second v) (third v)))

(defmethod rotate-around ((point-1 cons) (point-2 cons) θ)
  "@b(Описание:) метод @b(rotate-x) возвращает однородную матрицу
преобразования, которая вращает протсранство вокруг оси, заданной
точками point-1 и point-2"
  (let ((rotate-p1-p2 (rotate-v θ (normalize (mapcar #'- point-2 point-1))))
        (move-p1-p0   (move-v (mapcar #'- point-1)))
        (move-p0-p1   (move-v point-1)))
    (math/arr-matr:multiply
     (math/arr-matr:multiply move-p1-p0 rotate-p1-p2)
     move-p0-p1)))

(defmethod transform ((point cons) (matrix <matrix>))
  "@b(Описание:) метод @b(transform) возвращает координаты точки
  @b(point), преобразованные с помощью матрицы matrix."
  (let ((p (make-instance '<matrix> :dimensions '(1 4))))
    (setf (row p 0) (list (first point) (second point) (third point) 1.0))
    (nreverse (cdr (nreverse (row (multiply p matrix) 0))))))

(transform '(10 20 30) (move-xyz 10 20 30))

(rotate-around '(0 0 0) '(1 1 1) (dtr 90))
"//n133619/home/_namatv/epiven/Дым/10211.ДТ59.261ПМ_Определение дымления/NIL-6"
#+nil
(progn
  (defparameter *m* (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4)))

  (defparameter *p* (make-instance 'math/arr-matr:<matrix> :dimensions '(1 4)))

  (setf (math/arr-matr:row *p* 0) '(10.0 20.0 30.0 1.0))

  (math/arr-matr:multiply *p* (rotate-y (dtr -90.0)))
  
  (math/arr-matr:multiply *p* (move 10 20 30))

  (rotate-x (dtr 22.5) *m*)
  (math/arr-matr:multiply *p* (rotate-v (dtr -90.0) (normalize `(0 1 0))))
  (math/arr-matr:multiply *p* (rotate-y (dtr -90.0)))
  )



