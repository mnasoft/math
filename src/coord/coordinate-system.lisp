;;;; ./src/coord/coordinate-system.lisp
 
(defpackage #:math/coord
  (:use #:cl #:math/core)
  (:export dtr
           rtd
           polar->cartesian
           cartesian->polar
           spherical->cartesian
           cartesian->spherical
	   ))

(in-package :math/coord)

(defun dtr (degree)
"@b(Описание:) функция @b(dtr) переводит значение, заданное в градусах, в радианы.

 @b(Пример использования:)

@begin[lang=lisp](code)
 (dtr (rtd 1/2)) => 0.5d0
@end(code)
"
  (* pi 1/180 degree ))

(defun rtd (radian)
"@b(Описание:) функция @b(rtd) переводит значение, 
заданное в радианах, в градусы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (rtd (dtr 45)) => 45.0d0
@end(code)
"
  (/ radian pi 1/180))

(defun polar->cartesian (radius-angle)
"@b(Описание:) функция @b(polar->cartesian) переводит полярные координаты в декартовы.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (polar->cartesian (list 10.0 (dtr 45))) => (7.0710678118654755d0 7.071067811865475d0)
@end(code)
"
  (let ((radius (first  radius-angle))
	(angle  (second radius-angle)))
    (list (* radius (cos angle)) (* radius (sin angle)))))

(defun cartesian->polar (x-y)
"@b(Описание:) функция @b(cartesian->polar) переводит декартовы координаты в полярные.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->polar (list 10.0 10)) (14.142136 0.7853982)
@end(code)
"
  (let ((radius (sqrt (apply #'+ (mapcar #'square x-y))))
	(angle  (atan (second x-y) (first x-y))))
    (list radius angle)))

(defun spherical->cartesian (r φ θ)
"@b(Описание:) функция @b(spherical->cartesian) выполняет преобразование координат 
из сферических в декартовы.

 @b(Переменые:)
@begin(list)
 @item(r - расстояние от начала координат до заданной точки;)
 @item(φ - азимутальный угол (в плоскости X0Y);)
 @item(θ - зенитный угол.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (spherical->cartesian 100 (dtr 30) (dtr 45))
 => (61.237243569579455d0 35.35533905932737d0 70.71067811865476d0)
@end(code)
"
  (let ((x (* r (cos φ) (sin θ)))
	(y (* r (sin φ) (sin θ)))
	(z (* r (cos θ))))
    (list x y z)))

(defun cartesian->spherical (x-y-z)
"@b(Описание:) функция @b(cartesian->spherical) выполняет преобразование координат 
из декартовых в сферические.

 @b(Переменые:)
@begin(list)
 @item(x-y-z - список, содержащий соответствующие координаты.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->spherical '(61.237243569579455d0 35.35533905932737d0 70.71067811865476d0))
 =>(100.0d0 0.5235987755982988d0 0.7853981633974483d0)
@end(code)
"
  (let* ((x (first  x-y-z))
	 (y (second x-y-z))
	 (z (third  x-y-z))
	 (r (sqrt (apply #'+ (mapcar #'square x-y-z))))
	 (φ (atan y x))
	 (θ (atan (sqrt (+ (* x x) (* y y))) z)))
    (list r φ θ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmethod rotate-by-x ((α number))
  "@b(Описание:) метод @b(rotate-by-x) возвращает однородную матрицу
  преобразования, которая преобразует вращает систему координат на
  угол α вокруг оси x.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (progn (defparameter *p* (make-instance 'math/arr-matr:<matrix> :dimensions '(1 4)))
        (setf (math/arr-matr:row *p* 0) '(10.0 20.0 30.0 1.0))
        (math/arr-matr:multiply *p* (rotate-by-y (dtr 90.0))))
@end(code)
"
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(1.0     0.0         0.0   0.0))
      (setf (math/arr-matr:row matrix 1) `(0.0 ,(cos α) ,(- (sin α)) 0.0))
      (setf (math/arr-matr:row matrix 2) `(0.0 ,(sin α)    ,(cos α)  0.0))
      (setf (math/arr-matr:row matrix 3) `(0.0     0.0         0.0   1.0))
      matrix))

(defmethod rotate-by-y ((α number))
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(   ,(cos α)  0.0 ,(sin α) 0.0))
      (setf (math/arr-matr:row matrix 1) `(       0.0   1.0     0.0  0.0))
      (setf (math/arr-matr:row matrix 2) `(,(- (sin α)) 0.0 ,(cos α) 0.0))
      (setf (math/arr-matr:row matrix 3) `(       0.0   0.0     0.0  1.0))
      matrix))

(defmethod rotate-by-z ((α number))
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(,(cos α) ,(- (sin α)) 0.0 0.0))
      (setf (math/arr-matr:row matrix 1) `(,(sin α)    ,(cos α)  0.0 0.0))
      (setf (math/arr-matr:row matrix 2) `(    0.0         0.0   1.0 0.0))
      (setf (math/arr-matr:row matrix 3) `(    0.0         0.0   0.0 1.0))
      matrix))

(defmethod rotate-by-v ((θ number) (v cons))
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4)))
          (x (first  v))
          (y (second v))
          (z (third  v)))
      (setf (math/arr-matr:row matrix 0) `(,(+       (cos θ) (* (- 1 (cos θ)) x x)) ,(- (* (- 1 (cos θ)) x y) (* (sin θ) z)) ,(+ (* (- 1 (cos θ)) x z) (* (sin θ) y)) 0.0))
      (setf (math/arr-matr:row matrix 1) `(,(+ (* (- 1 (cos θ)) y x) (* (sin θ) z))       ,(+ (cos θ) (* (- 1 (cos θ)) y y)) ,(- (* (- 1 (cos θ)) y z) (* (sin θ) x)) 0.0))
      (setf (math/arr-matr:row matrix 2) `(,(- (* (- 1 (cos θ)) z x) (* (sin θ) y)) ,(+ (* (- 1 (cos θ)) z y) (* (sin θ) x))       ,(+ (cos θ) (* (- 1 (cos θ)) z z)) 0.0))
      (setf (math/arr-matr:row matrix 3) `(                                     0.0                                      0.0                                      0.0 1.0))
      matrix))

  (defmethod move ((dx number) (dy number) (dz number))
    (let ((matrix (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4))))
      (setf (math/arr-matr:row matrix 0) `(1.0 0.0 0.0 0.0))
      (setf (math/arr-matr:row matrix 1) `(0.0 1.0 0.0 0.0))
      (setf (math/arr-matr:row matrix 2) `(0.0 0.0 1.0 0.0))
      (setf (math/arr-matr:row matrix 3) `(,dx ,dy ,dz 1.0))
      matrix))

#+nil
(progn
  (defparameter *m* (make-instance 'math/arr-matr:<matrix> :dimensions '(4 4)))

  (defparameter *p* (make-instance 'math/arr-matr:<matrix> :dimensions '(1 4)))

  (setf (math/arr-matr:row *p* 0) '(10.0 20.0 30.0 1.0))

  (math/arr-matr:multiply *p* (rotate-by-y (dtr -90.0)))
  
  (math/arr-matr:multiply *p* (move 10 20 30))

  (rotate-by-x (dtr 22.5) *m*)
  (math/arr-matr:multiply *p* (rotate-by-v (dtr -90.0) (normalize `(0 1 0))))
  (math/arr-matr:multiply *p* (rotate-by-y (dtr -90.0)))
  )



