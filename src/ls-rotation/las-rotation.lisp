;;;; ./src/ls-rotation/las-rotation.lisp

(defpackage #:math/ls-rotation
  (:use #:cl );; #:math/arr-matr
  (:export solve-linear-system-rotation))

(in-package :math/ls-rotation)

(defun solve-linear-system-rotation (matr)
  (let ((n (array-dimension matr 0))	; Количество строк
	(m (array-dimension matr 1))	; Количество столбцов
	)
    (if (/= (1+ n) m)			; Проверка размерности
	(break "ERROR IN FUNC solve-linear-system-rotation:~%n+1 != m~%" ))
    (do ((i 0 (1+ i)))
	((not (< i n)) matr)
      (do ((a nil) (b nil) (c nil) (s nil) (tmp nil)
	   (j  (1+ i) (1+ j)))
	  ((not (< j n)) 'done-do-02)
	(setf a (aref matr i i)
	      b (aref matr j i)
	      c (/ a (sqrt (+ (* a a) (* b b))))
	      s (/ b (sqrt (+ (* a a) (* b b)))))
	(do ((k i (1+ k )))
	    ((not (<= k n)) 'done-do-03)
	  ;;	  (break "001 i=~A j=~A k=~A~%a=~A b=~A c=~A s=~A~%~S~%" i j k a b c s matr)
	  (setf tmp (aref matr i k)
		(aref matr i k) (+ (* c (aref matr i k)) (* s (aref matr j k)))
		(aref matr j k) (- (* c (aref matr j k)) (* s tmp))))))
    (do ((i (1- n) (1- i))		; Обратный ход метода Гаусса
	 (x (make-array n :initial-element 1.0d0))
	 (summ 0.0d0 0.0d0))
	((not (>= i 0)) x)
      (do ((j (1+ i) (1+ j)))
	  ((not (< j n)))
	(setf summ (+ summ (* (aref matr i j) (aref x j)))))
      (setf summ (- (aref matr i n) summ)
	    (aref x i) (/ summ (aref matr i i)))
      )))

