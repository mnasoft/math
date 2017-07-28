;;;; las-rotation.lisp
(in-package #:math)

(defun matr-rotation (matr)
  "Решение системы линейных алгебраических уравнений (СЛАУ) методом вращения, состоящего из:
- сведения СЛАУ к треугольной системе;
- нахождение корней методом обратного хода метода Гаусса
;
Возвращает вектор с количеством элементов равным количеству строк в СЛАУ
;
Matr - массив, у которого количество строк (первая размерность)
должно быть на единицу меньше количества столбцов (вторая размерность)
Данная матрица меняется в процессе в процессе вычисления функции
;
Пример использования:
(let* ((data '((0.0d0 0.0d0 1.0d0 3.0d0)
	       (0.0d0 1.0d0 0.0d0 2.0d0)
	       (1.0d0 0.0d0 1.0d0 4.0d0)))
       (mtr (make-array '(3 4) :initial-contents data))
       (m (cl-utilities:copy-array mtr)))
  (values (math:matr-rotation mtr)))
=> #(1.0d0 2.0d0 3.0d0)
;
(let* ((data '((1.0d0 0.0d0 1.0d0 4.0d0)
	       (0.0d0 0.0d0 1.0d0 3.0d0)
	       (0.0d0 1.0d0 0.0d0 2.0d0)
	       ))
       (mtr (make-array '(3 4) :initial-contents data))
       (m (cl-utilities:copy-array mtr)))
  (values (math:matr-rotation mtr)))
=> #(1.0d0 2.0d0 3.0d0)
;
(let ((m-test (make-array '(3 4)
			  :initial-contents
			  '((10.0d0 11.0d0  12.0d0  4.0d0)
			    (15.0d0 17.0d0  21.0d0  2.0d0)
			    (70.0 8.0  10.0 3.0))
			  )))
  (matr-rotation (copy-array m-test)))
=>#(0.03588235294117642d0 2.182352941176469d0 -1.6970588235294102d0)
;
Есть необходимость доработки с точки зрения решения разреженной СЛАУ!
"
  (let ((n (array-dimension matr 0))	; Количество строк
	(m (array-dimension matr 1))	; Количество столбцов
	)
    (if (/= (1+ n) m)			; Проверка размерности
	(break "ERROR IN FUNC matr-rotation:~%n+1 != m~%" ))
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

