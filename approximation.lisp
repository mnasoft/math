;;;; approximation.lisp

(in-package #:math)

(defun appr-line( XX PP0 PP1)
  (multiple-value-bind (x p0 p1)
      (values XX PP0 PP1)
    (+ (second p0)
       (/ (*
	   (- x (first p0))
	   (- (second p1) (second p0)))
	  (- (first p1) (first p0))))))

(defun appr-line-list( XX n0 n1 lst)
  (appr-line
   XX
   (nth n0 lst)
   (nth n1 lst)))

(defun appr_table (x table)
  "
Выполняет линейную интерполяцию и экстраполяцию для значения x на таблице table
Пример использования:
(appr_table 0.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 1.5 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
(appr_table 3.0 '((0.0 0.0) (1.0 1.0) (2.0 4.0) (4.0 0.0)))
"
  (do ((len (length table))
       (res nil)
       (i 0 (1+ i))
       )
      ((>= i len) (appr-line-list
		   x
		   (first res)
		   (second res )
		   table))
    (cond
      ((and (null res) (<= x (first (nth 0 table))))
       (setf res '(0 1)))
      ((and (null res) (<= (first (nth (1- len) table)) x))
       (setf res (list (- len 2) (- len 1))))
      ((and (null res)
	    (<= (first(nth i table)) x)
	    (<= x (first(nth (1+ i) table))))
       (setf res (list i (1+ i)))))))
