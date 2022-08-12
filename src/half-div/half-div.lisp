;;;; ./src/half-div/half-div.lisp

(defpackage #:math/half-div
  (:use #:cl)
  (:export h-div) 
  (:export h-div-lst)
  (:documentation
   "@b(Описание:) пакет @b( half-div) реализует алгоритм половинного
 деления для выполнения поиска корня функции на отрезке."))

(in-package #:math/half-div)

(defun boole-to-int (b) (if b 1 0))

(defun same-znak (a b)
  (if (zerop (logxor
	      (boole-to-int (minusp a))
	      (boole-to-int (minusp b))))
      t
      nil))

(defun epsylon (x &key (eps 1e-6))
  (+ (* (abs x) eps ) eps))

(defun h-div (a b func &key (eps 1e-6) (iters 1000))
  (do*
   ( (i 0 (1+ i))
     (fa (funcall func a))
     (fb (funcall func b))
     (x (/ (+ a b) 2) (/ (+ a b) 2))
     (fx (funcall func x)(funcall func x)))
   ((or
     (> i iters)
     (<= (abs(- b a))
	 (epsylon x :eps eps)))
    (values x
	    (not (> i iters))
	    (abs(- b a))
	    (epsylon x :eps eps)))
    (if (same-znak fa fx)
	(setf a x fa fx)
	(setf b x fb fx))))

(defun h-div-lst (a b func n p_lst &key (eps 1e-6) (iters 1000))
  (labels
      ((subst-by-no (lst el position)
         (substitute el (nth position lst) lst :start position)))
    (do*
     ( (i 0 (1+ i))
       (fa (apply func (subst-by-no p_lst a n)))
       (fb (apply func (subst-by-no p_lst b n)))
       (x (/ (+ a b) 2)
	  (/ (+ a b) 2))
       (fx (apply func (subst-by-no p_lst x n))
	   (apply func (subst-by-no p_lst x n))))
     ((or
       (> i iters)
       (<= (abs(- b a))
	   (epsylon x :eps eps)))
      (values x
	      (not (> i iters))
	      (abs(- b a))
	      (epsylon x :eps eps)))
      (if (same-znak fa fx)
	  (setf a x fa fx)
	  (setf b x fb fx)))))
