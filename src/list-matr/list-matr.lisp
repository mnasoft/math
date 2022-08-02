;;;; ./src/list-matr/list-matr.lisp

(defpackage #:math/list-matr
  (:use #:cl)
  (:export rows
           cols
           dimensions
           col
           row
           )
  (:export average-value
           average-not-nil-value
           average-row-value
           average-row-not-nil-value
           average-col-value
           average-col-not-nil-value
           )
  (:export max-row-not-nil-value
           max-col-not-nil-value)
  (:export transpose)
  (:export detach-last-col
           get-last-col)
  (:export prepend-row
           prepend-rows
           prepend-col)
  (:export append-row
           append-col)
  (:export lv-print
           lm-print)
  (:export unite-rows
           unite-cols)
  (:export make)
  (:documentation
   "@b(Описание:) пакет @b(math/list-matr) определяет некоторые
 операции над матрицами, представленными 2d-list, (списком состоящим
 из списков)"))

(in-package :math/list-matr)

(defun transpose (2d-list)
  (apply #'mapcar #'list 2d-list))

(defun unite-rows (lst)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(defun unite-cols (lst)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) lst)
    rez))

(defun make (rows cols lst)
  (let ((rez nil)
        (rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
        (push (nth (+ (* r cols) c) lst) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

(defun rows (2d-list)
  (length 2d-list))

(defun cols (2d-list)
  (math/stat:min-value (mapcar #'length 2d-list)))

(defun dimensions (2d-list)
  (list (rows 2d-list) (cols 2d-list)))

(defun row (row 2d-list)
  (nth row 2d-list))

(defun col (col 2d-list)
  (mapcar #'(lambda (el) (nth col el)) 2d-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun average-value (lst)
  (math/stat:average-value (unite-rows lst)))

(defun average-not-nil-value (lst)
  (math/stat:average-not-nil-value (unite-rows lst)))

;;;;

(defun average-row-value (lst)
  (mapcar #'math/stat:average-value lst))

(defun average-row-not-nil-value (lst)
  (mapcar #'math/stat:average-not-nil-value lst))

(defun max-row-not-nil-value (lst)
  (mapcar #'(lambda (el) (apply #'max (math/core:exclude-nil-from-list el))) lst))

;;;;

(defun average-col-value (lst)
  (average-row-value (transpose lst)))

(defun average-col-not-nil-value (lst)
  (average-row-not-nil-value (transpose lst)))

(defun max-col-not-nil-value (lst)
  (max-row-not-nil-value (transpose lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; lm-print

(defun lm-print (2d-list &key (fmt "~6,1f") (stream t))
  (format stream (concatenate 'string  "~{~{" fmt "~^ ~}~%~}") 2d-list))

(defun lv-print (lst &key (fmt "~6,1f") (stream t))
  (format stream (concatenate 'string  "~{" fmt "~^ ~}~%") lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; append

(defun append-col (c-lst 2d-list)
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (append l (list r)) rez))))

(defun append-row (c-lst 2d-list)
  (transpose (append-col c-lst (transpose 2d-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prepend

(defun prepend-col (c-lst 2d-list)
  (let ((rez nil)
        (r nil))
    (dolist (l 2d-list (reverse rez))
      (setf r (car c-lst)
            c-lst (cdr c-lst))
      (push (cons r l) rez))))

(defun prepend-row (c-lst 2d-list)
  (transpose (prepend-col c-lst (transpose 2d-list))))

(defun prepend-rows (rs-lst 2d-list)
  (reduce #'(lambda (x y) (prepend-row y x)) (reverse rs-lst) :initial-value 2d-list))

(defun detach-last-col (2d-list)
  (mapcar
   #'(lambda(el) (reverse (cdr (reverse el))))
   2d-list))

(defun get-last-col (2d-list)
  (mapcar
   #'(lambda(el) (car (last el)))
   2d-list))
