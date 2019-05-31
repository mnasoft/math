;;;; matr-generics.lisp

(in-package #:math)

(export 'matr-name-*)
(defgeneric matr-name-* (matrix) (:documentation "Matr"))

(defgeneric matr-copy-* (matrix) (:documentation "Matr"))

(defgeneric matr-ij-*   (matrix row col) (:documentation "Matr"))

(defgeneric matr-set-ij-* (matrix value row col) (:documentation "Matr"))

(defgeneric matr-rows-*   (matrix) (:documentation "Matr"))

(defgeneric matr-cols-*   (matrix) (:documentation "Matr"))

(defgeneric matr-set-row-* (matrix row lst) (:documentation "Matr"))

(defgeneric matr-get-row-* (matrix row) (:documentation "Matr"))

(defgeneric matr-set-col-* (matrix col lst) (:documentation "Matr"))

(defgeneric matr-get-col-* (matrix col)  (:documentation "Matr"))

(defgeneric matr-major-diagonal* (matrix)
  (:documentation 
   "Извлекает главную диагональ матрицы
Пример использования:
 (defparameter *mm* (make-instance 'matrix :rows 4  :data '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0)))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (major-diagonal *mm*)
 (1.0d0 5.0d0 9.0d0)
"))

(defgeneric matr-minor-diagonal* (matrix)
  (:documentation 
   "Извлекает побочную диагональ матрицы
Пример использования:
 (defparameter *mm* (make-instance 'matrix :rows 4  :data '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 9d0) (10d0 11d0 12d0))))
 =>
 Matr 4х3
 [ 1.0d0     2.0d0     3.0d0    ]
 [ 4.0d0     5.0d0     6.0d0    ]
 [ 7.0d0     8.0d0     9.0d0    ]
 [ 10.0d0    11.0d0    12.0d0   ]

 (minor-diagonal *mm*)
"))

(defgeneric matr-eval-* (matrix) (:documentation "Matr"))

(defgeneric matr-equal* (matrix1 matrix2 &key test) (:documentation "Проверка матриц на равенство"))

