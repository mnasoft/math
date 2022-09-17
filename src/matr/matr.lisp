;;;; ./src/matr/matr.lisp

(defpackage #:math/matr
  (:use #:cl #:math/coord)
  (:export initialize-instance)
  (:export matr-new
           make-vector-n
	   <matrix>
           )
  (:export swap-cols
           swap-cols*
           swap-rows
           swap-rows*)
  (:export matrix-data
           matrix->2d-list
           transpose 
           matr-eval-*
           )
  (:export equivalent)
  (:export mref
           copy
           dimensions
           rows
           cols
           row
           col
           main-diagonal
           squarep
           anti-diagonal
           add
           multiply)
  (:export average-value
           average-not-nil-value
           average-row-value
           average-row-not-nil-value
           average-col-value
           average-col-not-nil-value
           )
  (:export max-row-not-nil-value
           max-col-not-nil-value)
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
  (:export normalize
           rotate-x
           rotate-y
           rotate-z
           rotate-v
           rotate-around
           move-xyz
           move-v
           transform))

(in-package :math/matr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/arr-matr/package.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matr-eval-* (matrix))

(defgeneric matr-equal* (matrix1 matrix2 &key test) )

(defgeneric transpose (matrix))

(defgeneric matr-name-* (matrix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <matrix> ()
  ((data :accessor matrix-data :initform nil :initarg :data :documentation "Сдержимое матрицы.")))

(defmethod matr-name-* ((mm <matrix>))
  (type-of mm))

(defmethod print-object ((mm <matrix>) s)
  (format s "~A " (matr-name-* mm))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~8A " (aref (matrix-data mm) i j)))
	 (format s "]"))))

(defmethod initialize-instance ((mm <matrix>) &key dimensions initial-element initial-contents data (element-type t))
  (when (and (consp dimensions) (/= (length dimensions) 2))
    (error "(/= (length dimensions) 2):"))
  (cond
    (data (setf (matrix-data mm) data))
    ((and (consp dimensions) (= (length dimensions) 2) initial-element)
     (setf (matrix-data mm)
	   (make-array dimensions
		       :element-type element-type :initial-element initial-element)))
    ((and (consp dimensions) (= (length dimensions) 2) (null initial-element )
	  (setf (matrix-data mm)
		(make-array dimensions
			    :element-type element-type :initial-element 0))))
    ((and (consp initial-contents) (consp (first initial-contents)))
     (setf (matrix-data mm)
	   (make-array (list (length initial-contents) (length (first initial-contents)))
		       :element-type element-type :initial-contents initial-contents)))
    (t (error "(defmethod initialize-instance ((mm <matrix>) &key dimensions initial-element initial-contents (element-type t))
Что-то пошло не так!"))))

(defun matr-new (rows cols &optional (lst nil))
  (let ((mm (make-instance '<matrix> :dimensions (list rows cols) :initial-element 0.0d0))
	(ll lst))
    (when (consp lst)
	(loop :for i :from 0 :below (array-dimension (matrix-data mm) 0) :do
	     (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1) :do
		  (setf (aref (matrix-data mm) i j) (car ll)
			ll (cdr ll)))))
    mm))


(defmethod copy ((mm-ref <matrix>))
  (make-instance '<matrix> :data (cl-utilities:copy-array (matrix-data mm-ref))))

(defmethod dimensions ((mm <matrix>)) (array-dimensions (matrix-data mm)))

;;;;;;;;;; equivalent ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod equivalent ((m1 <matrix>) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (let ((rez t))
    (if (and (= (rows m1) (rows m2))
	     (= (cols m1) (cols m2)))
	(loop :for r :from 0 :below (rows m1) :do
	   (loop :for c :from 0 :below (cols m1) :do
		(setf rez (and rez (funcall test (mref m1 r c ) (mref m2 r c) )))))
	(setf rez nil))
    rez))

(defmethod equivalent ((a1 array) (a2 array) &key (test #'math/core:semi-equal))
   (declare (type (array * (* *)) a1 a2))
   (when (not (equal (array-dimensions a1)
		     (array-dimensions a2)))
     (return-from equivalent nil))
   (reduce #'(lambda (el1 el2) (and el1 el2))
	   (apply #'append
		  (loop :for i :from 0 :below (array-dimension a1 0)
			:collect
			(loop :for j :from 0 :below (array-dimension a1 1)
			      :collect
			      (funcall test (aref a1 i j) (aref a2 i j)))))
	   :initial-value t))

(defmethod equivalent ((m1 <matrix>) (a2 array) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a2))  
  (equivalent m1 (make-instance '<matrix> :data a2) :test test))

(defmethod equivalent ((a1 array) (m2 <matrix>) &key (test #'math/core:semi-equal))
  (declare (type (array * (* *)) a1))
  (equivalent (make-instance '<matrix> :data a1) m2 :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod main-diagonal ((mm <matrix>))
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:collect (mref mm i i)))

(defmethod (setf main-diagonal) (elements (mm <matrix>))
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:for el :in elements :by #'cdr  :do
	  (setf (mref  mm i i) el))
  mm)

(defmethod (setf main-diagonal) ((element number) (mm <matrix>) )
  (loop :for i :from 0 :below (min (rows mm) (cols mm))
	:do (setf (mref  mm i i) element))
  mm)

(defmethod squarep ((mm <matrix>))
  (= (cols mm) (rows mm) ))

(defmethod anti-diagonal ((mm <matrix>))
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :collect (mref mm c r)))

(defmethod (setf anti-diagonal) (elements (mm <matrix>) )
  (assert (squarep mm) (mm) "Матрица не является квадратной~%~S" mm)
  (loop
     :for c :from 0 :below (cols mm)
     :for r :downfrom (- (rows mm) 1) :to 0
     :for e :in elements :by #'cdr :do
       (setf (mref mm c r) e))
  mm)

(defmethod add ((a <matrix> ) (b <matrix>))
  (assert (and (= (rows a) (rows b)) (= (cols a) (cols b))) (a b)
	  "Матрицы A[~A,~A] и B[~A,~A] имеют отличаюшиеся размеры"
	  (rows a) (cols a) (rows b) (cols b))
  (let ((a+b (matr-new (rows a) (cols a))))
    (loop :for r :from 0 :below (rows a) :do
	 (loop :for c :from 0 :below (rows b) :do
	      (setf (mref a+b r c)
		    (+ (mref a r c ) (mref b r c)))))
    a+b))

(defmethod multiply ((a <matrix> ) (b <matrix>))
  (let ((a_n (rows a))
	(a_m (cols a))
	(b_n (rows b))
	(b_m (cols b))
	(c nil))
    (assert (= a_m b_n) (a_m b_n) "Запрещенные размеры матриц для перемножения: A[~A,~A] x B[~A,~A]" a_n a_m b_n b_m)
    (setf c (make-instance '<matrix> :dimensions (list a_n b_m) :initial-element 0))
    
    (do ((i 0 (1+ i))
	 (a-row nil))
	((>= i a_n))
      (setf a-row (row a i))
      (do ((j 0 (1+ j))
	   (b-col nil))
	  ((>= j b_m))
	(setf b-col (col b j))
	(setf (mref c i j)
	      (apply #'+ (mapcar #'* a-row b-col)))))
    c))

(defmethod multiply ((a number ) (b <matrix>))
  (let ((rez (make-instance '<matrix> :dimensions (dimensions b))))
    (loop :for i :from 0 :below (rows b) :do
	 (loop :for j :from 0 :below (cols b) :do
	      (setf (mref rez i j) (* a (mref b i j)))))
    rez))

(defmethod matrix->2d-list ((mm <matrix>))
  (loop :for i :from 0 :below (rows mm) :collect
					(row mm i)))



(defmethod swap-rows*  ((mm <matrix>) i j)
  (assert (and (< -1 i (rows mm)) (< -1 j (rows mm))))
  (when (/= i j)
    (let ((row-i (row mm i))
	  (row-j (row mm j)))
      (setf (row mm i) row-j
	    (row mm j) row-i)))
  mm)

(defmethod swap-cols*  ((mm <matrix>) i j)
  (assert (and (< -1 i (cols mm)) (< -1 j (cols mm))))
  (when (/= i j)
    (let ((col-i (col mm i))
	  (col-j (col mm j)))
      (setf (col mm i) col-j
	    (col mm j) col-i)))
  mm)

(defmethod swap-rows  ((mm <matrix>) i j)
  (swap-rows* (copy mm) i j))

(defmethod swap-cols  ((mm <matrix>) i j)
  (swap-cols* (copy mm) i j))

(defmethod matr-eval-* ((mm <matrix>))
  (let ((rows (rows mm))
	(cols (cols mm))
	(mm-cp (copy  mm)))
    (loop :for i :from 0 :below rows
       :do
	 (loop :for j :from 0 :below cols
	    :do (setf (aref (matrix-data mm) i j) (eval (aref (matrix-data mm) i j)))))
    mm-cp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; /src/2d-array/2d-array.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vector-n (element n)
  (make-array (list n) :initial-element element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ./src/list-matr/list-matr.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unite-rows (2d-list)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) 2d-list)
    rez))

(defun unite-cols (2d-list)
  (let ((rez nil))
    (mapcar #'(lambda (el) (setf rez (append rez el) ) ) 2d-list)
    rez))

(defun make (rows cols 2d-list)
  (let ((rez nil)
        (rw nil))
    (dotimes (r rows (reverse rez))
      (dotimes (c cols)
        (push (nth (+ (* r cols) c) 2d-list) rw))
      (push (reverse rw) rez)
      (setf rw nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; average - Вычисление среднего значения
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun average-value (2d-list)
  (math/stat:average-value (unite-rows 2d-list)))

(defun average-not-nil-value (2d-list)
  (math/stat:average-not-nil-value (unite-rows 2d-list)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ./src/transform/transform.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize (v)
    (let ((len (sqrt (apply #'+ (mapcar #'(lambda (el) (* el el)) v)))))
      (mapcar #'(lambda (el) (/ el len)) v)))

(defmethod rotate-x ((α number))
    (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
      (setf (row matrix 0) `(1.0     0.0         0.0   0.0))
      (setf (row matrix 1) `(0.0 ,(cos α) ,(- (sin α)) 0.0))
      (setf (row matrix 2) `(0.0 ,(sin α)    ,(cos α)  0.0))
      (setf (row matrix 3) `(0.0     0.0         0.0   1.0))
      matrix))

(defmethod rotate-y ((β number))
    (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
      (setf (row matrix 0) `(   ,(cos β)  0.0 ,(sin β) 0.0))
      (setf (row matrix 1) `(       0.0   1.0     0.0  0.0))
      (setf (row matrix 2) `(,(- (sin β)) 0.0 ,(cos β) 0.0))
      (setf (row matrix 3) `(       0.0   0.0     0.0  1.0))
      matrix))

(defmethod rotate-z ((γ number))
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(,(cos γ) ,(- (sin γ)) 0.0 0.0))
    (setf (row matrix 1) `(,(sin γ)    ,(cos γ)  0.0 0.0))
    (setf (row matrix 2) `(    0.0         0.0   1.0 0.0))
    (setf (row matrix 3) `(    0.0         0.0   0.0 1.0))
    matrix))

(defmethod rotate-v ((θ number) (v cons))
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4)))
        (x (first  v))
        (y (second v))
        (z (third  v)))
    (setf (row matrix 0) `(,(+       (cos θ) (* (- 1 (cos θ)) x x)) ,(- (* (- 1 (cos θ)) x y) (* (sin θ) z)) ,(+ (* (- 1 (cos θ)) x z) (* (sin θ) y)) 0.0))
    (setf (row matrix 1) `(,(+ (* (- 1 (cos θ)) y x) (* (sin θ) z))       ,(+ (cos θ) (* (- 1 (cos θ)) y y)) ,(- (* (- 1 (cos θ)) y z) (* (sin θ) x)) 0.0))
    (setf (row matrix 2) `(,(- (* (- 1 (cos θ)) z x) (* (sin θ) y)) ,(+ (* (- 1 (cos θ)) z y) (* (sin θ) x))       ,(+ (cos θ) (* (- 1 (cos θ)) z z)) 0.0))
    (setf (row matrix 3) `(                                     0.0                                      0.0                                      0.0 1.0))
    matrix))

(defmethod move-xyz ((dx number) (dy number) (dz number))
  (let ((matrix (make-instance '<matrix> :dimensions '(4 4))))
    (setf (row matrix 0) `(1.0 0.0 0.0 0.0))
    (setf (row matrix 1) `(0.0 1.0 0.0 0.0))
    (setf (row matrix 2) `(0.0 0.0 1.0 0.0))
    (setf (row matrix 3) `(,dx ,dy ,dz 1.0))
    matrix))

(defmethod move-v ((v cons))
  (move-xyz (first v) (second v) (third v)))

(defmethod rotate-around ((point-1 cons) (point-2 cons) θ)
  (let ((rotate-p1-p2 (rotate-v θ (normalize (mapcar #'- point-2 point-1))))
        (move-p1-p0   (move-v (mapcar #'- point-1)))
        (move-p0-p1   (move-v point-1)))
    (multiply
     (multiply move-p1-p0 rotate-p1-p2)
     move-p0-p1)))

(defmethod transform ((point cons) (matrix <matrix>))
  (let ((p (make-instance '<matrix> :dimensions '(1 4))))
    (setf (row p 0) (list (first point) (second point) (third point) 1.0))
    (nreverse (cdr (nreverse (row (multiply p matrix) 0))))))


