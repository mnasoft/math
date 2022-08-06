;;;; tests/matrix.lisp

(in-package #:math/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite list-matr-tests
  :description "Мастер-набор всех тестов системы math/list-matr."
  :in all)

(in-suite list-matr-tests)

(def-fixture fix-2dlist-m1-3x5 ()
  (let ((m1-3x5 '((14 55 68 16 20)
		  (41 20  4 57  2)
		  (85 54 96  3 91)))
	(m1-3x5+row-5 '((14 55 68 16 20)
			(41 20  4 57  2)
			(85 54 96  3 91)
			(11 12 13 14 15)))
	(row-5+m1-3x5 '((11 12 13 14 15)
			(14 55 68 16 20)
			(41 20  4 57  2)
			(85 54 96  3 91)))
	(m1-3x5+col-3 '((14 55 68 16 20 11)
			(41 20  4 57  2 12)
			(85 54 96  3 91 13)))
	(col-3+m1-3x5 '((11 14 55 68 16 20)
			(12 41 20  4 57  2)
			(13 85 54 96  3 91)))
	(m1-5x3 '((14 41 85)
		  (55 20 54)
		  (68  4 96)
		  (16 57 3)
		  (20  2 91)))
	(row-5     '(11 12 13 14 15))
	(col-3     '(11 12 13 ))
	(m1-3x5-nil '((14  55  68 nil  20)
		      (nil 20   4  57   2)
		      (85  nil 96   3 nil)))
	(m1-5x3-nil '((14 nil 85)
		      (55  20 nil)
		      (68   4  96)
		      (nil 57   3)
		      (20   2 nil))))
    (declare (ignore m1-3x5 m1-3x5+row-5
		     row-5+m1-3x5 m1-3x5+col-3 col-3+m1-3x5
		     m1-5x3 row-5 col-3 m1-3x5-nil m1-5x3-nil))
    (&body)))

(def-test list-matr-rows-cols-dimensions-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (= (math/matr:cols m1-3x5) 5))
    (is-true (= (math/matr:rows m1-3x5) 3))
    (is-true (equal (math/matr:dimensions m1-3x5) '(3 5)))))

(def-test list-matr-row-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:row 0 m1-3x5) (first  m1-3x5)))
    (is-true (equal (math/matr:row 1 m1-3x5) (second m1-3x5)))
    (is-true (equal (math/matr:row 2 m1-3x5) (third  m1-3x5)))))

(def-test list-matr-col-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:col 0 m1-3x5) '(14 41 85)))
    (is-true (equal (math/matr:col 1 m1-3x5) '(55 20 54)))
    (is-true (equal (math/matr:col 2 m1-3x5) '(68  4 96)))
    (is-true (equal (math/matr:col 3 m1-3x5) '(16 57  3)))
    (is-true (equal (math/matr:col 4 m1-3x5) '(20  2 91)))))

(def-test unite-rows-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (= (length (math/matr:unite-rows m1-3x5))     15))
    (is-true (= (length (math/matr:unite-rows m1-3x5-nil)) 15))
    (is-true (equal (math/matr:unite-rows m1-3x5)
		    '(14  55  68  16 20  41 20 4 57 2 85  54 96 3  91)))
    (is-true (equal (math/matr:unite-rows m1-3x5-nil)
		    '(14  55  68 nil 20 nil 20 4 57 2 85 nil 96 3 nil)))))

(def-test average-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (= (math/matr:average-value m1-3x5) 626/15))))

(def-test average-not-nil-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (= (math/matr:average-not-nil-value m1-3x5)     626/15))
    (is-true (= (math/matr:average-not-nil-value m1-3x5-nil) 424/11))))

(def-test average-row-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:average-row-value m1-3x5) '(173/5 124/5 329/5)))
    ))

(def-test average-row-not-nil-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:average-row-not-nil-value m1-3x5)     '(173/5 124/5 329/5)))
    (is-true (equal (math/matr:average-row-not-nil-value m1-3x5-nil) '(157/4  83/4 184/3)))
    ))

(def-test average-col-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:average-col-value m1-3x5)  '(140/3 43 56 76/3 113/3)))))

(def-test average-col-not-nil-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:average-col-not-nil-value m1-3x5)     '(140/3 43 56 76/3 113/3)))
    (is-true (equal (math/matr:average-col-not-nil-value m1-3x5-nil) '(99/2 75/2 56 30 11)))
    ))

(def-test max-row-not-nil-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:max-row-not-nil-value  m1-3x5)    '(68 57 96)))
    (is-true (equal (math/matr:max-row-not-nil-value m1-3x5-nil) '(68 57 96)))))

(def-test max-col-not-nil-value-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:max-col-not-nil-value  m1-3x5)    '(85 55 96 57 91)))
    (is-true (equal (math/matr:max-col-not-nil-value m1-3x5-nil) '(85 55 96 57 20)))))

(def-test transpose-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:transpose m1-3x5) m1-5x3))
    (is-true (equal (math/matr:transpose m1-5x3) m1-3x5))
    (is-true (equal (math/matr:transpose m1-3x5-nil) m1-5x3-nil))
    (is-true (equal (math/matr:transpose m1-5x3-nil) m1-3x5-nil))))

(def-test detach-last-col-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:detach-last-col m1-3x5+col-3) m1-3x5))
    ))

(def-test get-last-col-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:get-last-col m1-3x5+col-3) col-3))))

(def-test prepend-row-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:prepend-row row-5 m1-3x5) row-5+m1-3x5))
    ))

(def-test prepend-col-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal (math/matr:prepend-col col-3 m1-3x5) col-3+m1-3x5))
    ))

(def-test append-row-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
        (is-true (equal (math/matr:append-row row-5 m1-3x5) m1-3x5+row-5))))

(def-test append-col-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
        (is-true (equal (math/matr:append-col col-3 m1-3x5) m1-3x5+col-3))))

(def-test make-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (equal
	      (math/matr:make
	       3 5
	       (math/matr:unite-rows m1-3x5))
	      m1-3x5))
    (is-true (equal (math/matr:make 2 3 '(1 2 3))  
		    '((1 2 3)
		      (nil nil nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test lv-print-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
    (is-true (string= (math/matr:lv-print col-3 :stream nil)
	     "  11.0   12.0   13.0
"))))


(def-test lm-print-test ()
  "Проверка размеров матрицы."
  (with-fixture fix-2dlist-m1-3x5 ()
        (is-true (string= (math/matr:lm-print m1-3x5 :stream nil)
	     "  14.0   55.0   68.0   16.0   20.0
  41.0   20.0    4.0   57.0    2.0
  85.0   54.0   96.0    3.0   91.0
"))))
