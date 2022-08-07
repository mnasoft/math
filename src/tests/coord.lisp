;;;; tests/array.lisp

(in-package #:math/tests)

(def-suite coord
  :description "Мастер-набор всех тестов проекта math/matr для матрицы
  типа coord."
  :in all)

(in-suite coord)

(def-test coord-dtr-rtd ()
  (loop :for (degree radian)
          :in `((0 0)
                (15 ,(* pi 1/12))
                (30 ,(* pi 2/12))
                (45 ,(* pi 3/12))
                (60 ,(* pi 4/12))
                (75 ,(* pi 5/12))
                (90 ,(* pi 6/12)))
        :do
           (is-true (math/core:semi-equal (math/coord:dtr degree) radian))
           (is-true (math/core:semi-equal (math/coord:rtd radian) degree))))

(def-test coord-polar-cartesian ()
  (loop :for (polar cartesian)
          :in '(((10d0 0.0d0)                (10.0d0                    0.0d0))
                ((10d0 0.5235987755982988d0) ( 8.660254037844387d0      5.0d0))
                ((10d0 1.0471975511965976d0) ( 5.0d0                    8.660254037844386d0))
                ((10d0 1.5707963267948966d0) ( 6.123233995736766d-16   10.0d0))
                ((10d0 2.0943951023931953d0) (-5.0d0                    8.660254037844387d0))
                ((10d0 2.6179938779914944d0) (-8.660254037844387d0      5.0d0))
                ((10d0 3.141592653589793d0)  (-10.0d0                   0.0d0)))
        :do
           (is-true (math/core:semi-equal (math/coord:polar->cartesian polar) cartesian))
           (is-true (math/core:semi-equal (math/coord:cartesian->polar cartesian) polar))))

(def-test coord-spherical-cartesian ()
  (loop :for cartesian
          :in '(( 10d0  10.0d0  10.0d0)
                (-10d0  10.0d0  10.0d0)
                ( 10d0 -10.0d0  10.0d0)
                ( 10d0  10.0d0 -10.0d0)) 
        :do
           (is-true
            (math/core:semi-equal
             (math/coord:spherical->cartesian
              (math/coord:cartesian->spherical cartesian))
             cartesian)))
  (is-true
   (math/core:semi-equal
    (math/coord:spherical->cartesian `(100.0d0 0.0d0 ,(* pi 1/2)))
    '(100.0d0 0.0d0 0.0d0)))
  (is-true
   (math/core:semi-equal
    (math/coord:spherical->cartesian `(100.0d0 ,(* pi 1/3) ,(* pi 1/3)))
    '(43.301270189221945d0 75.0d0 50.0d0))))

