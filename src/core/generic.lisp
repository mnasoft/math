(in-package :math/core)

(export 'distance)

(defgeneric distance (x1 x2)
  (:documentation "Возвращает расстояние между x1 и x2"))

(export 'distance-relative)
(defgeneric distance-relative (x xi dx)
  (:documentation "Возвращает относительную длину между x и xi, длина приведения dx"))

(export 'summ-distance)
(defgeneric summ-distance (x1 x2)(:documentation "COOOL"))
