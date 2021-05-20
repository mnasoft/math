;;;; /src/core/generic.lisp

(in-package :math/core)

(export 'distance)

(defgeneric distance (x1 x2)
  (:documentation "@b(Описание:) обобщенная функция @b(distance)
возвращает расстояние между x1 и x2. Как корень квадратный из 
сумм квадратов расстояний по каждому направлению."))

(export 'distance-relative)

(defgeneric distance-relative (x xi dx)
  (:documentation "@b(Описание:) обобщенная функция @b(distance-relative)
возвращает относительную длину между x и xi, длина приведения dx.
Корень квадратный из сумм квадратов расстояний по каждому направлению
отнесенному к длине приведения."))

(export 'summ-distance)

(defgeneric summ-distance (x1 x2)
  (:documentation "@b(Описание:) обобщенная функция @b(summ-distance) 
возвращает сумму расстояний по каждому направлению."))
