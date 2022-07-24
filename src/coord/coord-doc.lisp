;;;; ./src/coord/coord-doc.lisp

(in-package :math/coord)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  #'MATH/COORD:POLAR->CARTESIAN 'function
  "@b(Описание:) функция @b(polar->cartesian) переводит полярные координаты в декартовы.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (polar->cartesian (list 10.0 (dtr 45))) => (7.0710678118654755d0 7.071067811865475d0)
@end(code)
")

(make-doc
  #'MATH/COORD:DTR 'function
  "@b(Описание:) функция @b(dtr) переводит значение, заданное в градусах, в радианы.

 @b(Пример использования:)

@begin[lang=lisp](code)
 (dtr (rtd 1/2)) => 0.5d0
@end(code)
")

(make-doc
  #'MATH/COORD:CARTESIAN->SPHERICAL 'function
  "@b(Описание:) функция @b(cartesian->spherical) выполняет преобразование координат 
из декартовых в сферические.

 @b(Переменые:)
@begin(list)
 @item(x-y-z - список, содержащий соответствующие координаты.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->spherical '(61.237243569579455d0 35.35533905932737d0 70.71067811865476d0))
 =>(100.0d0 0.5235987755982988d0 0.7853981633974483d0)
@end(code)
")

(make-doc
  #'MATH/COORD:CARTESIAN->POLAR 'function
  "@b(Описание:) функция @b(cartesian->polar) переводит декартовы координаты в полярные.

 @b(Переменые:)
@begin(list)
 @item(radius-angle - список, состоящий из двух элементов: радиус-вектора 
и угла, заданного в радианах.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (cartesian->polar (list 10.0 10)) (14.142136 0.7853982)
@end(code)
")

(make-doc
  #'MATH/COORD:SPHERICAL->CARTESIAN 'function
  "@b(Описание:) функция @b(spherical->cartesian) выполняет преобразование координат 
из сферических в декартовы.

 @b(Переменые:)
@begin(list)
 @item(r - расстояние от начала координат до заданной точки;)
 @item(φ - азимутальный угол (в плоскости X0Y);)
 @item(θ - зенитный угол.)
@end(list)

@b(Пример использования:)
@begin[lang=lisp](code)
 (spherical->cartesian 100 (dtr 30) (dtr 45))
 => (61.237243569579455d0 35.35533905932737d0 70.71067811865476d0)
@end(code)
")

(make-doc
  #'MATH/COORD:RTD 'function
  "@b(Описание:) функция @b(rtd) переводит значение, 
заданное в радианах, в градусы.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (rtd (dtr 45)) => 45.0d0
@end(code)
")
