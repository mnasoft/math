;;;; /src/quaternion/quaternion.lisp

(ql:quickload :3d-quaternions)

(defpackage :math/quaternion
  (:use #:cl
        #:org.shirakumo.flare.quaternion
        )
  (:export v q
           )
  (:export norma                        ; qlength
           normalize-q                  ; qunit nqunit
           conjugate-q
           multiply
           inverse-q
           divide
           slerp-q
           vector-to-quaternion-1
           add
           subtract
           quaternion-to-axis-angle
           negative
           q-equal
           not_equal
           multiply-scalar
           less
           less_equal 
           )
  (:export q-log
           q-exp
           q-power
           )

  (:documentation "quaternion"))

(in-package :math/quaternion)


(defclass v ()
  ((x :initarg :x :accessor v-x :type long-float)
   (y :initarg :y :accessor v-y :type long-float)
   (z :initarg :z :accessor v-z :type long-float)
   ))

(defun v (&rest rest)
  (let ((lst (mapcar
              #'(lambda (el) (coerce el 'long-float))
              rest)))
  (make-instance 'v :x (nth 0 lst) :y (nth 1 lst) :z (nth 2 lst))))

(defmethod print-object ((v v) stream)
  "Пример использования:
 (let ((v1 (make-instance 'v :x 1.0l0 :y 2.0 :z 3.0)))
   (print v1))
"
  (print-unreadable-object (v stream :type t :identity nil)
    (format stream "~A ~A ~A" (v-x v) (v-y v) (v-z v))))


(defclass q ()
  ((x :initarg :x :initform 0.0l0 :accessor q-x :type double-float)
   (y :initarg :y :initform 0.0l0 :accessor q-y :type double-float)
   (z :initarg :z :initform 0.0l0 :accessor q-z :type double-float)
   (w :initarg :w :initform 1.0l0 :accessor q-w :type double-float)))

(defun q (&rest rest)
  (let ((q (make-instance 'q)))
    (when (first  rest) (setf (q-x q) (coerce (first  rest) 'long-float)))
    (when (second rest) (setf (q-y q) (coerce (second rest) 'long-float)))
    (when (third  rest) (setf (q-z q) (coerce (third  rest) 'long-float)))
    (when (fourth rest) (setf (q-w q) (coerce (fourth rest) 'long-float)))
    q))

(defmethod print-object ((q q) stream)
  "
Пример использования:
  (make-instance 'q :x 1.0 :y 2.0 :z 3.0 :w 4.0) => #<Q 1.0 2.0 3.0 4.0>
"
  (print-unreadable-object (q stream :type t :identity nil)
    (format stream "~A ~A ~A ~A"
            (coerce (q-x q) 'single-float)
            (coerce (q-y q) 'single-float)
            (coerce (q-z q) 'single-float)
            (coerce (q-w q) 'single-float))))

(defmethod norma ((v v))
  (sqrt (+ (* (v-x v) (v-x v))
           (* (v-y v) (v-y v))
           (* (v-z v) (v-z v)))))

(defmethod norma ((q q))
  (sqrt (+ (* (q-x q) (q-x q))
           (* (q-y q) (q-y q))
           (* (q-z q) (q-z q))
           (* (q-w q) (q-w q)))))

(defmethod normalize-q ((q q))
  "Нормализация: Приводит кватернион к единичной длине (норма), что
важно для операций вращения.

 Пример использования:
  (normalize-q (q 0.75 0.75 .075 .23)) => #<Q 0.689402 0.689402 0.06894021 0.21141662>
"
  (let ((norm (sqrt (+ (* (q-x q) (q-x q))
                       (* (q-y q) (q-y q))
                       (* (q-z q) (q-z q))
                       (* (q-w q) (q-w q))))))
    (setf (q-x q) (/ (q-x q) norm)
          (q-y q) (/ (q-y q) norm)
          (q-z q) (/ (q-z q) norm)
          (q-w q) (/ (q-w q) norm))
    q))

(defmethod conjugate-q ((q q))
  "Сопряжение: Изменяет знак мнимой части кватерниона. Используется для
инвертирования вращения.

 Пример использования:
  (conjugate-q (q 0.75 0.75 .075 .23)) => #<Q -0.75 -0.75 -0.075 0.23>
"
  (make-instance 'q :x (- (q-x q))
                    :y (- (q-y q))
                    :z (- (q-z q))
                    :w (q-w q)))

(defmethod multiply ((q1 q) (q2 q))
    "Умножение: Композиция двух кватернионов, представляющих
 последовательные вращения."
  (let* ((w1 (q-w q1)) (x1 (q-x q1)) (y1 (q-y q1)) (z1 (q-z q1))
         (w2 (q-w q2)) (x2 (q-x q2)) (y2 (q-y q2)) (z2 (q-z q2))
         (w (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)))
         (x (+ (* w1 x2) (* x1 w2) (* y1 z2) (- (* z1 y2))))
         (y (+ (* w1 y2) (- (* x1 z2)) (* y1 w2) (* z1 x2)))
         (z (+ (* w1 z2) (* x1 y2) (- (* y1 x2)) (* z1 w2))))
    (make-instance 'q :x x :y y :z z :w w)))

(defmethod inverse-q (q)
  "Инверсный кватернион может быть определен как сопряженный
 кватернион, нормированный на квадрат модуля исходного кватерниона.

Вот шаги для вычисления инверсного кватерниона q−1q^{-1} для
кватерниона qq: Сопряжение кватерниона: Измените знак мнимых
компонентов xx, yy, zz. Норма кватерниона: Вычислите норму (или длину)
кватерниона. Модуль кватерниона: Возведите норму в квадрат. Иницируем
инверсный кватернион: Разделите сопряженный кватернион на квадрат
модуля.
"
  (let* ((q-conj (conjugate-q q))
         (norm2 (expt (norma q) 2)))
    (make-instance 'q
                   :x (/ (q-x q-conj) norm2)
                   :y (/ (q-y q-conj) norm2)
                   :z (/ (q-z q-conj) norm2)
                   :w (/ (q-w q-conj) norm2))))

(defmethod divide ((q1 q) (q2 q))
  "Деление. Деление одного кватерниона на другой определяется как
умножение на обратный (инверсный) кватернион.
"
  (multiply q1 (inverse-q q2)))

(defmethod slerp-q ((q1 q) (q2 q) (param number))
  "Интерполяция"
  (let ((dot (min 1 (max -1 (+ (* (q-x q1) (q-x q2)) 
                              (* (q-y q1) (q-y q2)) 
                              (* (q-z q1) (q-z q2)) 
                              (* (q-w q1) (q-w q2)))))))
    (if (>= dot 1.0)
        q1
        (let* ((theta-0 (acos dot))
               (sin-theta-0 (sin theta-0))
               (theta (* theta-0 param))
               (sin-theta (sin theta))
               (s1 (/ (sin (- theta-0 theta)) sin-theta-0))
               (s2 (/ sin-theta sin-theta-0)))
          (make-instance 'q
                         :x (+ (* s1 (q-x q1)) (* s2 (q-x q2)))
                         :y (+ (* s1 (q-y q1)) (* s2 (q-y q2)))
                         :z (+ (* s1 (q-z q1)) (* s2 (q-z q2)))
                         :w (+ (* s1 (q-w q1)) (* s2 (q-w q2))))))))

(defmethod vector-to-quaternion-1 ((v v) (angle number))
  "
Пример использования
 (vector-to-quaternion 1.0 0.0 0.0 (/ pi 2))
 
"
  (let* ((half-angle (/ angle 2))
         (sin-half-angle (sin half-angle))
         (qx (* (v-x v) sin-half-angle))
         (qy (* (v-y v) sin-half-angle))
         (qz (* (v-z v) sin-half-angle))
         (qw (cos half-angle)))
    (make-instance 'q :x qx :y qy :z qz :w qw)))

(defmethod add ((q1 q) (q2 q))
  "Сложение"
  (make-instance 'q
                 :x (+ (q-x q1) (q-x q2))
                 :y (+ (q-y q1) (q-y q2))
                 :z (+ (q-z q1) (q-z q2))
                 :w (+ (q-w q1) (q-w q2))))

(defmethod subtract ((q1 q) (q2 q))
  "Вычитание"
  (make-instance 'q
                 :x (- (q-x q1) (q-x q2))
                 :y (- (q-y q1) (q-y q2))
                 :z (- (q-z q1) (q-z q2))
                 :w (- (q-w q1) (q-w q2))))

(defmethod quaternion-to-axis-angle ((q q))
  "Преобразует кватернион Q в ось вращения и угол поворота."
  (let* ((x (q-x q))
         (y (q-y q))
         (z (q-z q))
         (w (q-w q))
         ;; Нормализуем кватернион
         (norm (sqrt (+ (* x x) (* y y) (* z z) (* w w))))
         (x (/ x norm))
         (y (/ y norm))
         (z (/ z norm))
         (w (/ w norm))
         ;; Вычисляем угол поворота
         (angle (* 2 (acos w)))
         ;; Вычисляем синус половинного угла
         (s (sqrt (max 0.0 (- 1.0 (* w w))))))
    (if (< s 0.001)
        ;; Если s близок к нулю, ось не имеет значения
        (values 1.0 0.0 0.0 0.0)
        ;; В противном случае вычисляем ось вращения
        (values (/ x s) (/ y s) (/ z s) angle))))

(defmethod negative ((q q))
  "Меняет знак всех компонентов."
  (make-instance 'q
                 :x (- (q-x q))
                 :y (- (q-y q))
                 :z (- (q-z q))
                 :w (- (q-w q))))

(defmethod q-equal ((q1 q) (q2 q))
  "Сравнение компонентов на равенство."
  (and (= (q-x q1) (q-x q2))
       (= (q-y q1) (q-y q2))
       (= (q-z q1) (q-z q2))
       (= (q-w q1) (q-w q2))))

(defmethod not_equal ((q1 q) (q2 q))
    "Сравнение компонентов на неравенство."
  (not (equal q1 q2)))


(defmethod q-log ((q q))
  "Логарифм кватерниона определяется на основе его модуля и направления.
 Вычисление логарифма требует разделения кватерниона на скалярную и
векторную части.
"
  (let* ((q-norm (norma q))
         (v (v (q-x q) (q-y q) (q-z q)))
         (v-norm (norma v))
         (theta (acos (/ (q-w q) q-norm)))
         (theta-v (if (zerop v-norm)
                     (v 0 0 0)
                     (v (* theta (/ (q-x q) v-norm))
                        (* theta (/ (q-y q) v-norm))
                        (* theta (/ (q-z q) v-norm))))))
    (q (v-x theta-v) (v-y theta-v) (v-z theta-v) (log q-norm))))

(defmethod q-exp ((q q))
  "Экспонента кватерниона тоже может быть определена."
  (let* ((v (v (q-x q) (q-y q) (q-z q)))
         (v-norm (norma v))
         (exp-w (exp (q-w q)))
         (coeff (if (zerop v-norm)
                    0
                    (/ (sin v-norm) v-norm))))
    (make-instance 'q
                   :x (* exp-w coeff (q-x q))
                   :y (* exp-w coeff (q-y q))
                   :z (* exp-w coeff (q-z q))
                   :w (* exp-w (cos v-norm)))))


(defmethod q-power ((q q) n)
  "Возведение кватерниона в степень можно выполнить через логарифм и
 экспоненту.

qn=exp⁡(n⋅log⁡(q))q^n = \exp(n \cdot \log(q))"
  (q-exp (multiply-scalar (q-log q) n)))

(defmethod multiply-scalar ((q q) (scalar number))
  (make-instance 'q
                 :x (* (q-x q) scalar)
                 :y (* (q-y q) scalar)
                 :z (* (q-z q) scalar)
                 :w (* (q-w q) scalar)))

(defmethod less ((q1 q) (q2 q))
  "Для кватернионов операция сравнения < не определена, так как
 кватернионы не являются упорядоченным полем. Тем не менее, если очень
 нужно, можно сравнивать их нормы."
  (< (absolute q1) (absolute q2)))

(defmethod less_equal ((q1 q) (q2 q))
  "Для кватернионов операция сравнения <= не определена, так как
 кватернионы не являются упорядоченным полем. Тем не менее, если очень
 нужно, можно сравнивать их нормы."
  (<= (absolute q1) (absolute q2)))

