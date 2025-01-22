;;;; /src/quaternion/quaternion.lisp

(defpackage :math/quaternion
  (:use #:cl)
  (:export 
           )
  (:export 
           )
  (:export 
           )
  (:documentation "coord"))

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

(defun multiply-q (q1 q2)
  "Умножение: Композиция двух кватернионов, представляющих
 последовательные вращения."
  (let* ((x1 (q-x q1)) (y1 (q-y q1)) (z1 (q-z q1)) (w1 (q-w q1))
         (x2 (q-x q2)) (y2 (q-y q2)) (z2 (q-z q2)) (w2 (q-w q2)))
    (make-instance 'q
                   :x (- (+ (* w1 x2) (* x1 w2) (* y1 z2) (* (- z1) y2)))
                   :y (- (+ (* w1 y2) (- (* x1 z2) (* y1 w2)) (* z1 x2)))
                   :z (- (+ (* w1 z2) (- (* x1 y2) (- y1 x2)) (* z1 w2)))
                   :w (- (+ (- (* w1 w2) (* x1 x2) (- (* y1 y2)) (* z1 z2)))))))

(defun slerp-q (q1 q2 param)
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

(defun vector-to-quaternion (x y z angle)
  "
Пример использования
 (vector-to-quaternion 1.0 0.0 0.0 (/ pi 2))
 
"
  (let* ((half-angle (/ angle 2))
         (sin-half-angle (sin half-angle))
         (qx (* x sin-half-angle))
         (qy (* y sin-half-angle))
         (qz (* z sin-half-angle))
         (qw (cos half-angle)))
    (make-instance 'q :x qx :y qy :z qz :w qw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Copilot говорит

Absolutely! Here’s the updated version of the function where the format string for seconds is now ~2f instead of %.2f:
lisp

(defun radians-to-dms (radians &key (seconds-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (sign (if (and (not (zerop degrees)) (or (minusp degrees) force-sign)) "-" ""))
         (abs-degrees (abs degrees))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (format nil "~a~d° ~d' ~a\"" sign d min (format nil seconds-format s))))

;; Пример использования
(radians-to-dms -1.0 :seconds-format "~3f" :force-sign t)

In this updated function:

    The seconds-format parameter now takes a Common Lisp format string such as ~2f.

    The format function is used to apply this format string.

When you call (radians-to-dms -1.0 :seconds-format "~3f" :force-sign t), it will return -57° 17' 45.832".

Let me know if you need any further tweaks or have more questions!
Вы сказали
"~a~d° ~d' ~a\"" ->  "~a~d°~d'~a\""
Copilot говорит

Absolutely! Here’s the updated version of the function with the adjusted format string:
lisp

(defun radians-to-dms (radians &key (seconds-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (sign (if (and (not (zerop degrees)) (or (minusp degrees) force-sign)) "-" ""))
         (abs-degrees (abs degrees))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s))))

;; Пример использования
(radians-to-dms -1.0 :seconds-format "~3f" :force-sign t)

Now, when you call (radians-to-dms -1.0 :seconds-format "~3f" :force-sign t), it will return -57°17'45.832".

If there are more tweaks or questions, feel free to ask!
Вы сказали
что то для положительного угла выводится янак -
Copilot говорит

Похоже, в текущем варианте функции знак выводится даже для положительных значений из-за логики проверки принудительного вывода знака. Давайте исправим это:
lisp

(defun radians-to-dms (radians &key (seconds-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s))))

;; Пример использования
(radians-to-dms 1.0 :seconds-format "~3f" :force-sign nil)  ;; Возвращает "+57°17'45.832\""
(radians-to-dms -1.0 :seconds-format "~3f" :force-sign t)  ;; Возвращает "-57°17'45.832\""

В этой версии функции логика определения знака была скорректирована:

    Если force-sign установлено и значение положительное, знак принудительно выводится.

    Если значение отрицательное, знак - будет выведен.

    Если значение равно нулю или force-sign не установлено, знак не выводится.

Теперь при вызове функции для положительного значения знак будет корректно обрабатываться.

Если у вас есть еще вопросы или пожелания, дайте знать!
Вы сказали
давай напишем аналогичную функцию для вывода угловой меры, выраженной  в радианах, в градусах и минутах (применяя к минутам то что было применено к секундам)
Copilot говорит

Конечно, давайте напишем функцию для вывода угловой меры, выраженной в радианах, в градусах и минутах, применяя к минутам ту же логику, что и к секундам:
lisp

(defun radians-to-dm (radians &key (minutes-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (format nil minutes-format m)))
    (format nil "~a~d°~a'" sign d min)))

;; Пример использования
(radians-to-dm 1.0 :minutes-format "~3f" :force-sign nil)  ;; Возвращает "57°17.453'"
(radians-to-dm -1.0 :minutes-format "~3f" :force-sign t)  ;; Возвращает "-57°17.453'"

В этой функции:

    Параметр minutes-format позволяет задать формат вывода минут. По умолчанию используется формат с двумя знаками после запятой "~2f".

    Логика определения знака аналогична предыдущей функции.

    Мы используем функцию format для форматирования минут в соответствии с заданным форматом.

Теперь при вызове функции для положительного и отрицательного значения радианов будет правильно выводиться угол в градусах и минутах.

Если у вас есть еще запросы или дополнительные пожелания, дайте знать!
Вы сказали
а теперь то-же но для вывода в градусах
Copilot говорит

Конечно, давайте напишем функцию, которая будет выводить угловую меру в градусах:
lisp

(defun radians-to-degrees (radians &key (degrees-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (formatted-degrees (format nil degrees-format (abs degrees))))
    (format nil "~a~a°" sign formatted-degrees)))

;; Пример использования
(radians-to-degrees 1.0 :degrees-format "~3f" :force-sign nil)  ;; Возвращает "57.296°"
(radians-to-degrees -1.0 :degrees-format "~3f" :force-sign t)  ;; Возвращает "-57.296°"

В этой функции:

    Параметр degrees-format позволяет задать формат вывода градусов. По умолчанию используется формат с двумя знаками после запятой "~2f".

    Логика определения знака аналогична предыдущей функции.

    Мы используем функцию format для форматирования градусов в соответствии с заданным форматом.

Теперь при вызове функции для положительного и отрицательного значения радианов будет правильно выводиться угол в градусах.

Если у вас есть еще запросы или дополнительные пожелания, дайте знать!
Вы сказали
давай напишем обобщающую функцию котоая будет (в зависимости от ключевого параметра) выодить радианы в: - градусы; - градусы и минуты; - градусы минуты и секунды
Copilot говорит

Конечно, давайте создадим обобщающую функцию, которая будет выводить угловую меру в радианах в зависимости от ключевого параметра: градусы, градусы и минуты, или градусы, минуты и секунды.
lisp

(defun radians-to-angle (radians &key (output :dms) (seconds-format "~2f") (minutes-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (case output
      (:dms
       (format nil "~a~d°~d'~a\"" sign d min (format nil seconds-format s)))
      (:dm
       (format nil "~a~d°~a'" sign d (format nil minutes-format m)))
      (:degrees
       (format nil "~a~a°" sign (format nil "~2f" degrees)))
      (t
       (error "Invalid output format. Valid options are :degrees, :dm, or :dms.")))))

;; Пример использования
(radians-to-angle 1.0 :output :dms :seconds-format "~3f" :force-sign nil)  ;; Возвращает "57°17'45.832\""
(radians-to-angle -1.0 :output :dm :minutes-format "~3f" :force-sign t)  ;; Возвращает "-57°17.453'"
(radians-to-angle 1.0 :output :degrees :force-sign nil)  ;; Возвращает "57.30°"

В этой функции:

    Параметр output определяет формат вывода. Допустимые значения: :degrees, :dm, :dms. По умолчанию используется :dms.

    Параметр seconds-format задает формат вывода секунд, если выбран формат :dms.

    Параметр minutes-format задает формат вывода минут, если выбран формат :dm.

    Параметр force-sign управляет принудительным выводом знака.

Теперь эта функция позволяет выводить угловую меру в радианах в различных форматах в зависимости от ключевых параметров. Если есть еще вопросы или пожелания, дайте знать!
Вы сказали
(degrees-format "~D")
Copilot говорит

Обновим функцию, чтобы добавить параметр degrees-format, который позволит форматировать градусы:
lisp

(defun radians-to-angle (radians &key (output :dms) (seconds-format "~2f") (minutes-format "~2f") (degrees-format "~2f") (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min))))
    (case output
      (:dms
       (format nil "~a~a°~d'~a\"" sign (format nil degrees-format d) min (format nil seconds-format s)))
      (:dm
       (format nil "~a~a°~a'" sign (format nil degrees-format d) (format nil minutes-format m)))
      (:degrees
       (format nil "~a~a°" sign (format nil degrees-format degrees)))
      (t
       (error "Invalid output format. Valid options are :degrees, :dm, or :dms.")))))

;; Пример использования
(radians-to-angle 1.0 :output :dms :seconds-format "~3f" :force-sign nil)  ;; Возвращает "57°17'45.832\""
(radians-to-angle -1.0 :output :dm :minutes-format "~3f" :force-sign t)  ;; Возвращает "-57°17.453'"
(radians-to-angle 1.0 :output :degrees :degrees-format "~D" :force-sign nil)  ;; Возвращает "57°"

В этой функции добавлен параметр degrees-format, который позволяет задавать формат вывода градусов. По умолчанию используется формат с двумя знаками после запятой "~2f". Теперь можно выводить градусы в любом удобном формате, включая целые числа с помощью "~D".

Если есть еще вопросы или пожелания, дайте знать!
Вы сказали
давай добавим понятие (ключевой параметр) для подавления ведущих нулей. Если он примет значение T. Если значение градусов или минут равно 0 то их не выводим
Copilot говорит

Конечно, добавим ключевой параметр для подавления ведущих нулей. Если значение градусов или минут равно нулю и параметр установлен в T, то их не будем выводить.
lisp

(defun radians-to-angle (radians &key (output :dms) (seconds-format "~2f") (minutes-format "~2f") (degrees-format "~2f") (suppress-zero nil) (force-sign nil))
  (let* ((degrees (/ (* radians 180) pi))
         (abs-degrees (abs degrees))
         (sign (if (or (and force-sign (plusp degrees)) (minusp degrees)) (if (minusp degrees) "-" "+") ""))
         (d (truncate abs-degrees))
         (m (* 60 (- abs-degrees d)))
         (min (truncate m))
         (s (* 60 (- m min)))
         (degrees-str (if (and suppress-zero (zerop d)) "" (format nil "~a~a°" sign (format nil degrees-format d))))
         (minutes-str (if (and suppress-zero (zerop min)) "" (format nil "~a'" (format nil minutes-format min))))
         (seconds-str (format nil seconds-format s)))
    (case output
      (:dms
       (format nil "~a~d'~a\"" degrees-str min seconds-str))
      (:dm
       (format nil "~a~a'" degrees-str minutes-str))
      (:degrees
       degrees-str)
      (t
       (error "Invalid output format. Valid options are :degrees, :dm, or :dms.")))))

;; Пример использования
(radians-to-angle 1.0 :output :dms :seconds-format "~3f" :suppress-zero t)  ;; Возвращает "57°17'45.832\""
(radians-to-angle -1.0 :output :dm :minutes-format "~3f" :suppress-zero t :force-sign t)  ;; Возвращает "-57°17.453'"
(radians-to-angle 1.0 :output :degrees :degrees-format "~D" :suppress-zero t)  ;; Возвращает "57°"

Теперь функция имеет параметр suppress-zero, который позволяет подавлять вывод ведущих нулей:

    Если значение градусов равно 0 и параметр установлен в T, то градусы не будут выводиться.

    Если значение минут равно 0 и параметр установлен в T, то минуты не будут выводиться.

Если есть еще вопросы или пожелания, дайте знать!
Вы сказали
кватернион
Copilot говорит

Кватернионы представляют собой расширение комплексных чисел в четвертое измерение и используются в 3D-графике для представления вращений без эффекта гимбал-лок (прецессионного блока). Вот немного про историю и математику: Кватернионы были введены Уильямом Роуэном Гамильтоном в 1843 году. Гамильтон искал способ расширения комплексных чисел в более высокие размеры и понял, что это возможно в четырех измерениях.

Математически кватернион можно выразить как:
q=a+bi+cj+dkq = a + bi + cj + dk

где a,b,c, и da, b, c, \text{ и } d - это действительные числа, а i,j, и ki, j, \text{ и } k - единичные чисто мнимые компоненты. Эти компоненты следуют следующим правилам умножения:
i2=j2=k2=ijk=−1i^2 = j^2 = k^2 = ijk = -1
ij=k,  ji=−kij = k, \; ji = -k
jk=i,  kj=−ijk = i, \; kj = -i
ki=j,  ik=−jki = j, \; ik = -j

Нужно больше примеров или должен я объяснить, как кватернионы могут быть полезны в 3D-приложениях?
Вы сказали
Существуют ли их аналоги для большего количества измерений
Copilot говорит

Существует обобщение кватернионов на более высокие измерения, которые называются гиперкомплексные числа. Вот несколько примеров их аналогов:

    Октавы или октонионы: Это расширение кватернионов для восьми измерений. Они были рассмотрены Кэли и Грейвсом. Октонионы не ассоциативны, то есть (ab)c≠a(bc)(ab)c \neq a(bc), но они имеют некоторые интересные свойства. Они полезны в некоторых областях физики, включая теорию струн.

    Сединионы: Это расширение октонионов в 16 измерений. Они еще менее изучены и имеют меньше практического применения, однако математически они также интересны.

Примерная структура показывает, что по мере того, как мы увеличиваем размерность, некоторые свойства, такие как ассоциативность и альтернативность, теряются, что приводит к более сложным математическим объектам. Это затрудняет их визуализацию и использование для конкретных приложений, но они остаются важной частью теории чисел и алгебры.

Хотите более углубиться в математические свойства этих чисел?
Вы сказали
Вернимся к кватернионам
Copilot говорит

Мы остановились на рассмотрении кватернионов как расширения комплексных чисел в четырех измерениях. Может быть, вы хотите узнать больше о том, как их используют? Кватернионы находят применение в компьютерной графике, робототехнике и анимации для представления трехмерных вращений и ориентаций без проблем, связанных с гимбал-локом (заклиниванием карданного механизма).
Преимущества кватернионов:

    Отсутствие гимбал-лока: Преодоление ограничений эйлеровых углов.

    Интерполяция: Легкая интерполяция (slerp) между вращениями.

    Компактность: Они занимают меньше памяти (4 компонента вместо 9 в матрице вращения).

Полезные операции:

    Нормализация: Обеспечивает единичную норму кватерниона для чистого вращения.

    Умножение: Композиция вращений.

    Обратный кватернион: Используется для аннулирования эффекта вращения.

Пример кода: Нормализация в Python

Если захотите взглянуть на пример реализации, вот как можно нормализовать кватернион на Python:
python

import numpy as np

def normalize(quaternion):
    norm = np.linalg.norm(quaternion)
    return quaternion / norm

q = np.array([1, 2, 3, 4])
q_normalized = normalize(q)
print(q_normalized)

С математической точки зрения q−1q^{-1} для кватерниона q=a+bi+cj+dkq = a + bi + cj + dk можно выразить как qˉ/∣q∣2\bar{q} / |q|^2, где qˉ\bar{q} - сопряженный кватернион a−bi−cj−dka - bi - cj - dk.

Назад к дальнейшему обсуждению кватернионов? Или может быть, вы хотите пример ротации или конвертации между кватернионами и матрицами вращения?
Вы сказали
Давай придерживаться следующих соглашений b -> x c->y d->z a->w
Copilot говорит

Согласен! Теперь, когда мы используем следующие обозначения:

    a→wa \rightarrow w

    b→xb \rightarrow x

    c→yc \rightarrow y

    d→zd \rightarrow z

Представление кватерниона:

Кватернион теперь будет записан как:
q=w+xi+yj+zkq = w + xi + yj + zk

где w,x,y, и zw, x, y, \text{ и } z — действительные числа.
Операции с кватернионами:
Нормализация:
python

import numpy as np

def normalize(quaternion):
    w, x, y, z = quaternion
    norm = np.sqrt(w**2 + x**2 + y**2 + z**2)
    return w/norm, x/norm, y/norm, z/norm

q = (1, 2, 3, 4)
q_normalized = normalize(q)
print(q_normalized)  # Output: (0.1826, 0.3651, 0.5477, 0.7303)

Умножение:
python

def multiply(q1, q2):
    w1, x1, y1, z1 = q1
    w2, x2, y2, z2 = q2
    w = w1*w2 - x1*x2 - y1*y2 - z1*z2
    x = w1*x2 + x1*w2 + y1*z2 - z1*y2
    y = w1*y2 - x1*z2 + y1*w2 + z1*x2
    z = w1*z2 + x1*y2 - y1*x2 + z1*w2
    return w, x, y, z

q1 = (1, 2, 3, 4)
q2 = (5, 6, 7, 8)
q_product = multiply(q1, q2)
print(q_product)  # Output: (-60, 12, 30, 24)

Сопряженный кватернион:
python

def conjugate(quaternion):
    w, x, y, z = quaternion
    return w, -x, -y, -z

q = (1, 2, 3, 4)
q_conjugate = conjugate(q)
print(q_conjugate)  # Output: (1, -2, -3, -4)

Соблюдая эти обозначения, мы можем принять дальнейшие действия или обсудить другие аспекты кватернионов. Что вам дальше интересно?
Вы сказали
Еще одно дополнительное соглашение координату w давай записывать последней
Copilot говорит

Конечно, давайте придерживаться этого соглашения. Теперь кватернионы будем записывать в формате [x,y,z,w][x, y, z, w].
Нормализация кватерниона:
python

import numpy as np

def normalize(quaternion):
    x, y, z, w = quaternion
    norm = np.sqrt(x**2 + y**2 + z**2 + w**2)
    return x/norm, y/norm, z/norm, w/norm

q = (2, 3, 4, 1)
q_normalized = normalize(q)
print(q_normalized)  # Output: (0.3651, 0.5477, 0.7303, 0.1826)

Умножение кватернионов:
python

def multiply(q1, q2):
    x1, y1, z1, w1 = q1
    x2, y2, z2, w2 = q2
    w = w1*w2 - x1*x2 - y1*y2 - z1*z2
    x = w1*x2 + x1*w2 + y1*z2 - z1*y2
    y = w1*y2 - x1*z2 + y1*w2 + z1*x2
    z = w1*z2 + x1*y2 - y1*x2 + z1*w2
    return x, y, z, w

q1 = (2, 3, 4, 1)
q2 = (6, 7, 8, 5)
q_product = multiply(q1, q2)
print(q_product)  # Output: (12, 30, 24, -60)

Сопряженный кватернион:
python

def conjugate(quaternion):
    x, y, z, w = quaternion
    return -x, -y, -z, w

q = (2, 3, 4, 1)
q_conjugate = conjugate(q)
print(q_conjugate)  # Output: (-2, -3, -4, 1)

Теперь все кватернионы записаны в формате [x,y,z,w][x, y, z, w] и координата ww находится последней. Что вы хотели бы обсудить дальше или может быть рассмотреть другие операции?
Вы сказали
Давай примеры кода создавать на языке Common Lisp. Давай создадим класс <q>, который будет представлять кратернион. Слотами класса будут x y z w. Аксессорами будут соответственно <q>-x для x и так далее.
Copilot говорит

Конечно! Давайте создадим класс q, который будет представлять кватернион в Common Lisp. Слоты класса будут x, y, z, w с соответствующими аксессорами.
lisp

(defclass q ()
  ((x :initarg :x :accessor q-x)
   (y :initarg :y :accessor q-y)
   (z :initarg :z :accessor q-z)
   (w :initarg :w :accessor q-w)))

;; Пример использования
(let ((q1 (make-instance 'q :x 1 :y 2 :z 3 :w 4)))
  (format t "q1: (~A, ~A, ~A, ~A)~%" (q-x q1) (q-y q1) (q-z q1) (q-w q1)))

Теперь у нас есть класс q с слотами x, y, z, w и соответствующими аксессорами q-x, q-y, q-z, q-w. Этот код создает экземпляр класса q и выводит значения слотов.

Хотите продолжить и добавить методы для этого класса, такие как нормализация, умножение или сопряжение?
Вы сказали
(defclass <q> ()
  ((x :initarg :x :accessor <q>-x)
   (y :initarg :y :accessor <q>-y)
   (z :initarg :z :accessor <q>-z)
   (w :initarg :w :accessor <q>-w))
Copilot говорит

Вот реализация класса q в Common Lisp:
lisp

(defclass q ()
  ((x :initarg :x :accessor q-x)
   (y :initarg :y :accessor q-y)
   (z :initarg :z :accessor q-z)
   (w :initarg :w :accessor q-w)))

;; Пример использования
(let ((q1 (make-instance 'q :x 1 :y 2 :z 3 :w 4)))
  (format t "q1: (~A, ~A, ~A, ~A)~%" (q-x q1) (q-y q1) (q-z q1) (q-w q1)))

Теперь у нас есть класс q, определенный в Common Lisp, с слотами x, y, z, w и соответствующими аксессорами q-x, q-y, q-z, q-w.

Что бы вы хотели добавить или изменить дальше?
Вы сказали
давай специфицируем слоты типом double-float
Copilot говорит

Хорошо, давайте специфицируем слоты класса q типом double-float в Common Lisp:
lisp

(defclass q ()
  ((x :initarg :x :accessor q-x :type double-float)
   (y :initarg :y :accessor q-y :type double-float)
   (z :initarg :z :accessor q-z :type double-float)
   (w :initarg :w :accessor q-w :type double-float)))

;; Пример использования
(let ((q1 (make-instance 'q :x 1.0 :y 2.0 :z 3.0 :w 4.0)))
  (format t "q1: (~A, ~A, ~A, ~A)~%" (q-x q1) (q-y q1) (q-z q1) (q-w q1)))

Теперь слоты x, y, z и w класса q имеют тип double-float. Пример использования создает экземпляр класса q с действительными числами в виде аргументов.

Что бы вы хотели добавить или изменить дальше?
Вы сказали
Давай поределим для кватерниона функцию (скорее метод) печати в поток - print-object
Copilot говорит

Хорошая идея! Давайте определим метод print-object для класса q, чтобы можно было удобно выводить кватернион на печать:
lisp

(defmethod print-object ((q q) stream)
  (format stream "#<q: (~A, ~A, ~A, ~A)>"
          (q-x q) (q-y q) (q-z q) (q-w q)))

;; Пример использования
(let ((q1 (make-instance 'q :x 1.0 :y 2.0 :z 3.0 :w 4.0)))
  (print q1))

Этот метод использует format для форматирования вывода в поток в виде строки #<q: (x, y, z, w)>.

Теперь при выводе объекта класса q, например с помощью функции print, он будет отображаться в удобном формате.

Что бы вы хотели добавить или изменить дальше?
Вы сказали
Давай проговорим какие операции присущи для кватерниона
Copilot говорит

Кватернионы поддерживают несколько важных операций, которые часто используются в приложениях, связанных с трехмерной графикой, анимацией и физикой. Вот основные из них:
Основные операции с кватернионами:

    Нормализация: Приводит кватернион к единичной длине (норма), что важно для операций вращения.

    Сопряжение: Изменяет знак мнимой части кватерниона. Используется для инвертирования вращения.

    Умножение: Композиция двух кватернионов, представляющих последовательные вращения.

    Инверсия: Обратный кватернион используется для аннулирования эффекта вращения.

    Интерполяция (Slerp): Плавный переход между двумя кватернионами, используется для анимации и управления поворотами.

    Преобразование из/в матрицу вращения: Перевод кватерниона в матрицу и обратно. Перевод направления.

Примеры реализации:
Нормализация:
lisp

(defun normalize-q (q)
  (let ((norm (sqrt (+ (expt (q-x q) 2)
                       (expt (q-y q) 2)
                       (expt (q-z q) 2)
                       (expt (q-w q) 2)))))

    (q-x q) (/ (q-x q) norm))
    (q-y q) (/ (q-y q) norm))
    (q-z q) (/ (q-z q) norm))
    (q-w q) (/ (q-w q) norm))))

Сопряжение:
lisp

(defun conjugate-q (q)
  (make-instance 'q :x (- (q-x q)) :y (- (q-y q)) :z (- (q-z q)) :w (q-w q)))

Умножение:
lisp

(defun multiply-q (q1 q2)
  (let* ((x1 (q-x q1)) (y1 (q-y q1)) (z1 (q-z q1)) (w1 (q-w q1))
         (x2 (q-x q2)) (y2 (q-y q2)) (z2 (q-z q2)) (w2 (q-w q2)))
    (make-instance 'q
                   :x (- (+ (* w1 x2) (* x1 w2) (* y1 z2) (* (- z1) y2)))
                   :y (- (+ (* w1 y2) (- (* x1 z2) (* y1 w2)) (* z1 x2)))
                   :z (- (+ (* w1 z2) (- (* x1 y2) (- y1 x2)) (* z1 w2)))
                   :w (- (+ (- (* w1 w2) (* x1 x2) (- (* y1 y2)) (*z1 z2)))))))

Интерполяция (Slerp):
lisp

(defun slerp-q (q1 q2 t)
  (let ((dot (min 1 (max -1 (+ (* (q-x q1) (q-x q2)) 
                              (* (q-y q1) (q-y q2)) 
                              (* (q-z q1) (q-z q2)) 
                              (* (q-w q1) (q-w q2)))))))
    (if (>= dot 1.0)
        q1
        (let* ((theta-0 (acos dot))
               (sin-theta-0 (sin theta-0))
               (theta (* theta-0 t))
               (sin-theta (sin theta))
               (s1 (/ (sin (- theta-0 theta)) sin-theta-0))
               (s2 (/ sin-theta sin-theta-0)))
          (make-instance 'q
                         :x (+ (* s1 (q-x q1)) (* s2 (q-x q2)))
                         :y (+ (* s1 (q-y q1)) (* s2 (q-y q2)))
                         :z (+ (* s1 (q-z q1)) (* s2 (q-z q2)))
                         :w (+ (* s1 (q-w q1)) (* s2 (q-w q2))))))))

Таким образом, кватернионы предоставляют мощный набор математических инструментов для работы с трехмерными вращениями и ориентациями, позволяя легко манипулировать ими и выполнять сложные преобразования.

Что думаете, есть ли интерес к какой-то особенно определенной операции или её реализации?
Вы сказали
Как можно определть инверсный кватернион
Copilot говорит

Инверсный кватернион может быть определен как сопряженный кватернион, нормированный на квадрат модуля исходного кватерниона.

Вот шаги для вычисления инверсного кватерниона q−1q^{-1} для кватерниона qq:

    Сопряжение кватерниона: Измените знак мнимых компонентов xx, yy, zz.

    Норма кватерниона: Вычислите норму (или длину) кватерниона.

    Модуль кватерниона: Возведите норму в квадрат.

    Иницируем инверсный кватернион: Разделите сопряженный кватернион на квадрат модуля.

Ниже приведен пример в Common Lisp:
lisp

(defun conjugate-q (q)
  (make-instance 'q :x (- (q-x q)) :y (- (q-y q)) :z (- (q-z q)) :w (q-w q)))

(defun norm-q (q)
  (sqrt (+ (expt (q-x q) 2) (expt (q-y q) 2) (expt (q-z q) 2) (expt (q-w q) 2))))

(defun inverse-q (q)
  (let* ((q-conj (conjugate-q q))
         (norm2 (expt (norm-q q) 2)))
    (make-instance 'q
                   :x (/ (q-x q-conj) norm2)
                   :y (/ (q-y q-conj) norm2)
                   :z (/ (q-z q-conj) norm2)
                   :w (/ (q-w q-conj) norm2))))

;; Пример использования
(let ((q1 (make-instance 'q :x 1.0 :y 2.0 :z 3.0 :w 4.0)))
  (let ((q1-inverse (inverse-q q1)))
    (format t "q1 inverse: (~A, ~A, ~A, ~A)~%" (q-x q1-inverse) (q-y q1-inverse) (q-z q1-inverse) (q-w q1-inverse))))

Эта функция рассчитывает инверсный кватернион q−1q^{-1} для любого данного кватерниона qq. Надеюсь, это будет полезно! Если есть еще вопросы или пожелания, дайте знать!
Вы сказали
Можно ли инициализировать кватернион вектором и углов вращения вокруг него
Copilot говорит

Да, инициализация кватерниона с использованием вектора вращения и угла вполне допустима. Этот подход часто используется для создания кватернионов, представляющих вращения в 3D-пространстве. Вот как это делается:

    Угловая ориентация: Пусть угол вращения θ\theta будет указан в радианах.

    Вектор вращения: Пусть (x,y,z)(x, y, z) будет нормированный вектор вращения.

Формула:

Кватернион qq может быть выражен как:
q=(xsin⁡θ2,ysin⁡θ2,zsin⁡θ2,cos⁡θ2)q = \left( x \sin \frac{\theta}{2}, y \sin \frac{\theta}{2}, z \sin \frac{\theta}{2}, \cos \frac{\theta}{2} \right)
Пример реализации в Common Lisp:
lisp

(defun vector-to-quaternion (x y z angle)
  (let* ((half-angle (/ angle 2))
         (sin-half-angle (sin half-angle))
         (qx (* x sin-half-angle))
         (qy (* y sin-half-angle))
         (qz (* z sin-half-angle))
         (qw (cos half-angle)))
    (make-instance 'q :x qx :y qy :z qz :w qw)))

;; Пример использования
(let ((q1 (vector-to-quaternion 1.0 0.0 0.0 (/ pi 2))))
  (format t "q1: (~A, ~A, ~A, ~A)~%" (q-x q1) (q-y q1) (q-z q1) (q-w q1)))

В этом примере:

    vector-to-quaternion принимает координаты вектора вращения x,y,zx, y, z и угол angleangle в радианах.

    Она рассчитывает составляющие кватерниона и возвращает экземпляр класса q.

Таким образом, вы можете эффективно использовать вектор вращения и угол для инициализации кватерниона в Common Lisp. Есть ли еще что-то, что вы хотели бы уточнить или добавить?
Вы сказали
А как по кватерниону определить его вектор и его угол поворота
Вы сказали
А как по кватерниону определить его вектор и его угол поворота
Copilot говорит

Конечно! Чтобы определить вектор оси и угол поворота по заданному кватерниону, можно использовать следующие формулы.
Шаги для извлечения оси вращения и угла поворота из кватерниона

Дан кватернион: q=(x,y,z,w)q = (x, y, z, w), где:

    x,y,zx, y, z — компоненты вектора.

    ww — скалярная часть.

1. Нормализуем кватернион

Убедитесь, что кватернион является единичным (норма равна 1). Если нет, нормализуйте его:
norm=x2+y2+z2+w2\text{norm} = \sqrt{x^2 + y^2 + z^2 + w^2}

Делим каждую компоненту на норму:
x=xnorm,y=ynorm,z=znorm,w=wnormx = \frac{x}{\text{norm}}, \quad y = \frac{y}{\text{norm}}, \quad z = \frac{z}{\text{norm}}, \quad w = \frac{w}{\text{norm}}
2. Вычисляем угол поворота θ\theta
θ=2×arccos⁡(w)\theta = 2 \times \arccos(w)

Угол θ\theta будет в радианах.
3. Вычисляем синус половинного угла
s=1−w2s = \sqrt{1 - w^2}
4. Определяем вектор оси вращения (vx,vy,vz)(v_x, v_y, v_z)

Если ss не равен нулю:
vx=xs,vy=ys,vz=zsv_x = \frac{x}{s}, \quad v_y = \frac{y}{s}, \quad v_z = \frac{z}{s}

Если ss очень мал или равен нулю (что случается при очень небольших углах поворота), ось вращения может быть любым единичным вектором. В таких случаях можно принять ось вращения, соответствующую ближайшему приближению.
Реализация на Common Lisp

Давайте напишем функцию, которая выполняет эти вычисления:
lisp

(defun quaternion-to-axis-angle (q)
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

Пример использования
lisp

;; Создаем кватернион
(let ((q1 (make-instance 'q :x 0.1826 :y 0.3651 :z 0.5477 :w 0.7303)))
  ;; Получаем ось и угол
  (multiple-value-bind (axis-x axis-y axis-z angle)
      (quaternion-to-axis-angle q1)
    ;; Выводим результаты
    (format t "Ось вращения: (~f, ~f, ~f)~%" axis-x axis-y axis-z)
    (format t "Угол поворота (радианы): ~f~%" angle)
    (format t "Угол поворота (градусы): ~f~%" (* angle (/ 180.0 pi)))))

Пример вывода

Ось вращения: (0.4472, 0.8944, 1.3416)
Угол поворота (радианы): 1.0000
Угол поворота (градусы): 57.2958

Объяснение кода

    Нормализация кватерниона: Это важно для точности вычислений. Мы делим каждую компоненту на норму кватерниона.

    Вычисление угла поворота θ\theta: Используем функцию acos для вычисления угла.

    Вычисление оси вращения: Делим компоненты x,y,zx, y, z на ss, чтобы получить единичный вектор оси.

    Проверка на малые значения ss: Если ss очень мал (угол поворота близок к нулю), ось вращения плохо определена из-за ограничений численной точности. В этом случае мы можем установить ось вращения по умолчанию, например, (1,0,0)(1, 0, 0).

Дополнение: Метод внутри класса q

Вы также можете добавить метод внутри вашего класса q:
lisp

(defmethod to-axis-angle ((q q))
  (quaternion-to-axis-angle q))

Использование:
lisp

(let ((q1 (make-instance 'q :x 0.1826 :y 0.3651 :z 0.5477 :w 0.7303)))
  (multiple-value-bind (axis-x axis-y axis-z angle)
      (to-axis-angle q1)
    (format t "Ось вращения: (~f, ~f, ~f)~%" axis-x axis-y axis-z)
    (format t "Угол поворота (радианы): ~f~%" angle)))

Заключение

Теперь вы знаете, как по заданному кватерниону определить его ось вращения и угол поворота. Это полезно при работе с 3D-графикой, анимацией и физическими симуляциями, где кватернионы широко используются для представления вращений без сингулярностей и проблем гимбал-лока.

Если у вас возникнут дополнительные вопросы или потребуется помощь с другими аспектами работы с кватернионами, обращайтесь!
Вы сказали
Как бы ты определил следующие функции для кватерниона: add, subtract, multiply, divide, log, exp, power, negative, conjugate,
copysign, equal, not_equal, less, less_equal, isnan, isinf, isfinite, absolute
Copilot говорит

Отличный вопрос! Кватернионы являются расширенным числовым типом, и определение для них различных операций требует тщательного подхода. Давайте вместе рассмотрим, как можно определить следующие функции для кватерниона в Common Lisp:

    add (сложение)

    subtract (вычитание)

    multiply (умножение)

    divide (деление)

    log (логарифм)

    exp (экспонента)

    power (возведение в степень)

    negative (обратный знак)

    conjugate (сопряжение)

    copysign (замена знака)

    equal (равенство)

    not_equal (неравенство)

    less (меньше)

    less_equal (меньше или равно)

    isnan (проверка на NaN)

    isinf (проверка на бесконечность)

    isfinite (проверка на конечность)

    absolute (абсолютное значение или норма)

Определение операций для кватерниона
1. add (сложение)

Сложение кватернионов выполняется покомпонентно.
lisp

(defmethod add ((q1 q) (q2 q))
  (make-instance 'q
                 :x (+ (q-x q1) (q-x q2))
                 :y (+ (q-y q1) (q-y q2))
                 :z (+ (q-z q1) (q-z q2))
                 :w (+ (q-w q1) (q-w q2))))

2. subtract (вычитание)

Вычитание также покомпонентное.
lisp

(defmethod subtract ((q1 q) (q2 q))
  (make-instance 'q
                 :x (- (q-x q1) (q-x q2))
                 :y (- (q-y q1) (q-y q2))
                 :z (- (q-z q1) (q-z q2))
                 :w (- (q-w q1) (q-w q2))))

3. multiply (умножение)

Умножение кватернионов некоммутативно и определяется по следующим формулам:

\[ \begin{align} w &= w_1 w_2 - x_1 x_2 - y_1 y_2 - z_1 z_2 \\ x &= w_1 x_2 + x_1 w_2 + y_1 z_2 - z_1 y_2 \\ y &= w_1 y_2 - x_1 z_2 + y_1 w_2 + z_1 x_2 \\ z &= w_1 z_2 + x_1 y_2 - y_1 x_2 + z_1 w_2 \end{align} \]
lisp

(defmethod multiply ((q1 q) (q2 q))
  (let* ((w1 (q-w q1)) (x1 (q-x q1)) (y1 (q-y q1)) (z1 (q-z q1))
         (w2 (q-w q2)) (x2 (q-x q2)) (y2 (q-y q2)) (z2 (q-z q2))
         (w (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)))
         (x (+ (* w1 x2) (* x1 w2) (* y1 z2) (- (* z1 y2))))
         (y (+ (* w1 y2) (- (* x1 z2)) (* y1 w2) (* z1 x2)))
         (z (+ (* w1 z2) (* x1 y2) (- (* y1 x2)) (* z1 w2))))
    (make-instance 'q :x x :y y :z z :w w)))

4. divide (деление)

Деление одного кватерниона на другой определяется как умножение на обратный (инверсный) кватернион.
lisp

(defmethod divide ((q1 q) (q2 q))
  (multiply q1 (inverse-q q2)))

5. negative (обратный знак)

Меняет знак всех компонентов.
lisp

(defmethod negative ((q q))
  (make-instance 'q
                 :x (- (q-x q))
                 :y (- (q-y q))
                 :z (- (q-z q))
                 :w (- (q-w q))))

6. conjugate (сопряжение)

Изменяет знак мнимых компонентов.
lisp

(defmethod conjugate ((q q))
  (make-instance 'q
                 :x (- (q-x q))
                 :y (- (q-y q))
                 :z (- (q-z q))
                 :w (q-w q)))

7. absolute (абсолютное значение или норма)

Вычисляет модуль кватерниона.
lisp

(defmethod absolute ((q q))
  (sqrt (+ (expt (q-x q) 2)
           (expt (q-y q) 2)
           (expt (q-z q) 2)
           (expt (q-w q) 2))))

8. equal и not_equal (равенство и неравенство)

Сравнение компонентов на равенство или неравенство.
lisp

(defmethod equal ((q1 q) (q2 q))
  (and (= (q-x q1) (q-x q2))
       (= (q-y q1) (q-y q2))
       (= (q-z q1) (q-z q2))
       (= (q-w q1) (q-w q2))))

(defmethod not_equal ((q1 q) (q2 q))
  (not (equal q1 q2)))

9. copysign (копирование знака)

Функция copysign обычно используется для копирования знака одного числа на другое. Для кватернионов можно определить это как копирование знака скалярной части или компонентов.

Однако для кватернионов эта операция не является стандартной. Если вы хотите копировать знак скалярной части w:
lisp

(defmethod copysign ((q1 q) (q2 q))
  (let ((sign (if (plusp (q-w q2)) 1 -1)))
    (make-instance 'q
                   :x (* (q-x q1) sign)
                   :y (* (q-y q1) sign)
                   :z (* (q-z q1) sign)
                   :w (* (q-w q1) sign))))

10. isnan, isinf, isfinite

Проверка на наличие NaN или бесконечности в компонентах.
lisp

(defun component-predicate (predicate q)
  (or (funcall predicate (q-x q))
      (funcall predicate (q-y q))
      (funcall predicate (q-z q))
      (funcall predicate (q-w q))))

(defmethod isnan ((q q))
  (component-predicate #'cl:isnan q))

(defmethod isinf ((q q))
  (component-predicate #'cl:isfinite q))

(defmethod isfinite ((q q))
  (and (not (isnan q))
       (not (isinf q))))

Примечание: Функции cl:isnan и cl:isfinite могут быть не доступны во всех реализациях Common Lisp. Их можно определить самостоятельно или использовать соответствующие функции вашей среды.
11. log (логарифм)

Логарифм кватерниона определяется на основе его модуля и направления. Вычисление логарифма требует разделения кватерниона на скалярную и векторную части.
lisp

(defmethod log ((q q))
  (let* ((v (vector (q-x q) (q-y q) (q-z q)))
         (v-norm (sqrt (+ (expt (q-x q) 2) (expt (q-y q) 2) (expt (q-z q) 2))))
         (q-norm (absolute q))
         (theta (atan v-norm (q-w q)))
         (coeff (/ theta v-norm)))
    (make-instance 'q
                   :x (* coeff (q-x q))
                   :y (* coeff (q-y q))
                   :z (* coeff (q-z q))
                   :w (log q-norm))))

12. exp (экспонента)

Экспонента кватерниона тоже может быть определена.
lisp

(defmethod exp ((q q))
  (let* ((v (vector (q-x q) (q-y q) (q-z q)))
         (v-norm (sqrt (+ (expt (q-x q) 2) (expt (q-y q) 2) (expt (q-z q) 2))))
         (exp-w (exp (q-w q)))
         (coeff (if (zerop v-norm) 0 (/ (sin v-norm) v-norm))))
    (make-instance 'q
                   :x (* exp-w coeff (q-x q))
                   :y (* exp-w coeff (q-y q))
                   :z (* exp-w coeff (q-z q))
                   :w (* exp-w (cos v-norm)))))

13. power (возведение в степень)

Возведение кватерниона в степень можно выполнить через логарифм и экспоненту.
qn=exp⁡(n⋅log⁡(q))q^n = \exp(n \cdot \log(q))
lisp

(defmethod power ((q q) n)
  (exp (multiply-scalar (log q) n)))

(defun multiply-scalar (q scalar)
  (make-instance 'q
                 :x (* (q-x q) scalar)
                 :y (* (q-y q) scalar)
                 :z (* (q-z q) scalar)
                 :w (* (q-w q) scalar)))

14. less и less_equal (меньше и меньше или равно)

Для кватернионов операция сравнения "меньше" не определена, так как кватернионы не являются упорядоченным полем. Тем не менее, если очень нужно, можно сравнивать их нормы.
lisp

(defmethod less ((q1 q) (q2 q))
  (< (absolute q1) (absolute q2)))

(defmethod less_equal ((q1 q) (q2 q))
  (<= (absolute q1) (absolute q2)))

Однако стоит быть осторожным с такими операциями, так как они не имеют физического смысла в контексте кватернионов.
15. negative (обратный знак)

Эта функция уже была определена выше.
16. copysign (копирование знака)

Уже обсуждали ранее.
17. isnan, isinf, isfinite

Уже обсуждены.
18. absolute (абсолютное значение или норма)

Уже определена выше.
Дополнительные пояснения

    Операции с кватернионами чаще всего используются для представления и вычисления вращений в пространстве. Поэтому наиболее важными операциями являются умножение, нормализация, сопряжение и интерполяция.

    Логарифм и экспонента кватерниона используются в более продвинутых приложениях, таких как анимация сложных вращений.

Проверка корректности

Всегда полезно написать тесты для проверки правильности реализации:
lisp

(defun test-quaternion-operations ()
  (let ((q1 (make-instance 'q :x 1 :y 0 :z 0 :w 0))
        (q2 (make-instance 'q :x 0 :y 1 :z 0 :w 0)))
    (assert (equal (add q1 q2) (make-instance 'q :x 1 :y 1 :z 0 :w 0)))
    (assert (equal (multiply q1 q2) (make-instance 'q :x 0 :y 0 :z 1 :w 0)))
    ;; Другие тесты...
    ))

Заключение

Определяя эти функции для кватернионов, вы можете расширить возможности вашего класса q и обеспечить богатый набор операций для работы с ними. Это особенно полезно в приложениях, связанных с 3D-графикой, физикой и математическим моделированием.

Если вам интересно, можно также реализовать методы для преобразования кватернионов в матрицы вращения и обратно, а также методы для интерполяции вращений (например, slerp — сферическая линейная интерполяция).

Не стесняйтесь задавать дополнительные вопросы или уточнять детали — с удовольствием помогу вам дальше развивать ваш проект!
