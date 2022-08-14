;;;; ./src/equation/equation-doc.lisp
(in-package :math/equation)

(defmacro make-doc (obj-name obj-type doc-string)
  `(setf (documentation ,obj-name ,obj-type)
         ,doc-string))

(defun find-slot (slot-name class)
  (find slot-name
        (sb-mop:class-direct-slots  (find-class class))
        :key #'sb-mop:slot-definition-name))



(make-doc
  (macro-function 'MATH/EQUATION::MAKE-DOC) t
  NIL)

(make-doc
  #'MATH/EQUATION::FIND-SLOT 'function
  NIL)

(make-doc
  #'MATH/EQUATION:FUNC 'function
  "@b(Описание:) обобщенная_функция @b(func) функцию одного
переменного, представляющее полином.")

(make-doc
  #'MATH/EQUATION:TAB 'function
  "@b(Описание:) обобщенная_функция @b(tab) возвращает значение фукции
 @b(eq) в точке @b(x).")

(make-doc
  #'MATH/EQUATION:COEFF-A 'function
  NIL)

(make-doc
  #'MATH/EQUATION:COEFF-D 'function
  NIL)

(make-doc
  #'MATH/EQUATION:COEFF-E 'function
  NIL)

(make-doc
  #'MATH/EQUATION:ROOTS 'function
  "@b(Описание:) обобщенная_функция @b(roots) возвращает корни
 уравнения @b(eq).")

(make-doc
  #'MATH/EQUATION:COEFF-B 'function
  NIL)

(make-doc
  #'MATH/EQUATION:COEFF-C 'function
  NIL)

(make-doc
  #'(setf MATH/EQUATION:COEFF-A)
  'function
  NIL)

(make-doc
  #'(setf MATH/EQUATION:COEFF-D)
  'function
  NIL)

(make-doc
  #'(setf MATH/EQUATION:COEFF-E)
  'function
  NIL)

(make-doc
  #'(setf MATH/EQUATION:COEFF-B)
  'function
  NIL)

(make-doc
  #'(setf MATH/EQUATION:COEFF-C)
  'function
  NIL)

(make-doc
  (find-class 'MATH/EQUATION:<LINEAR>) t
  "Линейное уравнение.")

(make-doc
  (find-class 'MATH/EQUATION:<QUADRIC>) t
  "Квадратное уравнение.")

(make-doc
  (find-class 'MATH/EQUATION:<QUARTIC>) t
  "Уравнение четвертой степени.")

(make-doc
  (find-class 'MATH/EQUATION:<CUBIC>) t
  "Кубическое уравнение.")

(make-doc
 (find-slot 'MATH/EQUATION::A 'MATH/EQUATION:<LINEAR>)
 t
 "Коэффициент при степени 1.")

(make-doc
 (find-slot 'MATH/EQUATION::B 'MATH/EQUATION:<LINEAR>)
 t
 "Коэффициент при степени 0.")

(make-doc
 (find-slot 'MATH/EQUATION::A 'MATH/EQUATION:<QUADRIC>)
 t
 "Коэффициент при степени 2.")

(make-doc
 (find-slot 'MATH/EQUATION::B 'MATH/EQUATION:<QUADRIC>)
 t
 "Коэффициент при степени 1.")

(make-doc
 (find-slot 'MATH/EQUATION::C 'MATH/EQUATION:<QUADRIC>)
 t
 "Коэффициент при степени 0.")

(make-doc
 (find-slot 'MATH/EQUATION::A 'MATH/EQUATION:<QUARTIC>)
 t
 "Коэффициент при степени 4.")

(make-doc
 (find-slot 'MATH/EQUATION::B 'MATH/EQUATION:<QUARTIC>)
 t
 "Коэффициент при степени 3.")

(make-doc
 (find-slot 'MATH/EQUATION::C 'MATH/EQUATION:<QUARTIC>)
 t
 "Коэффициент при степени 2.")

(make-doc
 (find-slot 'MATH/EQUATION::D 'MATH/EQUATION:<QUARTIC>)
 t
 "Коэффициент при степени 1.")

(make-doc
 (find-slot 'MATH/EQUATION::E 'MATH/EQUATION:<QUARTIC>)
 t
 "Коэффициент при степени 0.")

(make-doc
 (find-slot 'MATH/EQUATION::A 'MATH/EQUATION:<CUBIC>)
 t
 "Коэффициент при степени 3.")

(make-doc
 (find-slot 'MATH/EQUATION::B 'MATH/EQUATION:<CUBIC>)
 t
 "Коэффициент при степени 2.")

(make-doc
 (find-slot 'MATH/EQUATION::C 'MATH/EQUATION:<CUBIC>)
 t
 "Коэффициент при степени 1.")

(make-doc
 (find-slot 'MATH/EQUATION::D 'MATH/EQUATION:<CUBIC>)
 t
 "Коэффициент при степени 0.")

(make-doc
  (find-method #'MATH/EQUATION:ROOTS NIL '(MATH/EQUATION:<LINEAR>))
  t
  "@b(Описание:) метод @b(roots) возвращает список корней линейного
 уравнения @b(eq).

 @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance '<linear> :a 1 :b -2)) 
@end(code)")

(make-doc
  (find-method #'MATH/EQUATION:ROOTS NIL '(MATH/EQUATION:<QUADRIC>))
  t
  "@b(Описание:) метод @b(roots) возвращает список корней квадратного
уравнения @b(eq).

 @b(Пример использования:)
@begin[lang=lisp](code)
(roots (make-instance '<quadric> :a 1 :b -2 :c 1)) => (1.0 1.0)
(roots (make-instance '<quadric> :a 1 :b -4 :c 4)) => (2.0 2.0)
@end(code)")

(make-doc
  (find-method #'MATH/EQUATION:ROOTS NIL '(MATH/EQUATION:<CUBIC>))
  t
  "@b(Описание:) метод @b(roots) возвращает список корней кубического
 уравнения @b(eq).

 @b(Пример использования:) @begin[lang=lisp](code)
(roots (make-instance '<cubic> :a 2 :b -3 :c -3 :d 2)) 
=> (#C(-1.0d0 -0.0d0) 
    #C(0.5000000000000004d0 2.9605947323337506d-16) 
    #C(1.9999999999999996d0 -2.9605947323337506d-16))
(roots (make-instance '<cubic> :a 1 :b  2 :c 10 :d -20))
=> (#C(-1.6844040539106864d0 -3.4313313501976914d0) 
    #C(1.3688081078213714d0 1.9558293600573398d-16) 
    #C(-1.684404053910686d0 3.4313313501976905d0))
(roots (make-instance '<cubic> :a 1 :b -6 :c 12  :d -8))
=> (2.0d0 2.0d0 2.0d0)
@end(code)")

(make-doc
  (find-method #'MATH/EQUATION:ROOTS NIL '(MATH/EQUATION:<QUARTIC>))
  t
  "@b(Описание:) метод @b(roots) возвращает список корней кубического
  уравнения @b(eq).")

(make-doc
  (find-method #'MATH/EQUATION:TAB NIL '(MATH/EQUATION:<LINEAR> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/EQUATION:TAB NIL '(MATH/EQUATION:<QUADRIC> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/EQUATION:TAB NIL '(MATH/EQUATION:<CUBIC> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/EQUATION:TAB NIL '(MATH/EQUATION:<QUARTIC> T))
  t
  NIL)

(make-doc
  (find-method #'MATH/EQUATION:FUNC NIL '(MATH/EQUATION:<LINEAR>))
  t
  "(funcall (func (make-instance '<linear>)) 1)")

(make-doc
  (find-method #'MATH/EQUATION:FUNC NIL '(MATH/EQUATION:<QUADRIC>))
  t
  "(funcall (func (make-instance '<quadric>)) 2)")

(make-doc
  (find-method #'MATH/EQUATION:FUNC NIL '(MATH/EQUATION:<CUBIC>))
  t
  "(funcall (func (make-instance '<cubic>)) 5)")

(make-doc
  (find-method #'MATH/EQUATION:FUNC NIL '(MATH/EQUATION:<QUARTIC>))
  t
  "(funcall (func (make-instance '<quartic>)) 0)")

