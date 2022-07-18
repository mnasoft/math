;;;; math.asd

(defsystem "math"
  :description
  "Math is a math library, implementing some algorithms:
@begin(list)
 @item(linear algebra;)
 @item(operations with matrices;)
 @item(statistical functions;)
 @item(linear and bilinear interpolation;)
 @item(finding approximating polynomials, implemented in Common
       Lisp.)
@end(list)
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on ("cl-utilities"
	       "math/core"
	       "math/list-matr"
	       "math/2d-array"
	       "math/appr"
	       "math/stat"
	       "math/smooth"
	       "math/coord"
	       "math/arr-matr"
	       "math/gnuplot"
	       "math/ls-rotation"
               "math/geom"
               "math/transform"
               "math/equation"
	       ;; "math/x-o"
	       ) ;;;; "math/ls-solve" "math/tests"
  :components ((:module "src/math"
		:serial t
                :components ((:file "math"))))) ;;;; (:file "matr-temp")

(defsystem "math/core"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
  ;; :depends-on ()
  :components ((:module "src/core"
		:serial nil
                :components ((:file "core")
			     (:file "generic"      :depends-on ("core"))
			     (:file "generic-matr" :depends-on ("core"))
			     (:file "method"       :depends-on ("core" "generic"))))))

(defsystem "math/ls-rotation"
  :description "Реализация решение системы линейных уравнений методом вращения"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
  ;; :depends-on ("math/arr-matr")
  :components ((:module "src/ls-rotation"
		:serial t
                :components ((:file "las-rotation")))))

(defsystem "math/gnuplot"
  :description "Интерфейс к программе построения графиков gnuplot"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/gnuplot/tests")))
  :depends-on ("math/core" "font-discovery" "vgplot")
  :components ((:module "src/gnuplot"
		:serial t
                :components ((:file "gnuplot")))))

(defsystem "math/list-matr"
  :description "Реализация некоторых операций над матрицами,
представленными прямоугольными 2d-списками"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("cl-utilities" "math/stat")
  :components ((:module "src/list-matr"
		:serial t
                :components ((:file "list-matr")))))

(defsystem "math/ls-gauss"
  :description "Решение систем линейных уравнений методом Гаусса"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-gauss/tests")))
  :depends-on ("math/arr-matr")
  :components ((:module "src/ls-gauss"
		:serial t
                :components ((:file "ls-gauss")))))

(defsystem #:math/2d-array
  :description "Реализация некоторых операций над 
матрицами, представленными  2d-массивами"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/2d-array/tests")))
;;;  :depends-on (:math)
  :components ((:module "src/2d-array"
		:serial t
                :components ((:file "2d-array")))))

(defsystem "math/appr"
  :description "Describe math here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/appr/tests")))
  :depends-on ("math/core" "math/2d-array" "math/ls-gauss" "math/arr-matr" "math/smooth" ) ;;;; "math"
  :components ((:module "src/appr"
		:serial t
                :components ((:file "package")
			     (:file "appr-func-temptate") 
			     (:file "approximation")))))

(defsystem #:math/stat
  :description "Describe math here"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/stat/tests")))
  :depends-on ("math/core")
  :components ((:module "src/stat"
		:serial t
                :components ((:file "statistics")))))

(defsystem "math/smooth"
  :description "Весовые функции для методов сглаживания"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/smooth/tests")))
;;;  :depends-on (:math)
  :components ((:module "src/smooth"
		:serial t
                :components ((:file "smoothing")))))

(defsystem "math/coord"
  :description "Содержит функции преобразования
- угловой меры;
- координат точки между декартовой, полярной, сферической системами координат.
"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/coord/tests")))
  :depends-on ("math/core") 
  :components ((:module "src/coord"
		:serial t
                :components ((:file "coordinate-system")))))

(defsystem "math/arr-matr"
  :description "Реализация некоторых операций над 
матрицами, представленными классом <matrix>"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("math/core" "cl-utilities")
  :components ((:module "src/arr-matr"
		:serial t
                :components ((:file "arr-matr")
                             ;; (:file "package")
			     ;; (:file "matr-generics") 
			     ;; (:file "matr-class")
                             ))))

(defsystem "math/ls-solve"
  :description "Решение систем линейных уравнений при помощи библиотеки gsll"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on (:math :gsll)
  :components ((:module "src/ls-solve"
		:serial t
                :components ((:file "package")
			     (:file "lu-solve")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "math/tests"
  :description "Тестирование систем, входящих  в проект Math"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (:math :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :math-tests :test-math))
  :components ((:module "src/tests"
			:serial t
			:components ((:file "package")
				     (:file "all")
				     (:file "matrix")
				     (:file "linear-system-tests") 
				     (:file "approximation-tests") 
				     (:file "list-matr-tests") 
				     (:file "run")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "math/geom"
  :description "Функции вычисления площадей и объемов геометрических фигур и тел."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
;;;;  :depends-on ("math/arr-matr")
  :components ((:module "src/geom"
		:serial t
                :components ((:file "geom")))))

(defsystem "math/docs"
  :description "Зависимости для сборки документации"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("math"
               "mnas-package"
               "codex")
  :components
  ((:module "src/docs" :serial nil
    :components ((:file "docs")))))

(defsystem "math/transform"
  :description "@b(Описание:) система @b(math/transform) содержит
  функции преобразования в трехмерном пространстве."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
;;;;  :depends-on ("math/arr-matr")
  :components ((:module "src/transform"
		:serial t
                :components ((:file "transform")))))

(defsystem "math/equation"
  :description "@b(Описание:) система @b(math/equation) содержит
  функции для нахождения корней линейных, квадратных, кубических и
  уравнений 4-ой степени (последнее не реализовано)."
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
;;;;  :depends-on ("math/arr-matr")
  :components ((:module "src/equation"
		:serial t
                :components ((:file "equation")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "math/x-o"
  :description "Консольная игра крестики-нолики"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/x-o/tests")))
  :depends-on ( "math/core" "math/arr-matr") 
  :components ((:module "src/x-o"
		:serial t
                :components ((:file "x-o")))))
