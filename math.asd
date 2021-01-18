;;;; math.asd

(defsystem "math"
  :description
  "Math is a math library, implementing some algorithms:
- linear algebra;
- operations with matrices;
- statistical functions;
- linear and bilinear interpolation;
- finding approximating polynomials,
implemented in Common Lisp"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
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
	       "math/x-o"
	       ) ;;;; "math/ls-solve" "math/tests"
  :components ((:file "math")
	       (:module "src"
		:serial t
                :components ((:file "mult-matr-vect"))))) ;;;; (:file "matr-temp")

(defsystem "math/core"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/core/tests")))
;;;;  :depends-on ()
  :components ((:module "src/core"
		:serial nil
                :components ((:file "main")
			     (:file "generic"      :depends-on ("main"))
			     (:file "generic-matr" :depends-on ("main"))
			     (:file "method"       :depends-on ("main" "generic"))))))

(defsystem #:math/ls-rotation
  :description "Реализация решение системы линейных уравнений методом вращения"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
;;;;  :depends-on ("math/arr-matr")
  :components ((:module "src/ls-rotation"
		:serial t
                :components ((:file "las-rotation")))))

(defsystem #:math/gnuplot
  :description "Интерфейс к программе построения графиков gnuplot"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/gnuplot/tests")))
  :depends-on ("math/core" "font-discovery" "vgplot")
  :components ((:module "src/gnuplot"
		:serial t
                :components ((:file "package")
                             (:file "defclass")
                             (:file "gnuplot")))))

(defsystem #:math/list-matr
  :description "Реализация некоторых операций над матрицами,
представленными прямоугольными 2d-списками"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("cl-utilities" "math/stat")
  :components ((:module "src/list-matr"
		:serial t
                :components ((:file "list-matr")))))

(defsystem #:math/ls-gauss
  :description "Решение систем линейных уравнений методом Гаусса"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
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
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/2d-array/tests")))
;;;  :depends-on (:math)
  :components ((:module "src/2d-array"
		:serial t
                :components ((:file "2d-array")))))

(defsystem #:math/appr
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
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
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/stat/tests")))
  :depends-on ("math/core")
  :components ((:module "src/stat"
		:serial t
                :components ((:file "statistics")))))

(defsystem #:math/smooth
  :description "Весовые функции для методов сглаживания"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/smooth/tests")))
;;;  :depends-on (:math)
  :components ((:module "src/smooth"
		:serial t
                :components ((:file "smoothing")))))

(defsystem #:math/coord
  :description "Содержит функции преобразования
- угловой меры;
- координат точки между декартовой, полярной, сферической системами координат.
"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/coord/tests")))
  :depends-on ("math/core") 
  :components ((:module "src/coord"
		:serial t
                :components ((:file "coordinate-system")))))

(defsystem #:math/arr-matr
  :description "Реализация некоторых операций над 
матрицами, представленными классом <matrix>"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("math/core" "cl-utilities")
  :components ((:module "src/arr-matr"
		:serial t
                :components ((:file "package")
			     (:file "matr-generics") 
			     (:file "matr-class")))))

(defsystem #:math/ls-solve
  :description "Решение систем линейных уравнений при помощи библиотеки gsll"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on (:math :gsll)
  :components ((:module "src/ls-solve"
		:serial t
                :components ((:file "package")
			     (:file "lu-solve")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem #:math/tests
  :description "Тестирование систем, входящих  в проект Math"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (:math :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :math-tests :test-math))
  :components ((:module "tests"
			:serial t
			:components ((:file "package")
				     (:file "main")
				     (:file "matrix")
				     (:file "linear-system-tests") 
				     (:file "approximation-tests") 
				     (:file "list-matr-tests") 
				     (:file "main-run")))))

(defsystem #:math/docs
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on ("math"
               "mnas-package"
               "codex"
               ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem #:math/x-o
  :description "Консольная игра крестики-нолики"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/x-o/tests")))
  :depends-on ( "math/core" "math/arr-matr") 
  :components ((:module "src/x-o"
		:serial t
                :components ((:file "x-o")))))
