;;;; ./math.asd

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
  :license "GPL-3.0-or-later"
  :version "0.0.4"
  :serial t
  :in-order-to ((test-op (test-op "math-tests")))
  :depends-on ("cl-utilities"
	       "math/core"
               "math/coord"
               "math/matr"
               "math/geom"
               "math/equation"
               "math/stat"
               "math/smooth"
               "math/rnd"
               "math/ls-gauss"
               "math/ls-gsll"
               "math/ls-rotation"
               "math/gnuplot"
               "math/appr"
               "math/series"
               "math/half-div"
	       ;; "math/x-o"
	       )
  :components ((:module "src/math"
		:serial t
                :components ((:file "math"))))) ;;;; (:file "matr-temp")

(defsystem "math/half-div"
  :description "Describe half-div here"
  :components ((:module "src/half-div"
		:serial nil
                :components ((:file "half-div")
                             (:file "half-div-doc" :depends-on ("half-div"))))))

(defsystem "math/series"
  :description "@b(Описание:) система @b(math/series) определяет некоторые операции с
 прогрессиями."
  :depends-on ("math/half-div" "math/equation")
  :components ((:module "src/series"
		:serial nil
                :components ((:file "series")
                             (:file "series-doc" :depends-on ("series"))))))

(defsystem "math/rnd"
  :description "Содержит функции для генерирования случайных списков и 2d-списков."
  :depends-on ("math/matr")
  :components ((:module "src/rnd"
		:serial nil
                :components ((:file "rnd")
                             (:file "rnd-doc" :depends-on ("rnd"))))))

(defsystem "math/core"
  :description "Содержит некоторые функции и обобщенные функции,
используемые в проекте повсеместно"
  ;; :depends-on ()
  :components ((:module "src/core"
		:serial nil
                :components ((:file "core")
                             (:file "core-doc" :depends-on ("core"))))))

(defsystem "math/ls-rotation"
  :description "Реализация решение системы линейных уравнений методом вращения"
  :depends-on ("math/matr")
  :components ((:module "src/ls-rotation"
		:serial t
                :components ((:file "ls-rotation")
                             (:file "ls-rotation-doc")))))

(defsystem "math/gnuplot"
  :description "Интерфейс к программе построения графиков gnuplot"
  :depends-on ("math/core" "font-discovery" "vgplot")
  :components ((:module "src/gnuplot"
		:serial t
                :components ((:file "gnuplot")
                             (:file "gnuplot-doc" :depends-on ("gnuplot"))))))

(defsystem "math/ls-gauss"
  :description "Решение систем линейных уравнений методом Гаусса"
  :depends-on ("math/matr")
  :components ((:module "src/ls-gauss"
		:serial t
                :components ((:file "ls-gauss")
                             (:file "ls-gauss-doc")))))

(defsystem "math/appr"
  :description "Describe math here"
  :depends-on ("math/core" "math/matr" "math/ls-gauss" "math/smooth") 
  :components ((:module "src/appr"
		:serial t
                :components ((:file "appr")
                             (:file "appr-doc")))))

(defsystem "math/stat"
  :description "Describe math here"
  :depends-on ("math/core" "gsll")
  :components ((:module "src/stat"
		:serial t
                :components ((:file "stat")
                             (:file "stat-doc" :depends-on ("stat"))))))

(defsystem "math/smooth"
  :description "Весовые функции для методов сглаживания"
  :components ((:module "src/smooth"
		:serial t
                :components ((:file "smooth")
                             (:file "smooth-doc" :depends-on ("smooth"))))))

(defsystem "math/coord"
  :description "Содержит функции преобразования
- угловой меры;
- координат точки между декартовой, полярной, сферической системами координат.
"
  :depends-on ("math/core") 
  :components ((:module "src/coord"
		:serial t
                :components ((:file "coord")
                             (:file "coord-doc")))))

(defsystem "math/ls-gsll"
  :description "Решение систем линейных уравнений при помощи библиотеки gsll"
  :depends-on ("gsll" "math/matr")
  :components ((:module "src/ls-gsll"
		:serial t
                :components ((:file "ls-gsll")
                             (:file "ls-gsll-doc")))))

(defsystem "math/geom"
  :description "Функции вычисления площадей и объемов геометрических фигур и тел."
  :components ((:module "src/geom"
		:serial t
                :components ((:file "geom")
                             (:file "geom-doc")))))

(defsystem "math/docs"
  :description "
@b(Описание:) система @b(math/docs) содержит функции для извлечения
сборки и публикации документации.

Для публикации документации в системе должна быть установлена
программа @b(rsync)."
  :depends-on ("math"
               "mnas-package"
               "codex")
  :components
  ((:module "src/docs" :serial nil
    :components ((:file "docs")))))

(defsystem "math/equation"
  :description "@b(Описание:) система @b(math/equation) содержит
  функции для нахождения корней линейных, квадратных, кубических и
  уравнений 4-ой степени (последнее не реализовано)."
  :components ((:module "src/equation"
		:serial t
                :components ((:file "equation")
                             (:file "equation-doc")))))

(defsystem "math/matr"
  :description "@b(Описание:) система @b(math/matr) содержит
 некоторых операций над матрицами"
  :depends-on ("cl-utilities" "math/coord" "math/stat" "closer-mop")
  :components ((:module "src/matr"
		:serial t
                :components ((:file "matr")
                             (:file "generics")
                             (:file "methods/add")
                             (:file "methods/anti-diagonal")
                             (:file "methods/append")
                             (:file "methods/average")                             
                             (:file "methods/col")
                             (:file "methods/cols")
                             (:file "methods/copy")
                             (:file "methods/dimensions")
                             (:file "methods/equivalent")
                             (:file "methods/main-diagonal")
                             (:file "methods/mref")
                             (:file "methods/multiply")
                             (:file "methods/prepend")
                             (:file "methods/rotate")
                             (:file "methods/row")
                             (:file "methods/rows")
                             (:file "methods/squarep")
                             (:file "methods/swap-cols")
                             (:file "methods/swap-rows")
                             (:file "methods/transpose")
                             (:file "methods/unite-cols")
                             (:file "methods/unite-rows")
                             (:file "methods/rest-methods")
                             (:file "matr-doc")))))

(defsystem "math/obj"
  :description "@b(Описание:) система @b(math/obj) содержит описание некоторых
геометрических объектов."
  :depends-on ("gsll" "math/stat" "vgplot")  
  :components ((:module "src/obj"
		:serial t
                :components ((:file "obj")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "math/x-o"
  :description "Консольная игра крестики-нолики"
  :depends-on ( "math/core" "math/matr") 
  :components ((:module "src/x-o"
		:serial t
                :components ((:file "x-o")))))
