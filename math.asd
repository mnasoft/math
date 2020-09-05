;;;; math.asd

(defsystem #:math
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on ("cl-utilities"
	       "math/list-matr"
	       "math/2d-array"
	       "math/appr"
	       "math/stat"
	       "math/smooth"
	       "math/coord"
	       "math/arr-matr"
	       "math/gnuplot"
	       "math/ls-rotation"
;;;;           "math/ls-solve"
;;;;           "math/tests"
	       ) ;;;;   #:cl-annot :pcl-test :gsll 
  :components ((:file "package")
               (:file "math")
	       (:file "mult-matr-vect")
	       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
	       ;;	       (:file "matr-test")  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	       
;;;;	       (:file "x-o")	    ;; Игра крестики-нолики     

	       ))

(defsystem #:math/ls-rotation
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-rotation/tests")))
;;;;  :depends-on ()
  :components ((:module "ls-rotation"
		:serial t
                :components ((:file "las-rotation")))))

(defsystem #:math/gnuplot
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
;;;;  :depends-on ()
  :components ((:module "gnuplot"
		:serial t
                :components ((:file "gnuplot") 
			     ))))

(defsystem #:math/list-matr
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("cl-utilities")
  :components ((:module "list-matr"
		:serial t
                :components ((:file "list-matr") 
			     ))))

(defsystem #:math/ls-gauss
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/ls-gauss/tests")))
  :depends-on ("math/arr-matr")
  :components ((:module "ls-gauss"
		:serial t
                :components ((:file "ls-gauss")))))

(defsystem #:math/2d-array
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/2d-array/tests")))
;;;  :depends-on (:math)
  :components ((:module "2d-array"
		:serial t
                :components ((:file "2d-array")))))

(defsystem #:math/appr
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/appr/tests")))
  :depends-on ( "math/ls-gauss" "math/arr-matr" "math/smooth" ) ;;;; "math"
  :components ((:module "appr"
		:serial t
                :components ((:file "package")
			     (:file "appr-func-temptate"  ) 
			     (:file "approximation") 
			     ))))

(defsystem #:math/stat
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/stat/tests")))
;;;  :depends-on (:math)
  :components ((:module "stat"
		:serial t
                :components ((:file "statistics")))))

(defsystem #:math/smooth
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/smooth/tests")))
;;;  :depends-on (:math)
  :components ((:module "smooth"
		:serial t
                :components ((:file "smoothing")))))

(defsystem #:math/coord
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/smooth/tests")))
;;;  :depends-on (:math)
  :components ((:module "coord"
		:serial t
                :components ((:file "coordinate-system")))))

(defsystem #:math/arr-matr
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/arr-matr/tests")))
  :depends-on ("cl-utilities")
  :components ((:module "arr-matr"
		:serial t
                :components ((:file "package")
			     (:file "matr-generics") 
			     (:file "matr-class")))))
	       

(defsystem #:math/ls-solve
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on (:math :gsll)
  :components ((:module "ls-solve"
		:serial t
                :components ((:file "package")
			     (:file "lu-solve")
			     ))))


(defsystem #:math/tests
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
				     (:file "main-run")
				     ))))
