;;;; math.asd

(defsystem #:math
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :in-order-to ((test-op (test-op "math/tests")))
  :depends-on (
               :pcl-test
;;;; :gsll
	       )
  :components ((:file "package")
	       (:file "smoothing")
               (:file "math")
	       (:file "array")

	       (:file "gnuplot")	       
	       (:file "approximation")	       
	       (:file "statistics")
	       (:file "coordinate-system")
	       (:file "mult-matr-vect")
	       (:file "las-rotation")
	       (:file "list-matr")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	       (:file "matr-generics") 
	       (:file "matr-class") ;;
;;	       (:file "matr-test")  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	       
;;;;	       (:file "x-o")	    ;; Игра крестики-нолики     
	       ))

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
