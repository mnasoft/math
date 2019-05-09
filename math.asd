;;;; math.asd

(defsystem #:math
  :description "Describe math here"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :serial t
  :depends-on (:cl-utilities
;;;; :gsll
	       )
  :components ((:file "package")
               (:file "math")
               (:file "matr")
	       (:file "approximation")	       
	       (:file "statistics")
	       (:file "mult-matr-vect")
	       (:file "las-rotation")
	       (:file "list-matr")
;;;;	       (:file "lu-solve")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;	       (:file "matr-generics") ;;
	       (:file "matr-class") ;;
	       (:file "matr-test")  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	       
;;;;	       (:file "x-o")	       
	       ))
