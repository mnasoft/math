;;;; math.asd

(asdf:defsystem #:math
  :description "Describe math here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:cl-utilities)
  :components ((:file "package")
               (:file "math")
	       (:file "approximation")
               (:file "matr")
	       (:file "statistics")
	       (:file "mult-matr-vect")
	       (:file "las-rotation")
	       ))
