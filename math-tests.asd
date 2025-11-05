(defsystem "math-tests"
  :description "Тестирование систем, входящих  в проект Math"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "GPL-3.0-or-later"
  :version "0.0.5"
  :depends-on (:math :fiveam)
  :perform (test-op (o s)
		    (uiop:symbol-call :math/tests :run-tests))
  :components ((:module "tests"
		:serial t
		:components ((:file "package")
			     (:file "all")
                             (:file "core")
                             (:file "2d-array")
                             (:file "matrix")
                             (:file "list-matr-tests")
                             (:file "equation")
                             (:file "ls-gauss")
                             (:file "ls-gsll")
                             (:file "ls-rotation")
                             (:file "coord")
                             (:file "half-div")
                             (:file "appr")
			     ;; (:file "run")
                             ))))
