
(defpackage #:math/docs
  (:use #:cl ) 
  (:nicknames "MSTR/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(math/docs) содержит функции
  генерирования и публикации документации."))

(in-package :math/docs)

(defun make-document ()
  (loop
    :for i :in
    '(;; 1
      (:math             :math)
      ;; 2
      (:math/ls-rotation :math/ls-rotation)
      (:math/geom        :math/geom)
      (:math/appr        :math/appr)
      ;; 3
      (:math/matr        :math/matr)
      (:math/ls-gauss    :math/ls-gauss)
      (:math/smooth      :math/smooth)
      ;; 2
      #+nil
      (:math/x-o         :math/x-o)
      (:math/coord       :math/coord)
      ;; 3       
      (:math/stat        :math/stat)
      ;; 5
      (:math/core        :math/core)
      ;; 2
      (:math/gnuplot     :math/gnuplot)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:math
      :math/ls-rotation
      :math/geom
      :math/appr
      :math/matr
      :math/ls-gauss
      :math/smooth
      #+nil :math/x-o
      :math/coord
      :math/stat
      :math/core
      :math/gnuplot)
    :do (mnas-package:make-codex-graphs i i)))

#+nil
(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))

  (mnas-package:make-html-path :math)
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp '(:math :math/docs)
                                   "Math"
                                   '("Mykola Matvyeyev")
                                   (mnas-package:find-sources "math")
                                   :output-format of)
  (codex:document :math)
  (make-graphs)
  (mnas-package:copy-doc->public-html "math")
  (mnas-package:rsync-doc "math"))

  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/math.
"  
(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (mnas-package:make-html-path "math")
  (make-document)
  (mnas-package:make-mainfest-lisp '(:math)
                                   "Math"
                                   '("Mykola Matvyeyev")
                                   (mnas-package:find-sources "math")
                                   :output-format of)
  (codex:document :math)
  (make-graphs)
  (mnas-package:copy-doc->public-html "math")
  (mnas-package:rsync-doc "math")
  :make-all-finish)

;;;; (make-all)
