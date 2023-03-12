
(defpackage :math/docs
  (:use #:cl ) 
  (:export make-all)
  (:documentation "Пакет @b(math/docs) содержит функции
  генерирования и публикации документации."))

(in-package :math/docs)

(defun make-document ()
  (loop
    :for j :from 1
    :for i :in
    '(;; 1
      (:math             :math)
      (:math/ls-rotation :math/ls-rotation)
      (:math/geom        :math/geom)
      (:math/appr        :math/appr)
      (:math/matr        :math/matr)
      (:math/ls-gauss    :math/ls-gauss)
      (:math/smooth      :math/smooth)
      (:math/coord       :math/coord)
      (:math/stat        :math/stat)
      (:math/core        :math/core)
      (:math/gnuplot     :math/gnuplot)
      (:math/rnd         :math/rnd)
      (:math/series      :math/series)
      (#:math/stat       #:math/stat)
      #+nil
      (:math/x-o         :math/x-o)
      )
    :do (progn
          (apply #'mnas-package:document i)
          (format t "~A ~A~%" j i))))

(defun make-graphs ()
  (loop
    :for j :from 1
    :for i :in
    '(:math
      :math/ls-rotation
      :math/geom
      :math/appr
      :math/matr
      :math/ls-gauss
      :math/smooth
      :math/coord
      :math/stat
      :math/core
      :math/gnuplot
      :math/rnd
      :math/series
      #:math/stat
      #+nil :math/x-o      
      )
    :do (progn
          (mnas-package:make-codex-graphs i i)
          (format t "~A ~A~%" j i))))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string= :key #'first)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  (let* ((sys-symbol :math)
         (sys-string (string-downcase (format nil "~a" sys-symbol))))
    (mnas-package:make-html-path sys-symbol)
    (make-document)
    (mnas-package:make-mainfest-lisp `(,sys-symbol)
                                     (string-capitalize sys-string)
                                     '("Mykola Matvyeyev")
                                     (mnas-package:find-sources sys-symbol)
                                     :output-format of)
    (codex:document sys-symbol)
    (make-graphs)
    (mnas-package:copy-doc->public-html sys-string)
    (mnas-package:rsync-doc sys-string)
    :make-all-finish))

;;;; (make-all)
