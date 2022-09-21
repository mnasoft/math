
(defpackage #:math/docs
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
      #+nil :math/x-o
      :math/coord
      :math/stat
      :math/core
      :math/gnuplot)
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
