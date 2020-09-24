(in-package :math/gnuplot)

(export '*default-gnuplot-direcroty*)

(defparameter *default-gnuplot-direcroty*
  (ensure-directories-exist #P"~/gnuplot/")
  "Каталог для вывода по умолчанию.")

;;;; (directory-namestring *default-gnuplot-direcroty*)

(defun file-name (f-name &optional (f-ext ""))
  "Определяет имя файла в каталоге поумолчанию."
  (assert (stringp f-name))
  (assert (stringp f-ext))
  (if (string= "" f-ext)
      (concatenate 'string (directory-namestring *default-gnuplot-direcroty*)
                   f-name f-ext)
      (concatenate 'string (directory-namestring *default-gnuplot-direcroty*)
                   f-name "." f-ext)))

(defun find-font-family (&key (family "Times New Roman"))
  "@b(Описание:) функция @b(find-font-family) возвращает имя шрифта,
установленного в систему похожего на тот, что указан.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (find-font-family :family \"Arial\")
 (find-font-family :family \"Courier New\")
 (find-font-family :family \"Times New Roman\")
@end(code)
"
 (org.shirakumo.font-discovery:family
  (org.shirakumo.font-discovery:find-font :family family)))

(export 'rgb)

(defun rgb (aa rr gg bb)
  "@b(Описание:) функция @b(rgb) возвращает строковое представление цвета.

 @b(Переменые:)
@begin(list)
@iterm(aa = 0..255 яркость;) 
@iterm(rr = 0..255 насыщенность красного;) 
@iterm(gg = 0..255 насыщенность зеленого;) 
@iterm(bb = 0..255 насыщенность синего.) 
@end(list)
"
  (assert (and (<= 0 aa 255) (<= 0 rr 255) (<= 0 gg 255) (<= 0 bb 255)))
  (format nil "#~X~X~X~X" aa rr gg bb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <term> ()
  ((no-enhanced          :initarg no-enhanced          :initform :enhanced)
   (mono/color           :initarg mono/color           :initform :mono)
   (font                 :initarg font                 :initform (find-font-family :family "Times New Roman"))
   (fontscale            :initarg fontscale            :initform 12)
   (linewidth            :initarg linewidth            :initform 0.5)
   (rounded/butt/square  :initarg rounded/butt/square  :initform :rounded)
   (dashlength           :initarg dashlength           :initform 1.0)
   (background           :initarg background           :initform (rgb #xFF #xFF #xFF #xFF))
   (size                 :initarg size                 :initform '(15 10))
   (size-units           :initarg size-units           :initform :cm :documentation "По умолчанию - пиксели|пиксели=72*in|пиксели=72/25.4*cm")
   ))

(defclass term-pdfcairo (<term>)
  ()
  (:documentation
   "
 @b(Пример использования:)
@begin[lang=gnuplot](code)
 set term pdfcairo
             {{no}enhanced} {mono|color}
             {font <font>} {fontscale <scale>}
             {linewidth <lw>} {rounded|butt|square} {dashlength <dl>}
             {background <rgbcolor>}
             {size <XX>{unit},<YY>{unit}}
@end(code)
"))

(defclass term-pngcairo (<term>)
  ((no-transparent       :initarg no-transparent       :initform :no-transparent)
   (no-crop              :initarg no-crop              :initform :no-crop)
   (pointscale           :initarg pointscale           :initform 1.0))
  (:documentation 
   "
 @b(Пример использования:)
@begin[lang=gnuplot](code)
 set term pngcairo
             {{no}enhanced} {mono|color}
             {{no}transparent} {{no}crop} {background <rgbcolor>}
             {font <font>} {fontscale <scale>}
             {linewidth <lw>} {rounded|butt|square} {dashlength <dl>}
             {pointscale <ps>}
             {size <XX>{unit},<YY>{unit}}
@end(code)
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '*term-pdfcairo*)

(defparameter *term-pdfcairo* (make-instance 'term-pdfcairo))

(export '*term-pngcairo*)

(defparameter *term-pngcairo* (make-instance 'term-pngcairo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((term term-pdfcairo) os)
  (format os "~a"
          (with-output-to-string (stream)
            (format stream "set term pdfcairo")
            (block no-enhanced
              (when (eq :enhanced (slot-value term 'no-enhanced))
                (format stream " enhanced"))
              (when (eq :no-enhanced (slot-value term 'no-enhanced))
                (format stream " noenhanced")))
            (block mono/color
              (when (eq :mono (slot-value term 'mono/color))
                (format stream " mono"))
              (when (eq :color (slot-value term 'mono/color))
                (format stream " color")))
            (block font-fontscale
              (when (and (slot-value term 'font) (slot-value term 'fontscale))
                (format stream " font \"~a, ~d\"" (slot-value term 'font) (slot-value term 'fontscale))))
            (when (slot-value term 'linewidth)
              (format stream " linewidth ~a" (slot-value term 'linewidth)))
            (block rounded/butt/square
              (when (eq :rounded (slot-value term 'rounded/butt/square))
                (format stream " rounded"))
              (when (eq :butt (slot-value term 'rounded/butt/square))
                (format stream " butt"))
              (when (eq :square (slot-value term 'rounded/butt/square))
                (format stream " square")))
            (when (slot-value term 'dashlength)
              (format stream " dashlength ~a" (slot-value term 'dashlength)))
            (when (slot-value term 'background)
              (format stream " background ~s" (slot-value term 'background)))
            (block size/size-units
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :in (slot-value term 'size-units)))
                (format stream " size ~ain,~ain"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :cm (slot-value term 'size-units)))
                (format stream " size ~acm,~acm"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size)))))
            (format stream ";~%"))))

(defmethod print-object ((term term-pngcairo) os)
  (format os "~a"
          (with-output-to-string (stream)
            (format stream "set term pngcairo")
            (block no-enhanced
              (when (eq :enhanced (slot-value term 'no-enhanced))
                (format stream " enhanced"))
              (when (eq :no-enhanced (slot-value term 'no-enhanced))
                (format stream " noenhanced")))
            (block mono/color
              (when (eq :mono (slot-value term 'mono/color))
                (format stream " mono"))
              (when (eq :color (slot-value term 'mono/color))
                (format stream " color")))
            (block font-fontscale
              (when (and (slot-value term 'font) (slot-value term 'fontscale))
                (format stream " font \"~a, ~d\"" (slot-value term 'font) (slot-value term 'fontscale))))
            (when (slot-value term 'linewidth)
              (format stream " linewidth ~a" (slot-value term 'linewidth)))
            (block rounded/butt/square
              (when (eq :rounded (slot-value term 'rounded/butt/square))
                (format stream " rounded"))
              (when (eq :butt (slot-value term 'rounded/butt/square))
                (format stream " butt"))
              (when (eq :square (slot-value term 'rounded/butt/square))
                (format stream " square")))
            (when (slot-value term 'dashlength)
              (format stream " dashlength ~a" (slot-value term 'dashlength)))
            (when (slot-value term 'background)
              (format stream " background ~s" (slot-value term 'background)))
            (block no-transparent
              (when (eq :no-transparent (slot-value term 'no-transparent))
                (format stream " notransparent"))
              (when (eq :transparent (slot-value term 'no-transparent))
                (format stream " transparent")))
            (block no-crop
              (when (eq :no-crop (slot-value term 'no-crop))
                (format stream " nocrop"))
              (when (eq :crop (slot-value term 'no-crop))
                (format stream " crop")))
            (block pointscale
              (when (slot-value term 'pointscale)
                (format stream " pointscale ~a" (slot-value term 'pointscale))))
            (block size/size-units
              (when (and (slot-value term 'size)
                         (null (slot-value term 'size-units)))
                (format stream " size ~a,~a"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :in (slot-value term 'size-units)))
                (format stream " size ~ain,~ain"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size))))
              (when (and (slot-value term 'size)
                         (slot-value term 'size-units)
                         (eq :cm (slot-value term 'size-units)))
                (format stream " size ~acm,~acm"
                        (first (slot-value term 'size))
                        (second (slot-value term 'size)))))
            (format stream ";~%")
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod output ((term term-pdfcairo) (f-name string) os)
  (format os "~a" 
          (with-output-to-string (stream)
            (format stream "set output '~a'" (file-name f-name "pdf")))))

;;;; (get-output-stream-string stream) (format os "~a" )

(defmethod output ((term term-pngcairo) (f-name string) os)
  (format os "~a" 
          (with-output-to-string (stream)
            (format stream "set output '~a'" (file-name f-name "png")))))

;;;; (get-output-stream-string stream) (format os "~a" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format nil "~a" *term-pngcairo*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

