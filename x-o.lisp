;;;; x-o.lisp

(in-package #:math)

(defclass x-o (matrix) ())

(defmethod matr-name-* ((mm x-o)) "X-O")

(defmethod print-object ((mm x-o) s)
  (format s "~A " (matr-name-* mm))
  (when (and (matrix-data mm) (arrayp (matrix-data mm)))
    (format s "~{~A~^х~}" (array-dimensions (matrix-data mm)))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do
	 (format s "~%[")
	 (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	    :do (format s " ~A " (aref (matrix-data mm) i j)))
	 (format s "]"))))

(defmethod initialize-instance ((mm x-o) &key (rows 3) (cols 3) )
  (setf (matrix-data mm) (make-array (list rows cols) :initial-element 0)))

(defmethod x-o-reset ((mm x-o))
  (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
     :do
     (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	:do (setf (aref (matrix-data mm) i j) 0))))

(defparameter *xo* (make-instance 'x-o ))
  
;;;;;

(defun mate-value (value lst)
  (every #'(lambda (el) (eq value el)) lst))

(defun check-value (value lst)
  (let ((v-y (count-if     #'(lambda (el) (= value el)) lst))
	(v-n (- (count-if-not #'(lambda (el) (= value el)) lst)
		(count-if     #'(lambda (el) (= 0     el)) lst))))
    (and (<= v-n 0) (= v-y 2))))

(defun half-check-value (value lst)
  (let ((v-y (count-if     #'(lambda (el) (= value el)) lst))
	(v-n (- (count-if-not #'(lambda (el) (= value el)) lst)
		(count-if     #'(lambda (el) (= 0     el)) lst))))
     (and (<= v-n 0) (= v-y 1))))


(defmethod x-o-lines ((mm x-o))
  (append
   (loop :for r :from 0 :below (matr-rows-*  mm)
      :collect (matr-get-row-* mm r))
   (loop :for c :from 0 :below (matr-cols-*  mm)
      :collect (matr-get-col-* mm c))
   (list (major-diagonal mm) (minor-diagonal mm))))

(defmethod x-o-winp ((mm x-o) player)
  "Определяет для расстановки x-o выиграл-ли игрок player"
  (let ((lst (mapcar #'(lambda (el)
		     (mate-value player el) )
		 (x-o-lines mm))))
    (some #'(lambda (el) el) lst)))


(defmethod x-o-check-num ((mm x-o) player)
  "Определяет количество шахов"
  (let ((lst (mapcar #'(lambda (el)
			 (check-value player el) )
		     (x-o-lines mm))))
    (count t lst)))

(defmethod x-o-1/2-check-num ((mm x-o) player)
  "Определяет количество полушахов"
  (let ((lst (mapcar #'(lambda (el)
			 (half-check-value player el) )
		     (x-o-lines mm))))
    (count t lst)))



(defmethod free-fields ((mm x-o))
  "Возвращает свободные поля"
  (let ((rez nil))
    (loop :for i :from 0 :below (array-dimension (matrix-data mm) 0)
       :do (loop :for j :from 0 :below (array-dimension (matrix-data mm) 1)
	      :do (when (= 0 (aref (matrix-data mm) i j))
		    (push (list i j) rez))))
    rez))


(defmethod game-over-p ((mm x-o))
  "Возвращает статус игры"
  (let* ((first-winner (x-o-winp mm 1))
	 (second-winner (x-o-winp mm 2))
	 (game-error (and first-winner second-winner)))
    (cond
      (game-error              (format t "~%game-error: player-1 and player-2 both are winners!?") t)
      (first-winner            (print mm) (format t "~%player-1 win!") t)
      (second-winner           (print mm) (format t "~%player-2 win!") t)
      ((null (free-fields mm)) (print mm) (format t "~%Game over. No winners!") t)
      (t                  nil))))

(defun get-random-element (lst) (when lst (nth (random (length lst)) lst)))

(defmethod step-check-rule-back (rule (mm x-o) player r c)
  (let ((rez (funcall rule (matr-set-ij-* mm player r c) player)))
    (matr-set-ij-* mm 0 r c)
    (list rez r c)))

(defmethod check-rule (rule chek-for-payer (mm x-o) move-player r c)
  (let ((rez (funcall rule (matr-set-ij-* mm move-player r c) chek-for-payer)))
    (matr-set-ij-* mm 0 r c)
    (list rez r c)))

(defmethod map-check-rule (rule check-for-payer (mm x-o) move-player)
  (mapcar
   #'(lambda (el)
       (check-rule rule check-for-payer mm move-player (first el) (second el)))
   (free-fields mm)))

(defmethod map-step-check-rule-back (rule (mm x-o) player)
  (mapcar
   #'(lambda (el)
       (step-check-rule-back rule mm player (first el) (second el)))
   (free-fields mm)))

(defmethod may-win-next-step ((mm x-o) player)
  (let* ((rez (map-step-check-rule-back #'x-o-winp mm player ))
	   (win-coords nil ))
      (mapcar #'(lambda (el) (when (first el) (push (cdr el) win-coords))) rez)
      win-coords))

(defun other-player (player) (case player (1 2) (2 1)))

(defparameter *player* 1)

(defun next-player ()
  (setf *player* (other-player *player*)))

(defun collect-as-first-rank (triple-lst)
  (when triple-lst
    (let ((key (first (first triple-lst))))
      (loop :for i :in triple-lst
	 :when (= key (first i ))
	 :collect i))))

(defun rank-union (triple-lst-1 triple-lst-2)
  (union triple-lst-1 triple-lst-2 :key #'cdr :test #'equal))

(defun rank-intersection (triple-lst-1 triple-lst-2)
  (intersection triple-lst-1 triple-lst-2 :key #'cdr :test #'equal))

(defmethod move ((mm x-o) player)
  (unless (game-over-p mm)
    (let* ((may-win       (may-win-next-step mm player))
	   (min-check     (collect-as-first-rank
			   (sort (map-check-rule 'x-o-check-num (other-player player) mm player ) #'< :key #'first)))
	   (min-1/2-check (collect-as-first-rank
			   (sort (map-check-rule 'x-o-1/2-check-num (other-player player) mm player ) #'< :key #'first)))
	   (max-check     (collect-as-first-rank
			   (sort (map-check-rule 'x-o-check-num player mm player ) #'> :key #'first)))
	   (max-1/2-check (collect-as-first-rank
			   (sort (map-check-rule 'x-o-1/2-check-num player mm player ) #'> :key #'first)))
	   (mv
	    (cdr (cond
		   (may-win (cons t (get-random-element may-win)))
		   ((get-random-element (reduce #'rank-intersection (list min-check min-1/2-check max-check max-1/2-check))))
		   ((get-random-element (reduce #'rank-intersection (list min-check min-1/2-check max-check))))
		   ((get-random-element (reduce #'rank-intersection (list min-check min-1/2-check))))
		   ((get-random-element min-check)))))
	   (row (first  mv))
	   (col (second mv)))
      (matr-set-ij-* mm player row col))))

(defun xo-dialog (mm)
  (do ((rr nil) (done nil))
      (done)
    (if (game-over-p mm) (x-o-reset *xo*)
	(progn
	  (print mm)
	  (format t "~%Current player - ~A" *player*)
	  (format t "~%[help reset pl pl1 pl2 exit]<player row col>:")
	  (game-over-p mm)
	  (setf rr (read-line))
	  (print rr)

	  (cond
	    ((string=  rr "help")
	     (format t"
help   - Вывод настоящй справки;
reset  - Перезапуск игры;
pl     - Программа выполняет допустимый ход за текущего игрока;
pl1    - Программа выполняет допустимый ход за первого игрока;
pl2    - Программа выполняет допустимый ход за второго игрока;
exit   - Выход из игры
player - 1 или 2;
row    - [0..2] строка, в которую помещается ход игрока player;
col    - [0..2] столбец, в который помещается ход игрока player."))
	    ((string=  rr "exit")  (setf done t))
	    ((string=  rr "reset") (x-o-reset mm))
	    ((string=  rr "pl")    (move mm *player*) (next-player))
	    ((string=  rr "pl1")   (move mm 1)    (setf *player* 2))
	    ((string=  rr "pl2")   (move mm 2)    (setf *player* 1))
	    (t (apply
		'matr-set-ij-*
		(cons mm
		      (eval
		       (read-from-string (concatenate 'string "(list " rr ")")))))
	       (next-player)))))))

(export 'play)
(defun play ()
  (x-o-reset *xo*)
  (setf *player* 1)
  (xo-dialog *xo*))
