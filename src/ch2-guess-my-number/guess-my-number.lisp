;; Chapter2 数当てゲーム

;; usage read in REPL, and image your guess number from 1 to 100.
;; if game machine print number is bigger than your number, type (bigger)
;; and do same ad small.

(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1)) ;; ash はシフト関数 -1 で右に一つシフトする(半分にする)

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small+ 1)
  (defparameter *big* 100)
  (guess-my-number))
