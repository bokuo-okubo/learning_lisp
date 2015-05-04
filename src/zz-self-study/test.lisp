(defun init()
  (defparameter *small* 1)
  (defparameter *big* 100))
(defun guess-my-number()
  (ash (+ *small* *big*) -1))
(defun smaller()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))
(defun bigger()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))
(defun exact()
  (init))


(defparameter *arc-enemey* nil)
(defun pudding-eater(person)
  (case person
    ((henry) (setf *arc-enemey* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((johny) (setf *arc-enemey* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise '(why you ate my pudding stranger ?))))

(defun pudding-eater2(person)
  (cond ((eq person 'henry) (setf *arc-enemey* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((eq person 'johny) (setf *arc-enemey* 'useless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(t  '(why you ate my pudding stranger ?))))

(princ
 (if (member 1 '(3 4 1 5))
    "one is in the list"
    "one is not in the list"))

(member 1 '(3 4 1 5))
;; -> (1 5)

;;like this code is more buetiful
;; (if *file-modified*
;;     (if (ask-user-about-saving)
;;	        (save-file)))
