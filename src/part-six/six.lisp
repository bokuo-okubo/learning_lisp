;; part six

;; Saying hello to user

(defun say-hello()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you")
    (print name)))
