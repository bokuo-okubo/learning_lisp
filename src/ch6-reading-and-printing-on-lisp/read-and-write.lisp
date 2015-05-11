(print "foo")

(defun say-helo ()
  (print "please type your name:")
  (let ((name (read)))
    (print "Nice to meet you,")
    (print name)))
