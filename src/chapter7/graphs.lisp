;; visualizeing graphs

(defparameter *wizard-nodes* '((living-room (youare in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					ther is a well in fromt of you.))
			       (attic (you are in the attic. ther
				       is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
				(attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))


;; converting to DOT file.
(defun dot-name(exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; a node in DOT format can contain only leters, digits, and the underscore character. To make sure the node
;; identifier we're using is legal, we'll change any forbidden characters to underscores. Here are examples of the dot-name function in use.


;; adding labels to graph nodes.

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))


;; Generating the DOT Information for the Nodes.

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

;; Generating All the DOT data

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; Turning the DOT FILE into a Picture
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -0 " fname)))

;; Using Thunks
;; It is common in Lisp to create small functions that have zero arguments.
;; These functions are officially called nullary functions.


;; Writing to a File
(with-open-file (my-stream
		 "testfile.txt"
		 :direction :output
		 :if-exists :supersede)
  (princ "hello file!" my-stream))

;; Creating a Stream
