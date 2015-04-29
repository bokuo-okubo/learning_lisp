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
