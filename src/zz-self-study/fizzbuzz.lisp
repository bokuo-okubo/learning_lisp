(defun iota (current limit step)
  (if (> current limit)
      nil
      (cons current (iota (+ current step) limit step))))

(defun fizzbuzz(n)
  (cond ((zerop (mod n 15)) "FizzBuzz")
	((zerop (mod n 3)) "Fizz")
	((zerop (mod n 5)) "Buzz")
	(t                 n)))

(defun fizzbuzzing (list)
  (if list
      (progn (princ (fizzbuzz (car list)))
	     (fresh-line)
	     (fizzbuzzing (cdr list)))
      (fresh-line)))

(fizzbuzzing (iota 1 100 1))
