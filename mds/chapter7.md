#Chapter7 Going Beyond Basic Lists

###Converting Node Identifiers

when converting nodes into DOT format, the first thing we need to do is to convert the node identifiers into valid DOT identifiers. We do this by writing a dot-name function:

```
(defun dot-name(exp)
	(substitute-if #\_ (complement #'alphanumericp) (print-string exp)))
```

