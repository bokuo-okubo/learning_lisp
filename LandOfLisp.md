#Land of Lisp

##3 Exproling the Syntax of Lisp Code

###Syntax

The syntax of a piece of text represents the basic rules that it needs to follow to be a valid sentence.
 Here are some of the rules of sentences in the English language that this text obeys:
 
- The sentence ends in a punctuation mark.
- The sentence contains a subject and a verb.
- The sentence is made up of letters in the English alphabet (as opposed to Egyptian hieroglyphics or Sumerian cuneiform).



###Semantics
- It is usually possible to write a program that has the same semantics in different programming languages; that is, the program will do the same thing in both languages.


###The Building Block of Lisp syntax

- Lisp only has one way of organizing bits of code: It uses parentheses to organize the code into lists.

```
(defun square(n)
	(* n n))
```
- Here, we’ll look at these basic building blocks, or datatypes, you’ll use in Lisp. 

####*Symbols*

*Symbols* are a fundamental type of data in Lisp and used extensively.
A symbol in Lisp is a stand-alone word.

```
> (eq 'fooo 'FoOo) 
 T
```

####*Numbers*

