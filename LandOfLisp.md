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

- Lisp supports both floating-point numbers and integers. 
- When you write a number, the presence of a decimal point determines whether your number is seen as a floating-point number or an integer. 
- The numbers 1 and 1.0 are two different entities in Common Lisp.

####*Strings*- The last basic building block in Lisp is the string.
-  Although strings aren’t really that fundamental to Lisp from a theoretical standpoint, any program that communicates with a human will usually need strings, because humans like to communicate with text.


###How Lisp Distinguishes Between Code and Data
####Code Mode
- Whenever you type something into the Lisp REPL, the compiler assumes that you’re entering a command you want to execute. 
- In other words, Lisp always assumes that you’re writing code and defaults to code mode.

- When you write lisp as a code, it should be formed as a called 'form'.

```
(#command bla bla bla) ;; form.
```

####Data Mode

- As you might imagine, any stuff written in data mode is treated as data.
- This means the computer will not try to “execute” it, which allows us to have information in our code that’s just plain old data.- Let’s take a look at data mode in action. 
- We’ll enter the same form that we entered in code mode in the previous example, with one difference:

```> '(expt 2 3) 
(expt 2 3)
```

###Lists in Lisp
####Cons Cells
[コンスセル](http://d.hatena.ne.jp/qlisp/20091020/1256023803)

- Constraction of a cell
- car

slice the first symbol of the conscell

- cdr

pop the second symbol of the conscell
