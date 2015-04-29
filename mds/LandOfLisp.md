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

##4 Making Decisions with Conditions

####Using Functions that returns more than Just the truth

- Now let’s look at another benefit of Lisp’s simple way of thinking about true and false.
- *member* command can be used to check for list membership for an item

```
(princ
 (if (member 1 '(3 4 1 5))
    "one is in the list"
    "one is not in the list"))
```

####Comparing Stuff: eq, equal, and More

There’s a lot of beautiful symmetry in Lisp. One part of Lisp that isn’t so beautiful, though, involves the commands for comparing things.

--
**CONRAD'S RULE OF THUMB FOR COMPARING STUFF**

### use EQ to compare SYMBOLS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### use EQUAL for EVERYTHING ELSE !!!!!!!!!!!!!!!!!!!!!!!
--

####The eql command 
is similar to the eq command, but unlike eq, it also handles comparisons of numbers and characters:

```;;comparing symbols > (eql 'foo 'foo)             T             ;;comparing numbers> (eql 3.4 3.4)T;;comparing characters > (eql #\a #\a)T
```

####The equalp command 
is essentially the same as the equal command, except that it can handle some difficult comparison cases with a bit of extra sophisti- cation. For instance, it can compare strings with different capitalizations and can compare integers against floating-point numbers:

```;;comparing strings with different CAPS> (equalp "Bob Smith" "bob smith")T;;comparing integers against floating point numbers > (equalp 0 0.0)T
```


##5 Building a text game engine.


###The Wizard's Adventure Game
* -spec-
	* wecan visit three different locations
		* a living room
		* an attic
		* and a garden
* Players can move betwwn places using the door and the ladder to the attic.

* Think of this game world as a simple directed praph with three nodes and four edges.

* players move betwwn nodes by traveling along the edges in either direction.

### Basic Requirements

Our game code will need to handle a few basic things:

* Looking around
* Walking to diffrent locations
* Picking up objects
* Performing actions on the objects picked up

When looking around in our game world, you will be able to "see" three kinds of things from any locations

* basic scenery
* One or more paths to other locations
* Objects that you can pick up and manipulate

#### Describing the Scenery with an Association List

```
(defparameter *nodes* '((living-room (you are inthe living-room.
				      awizard is snorinf loudly on the couch.))
			(garden (you are in abeautiful garden.
				 there is a well in fromt of you.))
			(attic (you are in the attic.
				ther is a giant welding torch in the corner.))))

```

#### Describing the Location

```
(defparameter *edge* '((living-room (garden west door)

```


###What we've learned

- *Quasiquoting* is a technique that allows you to insert small bits of computer code into larger pieces of data.

- Some Lisp functions accept other functions as arguments.
- There are called *higher-order function*
- The **mapcar** function is the most popular higherorder function in Common Lisp.
k

## 6.  Interacting with the world: Reading and printing in Lisp

