(defstruct person
  name
  age
  waist-size
  favorite-color)


(defparameter *bob* (make-person :name "bob"
					  :age 35
					  :waist-size 32
					  :favorite-color "blue"))
;; *BOB*
;; make-personの定義はdefstructが自動的に行ってくれる

;; > *bob*
;; #S(PERSON :NAME "bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")

#|
> (person-age *bob*)
35
> (setf (person-age *bob*) 36)
36
setfと一緒に使って、年齢の変更にもつかえる！

> (defparameter *that-guy* #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue"))
*THAT-GUY*
> (person-age *that-guy*)
35

personの出力表記をそのまま読み込んで、personインスタンスにするkとができる。
これも、LIspにおける入力と出力の対称性の良い例だ！

mutableなデータと構造体は相性がいい

|#


;; 9.4 データをジェネリックに扱う

;;シーケンスを使う
#|
引数のデータ型によらず動作するコードを書く一番簡単な方法は、データ型のチェックを他の誰かにやってもらうことだ。
Common Lispのライブラリには、もともと様々なデータ型を受け取ってジェネリックな処理をする関数が揃っている。

最もよく使われるジェネリックな関数はシーケンス関数だ。

■ シーケンス関数は、Lispで値の列(シーケンス)を現す３つの主要なデータ型、リスト、配列、文字列を統一的に扱うことができる。

> (length '(a b c))
3
> (length "blub")
4
> (length (make-array 5))
5
|#


;; 探索のためのシーケンス関数
;; シーケンス関数のの中には、シーケンスから何かを探し出すためのものがいくつかある。
#|
- find-ifは与えた述語を満たす最初の要素を見つける。
- countは特定の要素がいくつシーケンス中にあるかを数える。
- positionは特定の要素がシーケンスのどの位置にあるかを教えてくれる。
- some と every はそれぞれ、シーケンス中に条件を満たす要素が最低一つあるか、あるいはシーケンス中の要素が全て条件を満たすか、を調べる。

(find-if #'numberp '(a b 5 b))
5
> (count #\s "mississippi")
4
> (position #\4 "2kewl4skewl")
5
> (some #'numberp '(a b 5 d))
T
> (every #'numberp '(a b 5 d))
NIL

|#

;; シーケンスの要素について繰り返す関数

;; (reduce #'+ '(3 4 6 5 2))
;; -> 全ての値を単一の値へと蒸留するのにつかえる。

#|

■ リストの中から最大の偶数を選びだす。
(reduceに渡す関数は、(縮約関数 とも呼ばれる))
---
> (reduce (lambda (best item)
		   (if (and (evenp item) (> item best))
		       item
		       best))
		 '(7 4 6 5 2)
		 :initial-value 0)
6
# (reduce #'func :initial-value val)
----
この例で、
reduceにw足して値を蒸留するのに使っているのは、２つの値を引数にとる関数だ。
最初の引数はそれまでに見つけた最良の値、つまりそれまでで最大の偶数で、二番目の引数がリストの次の値だ。

この関数は新たな最良値を返す。
つまり、リストの次の値がこれまでの最良値よりも良い値であればそれを返し、
ソウでなければこれまでの最良値をそのまま返す.

なにも指定しないと、シーケンスの最初の値が初期値として使われるのであった。
それで困る場合は、キーワード引数 :initial-value で初期値を与えることができる。

--


reduceに初期値を与えることはしばしば必要になる。
上の例では、初期値を与えないと、たまたま大きな奇数の値がリストの先頭に会った場合に、それを誤って最大の偶数地として返してしまう可能性がある。
|#


#|
■ ジェネリックな加算関数

> (defun sum (1st)
	   (reduce #'+ 1st))
SUM
> (sum '(1 2 3))
6
> (sum (make-array 5 :initial-contents
			  '(1 2 3 4 5)))
15

> (sum "blablabla")
+: #\b is not a number
   [Condition of type SIMPLE-TYPE-ERROR]
|#

;; map関数
;; リスト以外のものにもジェネリックに使える、という。mapcarとおんんあじ。
;; 返り値としてどのシーケンス型の値を返すか、という引数をとる。


#|
(map 'list
     (lambda (x)
       (if (eq x  #\s)
	   #\S
	   x))
     "this is a string")
(#\t #\h #\i #\S #\Space #\i #\S #\Space #\a #\Space #\S #\t #\r #\i #\n #\g)

(map 'string
     (lambda (x)
       (if (eq x  #\s)
	   #\S
	   x))
     "this is a string")
"thiS iS a String"
CL-USER>


# (map '返り値シーケンス 関数 渡すシーケンス)
|#

;; さらに重要な２つのシーケンス関数

;; # subseq 関数は、始点と終点を指定してシーケンスの一部分を取り出すのに使える。

;; > (subseq "america" 2 6)
;; "eric"

;; # sort 関数は、任意の比較関数を渡してシーケンスをソートする。

;; > (sort '(5 8 2 4 9 3 6) #'<)
;;(2 3 4 5 6 8 9)

;;-----------------------------------------------------------


;; 型熟語を使って自分でジェネリック関数を作る
#|
Common Lispは、他のほどんど全てのLispと同様、動的型付け言語だ。
つまり変数や引数はどんな方のデータでも格納できる。

シンボル、文字列、数値、関数、その他あらゆるデータ型だ。
同じ変数や引数が、
プログラム実行の別々の時点で、異なるかtのデータを持つこともよくある。
|#

#|
型述語を使えば色々な方の引数をジェネリックに取る関数を自分で会っくことがでkる。
例えば、数値同士とリスト同士を「たす」ことができる関数がほしいとしよう。
|#

;; 方法その一


(defun add (a b)
	   (cond ((and (numberp a) (numberp b)) (+ a b))
		 ((and (listp a) (listp b)) (append a b))))
#|
ADD
> (add 3 4)
7
> (add '(a b) '(c d))
(A B C D)
|#

#|
悪いところ
- 全ての方への対応コードがひとつの大きな関数に固まっている
- 新しい型のサポートを追加するのが大変
- 理解しにくい 大きなcondはダルい
- 性能 -> 最適化が利用できない可能性
|#


;; 一つの関数が引数の方に応じて処理を切り替えられるのはとても便利なので、
;; CommonLispはそれぞれの型に特化した複数の関数を同じ名前で定義できる、defmethodコマンドを備えている。
;; その関数が呼ばれると、Lispは自動的に引数の型を調べて、対応する関数本体を呼び出す。
;; このようにコンパイラ／インタプリタが複数の関数本体から引数の方に応じたものを選び出すことは、
;; 型によるディスパッチ (type-dispatch) と呼ばれる。


#|
> (defmethod add ((a number) (b number))
	   (+ a b))
#<STANDARD-METHOD (#<BUILT-IN-CLASS NUMBER> #<BUILT-IN-CLASS NUMBER>)>
> (defmethod add ((a list) (b list))
	   (append a b))
#<STANDARD-METHOD (#<BUILT-IN-CLASS LIST> #<BUILT-IN-CLASS LIST>)>
> (add 3 4)
7
> (add '(a b) '(c d))
(A B C D)


## defmetho関数は、defunと似ているけど、同じ名前で複数の関数が定義できる
defmethodを使うときに、関数引数のところでそれぞれの型を明示できる。
Lispはその方宣言を使ってどちらのadd関数を呼び出すべきかをそのつど判断できる。


|#
