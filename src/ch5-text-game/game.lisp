#|
  CHAPTER5
　３つのノードが存在する世界を行き来できるようなゲームを作ることを考える。
  - Required spec
   1. 周囲を見渡す
   2. 別の場所に移動する
   3. オブジェクトを拾う
   4. 拾ったオブジェクトで何かする
|#

;;## 連想リストを使って景色を描写する
(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizerd is snoring loudly on the couach.))
			(garden (you are in a beautiful garden.
				there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

;; トップレベル変数*nodes* は、３つの場所、及びそれぞれの記述のリスト。
;; *nodes*変数はキーとなる名前に結び付けられたデータを探し出すための構造といってもいい。
;; このような構造は連想リスト[association list]あるいは縮めてalistと呼ばれている。
;; alist の定義 ((key1 data1) (kay2 data2))
;; assocと対応して。キーとデータの対リスト、のリスト。がalist。assocはリスト全体からkeyに対応するリストを返す。 なければnilを返す。

;; Lisperは可能な限り、データをシンボルのままで扱いたい。


;;## 情景を描写する
;; assoc関数 -> リストの中からキーを下に欲しい要素を抜き出す
(defun describe-location(location nodes)
  (cadr (assoc location nodes)))
;; describe-locationで直接*node*変数を参照しないのはなぜだろうか
;; この関数は「関数型プログラミングスタイル」で書かれているから。
;; -> 関数は引数か、関数内で宣言された変数しか参照せず、また値を返す以外の動作をしない。

;;---------------------------------------------------------------------------------------
;;## 通り道を描写する
(defparameter *edges* '((living-room (garden west door)
			             (attic apstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs laddar))))
(defun describe-path(edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
;; 準クォート
#|
- データの一部に計算された情報を埋め込む仕組み。
` (バッククオート)でそのデータモードになったデータ列は、コードモードの埋め込み可能状態になり、
, (カンマ) 直後のリストはコードして解釈される。
|#


;;## 通り道を一度に描写する
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))
#|
一番内側 (assoc location edges) は簡単。 locationをキーにして、edgesからassoc関数で描写を抜き出す
> (cdr (assoc 'living-room *edges*))
((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDAR))

次に、得られたエッジからその描写を得る。
> (mapcar #'describe-path '((garden west door) (attic upstairs laddar)))
((THERE IS A DOOR GOING WEST FROM HERE.)
 (THERE IS A LADDAR GOING UPSTAIRS FROM HERE.))

- mapcar関数
引数に他の関数とリストを受け取って、
リストの要素の一つ一つについてそれを引数として受け取った関数を呼び出す。
> (mapcar #'sqrt '(1 2 3 4 5))
(1 1.4142135 1.7320508 2 2.236068)

#' function オペレータの略記法
Lispリーダは、この記号を含む式を評価すると、内部的に次の形に変換する
> (mapcar #'car '((foo baz) (baz qux)))
>>>> (mapcar (function car) '((foo baz) (baz qux)))
(foo baz)

☆☆
common lispでは、関数を値として扱う場合にfunctionオペレータを使ってそのことを明治しないとならない。
関数と変数で名前が衝突した場合にエラーを引き起こす場合があるから。
schemeは名前空間一緒。-> LISP1

- 描写を結合する
mapcarによって全てのエッジの記述のリストが得られたら、それを一つの描写にする必要がある。
いくつかのリストをつなげて一つのリストにするappend関数を使えばそれが実現できそうだ。

> (append '(mary had) '(a) '(little bomb))
(MARY HAD A LITTLE BOMB)

apped関数を使えば、通り道の描写のリストを一気に全体の描写へまtもめてしまうことができる。
しかし、appendには、くっつけたいリストを一つづづ、別々のリストとして渡さなければならない。

そこで
- apply関数
apply関数に関数とリストを渡すと、あたかもそのリストの各要素を引数として関数を呼び出したかのように動作する。

> (apply #'append '((mary had) (a) (little bomb)))
(MARY HAD A LITTLE BOMB)

----
desciribe-pathでみたような高階関数を多用するスタイルはLISPで頻繁に用いられる。

|#
;;---------------------------------------------------------------------------------------

;; ## 特定の場所にあるオブジェクトを描写する
;; オブジェクトのリスト
(defparameter *objects* '(whiskey bucket frog chain))

;; オブジェクトとその場所を格納する変数
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))
;; 与えられた場所から見えるオブジェクトのリストを返す関数
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj) ;; 局所関数
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;; 2: labelコマンドを用いてat-loc-pという局所関数を定義
;; 3: at-loc-p関数はオブジェクトの名前を表すシンボルを取り、それが場所locにあるかどうかを t か nil で返す。
;; -> 関数がnilか真かの値を帰す場合、関数の名前の最後にpをつける、という慣習がある。
;;    数値5が奇数かどうかを調べるのは、(oddp 5)という具合。
;;    真偽値を確かめる関数は述語(predicate)と呼ばれるから。
;; 4: remove-if-not カンスは、渡されたリストの各要素に第一引数の関数を適用し、それが真の値を返さなかったものを除いたリストを得もの。 at-loc-pが真を返すような要素だけを選び出すフィルタみたいなもん。

;;ある場所で見えるオブジェクト
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))
;; 2: 局所関数describe-obj関数を作っている
;; 3: この関数は与えられたオブジェクトが床にある、という文を、準クォートを使って創りだす。
;; 4: そして関数の本体では、現在の場所にある尾ぬジェクトをobjects-at関数を使って見つけ、そのオブジェクトのリストに対してdescribe-objをマップして、最後にappendで全ての描写をつなげて一つのリストにしている。



;; 現在地を保持する変数
(defparameter *location* 'living-room)

;; 見えるもの全てを描写する
(defun look()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; ゲーム世界を動き回る
(defun walk(direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
	(if next
	    (progn (setf *location* (car next))
		   (look))
	    '(you cannot go that way.))))
;; キーワード引数について
;;シンボルy をcadrに持つような最初の要素をリストから探し出す

;; (princ
;;  (find 'y '((5 x) (3 y) (7 z)) :key #'cadr))

;; オブジェクトを手に取る
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t       '(you cannot get that.))))

;; member関数はリストの中に要素があるかどうかを検査するのに使える。
;;オブジェクトが現在地にあれば、pushコマンドを使ってオブジェクトと新しい場所からなるリストを *object-locations*
;;に付け足す。

;; (push *foo*) == (setf *foo* (cons 7 *foo*))
;; assocコマンドは、常に見つかった最初のエントリを評価する
;; したがって、pushコマンドを使うと、assocにとってはそのオブジェクトに対する場所が更新されたのと同じ効果を持つ

;; 持っているものを調べる
(defun inventory()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; ゲーム世界を見回すにはlookコマンド
;; 場所を移動するにはwalk
;; ものを取るにはpickupコマンド
;; 今持っているものを表示するにはinvestoryコマンド

;; 準クオートを使えば大きなデータの中に、その一部分を計算するためのコードを埋め込むことができる

;; Adding a custom interface to Our game engine.

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

;; Writing custom function
(defun game-read()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")" ))))
  (flet ((quote-it (x)
	   (list 'quote x)))
    (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; game-eval, allows only certain commands to be called

(defparameter *allows-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allows-commands*)
      (eval sexp)
      '(i do not know that command)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	     (lit (cons item (tweak-text rest nil lit)))
	     ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	     (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
