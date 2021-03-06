#|
7.1 奇妙なリスト

Lispのリストはコンスセルから作られる。
コンスセルは２つのデータを結びつける構造。
リストの最期のコンスセルの右のスロットはnilになっている。

|#

'(1 2 3)
;と
(cons 1 (cons 2 (cons 3)))
;は等価。



;; ドットリスト
(cons 1 (cons 2 3))
;; -> (1 2 . 3)
#|
正式なリストの最期にあるべきnilが見つからなかったことを示すため、
lispは最期の要素の前にドットをおいて表示した。
このドットによって、Lispは「君がくれた構造をリストとして表示しようとしたけれど、リストの一番最後にnilじゃなくて3を見つけたんだ」
といっているわけ


ドット表記のもう一つの考えかたは、それを単にconsコマンドのデータモードで使われる別表記とかんがえること
実際、わざわざ面倒なことをしたければ、通常のリストを次の通りドット表記で買っくことだってできる
|#

'(1 . (2 . (3 . nil)))
;; -> (1 2 3)

;; この考え方で行けば、どっとリストのドットは、lispがリストを表示する際に、最後のコンスセルだけを明示する必要にかられてつけたものだともいえる。

;; ######################################################
;; 対
;; ######################################################

;; lispプログラムで、よく使われる実用的なドットリストは、対を簡潔に表現するためのものだ。
;; 例えば、数２と数３の対を表現したいとしよう。
;; 一つの方法は２つの数をコンスすること

(cons 2 3)
;; -> (2 . 3)

#|
この表記は、lispでは便利かつ効率的。
便利なのは、この対から値を取り出すのに、標準のcarとcdrが使えるから。また、ふたつの要素をつなぐのに一つだけコンスセルをアロケートすればよいので、比較的効率がよい。

この形の対はLispプログラムではよく使われる。
二次元の点のｘ座標とｙ座標、複雑なデータの中のキーと値のつい、等だ。後者の例は、これから連想リストについて説明するときに出てくる。
|#

;;循環リスト

;; 連想リスト 別名alist
;; alistはキーと値の対のリストだ。

#| 慣習的に、リスト中に同じキーが複数現れている場合、最初のものだけを有効とみなすことが多い
次の例はBill, Lisa, Johnのコーヒーを注文を表現するalistだ。
|#

;; > (assoc 'lisa *drink-order*)

;; この関数はりすとの最初から順にマッチするキーを探し、見つかればキーと値の対を返す。
;; ここで、貴方が注文をと入り終えて戻ろうとした時に、Lisaが注文を変更したらどうなるか。
;; push関数を使って彼女の注文を変更することができる。

;; > (push '(lisa . large-mocha--with-whipped-cream) *deink-order*)


;; 木構造のデータの可視化

(defparameter *house* '((walls (mortar (cement)
				       (water)
				       (sand))
			 (bricks))
			(windows (glass)
			 (frame)
			 (curtains))
			(roof (shingles)
			 (chimeny))))

#|
この構造は、家を構成する部品の階層構造を綺麗に反映している。
lispの式の構文を使っているので、階層構造の各レベルがリストで表現されている。
また、各リストの先頭にシンボルを置くという慣習にも習っている。

けれども、木構造より複雑なデータを扱い始めると、
たとえコンスセルに夜データの表現自体は簡単であっても、S式からデータの構造を見て取るのはむずかしくなる。
数学的なグラフ構造をS式で氷原することを考えてみよう。
あるノードが任意のノードとエッジで接続しているようなグラフは、
コンピュータプログラムで適切に回sかするのがとてもむずかしい。

>>> graph.lispへ
|#
