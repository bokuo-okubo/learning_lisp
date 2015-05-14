#|
現代のプログラミングにおいても、テキストの扱いは重要だ。
Common Lispは非常に高機能なテキスト表示関数を備えている。

CLで一番重要かつ高機能なテキスト表示関数がformatだ。
|#

;; 11.1  format関数の呼び出し方

(format t "Add onion rings for only ~$ dollars more!" 1.5)
#|
> (format t "Add onion rings for only ~$ dollars more!" 1.5)
Add onion rings for only 1.50 dollars more!
NIL

## (format 出力先 "制御文字列 ~$ <- 制御シーケンス" 値引数)

###出力先
format関数が生成するテキストをどこに送るか、という出力先を示す
許される値は次のとおり

nil : 出力するかわりに、生成されたテキストを文字列として返す
t : 結果をコンソールに出力する(この場合返り値はNIL)
stream : データを出力ストリームに書き出す


|#

(princ (reverse
	(format nil "Add onion rings for only ~$ dollars more!" 1.5)))
#|
!erom srallod 05.1 ylno rof sgnir noino ddA
"!erom srallod 05.1 ylno rof sgnir noino ddA"
|#


#|
(format t "I am printing ~s in the middle of this sentence." "foo")
I am printing "foo" in the middle of this sentence.
NIL
CL-USER> (format t "I am printing ~a in the middle of this sentence." "foo")
I am printing foo in the middle of this sentence.
|#


#|
(format t "I am printing ~10a ehit in ten spaces of room." "foo")
I am printing foo        ehit in ten spaces of room.
NIL
CL-USER> (format t "I am printing ~10@a ehit in ten spaces of room." "foo")
I am printing        foo ehit in ten spaces of room.
NIL
|#

#|
(format t "I am printing ~,,4a ehit in ten spaces of room." "foo")
I am printing foo     ehit in ten spaces of room.
NIL
|#


;;  (format t "The word ~,,4,'!a feels very important." "foo")
;; The word foo!!!! feels very important.
;; NIL


;; 数値を整形するシーケンス

;; 整数の整形

;; formatは色々な基数で数値を表示できる。

;; １６進数 ~x
;; (format t "the number 1000 in hexadecimal is ~x" 1000)
;; the number 1000 in hexadecimal is 3E8
;; NIL

;; ２進数 ~b
;; (format t "the number 1000 in hexadecimal is ~b" 1000)
;; the number 1000 in hexadecimal is 1111101000
;; NIL

;; 十進数を明示
; No value
;; CL-USER> (format t "the number 1000 in hexadecimal is ~d" 1000)
;; the number 1000 in hexadecimal is 1000
;; NIL

;; 汎用的な~aを使っても同じ出力が得られるだろう、しかし~dを使うと、数値の整形に特有のパラメータやフラグを追加できる。


;; コロンを制御シーケンスを入れると、コンマによって桁をグループ化してくれる
;; (format t "numvers with  commaqs in them are ~:d times better." 1000000)
;; numvers with  commaqs in them are 1,000,000 times better.
;; NIL

;; 11.4 複数行出力
;; common lispで表示中に新しい行を始めるには、２つの方法がある。
;; terpriコマンドは現在の行を終了して、続く出力が新たな行に現れるようにするコマンドだ。
;; 例えば二打の数値をそれぞれ独立した行に表示したければ、次の通りに書ける。

;; (progn (princ 22)
;; 		(terpri))
;; 22
;; NIL
;; CL-USER> (progn (princ 22)
;; 		(terpri)
;; 		(princ 23))
;; 22
;; 23
;; 23
;; CL-USER> (progn (princ 22)

;; 		(princ 23))
;; 2223
;; 23
;; CL-USER> (progn (princ 22)
;; 		(fresh-line)
;; 		(fresh-line)
;; 		(princ 23))
;; 22
;; 23
;; 23
;; CL-USER> (progn (princ 22)
;; 		(fresh-line)
;; 		(princ 23))
;; 22
;; 23
;; 23
;; CL-USER>

;; formatコマンドは、terpri, fresh-lineそれぞれに対応する２つの制御シーケンスを備えている。

;; ~% は常に改行
;; ~& は必要なときに改行


;; (progn (format t "this is on one line ~%")
;; 		(format t "~%this is on one line"))
;; this is on one line

;; this is on one line
;; NIL
;; CL-USER> (progn (format t "this is on one line ~&")
;; 		(format t "~&this is on one line"))
;; this is on one line
;; this is on one line
;; NIL



;; (progn (format t "this is on one line ~&")
;; 		(format t "~5&this is on one line"))
;; this is on one line




;; this is on one line
;; NIL


;; 11.5テキストを揃える

;; formatコマンドには、テキストを揃えるための機能も沢山備わっている。
;; 例えばテーブルを作ったり、センタリングしたりする制御シーケンスがある。

;; テキストを揃える機能を例で理解するために、まず文字数の異なる動物名をランダムに返す関数を作ろう

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

;; 次に、ランダムな動物名を表にしてみる。
;; 表を作るには、~t 制御シーケンスが便利だ。
;; この制御シーケンスのパラメタは、整形後のテキストが現れるべき絡む位置だ。
;; ここでは、動物名を5 15 25カラム目から、3列で表示しよう。

(loop repeat 10
   do (format t "~5t~a ~15t~a ~25t~a~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

     ;; dog       dog       tiger
     ;; tick      tiger     dog
     ;; walrus    dog       kangaroo
     ;; walrus    walrus    tiger
     ;; dog       kangaroo  walrus
     ;; dog       dog       walrus
     ;; tick      tick      tick
     ;; walrus    tiger     walrus
     ;; tiger     tiger     tick
     ;; kangaroo  tiger     dog

;; 一行の中で動物名がなるべく等しい距離をとって表示されるようにしてみる。
;; このためには、~< ~> 制御シーケンスを次のとおりに使う。

(loop repeat 10do (format t "~30<~a~;~a~;~a~>~%"
			    (random-animal)
			    (random-animal)
			    (random-animal)))


;; > (loop repeat 10do (format t "~30<~a~;~a~;~a~>~%"
;; 			    (random-animal)
;; 			    (random-animal)
;; 			    (random-animal)))

;; tick     kangaroo     kangaroo
;; walrus       tick       walrus
;; dog          tiger         dog
;; tick       kangaroo      tiger
;; dog        kangaroo       tick
;; kangaroo      walrus      tick
;; tiger      kangaroo      tiger
;; dog        dog        kangaroo
;; tick        tiger       walrus
;; walrus      tiger     kangaroo
;; NIL

#|
> (loop repeat 10do (format t "~30<~a~;~a~;~a~>~%"
			    (random-animal)
			    (random-animal)
			    (random-animal)))

~30< 文字揃えの開始 全体で30文字分の幅
~a ３つの値を表示する
~;新たに揃える値を始める

|#

;; 11.6 繰り返しの制御シーケンス

(defparameter *animals* (loop repeat 10 collect (random-animal)))
