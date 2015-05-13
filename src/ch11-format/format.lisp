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
