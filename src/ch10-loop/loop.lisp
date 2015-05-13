;; # 10.1 loopマクロ

(loop for i
   below 5
   sum i)
;10

;; loopの使用例

;; 始点と終点を指定して数える

(loop for i
   from 5
   to 10
   sum i)
; 45

;; リストの要素について繰り返す
(loop for i
   in '(100 20 3)
   sum i)
; 123


;; 繰り返しの中に処理を記述する
(loop for i
   below 5
   do (print i))
#|
0
1
2
3
4
NIL
|#

;; 特定の条件が満たされた時だけ何かする
(loop for i
   below 10
   when (oddp i)
   sum i)
; 25

;; loopを途中で脱出する
(loop for i
   from 0
   do (print i)
   when (= i 5)
   return 'falafel)
#|

0
1
2
3
4
5
FALAFEL
|#

;; 値を集めてリストにする

(loop for i
   in '(2 3 4 5 6)
   collect (* i i))

;(4 9 16 25 36)

;; 複数のfor節を使う
;; loopマクロは複数のfor節を取ることもできる。

(loop for x below 10
   for y below 10
   collect (+ x y))
;(0 2 4 6 8 10 12 14 16 18)


;; loopのネスト
(loop for x below 10
	      collect (loop for y below 10
			 collect (+ x y)))
#|
((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11)
 (3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13) (5 6 7 8 9 10 11 12 13 14)
 (6 7 8 9 10 11 12 13 14 15) (7 8 9 10 11 12 13 14 15 16)
 (8 9 10 11 12 13 14 15 16 17) (9 10 11 12 13 14 15 16 17 18))
|#

;; 添字つき
(loop for i
   from 0
   for day
   in '(monday tuesday wednesday thursday friday saturday sunday)
   collect (cons i day))

#|
((0 . MONDAY) (1 . TUESDAY) (2 . WEDNESDAY) (3 . THURSDAY) (4 . FRIDAY)
 (5 . SATURDAY) (6 . SUNDAY))
|#
