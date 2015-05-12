(load "../ch7-graphize-data/graph-util")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30) 􏰂
(defparameter *edge-num* 45) 􏰃
(defparameter *worm-num* 3) 􏰄
(defparameter *cop-odds* 15)

;; Generating Rondom Edges
;; ランダムなNode番号を返すrandom-node関数
;; (random 整数)でランダムにその値域のなかから

(defun random-node ()
  (1+ (random *node-num*))) ;; 0から20ではなく1から30になる

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		     collect (edge-pair (random-node) (random-node)))))
;; Looping with Loop Command

#|
Our make-edge-list function employs the powerful loop command, which can be used to loop over various types of data.
We’ll be looking at loop in detail in Chapter 10.
However, our game uses loop a few times, so let’s consider some simple examples to clarify how it works.

---
> (loop repeat 10
        collect 1)
(1 1 1 1 1 1 1 1 1 1)
---

Within the loop command, we specify how many times to repeat, and then specify an object to collect with every loop (in this case, the number 1).


Sometimes, we want to keep a running count as we’re looping.
We can do this with the following syntax:

---
> (loop for n from 1 to 10
        collect n)
(1 2 3 4 5 6 7 8 9 10)
---

In this example, we are saying that n should loop from 1 to 10.
Then we collect each n and return it as a list.
Actually, we can put any Lisp code in the collect part of the loop.
In the following example, we add 100 as we do our collecting:

---
> (loop for n from 1 to 10
        collect (+ 100 n))
(101 102 103 104 105 106 107 108 109 110)
---


|#


;; Preventing Island.

#|
ランダムにエッジをつくれるようになったが、
ランダムにえらんだ　Nodeをつなぐだけでは、
全てのコンジェくチョンシティが接続されるという保証がない

エッジリストを調べて接続されていないNodeを見つけ出し、孤島をまちの他の場所とつなぐコードをかこう

|#

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))
;;  finds all the edges in an edge list that start from a given node.

(defun get-connected (node edge-list)
  (let ((visitted nil))
    (labels ((traverse (node) ;; 局所関数
	       (unless (member node visitted)
		 (push node visitted)
		 (mapc (lambda (edge) ;;mapcarはリストを返す
			 (traverse (cdr edge)));;mapc はfirstargを実行するだけ
		       (direct-edges node edge-list)))))
      (traverse node))
    visitted))
;; そのnodeから到達可能な全てのnodeのリストをつくる
#|
探索済みのNodeのリストを用意して、
最初のNodeから到達可能なNodeを探していく。
xにない新しいNodeをみつけたら、そのたびに探索済みNodeにpushする
そして、その子ノードをさらにtravarseする

|#

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))
#|
グラフから孤島を探す関数
Nodeのリストの最初のリストをとり、そこにつながっているNodeを全てget-connected関数を使って探し、見つかったNodeを全ノードリストからset-difference関数を使って取り除く。
(set-difference関数は、２つのリストをとり、最初のリストにあって２つ目のリストにないものを取り除いたものを返す)
|#

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;;Building the Final edes for congestion city
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		   collect 1))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test  #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))


(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
		      collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
		       (cond ((eql n wumpus) '(wumpus))
			     ((within-two n wumpus edge-alist) '(blood!)))
		       (cond ((member n glow-worms)
			      '(glow-worms))
			     ((some (lambda (worm)
				      (within-one n worm edge-alist))
				    glow-worms)
			      '(lights!)))
		       (when (some #'cdr (cdr  (assoc n edge-alist)))
			 '(sirens!))))))

;; initializing a new game of grand theft wumpus
(defparameter *player-pos* nil)
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(defun find-empty-node  ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))

;; Drawing a city from partial knowledge
;; これまでに訪れた位置だけを表示するマップ

(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*)
		      (append n '(*))
		      n)
		  (list node '?))))
	  (remove-duplicates
	     (append *visited-nodes*
		     (mapcan (lambda (node)
			       (mapcar #'car
				       (cdr (assoc node
						   (cdr *congestion-city-edges*)))))
			     *visited-nodes*)))))
;; Known edge
;; まだ訪れていない道にいる警官のサイレンの情報を取り除いたエッジのalist

(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*)
				     x
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*)))))
	  *visited-nodes*))

;; mapcan関数
#|
mapcan関数は、mapcarと違い、**渡す関数は必ずリストを返さなければならない**
mapcanは返されたリストを全てつなぎあわせたリストを返す。
入力リストの項目と出力リストの項目が必ずしも1対1対応するとは限らない場合に便利な関数。

例えば、3種類のバーガーを売っているハンバーガーショップがあるとしよう。
メニューは、バーガー,、ダブルバーガー、ダブルチーズバーガーだ。


> (defun ingredients (order)
  (mapcan (lambda (burger)
	    (case burger
	      (single (list 'patty))
	      (double (list 'patty 'patty))
	      (double-cheese (list 'patty 'patty 'cheese))))
	  order))

(PATTY PATTY PATTY CHEESE PATTY PATTY)
|#
