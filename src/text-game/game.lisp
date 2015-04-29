(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizerd is snoring loudly on the couach.))
			(garden (you are in a beautiful garden.
				there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

(defun describe-location(location nodes)
  (cadr (assoc location nodes)))

;;describing the paths
(defparameter *edges* '((living-room (garden west door)
			             (attic apstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs laddar))))

(defun describe-path(edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describing multiple paths at once

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; オブジェクトのリスト
(defparameter *objects* '(whiskey bucket frog chain))

;; オブジェクトとその場所を格納する変数
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; 与えられた場所から見えるオブジェクトのリストを返す関数
(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

;;ある場所で見えるオブジェクト
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))



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
