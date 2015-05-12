;; visualizeing graphs

(defparameter *wizard-nodes* '((living-room (youare in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					ther is a well in fromt of you.))
			       (attic (you are in the attic. ther
				       is a giant welding torch in the corner.))))
(defparameter *wizard-edges* '((living-room (garden west door)
				(attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))


;; converting to DOT file.
(defun dot-name(exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

;; a node in DOT format can contain only leters, digits, and the underscore character. To make sure the node
;; identifier we're using is legal, we'll change any forbidden characters to underscores. Here are examples of the dot-name function in use.


;; adding labels to graph nodes.
(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))


;; Generating the DOT Information for the Nodes.

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

;; いったんコンソールに表示する
;; REPLで関数を実行して結果をすぐに見られるので、デバッグが簡単になる。

;; Generating All the DOT data

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

;; Turning the DOT FILE into a Picture
(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		   fname
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -0 " fname)))

;; Using Thunks
;; It is common in Lisp to create small functions that have zero arguments.
;; These functions are officially called nullary functions.
#|
Lispでは、引数を取らない￥関数がよく使われる。
これらの関数は正式にはゼロ項(nullary)関数と呼ばれる。
Lisperは、いますぐに実行したくない計算を包んでおくのに、ゼロ引数の関数をよく使う。
この目的で使われる無引数関数は通常(thunk)やサスペンション(suspension)と呼ばれている。
今の例では、dot->pngでサンクを使うんだろう。
どうしてdot-pngでサンクを使うのか。
graph->dotや他のDOTフォーマッティング関数を作る際に、デバッグのしやすさを考えて出力をコンソールに書き出していた。
graph->dotを呼ぶと、結果は返り値としてではなく、コンソール出力という副作用で得られる、ということだ。
だから、dot-pngにgraph-dotの返り値を渡す、というわけにはいかない。
かわりにここではgraph-dotをサンクとして渡す。
dot->pngは必要なときにgraph->dotを呼び、出力を捕まえてファイルへとおくる。

プログラムからテキスト形式のデータを生成するというのは　よくあることなので、
このテクニックはLISPコードではよく使われる。
まずコンソールに正しい出力が得られるようにコードを書く。
次にそれをサンクにくるんで、結果を他の場所におくる。
|#


;; Writing to a File
(with-open-file (my-stream
		 "testfile.txt"
		 :direction :output
		 :if-exists :supersede)
  (princ "hello file!" my-stream))

;; Creating a Stream
#|
princ等の出力関数は、省略可能な引数としてストリームを受け取る。
すると関数はコンソールではなく、そのストリームオブジェクトへとデータを出力する。
|#

;; キーワード引数を理解する

;; with-open-fileはまた、沢山のキーワード引数を使う。
#|
それぞれのキーワード引数は２つの部分からなる。
引数の名前と、渡す値だ。
キーワード引数の名前はいつでも、ころんで始まるシンボル。
この例では２つのキーワード引数がつかわれている　。
:direction 引数には値:outputが渡され、ファイルを読み込みではなく書き出しようにオープンすることを示す。
:if-exits引数には:supersede関数が渡され、同盟のが言うrが既に存在していた場合は、以前の内容を捨てるように指示をしている。


キーワードシンボルは、そのキーワード自身をしめす。
> :cigar
:CIGAR

> (let ((:cigar 5))
         :cigar)
# これはあかんのよ。
|#



;; グラフを画像にする

(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (graph->dot nodes edges))))
