#|
### 10.2 loopを使って進化ゲームを作ろう

マップは単なる長方形で,上端と下端、右端と左端がつながっている(トーラス)
マップの大部分は乾燥帯で、食用になる植物はほとんど生えていない。
マップの中央にはジャングルがあり、そこでは植物はずっと早く成長する
ここに、食物を求めて動き回る、草食動物を放つ。
|#

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10)) ;; 左端,上端,x,y
(defparameter *plant-energy* 80) ;;動物がこの植物を食べたら、80日間生きられる

;; マップに草を生やそう
;; コンピュータで進化シュミレートをするのは結構重い.効率は大事。

(defparameter *plants* (make-hash-table :test #'equal))

#|
特に指定しなければCommonLispのハッシュテーブルは、キーを比較するのにeqlを使う。
だがいまはx座標とy座標のペアをキーとしたいので、equalを使うよう、:testキーワード引数を与えた。
コンスセルを比較するには、equalが必要
|#

;; 新しい植物を生やす関数

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;; 動物をつくる
(defstruct animal x y energy dir genes)

#|
energy フィールドは、あと何日その動物が活動できるかをしめしている
dirフィールドは、方向。
-----
0 1 2
7 m 3
6 5 4
-----

genesフィールドは、その動物の遺伝子を保持している
遺伝子は8つの正整数リストで表現され、それぞれの要素が動物の向きと図のように対応している
毎日、動物は向きをそのままにするか、ランダムに変えるかを決めるのだが、
それにこのリストが使われる。
各方向へと向きを変える確率が、リストの対応する各要素に比例している

(1 1 10 1 1 1 1 1)

kのような遺伝子を持っている場合、スロット2の値だけとても大きいのがわかる。動物は結構くるくるまわる
|#

(defparameter *animals*
  (list (make-animal :x      (ash *width* -1)
		     :y      (ash *height* -1)
		     :energy 1000
		     :dir    0
		     :genes (loop repeat 8
			       collecting (1+ (random 10))))))


;; 動物を動かす
;; move関数は動物を引数に取り、先述べた方向の基準に従って、上下左右、あるいは斜めに移動させる

(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1)
					  ((or (= dir 1) (= dir 5)) 0)
					  (t -1)))
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0)))
				 *height*))
    (decf (animal-energy animal))))

#|
新たなx座標を計算するために、condを使って方向が2,3,4のいずれかであるかをまず調べる。
方向がこれらの値であれば、動物はマップの東方向を向いているということで、値を1増やす
1か5の場合は、真北か真南を向いているから、値は変わらない。
それ以外の場合は、西を向いているということだから、値を１減らす

マップは左右、上下それぞれつながっているので、座標値の計算はmod関数を使って剰余演算をしている
|#

;; 動物の向きを変える
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
	       (let ((xnu (- x (car genes))))
		 (if (< xnu 0)
		     0
		     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
		 8)))))

;; 動物に食べさせる

;; 動物に現在の位置に植物があるかどうか調べ、あればそれを消費する
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))


;; 繁殖アルゴリズム
;; 面倒くさいので無性生殖
;; 遺伝子コピーの際に誤差を入れて、突然変異を起こさせる。

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
	    (genes (copy-list (animal-genes animal)))
	    (mutation (random 8)))
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))
;; copy-structureは浅いコピーを行う -> 参照先オブジェクトの変更は反映されてしまう


;; シュミレーション世界の一日

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0))
			     *animals*))
  (mapc (lambda (animal)
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))


;; 世界を描く

(defun draw-world ()
  (loop for y
     below *height*
     do (progn (fresh-line)
	       (princ "|")
	       (loop for x
		  below *width*
		  do (princ (cond ((some (lambda (animal)
					   (and (= (animal-x animal) x)
						(= (animal-y animal) y)))
					 *animals*)
				   (print-animal x y))
				  ((gethash (cons x y) *plants*) #\*)
				  (t #\space))))
	       (princ "|"))))



(defun add (list)
	   (apply #'+ list))

(defun assort-genes (fi-genes sec-genes)
  (if (> (add fi-genes) (add sec-genes))
      #\H
      #\E))

(defun assort-animal (animal)
  (let* ((genes (animal-genes animal))
	 (half-length (ash (length genes) -1))
	 (fi-genes (subseq genes 0 half-length))
	 (sec-genes (nthcdr half-length genes)))
    (assort-genes fi-genes sec-genes)))

(defun print-animal (x y)
  (assort-animal (find-if (lambda (animal)
			    (and (= (animal-x animal) x)
				 (= (animal-y animal) y)))
			  *animals*)))



;; シュミレーションのユーザ・インタフェイス

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
		      below x
		      do (update-world)
		      if (zerop (mod i 1000))
		      do (princ #\.))
		   (update-world))
	       (evolution))))))
