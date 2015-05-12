#|
9.5 オーク・バトル

オークバトルでは、プレーヤーは12対のモンスターに囲まれ、命を書けた戦いに挑む騎士となる。
あなたは優れた起点と多様な剣術を使い、オークやヒドラを始めとする危険な敵との戦いの
戦略を慎重に考えなければならない。
一手まちがえたら、全員を倒す前に数で圧倒されてしまうかもしれない。
defmethodとdefstructをつかってあひょひょひょ


|#

#| プレーヤとモンスターのグローバル変数
プレーヤーにかんしては、３つの状態を追跡する。
体力(helath), 素早さ(agility), 力(stlength)だ。
- プレーヤの体力が0になると、プレーヤは死ぬ。
- 素早さは戦いの1ラウンドでプレーヤが何回攻撃できるのかを決める。
- 力は攻撃の威力を決める。
|#

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

#|
また、*mosters*という変数にモンスターについての情報を保持する配列を格納しよう。
この配列は非均質、つまりオークやヒドラ等要素ごとに種類の異なるモンスターを保持できる。
モンスターの種類はdefstructを使って定義する。

モンスターを作成する関数のリストを定義し、変数*monster-builders*に格納する。
モンスターの種類を定義するたびに、そのモンスターを作り出す関数も一緒に定義し、
その関数をこの変数にpushしてゆく。

さいごに、騎士が戦うモンスターの数を変数*monster-num*に格納しておく。
この変数を増減すれば、オークバトルの難易度を変えられる。
|#

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;;ゲームのメイン関数
;; モンスターの初期化
;; ゲームループのスタート
;; げーむ終了時に勝者を決定し、ふさわしいメッセージを表示する

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "お前は既に死んでいる。終了。"))
  (when (monsters-dead)
    (princ "おめでとう。 敵を全て殺したぜ。ヤッハー")))

;; ゲームのメインループ
;; バトルの1ラウンドを処理し、自分を再帰的に呼び出してループする。

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))


;; プレイヤーを管理する関数

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "君は勇者で体力こんだけ: ") (princ *player-health*)
  (princ ", 素早さは: ")
  (princ *player-agility*)
  (princ ", 力は: ")
  (princ *player-strength*))

;; プレイヤーの攻撃を処理する

(defun player-attack ()
  (fresh-line)
  (princ "攻撃方法: [s]突く [d]二度斬り [r]なぎ払う:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "あなたの二度斬りの強さは")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))
#|
この関数はまず、プレーヤが選べるいくつかの攻撃コマンドを出力売る
プレーヤは３つの攻コマンドを選択できる。

- 突くは一体の敵だけに向けて繰り出す、最も強烈な攻撃だ。
対象が一匹なので、さいしょにpick-monster関数を使ってプレイヤに敵を選ばせる。
ダメージは、*player-strength*と、ランダムな係数、そして適度な調整によって計算される


- 二度斬りは攻撃力は弱いが、二体のモンスタに攻撃できる。
さらに、プレイヤはコウゲキ開始じに与えるダメージがどれ位か知ることができる.

- なぎはらう
は敵をえらばない
プレイヤのちからに応じた回数、dotimesを回してランダムな的に攻撃をしかけ続ける
攻撃力は1
|#

;; randval補助関数
(defun randval (n)
  (1+ (random (max 1 n))))


;;プレイヤの攻撃に使う補助関数

;; 無差別なぎ払い攻撃のためのrandom-monster関数
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))



;;攻撃するモンスターを選ぶ関数
(defun pick-monster ()
  (fresh-line)
  (princ "Monster #: ")
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn (princ "モンスターの番号としておかしいれす")
	       (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn (princ "そのモンスタは既に死んどる")
		     (pick-monster))
	      m)))))

;;モンスターを管理する関数

(defun init-monsters()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))
#|
必要な大きさのからの配列をつくり、165
その配列にモンスタ作成のためのlambda関数をmapすることで、モンスターに満たされた配列を構築する。

|#

;; モンスターが全て倒されたかどうかを調べる関数
(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "あなたの敵:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x)) ;; インクリメント
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn (princ "(Health=")
		      (princ (monster-health m))
		      (princ ") ")
		      (monster-show m))))
	 *monsters*)))


;; モンスターたち

;; ジェネリックなモンスター

;; どのモンスターも、体力のゲージによって、どれだけダメージを受けたら死ぬかが決まる。

(defstruct monster (health (randval 10)))

#|
このdefstructの書き方は、ひとつの新しい機能を使っている。

構造体のスロット(上の例ではhealth)を宣言するときに、
それをかっこの中にいれると、
同時にそのスロットの規定値を指定することができる。
|#


;; 攻撃のダメージを受けた時ににモンスターの体力を減らしてゆく関数

;; 呼ばれたときになにが起きたかを説明するメッセージを出力し、
;; モンスターが死んだときにはそのことも表示
;; defmethodをつかうことで,勇者が特定のモンスターを倒して時に、モンスター特有のメッセージを付け加えることができる

(defmethod monster-hit (m x)
  (decf (monster-health m) x) ;; decf関数は変数の中身を減らす
  (if (monster-dead m)
      (progn (princ "お前が ")
	     (princ (type-of m))
	     (princ "を殺したっっ! "))
      (progn (princ "おまえは")
	     (princ (type-of m));; 型を知る
	     (princ "を攻撃したっっ！")
	     (princ x)
	     (princ " こんだけな。"))))

;; > (type-of (make-monster))
;; MONSTER

(defmethod monster-show (m)
  (princ "一匹の獰猛な。。")
  (princ (type-of m)))

(defmethod monster-attack (m))

;;具体的なモンスター作成！

;; オーク
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "オークだよ、レベル: ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "オークがこん棒を振って攻撃してきた")
    (princ x)
    (princ "だけあなたの体力減る")
    (decf *player-health* x)))



;; ヒドラ
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "手強いヒドラやん、あたま、 ")
  (princ (monster-health m))
  (princ "個もあるで。"))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "ヒドラの頭を全部潰して、ヒドラは倒れた!")
      (progn (princ "あなたはヒドラの")
	     (princ x)
	     (princ "本の首を落とした！ "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "ヒドラがあなたに攻撃してきた")
    (princ x)
    (princ "本の頭でな！そして頭は一つ再生しようとしておるっ！！")
    (incf (monster-health m))
    (decf *player-health* x)))



;; ベタベタスライム

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "スライムだお、ベタベタ度はこんなもん:")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "スライムは貴方の足を掴んで、素早さをこれだけ下げた：")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "なんか汁を出してきて貴方の体力も一減った！！")
      (decf *player-health*))))


;; 狡猾なブリガンド

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "ブリガンドはスリングショットを使って貴方の体力を2減らした！")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "ブリガンドは貴方の足を掴んで鞭を叩き、素早さを２減らした!")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (princ "ブリガンドは貴方の腕をぷぎゃぁして、攻撃力を2減らした！")
	   (decf *player-strength* 2)))))
