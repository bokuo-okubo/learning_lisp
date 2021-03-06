

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)


(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)


(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "お前は既に死んでいる。終了。"))
  (when (monsters-dead)
    (princ "おめでとう。 敵を全て殺したぜ。ヤッハー")))


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

(defun randval (n)
  (1+ (random (max 1 n))))



(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))



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


(defun init-monsters()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

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





(defstruct monster (health (randval 10)))





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


(defmethod monster-show (m)
  (princ "一匹の獰猛な。。")
  (princ (type-of m)))

(defmethod monster-attack (m))


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
