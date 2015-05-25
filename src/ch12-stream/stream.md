# chapter 12 Stream

## ストリームの種類

### リソースの種類による分類

- コンソールストリーム
- ファイルストリーム
- ソケットストリーム
- 文字列ストリーム

※文字列ストリームだけ、外の世界との通信のためではない、という意味でちょっと特殊


### 向きによる分類
リソースにデータを書き出すには出力ストリームを
データを読み込むには入力ストリームを使う。


#### 出力ストリーム
出力ストリームは、replに文字を表示したり、ファイルに書き出したり、ソケットを通じてデータを送ったりするのに使われる。
出力ストリームの、最も基本となる操作は次の２つ。

- 出力ストリームかどうか調べる

-- ```output-stream-p```

```
(output-stream-p *standard-output*)
T
```

- データをストリームへと送り出す

```
(write-char #\x *standard-output*)
x
#\x

```

### 入力ストリーム

- 入力ストリームかどうか調べる

```
> (input-stream-p *standard-input*)
T

```

- ストリームから要素を一つ取り出す

```
(read-char *standard-input*)
123
#\1
```

## 12.2 ファイルの読み書き
Clでファイルストリームを作る方法はいくつかあるけど、一番いいのは　with-open-fileコマンドを使うことだ。

-> 他のファイルコマンドよりも安全

```
(with-open-file (my-stream "data.txt" :direction :output)
	   (print "my-data" my-stream))

"my-data"
```

この例では、with-open-fileは出力ストリームを作ってmy-streamというという変数に格納している
このストリームは、with-open-fileの本体、つまり最期のカッコが閉じられるまで有効。
そしてこの間、このストリームに送られたデータはディスク上のdata.txtというファイルに書きだされる。
printコマンドがmy-streamを出力先として指定しているから。

with-open-fileの:direction に:outputを渡すと、出力ストリームが作られる。入力ストリームを作るには、そこを:inputにすればよい

```
> (with-open-file (my-stream "data.txt" :direction :input)
   (read my-stream))

"my-data"
```


```
(let ((animal-noises '((dog . woof)
				(cat . meow))))
	   (with-open-file (my-stream "animal-noises.txt" :direction :output)
	     (print animal-noises my-stream)))


((DOG . WOOF) (CAT . MEOW))


> (with-open-file (my-stream "animal-noises.txt" :direction :input)
    (read my-stream))
((DOG . WOOF) (CAT . MEOW))

```


- with-open-file は様々なキーワード引数で動作を調整できる。
例えば、作ろうとしたファイルが既に存在した場合にどうするかを指定するのは:if-existsキーワード引数だ。
次の例ではその場合に絵４ラーを表示するように指定している。

```
(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
	   (print "my data" my-stream))

```
