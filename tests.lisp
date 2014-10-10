(in-package :ichiran/test)

(defmacro assert-segment (str &rest segmentation)
  `(assert-equal ',segmentation
                 (mapcar (lambda (wi) (if (eql (word-info-type wi) :gap) :gap
                                          (word-info-text wi)))
                         (simple-segment ,str))
                 ))

(define-test segmentation-test
  (assert-segment "ご注文はうさぎですか" "ご注文" "は" "うさぎ" "です" "か")
  (assert-segment "しませんか" "しません" "か")
  (assert-segment "ドンマイ" "ドンマイ")
  (assert-segment "みんな土足でおいで" "みんな" "土足で" "おいで")
  (assert-segment "おもわぬオチ提供中" "おもわぬ" "オチ" "提供" "中")
  (assert-segment "わたし" "わたし")
  (assert-segment "お姉ちゃんにまかせて地球まるごと"
                  "お姉ちゃん" "に" "まかせて" "地球" "まるごと")
  (assert-segment "大人になってるはず"
                  "大人" "に" "なってる" "はず")
  (assert-segment "いいとこ" "いいとこ")
  (assert-segment "そういうお隣どうし"
                  "そういう" "お" "隣" "どうし")
  (assert-segment "はしゃいじゃう" "はしゃいじゃう")
  (assert-segment "分かっちゃうのよ" "分かっちゃう" "の" "よ")
  (assert-segment "懐かしく新しいまだそしてまた"
                  "懐かしく" "新しい" "まだ" "そして" "また")
  (assert-segment "あたりまえみたいに思い出いっぱい"
                  "あたりまえ" "みたい" "に" "思い出" "いっぱい")
  (assert-segment "何でもない日々とっておきのメモリアル"
                  "何でもない" "日々" "とっておき" "の" "メモリアル")
  (assert-segment "しつれいしなければならないんです"
                  "しつれいしなければ" "ならない" "ん" "です")
  (assert-segment "だけど気付けば馴染んじゃってる"
                  "だけど" "気付けば" "馴染んじゃってる")
  (assert-segment "飲んで笑っちゃえば"
                  "飲んで" "笑っちゃえば")
  (assert-segment "なんで" "なんで")
  (assert-segment "遠慮しないでね" "遠慮しないで" "ね")
  (assert-segment "出かけるまえに" "出かける" "まえ" "に")
  (assert-segment "感じたいでしょ" "感じたい" "でしょ")
  (assert-segment "まじで" "まじ" "で")
  (assert-segment "その山を越えたとき" "その" "山" "を" "越えた" "とき")
  (assert-segment "遊びたいのに" "遊びたい" "のに")
  (assert-segment "しながき" "しながき")
  (assert-segment "楽しさ求めて" "楽" "しさ" "求めて")
  (assert-segment "日常のなかにも" "日常" "の" "なか" "に" "も")
  ;;(assert-segment "ほんとは好きなんだと" "ほんと" "は" "好き" "なん" "だと")
  (assert-segment "内緒なの" "内緒" "なの")
  (assert-segment "魚が好きじゃない" "魚" "が" "好き" "じゃない")
  (assert-segment "物語になってく" "物語" "に" "なってく")
  (assert-segment "書いてきてくださった" "書いてきて" "くださった")
  (assert-segment "今日は何の日" "今日" "は" "何" "の" "日")
  (assert-segment "何から話そうか" "何から" "話そう" "か")
  (assert-segment "話したくなる" "話したく" "なる")
  (assert-segment "進化してく友情" "進化してく" "友情")
  (assert-segment "私に任せてくれ" "私" "に" "任せてくれ")
  (assert-segment "時までに帰ってくると約束してくれるのなら外出してよろしい"
                  "時" "まで" "に" "帰ってくる" "と"
                  "約束してくれる" "の" "なら" "外出して" "よろしい")
  (assert-segment "雨が降りそうな気がします" "雨" "が" "降り" "そう" "な" "気がします")
  (assert-segment "新しそうだ" "新しそう" "だ")
  (assert-segment "本を読んだりテレビを見たりします"
                  "本" "を" "読んだり" "テレビ" "を" "見たり" "します")
  (assert-segment "今日母はたぶんうちにいるでしょう"
                  "今日" "母" "は" "たぶん" "うち" "に" "いる" "でしょう")
  (assert-segment "赤かったろうです" "赤かったろう" "です")
  (assert-segment "そう呼んでくれていい" "そう" "呼んでくれていい")
  (assert-segment "払わなくてもいい" "払わなくて" "も" "いい")
  (assert-segment "体に悪いと知りながらタバコをやめることはできない"
                  "体" "に" "悪い" "と" "知り" "ながら" "タバコ" "を" "やめる" "こと" "は" "できない")
  (assert-segment "いつもどうり" "いつも" "どうり")
  (assert-segment "微笑みはまぶしすぎる" "微笑み" "は" "まぶし" "すぎる")
  )

(defun run-all-tests ()
  (let ((res (run-tests :all :ichiran/test)))
    (print-failures res)
    res))
