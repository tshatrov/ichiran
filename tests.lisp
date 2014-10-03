(in-package :ichiran/test)

(defmacro assert-segment (str &rest segmentation)
  `(assert-equal ',segmentation
                 (mapcar #'word-info-text (simple-segment ,str))
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
                  "しつれい" "しなければ" "ならない" "ん" "です")
  (assert-segment "だけど気付けば馴染んじゃってる"
                  "だけど" "気付けば" "馴染んじゃってる")
  (assert-segment "飲んで笑っちゃえば"
                  "飲んで" "笑っちゃえば")
  (assert-segment "なんで" "なんで")
  (assert-segment "遠慮しないでね" "遠慮" "しないで" "ね")
  (assert-segment "出かけるまえに" "出かける" "まえ" "に")
  (assert-segment "感じたいでしょ" "感じたい" "でしょ")
  (assert-segment "まじで" "まじ" "で")
  (assert-segment "その山を越えたとき" "その" "山" "を" "越えた" "とき")
  (assert-segment "遊びたいのに" "遊びたい" "のに")
  (assert-segment "しながき" "しながき")
  )

(defun run-all-tests ()
  (run-tests :all :ichiran/test))
