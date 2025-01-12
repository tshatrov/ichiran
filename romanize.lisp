;; Transliteration module (for hiragana / katakana only)

(in-package #:ichiran)

(defun get-character-classes (word)
  "Transforms a word (or a string) into a list of character classes"
  (map 'list (lambda (char) (gethash char *char-class-hash* char)) word))

(defun process-iteration-characters (cc-list)
  "Replaces iteration characters in a character class list"
  (loop with prev
     for cc in cc-list
     if (eql cc :iter) if prev collect prev end
     else if (eql cc :iter-v) if prev collect (voice-char prev) end
     else collect cc and do (setf prev cc)))

(defun process-modifiers (cc-list)
  (loop with result
       for (cc . rest) on cc-list
       if (eql cc :sokuon)
         do (push (cons cc (process-modifiers rest)) result) (loop-finish)
       else if (member cc *modifier-characters*)
         do (push (list cc (pop result)) result)
       else do (push cc result)
       finally (return (nreverse result))))

(defun leftmost-atom (cc-list &aux (first (car cc-list)))
  (cond ((atom first) first)
        (t (leftmost-atom (cdr first)))))

(defun romanize-core (method cc-tree)
  (with-output-to-string (out)
    (dolist (item cc-tree)
      (cond ((null item))
            ((characterp item) (princ item out))
            ((atom item) (princ (r-base method item) out))
            ((listp item) (princ (r-apply (car item) method (cdr item)) out))))))

(defgeneric r-base (method item)
  (:documentation "Process atomic char class")
  (:method (method item)
    (string-downcase item)))

(defgeneric r-apply (modifier method cc-tree)
  (:documentation "Apply modifier to something")
  (:method ((modifier (eql :sokuon)) method cc-tree)
    (let ((inner (romanize-core method cc-tree)))
      (if (or (zerop (length inner))
              (not (cl-unicode:has-property (char inner 0) "Block:Basic Latin")))
          inner
          (format nil "~a~a" (char inner 0) inner))))
  (:method ((modifier (eql :long-vowel)) method cc-tree)
    (romanize-core method cc-tree))
  (:method ((modifier symbol) method cc-tree)
    (format nil "~a~a" (romanize-core method cc-tree) (string-downcase modifier))))

(defgeneric r-simplify (method str)
  (:documentation "Simplify the result of transliteration")
  (:method (method str) str))


(defclass generic-romanization ()
  ((kana-table :reader kana-table
               :initform (make-hash-table))))

(defmethod r-base ((method generic-romanization) item)
  (or (gethash item (kana-table method)) (call-next-method)))

(defmethod r-apply ((modifier symbol) (method generic-romanization) cc-tree)
  (let ((yoon (gethash modifier (kana-table method))))
    (if yoon
        (case (car cc-tree)
          (:u (format nil "w~a" yoon))
          ((:a :i :e :o) (format nil "~a~a" (gethash (car cc-tree) (kana-table method)) yoon))
          (t (let ((inner (romanize-core method cc-tree)))
               (format nil "~a~a" (subseq inner 0 (max 0 (1- (length inner)))) yoon))))
        (call-next-method))))

;; Hepburn

(hash-from-list *hepburn-kana-table*
                '(:a "a"      :i "i"      :u "u"      :e "e"      :o "o"
                  :ka "ka"    :ki "ki"    :ku "ku"    :ke "ke"    :ko "ko"
                  :sa "sa"    :shi "shi"  :su "su"    :se "se"    :so "so"
                  :ta "ta"    :chi "chi"  :tsu "tsu"  :te "te"    :to "to"
                  :na "na"    :ni "ni"    :nu "nu"    :ne "ne"    :no "no"
                  :ha "ha"    :hi "hi"    :fu "fu"    :he "he"    :ho "ho"
                  :ma "ma"    :mi "mi"    :mu "mu"    :me "me"    :mo "mo"
                  :ya "ya"                :yu "yu"                :yo "yo"
                  :ra "ra"    :ri "ri"    :ru "ru"    :re "re"    :ro "ro"
                  :wa "wa"    :wi "wi"                :we "we"    :wo "wo"
                  :n "n'"
                  :ga "ga"    :gi "gi"    :gu "gu"    :ge "ge"    :go "go"
                  :za "za"    :ji "ji"    :zu "zu"    :ze "ze"    :zo "zo"
                  :da "da"    :dji "ji"   :dzu "zu"   :de "de"    :do "do"
                  :ba "ba"    :bi "bi"    :bu "bu"    :be "be"    :bo "bo"
                  :pa "pa"    :pi "pi"    :pu "pu"    :pe "pe"    :po "po"
                  :+a "a"     :+i "i"     :+u "u"     :+e "e"     :+o "o"
                  :+ya "ya"               :+yu "yu"               :+yo "yo"
                  :vu "vu"    :+wa "wa"
                  ))

(defclass generic-hepburn (generic-romanization)
  ((kana-table :initform (alexandria:copy-hash-table *hepburn-kana-table*))))

(defmethod r-apply ((modifier (eql :sokuon)) (method generic-hepburn) cc-tree)
  (if (eql (leftmost-atom cc-tree) :chi)
      (concatenate 'string "t" (romanize-core method cc-tree))
      (call-next-method)))

(defmethod r-apply ((modifier (eql :+ya)) (method generic-hepburn) cc-tree)
  (case (car cc-tree)
    (:shi "sha")
    (:chi "cha")
    ((:ji :dji) "ja")
    (t (call-next-method))))

(defmethod r-apply ((modifier (eql :+yu)) (method generic-hepburn) cc-tree)
  (case (car cc-tree)
    (:shi "shu")
    (:chi "chu")
    ((:ji :dji) "ju")
    (t (call-next-method))))

(defmethod r-apply ((modifier (eql :+yo)) (method generic-hepburn) cc-tree)
  (case (car cc-tree)
    (:shi "sho")
    (:chi "cho")
    ((:ji :dji) "jo")
    (t (call-next-method))))

(defmethod r-simplify ((method generic-hepburn) str)
  ;; remove apostrophe after n if the next character isn't a vowel
  (ppcre:regex-replace-all "n'([^aiueoy]|$)" str "n\\1"))

(defclass simplified-hepburn (generic-hepburn)
  ((simplifications :initform nil :initarg :simplifications :reader simplifications
                    :documentation "List of simplifications e.g. (\"ou\" \"o\" \"uu\" \"u\")"
                    )))

(defmethod r-simplify ((method simplified-hepburn) str)
  (simplify-ngrams (call-next-method) (simplifications method)))

(defparameter *hepburn-basic* (make-instance 'generic-hepburn))

(defparameter *hepburn-simple* (make-instance 'simplified-hepburn
                                              :simplifications '("oo" "o" "ou" "o" "uu" "u")))

(defparameter *hepburn-passport* (make-instance 'simplified-hepburn
                                                :simplifications '("oo" "oh" "ou" "oh" "uu" "u")))

(defclass traditional-hepburn (simplified-hepburn)
  ((simplifications :initform '("oo" "ō" "ou" "ō" "uu" "ū"))))

(defmethod r-simplify ((method traditional-hepburn) str)
  (let ((str (call-next-method)))
    (setf str (ppcre:regex-replace-all "n'([aiueoy])" str "n-\\1"))
    (ppcre:regex-replace-all "n([mbp])" str "m\\1")))

(defparameter *hepburn-traditional* (make-instance 'traditional-hepburn))

(defclass modified-hepburn (simplified-hepburn)
  ((simplifications :initform '("oo" "ō" "ou" "ō" "uu" "ū" "aa" "ā" "ee" "ē"))))

(defmethod initialize-instance :after ((obj modified-hepburn) &key)
  (setf (gethash :wo (slot-value obj 'kana-table)) "o"))

(defparameter *hepburn-modified* (make-instance 'modified-hepburn))

;; Kunrei-siki

(hash-from-list *kunrei-siki-kana-table*
                '(:a "a"      :i "i"      :u "u"      :e "e"      :o "o"
                  :ka "ka"    :ki "ki"    :ku "ku"    :ke "ke"    :ko "ko"
                  :sa "sa"    :shi "si"   :su "su"    :se "se"    :so "so"
                  :ta "ta"    :chi "ti"   :tsu "tu"   :te "te"    :to "to"
                  :na "na"    :ni "ni"    :nu "nu"    :ne "ne"    :no "no"
                  :ha "ha"    :hi "hi"    :fu "hu"    :he "he"    :ho "ho"
                  :ma "ma"    :mi "mi"    :mu "mu"    :me "me"    :mo "mo"
                  :ya "ya"                :yu "yu"                :yo "yo"
                  :ra "ra"    :ri "ri"    :ru "ru"    :re "re"    :ro "ro"
                  :wa "wa"    :wi "i"                 :we "e"     :wo "o"
                  :n "n'"
                  :ga "ga"    :gi "gi"    :gu "gu"    :ge "ge"    :go "go"
                  :za "za"    :ji "zi"    :zu "zu"    :ze "ze"    :zo "zo"
                  :da "da"    :dji "zi"   :dzu "zu"   :de "de"    :do "do"
                  :ba "ba"    :bi "bi"    :bu "bu"    :be "be"    :bo "bo"
                  :pa "pa"    :pi "pi"    :pu "pu"    :pe "pe"    :po "po"
                  :+a "a"     :+i "i"     :+u "u"     :+e "e"     :+o "o"
                  :+ya "ya"               :+yu "yu"               :+yo "yo"
                  :vu "vu"    :+wa "wa"
                  ))

(defclass kunrei-siki (generic-romanization)
  ((kana-table :initform (alexandria:copy-hash-table *kunrei-siki-kana-table*))))

(defmethod r-simplify ((method kunrei-siki) str)
  (let ((str (ppcre:regex-replace-all "n'([^aiueoy]|$)" str "n\\1")))
    (simplify-ngrams str '("oo" "ô" "ou" "ô" "uu" "û"))))

(defparameter *kunrei-siki* (make-instance 'kunrei-siki))

(defvar *default-romanization-method* *hepburn-traditional*)

(defun romanize-list (cc-list &key (method *default-romanization-method*))
  "Romanize a character class list according to method"
  (let ((cc-tree (process-modifiers (process-iteration-characters cc-list))))
    (values (r-simplify method (romanize-core method cc-tree)))))

(defgeneric r-special (method word)
  (:documentation "Romanize words that are exceptions, return nil otherwise")
  (:method-combination or)
  (:method or (method word)
           (cond ((equal word "っ") "!")
                 ((equal word "ー") "~"))))

;; (defmethod r-special or ((method generic-hepburn) word)
;;   (cond ((equal word "は") "wa")
;;         ((equal word "へ") "e")))

;; (defmethod r-special or ((method modified-hepburn) word)
;;   (when (equal word "を") "o"))

(defun romanize-word (word &key (method *default-romanization-method*) original-spelling (normalize t))
  "Romanize a word according to method"
  (when normalize
    (setf word (normalize word)))
  (or (r-special method (or original-spelling word))
      (let ((word (process-hints word)))
        (romanize-list (get-character-classes word) :method method))))

(defun romanize-word-geo (input &key (method *hepburn-simple*))
  (string-capitalize (romanize-word input :method method :normalize t)))

(defun join-parts (parts)
  (with-output-to-string (s)
    (loop with last-space = t
         for part in parts
         for len = (length part) do
         (when (and (not (zerop len))
                    (not last-space)
                    (alphanumericp (char part 0)))
           (princ #\Space s))
         (princ part s)
         (unless (zerop len)
           (setf last-space (cl-unicode:has-property (char part (1- len)) "WhiteSpace"))))))

(defun romanize-word-info (word-info &key (method *default-romanization-method*))
  "Romanize word-info instance"
  (let* ((orig-text (word-info-text word-info)))
    (map-word-info-kana
     (if (eql method :kana)
         (lambda (wk) (strip-hints wk))
         (lambda (wk) (romanize-word wk :method method :original-spelling orig-text :normalize nil)))
     word-info)))

(defun romanize (input &key (method *default-romanization-method*) (with-info nil))
  "Romanize a sentence according to method"
  (setf input (normalize input :context method))
  (loop with definitions = nil
     for (split-type . split-text) in (basic-split input)
     nconc
       (if (eql split-type :word)
           (mapcar (lambda (word)
                     (let ((rom (romanize-word-info word :method method)))
                       (when with-info
                         (push (cons rom (word-info-str word)) definitions))
                       rom))
                   (simple-segment split-text))
           (list split-text)) into parts
     finally (return (values (join-parts parts) (nreverse definitions)))))

(defun romanize* (input &key (method *default-romanization-method*) (limit 5) (wordprop-fn (constantly nil)))
  "Romanizes text with very detailed metadata"
  (setf input (normalize input :context method))
  (loop for (split-type . split-text) in (basic-split input)
     collect
       (if (eql split-type :word)
           (mapcar (lambda (pair)
                     (let ((word-list (car pair))
                           (score (cdr pair)))
                       (list
                        (mapcar (lambda (word)
                                  (let* ((romanized (romanize-word-info word :method method))
                                         (prop (funcall wordprop-fn romanized word)))
                                    (list romanized word prop)))
                                word-list)
                        score)))
                   (dict-segment split-text :limit limit))
           split-text)))
