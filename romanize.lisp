;; Transliteration module (for hiragana / katakana only)

(in-package #:ichiran)

;; Hepburn
;; Revised Hepburn
;; Kunrei-shiki

(defparameter *default-romanization-method* nil)

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
      (if (zerop (length inner)) inner
          (format nil "~a~a" (char inner 0) inner))))
  (:method ((modifier (eql :long-vowel)) method cc-tree)
    (romanize-core method cc-tree))
  (:method ((modifier symbol) method cc-tree)
    (format nil "~a~a" (romanize-core method cc-tree) (string-downcase modifier))))

(defgeneric r-simplify (method str)
  (:documentation "Simplify the result of transliteration")
  (:method (method str) str))


(defparameter *generic-kana-table* (make-hash-table))

(defclass generic-romanization ()
  ((kana-table :reader kana-table
               :initform *generic-kana-table*)))

(defmethod r-base ((method generic-romanization) item)
  (or (gethash item (kana-table method)) (call-next-method)))

(defmethod r-apply ((modifier symbol) (method generic-romanization) cc-tree)
  (let ((yoon (gethash modifier (kana-table method))))
    (if yoon
        (let ((inner (romanize-core method cc-tree)))
          (format nil "~a~a" (subseq inner 0 (max 0 (1- (length inner)))) yoon))
        (call-next-method))))

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
                  ))

(defclass generic-hepburn (generic-romanization)
  ((kana-table :initform *hepburn-kana-table*)))

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

(defmethod r-simplify((method generic-hepburn) str)
  ;; remove apostrophe after n if the next character isn't a vowel
  (ppcre:regex-replace-all "n'([^aiueoy]|$)" str "n\\1"))


(defun romanize-list (cc-list &key (method *default-romanization-method*))
  "Romanize a character class list according to method"
  (let ((cc-tree (process-modifiers (process-iteration-characters cc-list))))
    (r-simplify method (romanize-core method cc-tree))))

(defun romanize (word &key  (method *default-romanization-method*))
  "Romanize a word according to method"
  (romanize-list (get-character-classes word) :method method))
  
