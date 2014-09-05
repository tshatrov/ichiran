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
            ((listp item) (princ (r-apply method (car item) (cdr item)) out))))))
  
(defgeneric r-base (method item)
  (:documentation "Process atomic char class")
  (:method (method item)
    (string-downcase item)))

(defgeneric r-apply (method modifier cc-tree)
  (:documentation "Apply modifier to something")
  (:method (method (modifier (eql :sokuon)) cc-tree)
    (let ((inner (romanize-core method cc-tree)))
      (if (zerop (length inner)) inner
          (format nil "~a~a" (char inner 0) inner))))
  (:method (method (modifier (eql :long-vowel)) cc-tree)
    (romanize-core method cc-tree))
  (:method (method (modifier symbol) cc-tree)
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

(defun romanize-list (cc-list &key (method *default-romanization-method*))
  "Romanize a character class list according to method"
  (let ((cc-tree (process-modifiers (process-iteration-characters cc-list))))
    (r-simplify method (romanize-core method cc-tree))))

(defun romanize (word &key  (method *default-romanization-method*))
  "Romanize a word according to method"
  (romanize-list (get-character-classes word) :method method))
  
