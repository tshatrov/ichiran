(in-package :ichiran/dict)

;; SPLITS (words that should be scored as two or more other words)

(defparameter *split-map* (make-hash-table)) ;; seq -> split function

(defmacro defsplit (name seq (reading-var) &body body)
  `(progn
     (defun ,name (,reading-var) ;; reading -> (values parts score-bonus)
       ,@body)
     (setf (gethash ,seq *split-map*) ',name)))

(defmacro def-simple-split (name seq score (&optional length-var text-var) &body parts-def)
  "each part is (seq length-form)"
  (alexandria:with-gensyms (reading-var offset parts)
    (unless length-var (setf length-var (gensym "LV")))
    (unless text-var (setf text-var (gensym "TV")))
    `(defsplit ,name ,seq (,reading-var)
       (let* ((,text-var (text ,reading-var))
              (,length-var (length ,text-var))
              (,offset 0)
              (,parts nil))
         ,@(loop for (part-seq part-length-form conj-p) in parts-def
              collect
                `(let ((pseq ,(if (listp part-seq)
                                  `(seq (car (find-word-conj-of ,@part-seq)))
                                  part-seq))
                       (part-length ,part-length-form))
                   (push (car (,(if conj-p
                                    'find-word-conj-of
                                    'find-word-seq)
                                (subseq ,text-var ,offset 
                                        (and part-length (+ ,offset part-length)))
                                pseq))
                         ,parts)
                   (incf ,offset part-length)))
         (values (nreverse ,parts) ,score)))))


(defun get-split (reading &optional conj-of)
  (let ((split-fn (gethash (seq reading) *split-map*)))
    (if split-fn
        (funcall split-fn reading)
        (loop for seq in conj-of
           for split-fn = (gethash seq *split-map*)
           when split-fn do (return (funcall split-fn reading))))))

;; split definitions

;; -de expressions (need to be split otherwise -desune parses as -de sune)

(defmacro def-de-split (seq seq-a &optional (score 15))
  (let ((name (intern (format nil "~a~a" :split-de- seq))))
    `(def-simple-split ,name ,seq ,score (len)
       (,seq-a (- len 1))
       (2028980 1))))

(def-de-split 1163700 1576150) ;; 一人で

(def-de-split 1611020 1577100) ;; 何で

(def-de-split 1004800 1628530) ;; これで

;; nakunaru split: because naku often attaches to previous word

(def-simple-split split-nakunaru 1529550 30 (len) ;; 無くなる
  (("無く" 1529520) 2)
  (1375610 (- len 2) t))

(def-simple-split split-nakunaru2 1518540 10 (len) ;; 亡くなる
  (("亡く" 1518450) 2)
  (1375610 (- len 2) t))

