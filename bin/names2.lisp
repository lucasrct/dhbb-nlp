(ql:quickload :cl-conllu)
(ql:quickload :rcl)
;; (ql:quickload :cl-string-match)

(in-package :cl-conllu)

(defun sentences ()
  (reduce (lambda (l a) (append l (read-conllu a)))
	  ;; (directory #P"../udp/100*.conllu")
	  (directory #P"../udp/21*.conllu")
	  :initial-value nil))

(defun names ()
  (with-open-file (in #P"../../dhbb/dic/pessoa-individuo.txt")
    (loop for line = (read-line in nil nil)
	  while line
	  collect (expand-names (cl-ppcre:split "[ ]+"
						(string-trim '(#\Space #\Tab) line))))))

;; (defun sentences ()
;;   (mapcar (lambda (s) (mapcar #'token-form (sentence-tokens s)))
;; 	  (reduce (lambda (l a) (append l (read-conllu a)))
;; 		  (directory #P"../udp/100*.conllu")
;; 		  :initial-value nil)))

(defun names-from-list (sentences name-list &key (include-token-range t))
  "Finds and exhibit names from NAMES-LIST found in sentences elements
of SENTENCES."
  (if include-token-range
      (mapcar
       #'include-token-range
       sentences))
  (process-pattern-in-sentences
   sentences
   #'(lambda (sentence)
       (mapcar
	#'(lambda (name-range)
	    (string-range-to-token-list
	     sentence
	     name-range))
	(filter-names
	 (find-names-in-sentence
	  sentence
	  name-list))))
   #'(lambda (tks)
       (mapcar
	#'(lambda (tk)
	    (with-slots
		  (list id form head deprel)
		tk
	      (format nil "~a: ~a(~a, ~a)"
		      form deprel head id)))
	tks))
   :filter t))
   
   
(defun process-pattern-in-sentences (sentences pattern process &key (filter t) )
  "(list-of sentences) ->
   (sentence -> (list-of (list-of tokens))) ->
   ((list-of tokens) -> X) ->
   (list-of (sentence . list-of X))

   In each sentence in SENTENCES, search for lists of tokens
   satisfying PATTERN. Then, applies PROCESS in each list.
   Results are returned as a list of pairs (sentence . Result-for-Sentence)
   
   If FILTER, removes nil results."
  ;; tentative type, but changed in favor of current one:
  ;; (list-of sentences) -> (sentence -> (list-of (list-of tokens))) -> ((list-of tokens) -> X) -> (list-of (list-of X))

  (funcall
   (if filter
       #'(lambda (x) (remove nil x :key #'cdr))
       #'identity)
   (mapcar
    (lambda (sentence)
      (cons sentence
	    (mapcar
	     process
	     (pattern-in-sentences sentence pattern))))
    sentences)))

(defun pattern-in-sentences (sentence pattern)
  "sentence -> (sentence -> (list-of (list-of tokens))) -> (list-of (list-of tokens))"
  (funcall pattern sentence))
 
  
(defun flatten-names-in-sentences (sentences name-list)
  (mapcar
   #'(lambda (sentence)
       (flatten-names-in-sentence sentence name-list))
   sentences))

(defun flatten-names-in-sentence (sentence name-list &key (include-ranges t) (ignore-closed-class t))
  "Receives one sentence and a list of names.
   NAME-LIST is a list of lists (splitted strings).
   Returns a modified sentence with flattened tokens relative to occurring names.

   'flattened' here means that they form a 'flat:name' subtree"
  (let* ((sentence
	  (if include-ranges
	      (include-token-range sentence)
	      sentence))
	 (tokens-found
	  (remove nil
		  ;; This is necessary in order to remove (warned)
		  ;; false positives, which is a problem of searching
		  ;; by string.
		  (mapcar
		   #'(lambda (name-range)
		       (string-range-to-token-list
			sentence
			name-range))
		   (filter-names
		    (find-names-in-sentence
		     sentence
		     name-list))))))
    (mapc
     #'(lambda (token-list)
	 (flatten token-list :ignore-closed-class ignore-closed-class))
     tokens-found)
    sentence))

(defun filter-names (range-list)
  "Receives a list of (start end) lists, each representing a name in
the sentence, and removes 'problematic' ones.

   Let X and Y be (start end) lists.
   If X is contained in Y, X will be removed.
   If there's an overlap between X and Y, neither contains the other
   and X starts before, then Y will be removed."
  (labels ((aux (range-list
		 lower-bound
		 upper-bound
		 filtered)
	     (if
	      (null range-list)
	      (reverse filtered)
	      (let* ((current-interval (first range-list))
		     (start (first current-interval))
		     (end (nth 1 current-interval)))
		;; by construction, starts are sorted, therefore we never
		;; have (< start upper-bound)
		(if (= start lower-bound)
		    (if (<= end upper-bound)
			;; current-interval is contained in previous one,
			;; therefore won't be added:
			(aux (rest range-list)
			     lower-bound upper-bound filtered)
			;; current-interval contains previous one,
			;; therefore the previous one is removed and
			;; current-interval is inserted and defines new
			;; bounds:
			(aux (rest range-list)
			     start end (cons current-interval (rest filtered))))
		    ;; else: (> start lower-bound)
		    (if (< start upper-bound)
			;; current-interval either is contained or has
			;; overlap and starts later, therefore won't be
			;; added:
			(aux (rest range-list)
			     lower-bound upper-bound filtered)
			;; current-interval has no overlap at all, then
			;; is added and defines new bounds. This is the
			;; expected non-problematic case:
			(aux (rest range-list)
			     start end (cons current-interval filtered))))))))
    (aux (sort range-list
	       #'<
	       :key #'first)
	 0 0 nil)))

(defun find-names-in-sentence (sentence name-list)
  "Returns a list of (start-position end-position) lists, where
   each such list marks the interval in which a name in NAME-LIST occurrs in the sentence.
   The positions are relative to the string of the sentence
   (sentence->text sentence ignore-mtokens t)."
  (let ((names-string (mapcar
		       #'list-to-string
		       name-list))
	(sent-text (sentence->text sentence :ignore-mtokens t)))
    (mapcan #'(lambda (n)
		(mapcar
		 #'(lambda (start-position)
		     (list
		      start-position
		      (+ start-position
			 (length n))))
		 (search-all n sent-text :test #'equal)))
	    names-string)))

(defun flatten (token-list &key (ignore-closed-class t))
  "Receives a list of tokens representing a single name and transforms
them into a flat:name structure."
  ;; not flat prepositions/closed class words?
  ;; warning for non contiguous tokens?
  (let* ((tokens (sort token-list
		       #'<
		       :key #'token-id))
	 (first-token-id (token-id (first tokens))))
    (dolist (tk (rest tokens))
      (unless (and ignore-closed-class
		   (in-closed-class? tk))
	(setf (token-head tk) first-token-id)
	(setf (token-deprel tk) "flat:name")))))


(defun in-closed-class? (token)
  "This token's Universal Part-of-Speech is closed class."
  (let ((closed-class-upostags
	 (list
	  "ADP"
	  "AUX"
	  "CCONJ"
	  "DET"
	  "NUM"
	  "PART"
	  "PRON"
	  "SCONJ")))
    (with-slots 'upostag token
      (find
       upostag
       closed-class-upostags
       :test #'equal))))


(defun string-range-to-token-list (sentence string-range)
  "Receives a list STRING-RANGE equal to (START END) corresponding to
starting and ending position of a name and a sentence.
   Returns a list of tokens corresponding to this name."
  (let* ((token-ranges
	  (mapcar
	   #'(lambda (tk)
	       (mapcar #'parse-integer
		       (coerce
			(nth-value 1
				   (cl-ppcre:scan-to-strings
				    "TokenRange=\(\\d+\)\\:\(\\d+\)"
				    (token-misc tk)))
			'list)))
	   (sentence-tokens sentence)))
	 (start (first string-range))
	 (end (nth 1 string-range))
	 (start-position (position start token-ranges
				   :key #'(lambda (x) (first x))
				   :test #'equal))
	 (end-position (position end token-ranges
				 :key #'(lambda (x) (nth 1 x))
				 :test #'equal
				 :start (if start-position
					    start-position
					    0))))
    (cond
      ((not start-position)
       (warn "There's no token starting at position ~a
This interval doesn't represent a name really occurring in the sentence ~a.
Sentence text: ~a"
	     start sentence (sentence-meta-value sentence "text")))
      ((not end-position)
       (warn "There's no token ending at position ~a.
This interval doesn't represent a name really occurring in the sentence ~a.
Sentence text: ~a"
	     end sentence (sentence-meta-value sentence "text")))
      (t
       (subseq (sentence-tokens sentence)
	       start-position
	       (+ 1 end-position))))))

(defun add-to-misc (token string)
  ;; TODO: include in cl-conllu
  (let ((current-misc (token-misc token)))
    (setf (token-misc token)
	  (if (equal "_"
		     current-misc)
	      string
	      (concatenate
	       'string
	       current-misc
	       "|"
	       string)))))

(defun include-token-range (sentence)
  ;; TODO: include in cl-conllu
  "Receives a sentence and, for each token, includes word range information in the MISC field.
   Its format is TokenRange=A:B, where A is the start position and B the end position.
   These positions are relative to the text string of the sentence constituted by tokenized words."
  (let ((tokens (sentence-tokens sentence))
	(text (sentence->text sentence :ignore-mtokens t))
	(scan-point 0))
    (dolist (token tokens sentence)
      (let* ((tk-form (token-form token))
	     (start (search tk-form text
			    :start2 scan-point))
	     (end (if start
		      (+ start (length tk-form))
		      (error "Scan error: word ~a couldn't be found in
		      sentence. ~% Scan position at error time: ~a"
			     tk-form
			     scan-point))))
	;; Checks if there isn't already an TokenRange field in MISC
	(if (search "TokenRange=" (token-misc token))
	    (error "There's already a TokenRange field at MISC field
                   in token ~a. ~%
                   WARNING: there's no rollback. Some tokens may have been already
                   modified."
		   token))
	;;
	(add-to-misc token
		     (format nil "TokenRange=~a:~a"
			     start
			     end))
	(setf scan-point end)))))

(defun list-to-string (alist)
  "Receives a list of strings and return them concatenate separated by spaces."
  (reduce
   #'(lambda (x y)
       (concatenate 'string x " " y))
   alist))

;; Util
(defun search-all (sequence-1 sequence-2 &key from-end (test #'equal) test-not key (start1 0) (start2 0) end1 end2)
  "Finds every match of sequence-1 in sequence-2."
  (let ((n (length sequence-1)))
    (labels ((search-all-aux (sequence-1 sequence-2 accumulator-answer &key from-end test test-not key start1 start2 end1 end2)
	       (let ((current-answer
		      (search sequence-1 sequence-2
			      :from-end from-end
			      :test test
			      :test-not test-not
			      :key key
			      :start1 start1
			      :start2 start2
			      :end1 end1
			      :end2 end2)))
		 (if current-answer
		     (search-all-aux sequence-1
				     sequence-2
				     (cons
				      current-answer
				      accumulator-answer)
				     :from-end from-end
				     :test test
				     :test-not test-not
				     :key key
				     :start1 start1
				     :start2 (+ current-answer n)
				     :end1 end1
				     :end2 end2)
		     (reverse accumulator-answer)))))
      (search-all-aux sequence-1
		      sequence-2
		      nil
		      :from-end from-end
		      :test test
		      :test-not test-not
		      :key key
		      :start1 start1
		      :start2 start2
		      :end1 end1
		      :end2 end2))))

;; names: sequence of names from a gazette

(defun expand-names (names &optional res)
  (if (null names)
      (nreverse res)
      (expand-names (cdr names)
		    (cond ((member (car names) '("dos" "do")
				   :test #'equal)
			   (cons "o" (cons "de" res)))
			  ((member (car names) '("das" "da")
				   :test #'equal)
			   (cons "a" (cons "de" res)))
			  (t (cons (car names) res))))))

;; problems in the misc field

(defun fix-sentence (s)
  (dolist (alist (list (sentence-tokens s) (sentence-mtokens s)) s)
    (dolist (tk alist)
      (let ((nval (string-trim '(#\Space #\Tab #\NO-BREAK_SPACE)
			       (cl-ppcre:regex-replace "SpacesAfter=[  ]*(\\\\s|\\\\n)*[  ]*"
						       (slot-value tk 'misc) ""))))
	(if (not (equal nval (slot-value tk 'misc)))
	    (setf (slot-value tk 'misc)
		  (if (equal nval "") "_" nval)))))))

(defun fix-sentences (filename)
  (write-conllu (mapcar #'fix-sentence (read-conllu filename))
		(make-pathname :type "new" :defaults filename)))


;; ---
;; (defun find-occurrences (sentences names)
;;   ;; Uses Aho-Corasick
;;   ;;
;;   ;; This isn't correct, as doesn't find every occurrence in every
;;   ;; substring, but only one occurrence for substring.
;;   ;;
;;   ;; Ex.: in "Universidade Federal do Rio de Janeiro", the name
;;   ;; "Universidade Federal do Rio de Janeiro" would be found, but "Rio
;;   ;; de Janeiro" wouldn't
;;   (let* ((idx (cl-string-match:initialize-ac names))
;; 	 (count (make-hash-table :test #'equal
;; 				 :size (length names)))
;; 	 (name-hash (make-hash-table :size (length names))))
;;     ;; starting hash counts for each name as 0
;;     (dotimes (n (length names))
;;       (setf (gethash n count) 0))
;;     ;; starting hash for index-name
;;     (let ((n 0))
;;       (dolist (name names)
;; 	(setf (gethash n name-hash) name)
;; 	(incf n)))
;;     (time
;;      (dolist (sent-text sentences count)
;;        (let ((current-text sent-text))
;; 	 (loop ;; Loop for consuming current-text
;; 	    (multiple-value-bind (match index)
;; 		(cl-string-match:search-ac idx
;; 					   current-text)
;; 	      (if match
;; 		  (progn
;; 		    (incf (gethash index count))
;; 		    (setf current-text
;; 			  (subseq current-text
;; 				  (+ match
;; 				     (length (gethash index name-hash))))))
;; 		  (return)))))))))


;; (defun block-scanner (trigger)
;;   (let* ((curr trigger))
;;     (lambda (data)
;;       (dolist (w data (not curr))
;; 	(if curr
;; 	    (setq curr (if (equal (car curr) w)
;; 			   (cdr curr)
;; 			   trigger)))))))


;; (defun find-occurrences ()
;;   (let ((sents (sentences))
;; 	(names (names)))
;;     (time (loop for n in names
;; 		for v = (remove-if-not (lambda (sent)
;; 					 (search n sent :test #'equal))
;; 				       sents)
;; 		when v
;; 		collect (list n (length v))))))

;; (defun find-occurrences ()
;;   (let* ((sents (sentences))
;; 	(names (names))
;; 	(count-list (alexandria:iota (length names))))
;;     (time (loop for sent in sents
;; 	     for v = (remove
;; 		      nil
;; 		      (mapcar
;; 		       #'(lambda (x y)
;; 			   (list x y))
;; 		       (mapcar #'(lambda (n)
;; 				   (search-all n sent :test #'equal))
;; 			       names)
;; 		       count-list)
;; 		      :key #'first)
;; 	     collect (list sent v)))))


;; (defun find-occurrences ()
;;   (let ((sents (sentences))
;; 	(names (names)))
;;     (time (loop for n in names
;; 	     for scanner = (block-scanner n)
;; 	     for v = (remove-if-not (lambda (sent)
;; 				      (funcall scanner sent))
;; 				    sents)
;; 	     when v
;; 	     collect (list n (length v))))))



;; (with-open-file (in #P"../../dhbb/dic/pessoa-individuo.txt")
;;   (let ((names (loop for line = (read-line in nil nil)
;; 		     while line
;; 		     collect (string-trim '(#\Space #\Tab) line)))
;; 	(sents (mapcar (lambda (sent) (sentence-meta-value sent "text"))
;; 		       (cl-conllu:read-conllu #P"../dhbb-nlp/udp/"))))
;;     (time (loop for n in names
;; 		for v = (remove-if-not (lambda (sent) (search n sent :test #'equal))
;; 				       sents)
;; 		when v
;; 		collect (list n (length v))))))

;; (defvar teste *)

;; (in-package :rcl)
;; (r-init)

;; (with-open-file (out "saida.txt" :direction :output)
;;   (format out "~a~%~%" cl-conllu::teste)
;;   (format out "~a" (r "summary" (mapcar #'cadr cl-conllu::teste))))
