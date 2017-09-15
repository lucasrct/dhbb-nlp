(ql:quickload :cl-conllu)
;; (ql:quickload :rcl)
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

(defun names-from-list (sentences name-list)
  "Finds and exhibit names from NAMES-LIST found in sentences elements
   of SENTENCES.
   NAMES-LIST is a list of names, where each name is a list
   of strings (tokenized names).

   Output is a list of pairs (sentence . found-names-list),
   where found-names-list's elements are tokens found that
   correspond to a name."
  (process-pattern-in-sentences
   sentences
   #'(lambda (sentence)
       (find-names-in-sentence
	sentence
	name-list))
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

(defun find-names-in-sentence (sentence name-list)
  (let ((tokens (sentence-tokens sentence)))
    (mapcar
     #'(lambda (pair)
	 (let ((start (car pair))
	       (length (cdr pair)))
	   (subseq tokens
		   start
		   (+ start length))))
     (remove
      nil
      (mapcar
       #'(lambda (name)
	   (cons
	    (search name
		    (mapcar #'token-form
			    tokens)
		    :test #'equal)
	    (length name)))
       name-list)
      :key #'car))))
   
(defun process-pattern-in-sentences (sentences pattern process &key (filter t) )
  "(list-of sentence) ->
   (sentence -> (list-of (list-of token))) ->
   ((list-of token) -> X) ->
   (list-of (sentence . list-of X))

   In each sentence in SENTENCES, search for lists of tokens
   satisfying PATTERN. Then, applies PROCESS in each list.
   Results are returned as a list of pairs (sentence . Result-for-Sentence)
   
   If FILTER, removes nil results."
  ;; tentative type, but changed in favor of current one:
  ;; (list-of sentences) -> (sentence -> (list-of (list-of tokens))) -> ((list-of tokens) -> X) -> (list-of (list-of X))
  ;; TODO: to cl-conllu
  (funcall
   (if filter
       #'(lambda (x) (remove nil x :key #'cdr))
       #'identity)
   (mapcar
    #'(lambda (sentence)
	(process-pattern-in-sentence
	 sentence
	 pattern
	 process))
    sentences)))
  
(defun process-pattern-in-sentence (sentence pattern process)
  "sentence ->
   (sentence -> (list-of (list-of token))) ->
   ((list-of token) -> X) ->
   (sentence . list-of X)

   Search for lists of tokens satisfying PATTERN. Then, applies
   PROCESS in each list. Returned result is a pair
   (sentence . Result-for-Sentence)."
  ;; tentative type, but changed in favor of current one:
  ;; sentence -> (sentence -> (list-of (list-of token))) -> ((list-of token) -> X) -> (list-of (list-of X))
  ;; that is, I've decided to return the original sentence in order to
  ;; make it more traceable
  ;; this can be changed/discussed
  ;; TODO: to cl-conllu

  (cons sentence
	(mapcar
	     process
	     (pattern-in-sentence sentence pattern))))

(defun pattern-in-sentence (sentence pattern)
  "sentence ->
   (sentence -> (list-of (list-of tokens))) ->
   (list-of (list-of tokens))"
  (funcall pattern sentence))
  
(defun flatten-names-in-sentence (sentence name-list &key (ignore-closed-class t) (flatten-descendants t))
  "Receives one sentence and a list of names.
   NAME-LIST is a list of lists (splitted strings).
   Returns a modified sentence with flattened tokens relative to occurring names.

   If there are descendants that aren't included in the name, they are warned. They are also flattened iff FLATTEN-DESCENDANTS.

   'flattened' here means that they form a 'flat:name' subtree"
  (process-pattern-in-sentence
   sentence
   #'(lambda (sentence)
       (find-names-in-sentence
	sentence
	name-list))
   #'(lambda (token-list)
       (flatten sentence
		token-list
		:ignore-closed-class ignore-closed-class
		:flatten-descendants flatten-descendants))))
 
(defun get-descendants (sentence token-list)
  "(sentence, (list-of token)) -> (list-of token)
   Returns every token in SENTENCE that is descendant of at least one
   token of TOKEN-LIST.

   Order is not guaranteed (that is, results may appear in any order)"
  ;; TODO: to cl-conllu?
  (let* ((all-tokens (sentence-tokens sentence))
	 (adjacency-array (make-array (1+ (length all-tokens)) :initial-element nil))
	 (visited-array (make-array (1+ (length all-tokens)) :initial-element nil)))
    ;; position i is a list of ids of tokens whose heads are i
    (mapcar
     #'(lambda (tk)
	 (push tk
	       (elt adjacency-array
		    (token-head tk))))
     all-tokens)
    ;; (format t "array: ~a~%" adjacency-array)

    ;; I've given up using a recursive approach because I'm using (and
    ;; changing) an array for visited or non-visited (which is faster
    ;; and clearer)
    ;; (labels ((aux (sentence stack found)
    ;; 	       (if (null stack)
    ;; 		   found
    ;; 		   (let* ((explore (first stack))
    ;; 			  (unvisited-descendants
    ;; 			   (remove-if
    ;; 			    #'(lambda (x)
    ;; 				(member x found :test #'equal :key #'token-id))
    ;; 			        ;; could be faster with array/vector
    ;; 			        ;; instead of list
    ;; 			    (elt adjacency-array explore))))
    ;; 		     ;; (format t "stack: ~a; found: ~a~%" stack found)
    ;; 		     (aux sentence
    ;; 			  (append (mapcar
    ;; 				   #'token-id
    ;; 				   unvisited-descendants)
    ;; 				  (rest stack))
    ;; 			  (append unvisited-descendants
    ;; 				  found))))))
    ;;   (aux sentence
    ;; 	   (mapcar #'token-id token-list)
    ;; 	   token-list))))
    (mapcar
     #'(lambda (tk)
	 (setf (elt visited-array
		    (token-id tk))
	       t))
     token-list)
    (let ((stack (mapcar #'token-id token-list)))
      (flet ((visited? (tk)
	       (equal
		(elt visited-array
		     (token-id tk))
		t)))
	(loop (when (null stack)
		(return (remove-if-not
			 #'visited?
			 all-tokens)))
	   ;; (format t "stack: ~a; found: ~a~%" stack (remove-if-not
	   ;; 		 #'visited?
	   ;; 		 all-tokens))
	   (let* ((explore (pop stack)) ;; token id
		  (unvisited-descendants
		   (mapcar
		    #'token-id
		    (remove-if
		     #'visited?
		     (elt adjacency-array explore)))))
	     (setf (elt visited-array explore) t)
	     (setf stack
		   (append unvisited-descendants
			   stack))))))))
	
	  
    
			  

(defun flatten (sentence token-list &key (ignore-closed-class t) (flatten-descendants t))
  "Receives a list of tokens representing a single name and transforms
them into a flat:name structure."
  ;; warning for non contiguous tokens?
  (let* ((tokens (sort
		  (if flatten-descendants
		      (get-descendants
			sentence
			token-list)
		      token-list)
		  #'<
		  :key #'token-id))
	 (first-token-id (token-id (first tokens)))
	 (current-id first-token-id))
    (dolist (tk (rest tokens) tokens)
      (progn ;; check if contiguous
	(incf current-id)
	;; (break "tk: ~a; curr-id: ~a~%" tk current-id)
	(when (not (equal (token-id tk)
			  current-id))
	  (warn "Non-contiguous tokens are being flattened.
                 Skipped tokens: ~{~a~^,~}~%"
		(alexandria:iota
		 (- (token-id tk)
		    current-id)
		 :start current-id))))
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
  ;; TODO: check if consistent with udpipe tokenizer=TokenRange option
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

(defun list-to-string (alist)
  "Receives a list of strings and return them concatenate separated by spaces."
  (reduce
   #'(lambda (x y)
       (concatenate 'string x " " y))
   alist))

;; Util
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
