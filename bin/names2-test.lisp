;; File for tests using :prove package ( https://github.com/fukamachi/prove )

(ql:quickload :prove)
;; (setf prove:*enable-colors* t)
(setf prove:*default-test-function* #'equal)

(defpackage my-test
  (:use :cl
	:prove
	:cl-conllu))

(load "./names2.lisp")

(defparameter *s* (car (read-conllu "/home/gppassos/Documentos/nlp-general/dhbb-nlp/bin/names2-test.conllu")))

;; (plan 1)

(let ((names (list (list "Machado" "Coelho")))
      (token-machado (nth 2 (cl-conllu:sentence-tokens *s*)))
      (token-coelho (nth 3 (cl-conllu:sentence-tokens *s*)))
      (token-de (nth 4 (cl-conllu:sentence-tokens *s*)))
      (token-castro (nth 5 (cl-conllu:sentence-tokens *s*))))
  (prove:subtest "Testing find-names-in-sentence"
    (prove:is 
     (find-names-in-sentence *s* names)
     (list (list token-machado token-coelho))))
  (prove:subtest "Testing get-descendants"
    (prove:ok 				; tests if non-nil
     (let ((x (get-descendants *s* (list token-machado token-coelho)))
	   (y (list token-machado token-coelho
		    token-de token-castro)))
       ;; (format t "~a~%~a" x y)
       (null (set-exclusive-or x y))))))

;; (finalize)
