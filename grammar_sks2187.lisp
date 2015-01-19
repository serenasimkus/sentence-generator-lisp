(defparameter *grammar*
  '((S -> (IndClause) (SubConj IndClause VP) (IndClause Conj IndClause))
    (IndClause -> (NP VP) (NThere VP) (Adv NP VP))
    (NP -> (Adj NP) (NP PP) (N) (NP RelativeClause PP) (NP Conj NP) (NPSpecific) (Adj PP) (Pronoun) (N NP) (NPSpecific NP))
    (NPSpecific -> (DetSingular Adj NSingular) (DetPlural Adj NPlural) (Adj NSingular) (Adj NPlural) (DetSingular NSingular) (DetPlural NPlural))
    (VP -> (VP NP) (Aux Vs NP) (Aux Vs) (Aux Adv VP) (V) (VtakeInfinitive InfinitiveVP) (Vs Adv) (Aux VP))
	(PP -> (Prep NP))
	(RelativeClause -> (RCConjunction VP))
	(RCConjunction -> that which who)
	(InfinitiveVP -> (to IntransitiveV) (to IntransitiveV Adv) (to be) (to be Adj) (to be DetSingular NSingular) (to be DetPlural NPlural) (to TransitiveV) (to TransitiveV Adv))
	(VtakeInfinitive -> tend (be able))
	(DetSingular -> the this a)
	(DetPlural -> the several (a few))
	(Pronoun -> it he she you)
	(N -> NSingular NPlural)
	(NSingular -> education narrative polarization job business health care strategy intent asteroid eye prosecution death penalty case defense judge computer cookie)
	(NPlural -> gains industries services astronomers telescopes weeks cases jobs cookies)
	(NThere -> there)
	(SubConj -> whether)
	(Adj -> higher prevailing solid professional flawed desired visible naked amateur decided their)
	(V -> Vs SpecialV)
	(Vs -> IntransitiveV IntransitiveVPast TransitiveV)
	(SpecialV -> SingularSpecialV PluralSpecialV)
	(PluralSpecialV -> were)
	(SingularSpecialV -> was)
	(TransitiveV -> counter achieve see seek)
	(IntransitiveV -> counter pay achieve be see seek decide)
	(IntransitiveVPast -> made)
	(Prep -> of in to with (such as))
	(Conj -> and or but for nor yet so)
    (Aux -> must should will have)
	(Adv -> well hardly not now))
  "A grammar for a trivial subset of English.")

(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order
;(THE MAN LIKED A WOMAN)
;(defparameter rules1 '((sentence 0) (noun-phrase 0) (Article 0) (Noun 0) (verb-phrase 0) (Verb 3) (noun-phrase 0) (Article 1) (Noun 2)))
(defparameter rules1 '((S 0) (IndClause 0) (NP 5) (NPSpecific 2) (Adj 0) (NSingular 0) (VP 0) (VP 2) (Aux 0) (Vs 2) (TransitiveV 0) (NP 1) (NP 5) (NPSpecific 0) (DetSingular 0) (Adj 1) (NSingular 1) (PP 0) (Prep 0) (NP 2) (N 0) (NSingular 2)))
(defparameter rules2 '((S 0) (IndClause 0) (NP 5) (NPSpecific 0) (DetSingular 1) (Adj 4) (NSingular 7) (VP 3) (Aux 2) (Adv 1) (VP 0) (VP 4) (V 0) (Vs 2) (TransitiveV 1) (NP 5) (NPSpecific 0) (DetSingular 0) (Adj 5) (NSingular 8)))
(defparameter rules3 '((S 0) (IndClause 1) (NThere 0) (VP 0) (VP 4) (V 1) (SpecialV 1) (PluralSpecialV 0) (NP 0) (Adj 2) (NP 8) (N 0) (NSingular 3) (NP 1) (NP 2) (N 1) (NPlural 0) (PP 0) (Prep 1) (NP 3) (NP 5) (NPSpecific 5) (DetPlural 1) (NPlural 1) (RelativeClause 0) (RCConjunction 0) (VP 5) (VtakeInfinitive 0) (InfinitiveVP 1) (IntransitiveV 1) (Adv 0) (PP 0) (Prep 4) (NP 4) (NP 4) (NP 2) (N 0) (NSingular 4) (Conj 0) (NP 0) (Adj 3) (NP 2) (N 1) (NPlural 2) (Conj 0) (NP 8) (N 0) (NSingular 5) (NP 2) (N 0) (NSingular 6)))
(defparameter rules4 '((S 2) (IndClause 0) (NP 5) (NPSpecific 4) (DetSingular 0) (NSingular 9) (VP 3) (Aux 2) (Adv 2) (VP 0) (VP 4) (V 0) (Vs 0) (IntransitiveV 3) (NP 6) (Adj 6) (PP 0) (Prep 2) (NP 5) (NPSpecific 0) (DetSingular 0) (Adj 7) (NSingular 10) (Conj 2) (IndClause 0) (NP 5) (NPSpecific 3) (Adj 8) (NPlural 3) (VP 0) (VP 7) (Aux 1) (VP 5) (VtakeInfinitive 1) (InfinitiveVP 6) (TransitiveV 2) (NP 1) (NP 7) (Pronoun 0) (PP 0) (Prep 3) (NP 2) (N 1) (NPlural 4)))
(defparameter rules5 '((S 1) (SubConj 0) (IndClause 0) (NP 5) (NPSpecific 4) (DetSingular 0) (NSingular 11) (VP 0) (VP 2) (Aux 2) (Vs 2) (TransitiveV 3) (NP 1) (NP 9) (NPSpecific 4) (DetSingular 0) (NSingular 12) (NP 2) (N 0) (NSingular 13) (PP 0) (Prep 1) (NP 5) (NPSpecific 4) (DetSingular 0) (NSingular 14) (VP 0) (VP 2) (Aux 2) (Vs 0) (IntransitiveV 3) (NP 6) (Adj 9) (PP 0) (Prep 1) (NP 5) (NPSpecific 5) (DetPlural 2) (NPlural 5)))
(defparameter rules6 '((S 2) (IndClause 0) (NP 4) (NP 5) (NPSpecific 4) (DetSingular 0) (NSingular 11) (Conj 0) (NP 2) (N 0) (NSingular 15) (VP 1) (Aux 3) (Vs 1) (IntransitiveVPast 0) (NP 0) (Adj 10) (NP 2) (N 1) (NPlural 6) (Conj 0) (IndClause 2) (Adv 3) (NP 5) (NPSpecific 4) (DetSingular 0) (NSingular 16) (VP 2) (Aux 0) (Vs 0) (IntransitiveV 6)))

;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)

(defun apply-rules (rules sentence)
  (cond 
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule)))))) 

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t 
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))
      

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun random-sentence (phrase)
  (let ((s (random-sentence-helper phrase N2))) (if (null s) (random-sentence phrase) s)))

(defun random-sentence-helper (phrase depth)
  "Generate a random sentence or phrase and check for tree 
  recursion depth"
  (cond ((= depth 0)
  		 NIL)
        ((listp phrase)
         (mappend #'random-sentence-helper phrase (make-list (length phrase) :initial-element depth)))
        ((rewrites phrase)
		 (random-sentence-helper (random-elt (rewrites phrase)) (- depth 1)))
		(t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list depth)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list depth)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))


; My additions: code for Part III
(defparameter max-length 60)
(defparameter N2 20)
(defparameter N3 5)
(defparameter min-length 2)

(defun non-terminalp (phrase)
  (cond ((listp phrase)
         (reduce 'new-or (mapcar 'non-terminalp phrase)))
        (t (rewrites phrase))))

(defun check-valid-words (phrase)
  (non-terminalp phrase))

(defun new-or (a b)
  (or a b))

(defparameter valid-repeated-words '(of in to with such as and or but the this a several few for nor yet so it he she you))

(defun check-in-list (word words)
  (if (null words) NIL 
  (if (equal word (car words)) 
      T 
	  (check-in-list word (rest words)))))

(defun count-in-list (word words cnt)
  (if (null words) 0
  (if (equal word (car words))
      (count-in-list word (rest words) (+ cnt 1))
	  (count-in-list word (rest words) cnt))))

(defun check-multiples (phrase)
  (if (null phrase) T
  (if (and (check-in-list (car phrase) (rest phrase)) 
           (not (check-in-list (car phrase) valid-repeated-words))) 
      NIL 
	  (check-multiples (rest phrase)))))

(defun limit-repeated-words (phrase)
  (if (null phrase) T
  (if (> (count-in-list (car phrase) (rest phrase) 0) N3)
      NIL
	  (limit-repeated-words (rest phrase)))))

(defparameter aux-verbs '(must should will have))
(defparameter past-verbs '(were was made))

(defun check-following-word (phrase words-to-avoid)
  (if (null words-to-avoid) T
  (if (equal (car phrase) (car words-to-avoid))
      NIL
	  (check-following-word phrase (rest words-to-avoid)))))

(defun check-verb-form-after-aux (phrase)
  (if (null phrase) T
  (if (check-in-list (car phrase) aux-verbs)
      (check-following-word (rest phrase) past-verbs)
	  (check-verb-form-after-aux (rest phrase)))))

(defun check-the-their (phrase)
  (if (null phrase) T
  (if (check-in-list (car phrase) '(the))
      (check-following-word (rest phrase) '(their))
	  (check-the-their (rest phrase)))))

(defun check-will-well (phrase)
  (if (null phrase) T
  (if (check-in-list (car phrase) '(will))
      (check-following-word (rest phrase) '(well))
	  (check-will-well (rest phrase)))))

(defun check-should-will (phrase)
  (if (null phrase) T
  (if (check-in-list (car phrase) '(should))
      (check-following-word (rest phrase) '(will))
	  (check-should-will (rest phrase)))))

(defun check-weird-things (phrase)
  (if (and (and (check-the-their phrase) (check-will-well phrase)) (check-should-will phrase))
      T))

(defun validp (phrase)
  (cond ((check-valid-words phrase)
         NIL)
        ((> (length phrase) max-length)
		 NIL)
		((< (length phrase) min-length)
		 NIL)
		((not (check-multiples phrase))
		 NIL)
		((not (limit-repeated-words phrase))
		 NIL)
		((not (check-verb-form-after-aux phrase))
		 NIL)
		((not (check-weird-things phrase))
		 NIL)
		(t (list phrase))))

(defun generateValid (phrase)
  (setq x (random-sentence phrase))
  (cond ((validp x)
         (format t "~a~%" x))))

(defun write-to-file (sentence)
  (with-open-file (str "output_sks2187.txt"
                       :direction :output
					   :if-exists :append
					   :if-does-not-exist :create)
      (format str sentence)))

(defun run2 ()
  (let ((sent (random-sentence 'S)))
      (write-to-file (if (validp sent) (format nil "+ ~S ~%" sent) (format nil "- ~S ~%" sent)))))

(defun loop-run (N)
  (loop for i from 1 to N do (run2)))

(random-sentence 'S)
