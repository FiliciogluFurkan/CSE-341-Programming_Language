(defun find-binding (var bindings)
  "Lookup the value of VAR in BINDINGS."
  (reduce (lambda (acc pair)
            (find-binding-helper var pair acc))
          bindings
          :initial-value var))

(defun find-binding-helper (var pair acc)
  "Helper function to check if VAR matches the first element of PAIR."
  (if (string= (first pair) var)
      (second pair)
      acc))

(defun is-variable (string)
  "Check if STR is a variable (starts with an uppercase letter)."
  (and (stringp string)
       (uppercase-letter (char string 0))))

(defun uppercase-letter (char)
  "Check if CHAR is an uppercase letter."
  (let ((char-code (char-code char)))
    (and (>= char-code (char-code #\A))
         (<= char-code (char-code #\Z)))))
              

;; match-predicates
(defun match-predicates (pred1 pred2 bindings)
  "match PRED1 and PRED2 with BINDINGS."
  (if (or (/= (length pred1) (length pred2))
          (not (string= (car pred1) (car pred2))))
      nil
      (resolve-arguments
 (cdr pred1) (cdr pred2) bindings)))

(defun resolve-arguments
 (args1 args2 bindings)
  "Helper function to unify the arguments of two predicates."
  (let ((updated-bindings bindings))
    (mapc (lambda (pair)
            (let* ((val1 (find-binding (car pair) updated-bindings))
                   (val2 (find-binding (cdr pair) updated-bindings)))
              (cond ((string= val1 val2)) ; if equals continue
                    ((is-variable val1)
                     (push (list val1 val2) updated-bindings)) ; add if val1 is a variable
                    ((is-variable val2)
                     (push (list val2 val1) updated-bindings)) ;add if val2 is a variable
                    (t (return-from resolve-arguments
                     nil))))) ; Otherwise stop the process
          (mapcar #'cons args1 args2))
    updated-bindings))


(defun verify-goal (goals axioms bindings)
  "Prove GOALS using AXIOMS and BINDINGS."
  (if (null goals)
      (list bindings) ; Return current binding if targets are empty
      (goal-verification (car goals) (cdr goals) axioms bindings)))

(defun goal-verification (current-goal remaining-goals axioms bindings)
  "Helper function to prove a single goal."
  (reduce (lambda (results axiom)
            (let ((unified-bindings (match-predicates current-goal (car axiom) bindings)))
              (if unified-bindings
                  (append results
                          (if (= (length axiom) 1)
                              (verify-goal remaining-goals axioms unified-bindings) ; A one-item axiom
                              (verify-goal (append (cddr axiom) remaining-goals) axioms unified-bindings))) ; if needs more prove
                  results))) ; If there is any incompatibility, leave it as it is.
          axioms
          :initial-value nil))

;; Main entry fonksiyonu
(defun prolog_prove (axioms query)
  (let ((results (verify-goal query axioms nil)))
    (if results
        (remove-duplicates
         (collect-variable-bindings results query)
         :test #'equal)
        nil)))

(defun collect-variable-bindings (results query)
  "Collect variable bindings from RESULTS based on QUERY."
  (loop for bindings in results
        append (loop for pred in query
                     append (collect-bindings-from-predicate pred bindings))))

(defun collect-bindings-from-predicate (pred bindings)
  "Collect bindings from a single PREDICATE."
  (loop for term in (rest pred)
        when (is-variable term)
        collect (list term (find-binding term bindings))))

;; Test function
(defun test-prolog ()
  (let ((axioms '(
                  ;; faxts
                  (("father" "jim" "jill"))
                  (("mother" "mary" "jill"))
                  (("father" "samm" "jim"))
                  (("sibling" "samm" "bob"))
                  (("father" "bob" "alice"))

                  ;; rules of parent
                  (("parent" "X" "Y") "<" ("father" "X" "Y"))
                  (("parent" "X" "Y") "<" ("mother" "X" "Y"))

                  ;; ancestor rules
                  (("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
                  (("ancestor" "samm" "jill") "<")  ; Directly state that samm is Jill's ancestor
                  
                  ;; Uncle  rules
                  (("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y")))))

    ;; test cases
    (format t "~%=== Test cases ===~%")

    ;; Test 1: Father
    (format t "~%Test 1: who is the father of jill?~%")
    (let ((query1 '(("father" "X" "jill"))))
      (format t "Result: ~A~%" (prolog_prove axioms query1)))

    ;; Test 2: Parent
    (format t "~%Test 2: who is the parent of jill?~%")
    (let ((query2 '(("parent" "X" "jill"))))
      (format t "Result: ~A~%" (prolog_prove axioms query2)))

    ;; Test 3: Ancestor
    (format t "~%Test 3: who is the ancestors of jill?~%")
    (let ((query3 '(("ancestor" "X" "jill"))))
      (format t "Result: ~A~%" (prolog_prove axioms query3)))

    ;; Test 4: Uncle
    (format t "~%Test 4: who is the uncle of alice?~%")
    (let ((query4 '(("uncle" "X" "alice"))))
      (format t "Result: ~A~%" (prolog_prove axioms query4)))
      
         ;; Test 5: not relation test
   (format t "~%Test 5: who is the marry's father (not defined)~%")
   (let ((query5 '(("father" "X" "mary"))))
     (format t "Result: ~A~%" (prolog_prove axioms query5)))

   ;; Test 6: multiple target test
   (format t "~%Test 6: is there any one both ancestor and mother?~%")
   (let ((query6 '(("ancestor" "X" "jill") ("mother" "X" "bob"))))
     (format t "Result: ~A~%" (prolog_prove axioms query6)))))
(test-prolog)