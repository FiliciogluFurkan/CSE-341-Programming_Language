(defstruct (structure_of_function
            (:constructor make-structure_of_function)
            (:constructor make-structure_of_function
                          (name operator operand1 operand2 argument-number)))
  name
  operator
  operand1
  operand2
  argument-number)
(defvar *hashmap_of_function* (make-hash-table :test 'equal)) ; burada test, fonksiyon bilgilerine erişebilmek için anahtar görevi görür
(defvar *terminate-program*  nil)

(defun gppinterpreter (&optional file-names)
  "GPP Interpreter: Dosyadan veya komut satırından input alır ve işler."
  (with-open-file (out-stream "output.txt" 
                   :direction :output 
                   :if-exists :supersede 
                   :if-does-not-exist :create)
    (flet ((process-input (input)
             (parser input out-stream)))
      (if file-names
          (dolist (file-name file-names)
            (process-input (gpp-lexer file-name)))
          (process-input (gpp-lexer))))
    out-stream))

(defun parser (tokens out-stream)
  "Tokenları parse eden ve sonuçları işleyen ana parser fonksiyonu."
  (labels 
      ((recursive-parse (current-tokens)
         ;; Tokens boş mu kontrolü
         (when (null current-tokens)
           (return-from parser nil))
         
         ;; START non-terminalinden sonucu al
         (let* ((result (START current-tokens))
                (out-val (car result))
                (remaining-tokens (cadr result)))
           
           (cond 
             (out-val 
              ;; Hem dosyaya hem ekrana çıktı yaz
              (let ((formatted-val 
                     (if (numberp out-val) 
                         (format-valuef out-val) 
                         out-val)))
                (format out-stream "~a~%" formatted-val)
                (format t "~a~%" formatted-val)) 
              (if (not *terminate-program*)        ;; Exit flag kontrolü
                  (recursive-parse remaining-tokens)
                  result))(t 
              (format out-stream "Syntax Error!~%")
              (format t "Syntax Error!~%")
              nil))       
           result)))
    (recursive-parse tokens)))

(defun START (tokens) ; evaluating START non-terminal
  "Evaluates the START non-terminal by handling EXIT-PROGRAM, EXP, or FUNCTION directly"
  
  (let ((result))  ; Common result variable
    (if (car (setq result (EXIT-PROGRAM tokens)))    ;; Check for EXIT-PROGRAM
        result
        (progn
          ;; Check for EXP (arithmetic expression or identifier, etc.)
          (if (car (setq result (EXPR tokens)))
              result
              (progn
                ;; Check for FUNCTION definition
                (if (car (setq result (FUNCTION_DEFINITION tokens)))
                    result
                    (evaluate-res nil tokens))))))))

(defun EXIT-PROGRAM (tokens)
  "Evaluates the EXIT non-terminal"
  (let ((result nil))
    (when (string= (next-token tokens) "OP_OP")    ;; OP_OP kontrolü
      (setq tokens (cdr tokens)))
    (when (string= (next-token tokens) "KW_EXIT")    ;; KW_EXIT kontrolü
      (setq tokens (cdr tokens)))

    (if (string= (next-token tokens) "OP_CP")  ;; OP_CP kontrolü
        (progn
          (setq *terminate-program* t);; Exit flag'i true yap
          (setq result (evaluate-res "$_" tokens)))  ;; Sonuç olarak "$_" döndür
      (return-from EXIT-PROGRAM (evaluate-res nil tokens))) result))

(defun EXPR (tokens) ; evaluating EXP non-terminal
  (let ((result nil)) ; EXP := ARITHMETIC | IDENTIFIER | VALUEF | FCALL
    (cond
     ((let ((temp (ARITHMETIC tokens)))      ;; ARITHMETIC kontrolü
        (if (car temp) 
            (setq result temp)))
      result)
     ((let ((temp (IDENTIFIER tokens)))      ;; IDENTIFIER kontrolü
        (if (car temp) 
            (setq result temp)))    
      result)
     ;; VALUEF kontrolü
     ((let ((temp (VALUEF tokens))) 
        (if (car temp) 
            (setq result temp))) result)
     ;; FUNCTION-CALL kontrolü
     ((let ((temp (FUNCTION-CALL tokens))) 
        (if (car temp) 
            (setq result temp))) result)
        ((let ((temp (FOR-EXPRESSION tokens))) 
        (if (car temp) 
            (setq result temp)))
      
      result)
     ;; WHILE kontrolü
     ((let ((temp (WHILE-EXPRESSION tokens))) 
        (if (car temp) 
            (setq result temp)))result)
     ;; Default durumu
     (t (evaluate-res nil tokens)))))
(defun FOR-EXPRESSION (tokens)
  (let ((result nil)
        (loop-var nil)
        (start nil)
        (end nil)
        (body nil))

    (unless (string= (next-token tokens) "OP_OP")    ;; OP_OP kontrolü
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    (unless (string= (next-token tokens) "KW_FOR")    ;; KW_FOR kontrolü
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    (unless (string= (next-token tokens) "OP_OP")    ;; OP_OP kontrolü (loop değişkenleri için)
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    (setq result (IDENTIFIER tokens))    ;; Değişken adı (IDENTIFIER) kontrolü
    (unless (car result)
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq loop-var (car result))
    (setq tokens (cadr result))

    (setq result (EXPR tokens))    ;; Başlangıç değeri (VALUEF veya IDENTIFIER)
    (unless (car result)
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq start (car result))
    (setq tokens (cadr result))

    ;; Bitiş değeri (VALUEF veya IDENTIFIER)
    (setq result (EXPR tokens))
    (unless (car result)
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq end (car result))
    (setq tokens (cadr result))

    ;; OP_CP kontrolü (loop değişkenleri için)
    (unless (string= (next-token tokens) "OP_CP")
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    ;; Loop body
    (setq result (EXPR tokens))
    (unless (car result)
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq body (car result))
    (setq tokens (cadr result))

    (unless (string= (next-token tokens) "OP_CP")    ;; OP_CP kontrolü
      (return-from FOR-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    (let ((result-value 0))  ;; Döngü işlemi
      (loop for i from start to end
            do (setq result-value (evaluate-valuef "OP_PLUS" result-value body)))
      (setq result (evaluate-res result-value tokens))) result))

(defun WHILE-EXPRESSION (tokens)
  (let ((result nil)
        (condition nil)
        (body nil))

    ;; OP_OP kontrolü
    (unless (string= (next-token tokens) "OP_OP")
      (return-from WHILE-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))
    ;; KW_WHILE kontrolü
    (unless (string= (next-token tokens) "KW_WHILE")
      (return-from WHILE-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))
    ;; Condition
    (setq result (EXPR tokens))
    (unless (car result)
      (return-from WHILE-EXPRESSION (evaluate-res nil tokens)))
    (setq condition (car result))
    (setq tokens (cadr result))
    ;; Loop body
    (setq result (EXPR tokens))
    (unless (car result)
      (return-from WHILE-EXPRESSION (evaluate-res nil tokens)))
    (setq body (car result))
    (setq tokens (cadr result))
    ;; OP_CP kontrolü
    (unless (string= (next-token tokens) "OP_CP")
      (return-from WHILE-EXPRESSION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))
    (let ((result-value 0)
          (loop-condition condition))
      (loop while (not (zerop loop-condition))
            do (progn
                 (setq result-value (evaluate-valuef "OP_PLUS" result-value body))
                 (setq loop-condition (1- loop-condition))))
      (setq result (evaluate-res result-value tokens)))
    result))

(defun ARITHMETIC (tokens)
  (let ((result nil)
        (param1 nil)
        (param2 nil)
        (operator nil))

    ;; OP_OP kontrolü
    (unless (string= (next-token tokens) "OP_OP")
      (return-from ARITHMETIC (evaluate-res nil tokens)))

    (setq tokens (cdr tokens))  ;; OP_OP durumunda token listesi güncelleniyor

    (if (is-my-operator (next-token tokens))    ;; Operatör kontrolü
        (progn
          (setq operator (next-token tokens))
          (setq tokens (cdr tokens)))
        (return-from ARITHMETIC (evaluate-res nil tokens)))

    ;; Birinci parametreyi işle
    (setq result (EXPR tokens))
    (if (car result)
        (progn
          (setq param1 (car result))
          (setq tokens (cadr result)))
        (return-from ARITHMETIC (evaluate-res nil tokens)))

    ;; İkinci parametreyi işle
    (setq result (EXPR tokens))
    (if (car result)
        (progn
          (setq param2 (car result))
          (setq tokens (cadr result)))
        (return-from ARITHMETIC (evaluate-res nil tokens)))
    (if (string= (next-token tokens) "OP_CP")
        (setq result (evaluate-res (evaluate-valuef operator param1 param2) tokens))
        (return-from ARITHMETIC (evaluate-res nil tokens))) result))

(defun FUNCTION_DEFINITION (tokens)
  (let ((result nil)(name nil)(operator nil)(param1 nil) 
        (param2 nil)(operand1 nil)(operand2 nil)(argument-number 0))

    (if (string= (next-token tokens) "OP_OP")    ;; OP_OP kontrolü
        (setq tokens (cdr tokens))
        (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))

    (if (string= (next-token tokens) "KW_DEF")    ;; KW_DEF kontrolü
        (setq tokens (cdr tokens))
        (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))

    (setq result (IDENTIFIER tokens))    ;; IDENTIFIER (fonksiyon adı) kontrolü
    (cond 
      ((null (car result))
       (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))
      (t
        (setq name (car result))
        (setq tokens (cadr result))
        (when (gethash name *hashmap_of_function*)
          (format t "Function ~s is already defined~%" name)
          (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))))

    (setq result (IDENTIFIER tokens))
    (when (car result)
      (setq tokens (cadr result))
      (incf argument-number))

    (setq result (IDENTIFIER tokens))
    (when (car result)
      (setq tokens (cadr result))
      (incf argument-number))

    (if (string= (next-token tokens) "OP_OP")
        (setq tokens (cdr tokens))
        (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))

    (setq operator (next-token tokens))
    (unless (is-my-operator operator)
      (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))
    (setq result (OPERAND tokens))    ;; process first operand
    (cond 
      ((null (car result))
       (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))
      ((equal (type-of (car result)) (intern "RATIO"))
       (setq operand1 (car result)))) 

    (setq tokens (cadr result))
    (setq result (OPERAND tokens))    ;; process second operand
    (cond 
      ((null (car result))
       (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))  
      ((equal (type-of (car result)) (intern "RATIO"))
       (setq operand2 (car result))) )
    (setq tokens (cadr result))

    (unless (string= (next-token tokens) "OP_CP")    ;; first op_cp control
      (return-from FUNCTION_DEFINITION (evaluate-res nil tokens)))
    (setq tokens (cdr tokens))

    (if (string= (next-token tokens) "OP_CP")    ;; second op_cp control
        (progn
          ;; Syntax Error olmadığına göre tanımı yap
          (setf (gethash name *hashmap_of_function*) 
                (make-structure_of_function 
                 :name name
                 :operator operator
                 :operand1 operand1
                 :operand2 operand2
                 :argument-number argument-number)
          )
          (setq result (evaluate-res "#function" tokens))
        )
        (return-from DEF_FUNC (evaluate-res nil tokens))
    ) (setq tokens (cadr tokens)) result))

(defun FUNCTION-CALL (tokens)
  (let ((result nil)(name nil)(operator nil)(parameter1 nil)(parameter2 nil)(argument1 nil) 
        (argument2 nil)(argument-number nil))

    (if (string= (next-token tokens) "OP_OP")    ;; OP_OP kontrolü
        (progn
          (setq tokens (cdr tokens)))
        (return-from FUNCTION-CALL (evaluate-res nil tokens)))

    (setq result (IDENTIFIER tokens))    ;; Fonksiyon adı (IDENTIFIER) kontrolü
    (when (car result)
      (setq name (car result))
      (setq tokens (cadr result))

      (if (not (gethash name *hashmap_of_function*))      ;; Fonksiyon adı hash-map'te tanımlı mı?
          (progn
            (format t "Undefined Function: ~s~%" name)
            (return-from FUNCTION-CALL (evaluate-res nil tokens)))
          (let ((structure_of_function  (gethash name *hashmap_of_function*)))
            (setq name (structure_of_function-name structure_of_function ))
            (setf operator (structure_of_function-operator structure_of_function ))
            (setf argument1 (structure_of_function-operand1 structure_of_function ))
            (setf argument2 (structure_of_function-operand2 structure_of_function ))
            (setf argument-number (structure_of_function-argument-number structure_of_function )))))

    (setq result (OPERAND tokens))    ;; VALUEF (parametre1) kontrolü
    (when (car result)
      (setq parameter1 (car result))
      (setq tokens (cadr result)))

    (setq result (OPERAND tokens))    ;; VALUEF (parametre2) kontrolü
    (when (car result)
      (setq parameter2 (car result))
      (setq tokens (cadr result)))

    (if (string= (next-token tokens) "OP_CP")    ;; OP_CP kontrolü
        (progn
          (cond
            ((= argument-number 0)
             (setq result (evaluate-res (evaluate-valuef operator argument1 argument2) tokens)))
            ((= argument-number 1)
             (setq result (evaluate-res (evaluate-valuef operator parameter1 argument2) tokens)))
            ((= argument-number 2)
             (setq result (evaluate-res (evaluate-valuef operator parameter1 parameter2) tokens)))))
        (return-from FUNCTION-CALL (evaluate-res nil tokens)))
    (setq tokens (cadr tokens)) result))

(defun IDENTIFIER (tokens)
  "IDENTIFIER terminal sembolünü işler"
  (if (string= (next-token tokens) "IDENTIFIER")
      (evaluate-res (next-value tokens) tokens)
      (evaluate-res nil tokens)))

(defun VALUEF (tokens)
  "VALUEF terminal sembolünü işler"
  (let ((token (next-token tokens)) 
        (variable nil))
    (when (string= token "VALUEF")
      (setq variable (next-value tokens)))
    (evaluate-res variable tokens)))
(defun OPERAND (tokens)
  "Operand tanımlamasını kontrol eden fonksiyon"
  (labels ((check-operand-type (parser-func)
             "Belirli bir parser fonksiyonu ile operand kontrolü yapar"
             (let ((result (funcall parser-func tokens)))
               (when (car result)
                 (return-from OPERAND result)))))
    (or     ;; Operand tipleri için sırayla kontrol
     (check-operand-type #'IDENTIFIER)     ;; IDENTIFIER kontrolü
     (check-operand-type #'VALUEF)     ;; VALUEF kontrolü
     (evaluate-res nil tokens))))  ;; Tanımsız operand durumu

(defun evaluate-valuef (operator param1 param2)
  (cond
    ((string= operator "OP_PLUS") (+ param1 param2))
    ((string= operator "OP_MINUS") (- param1 param2))
    ((string= operator "OP_DIV") (/ param1 param2))
    ((string= operator "OP_MULT") (* param1 param2))))

(defun print-hash-table (entry-key entry-value)
  (let ((formatted-output 
          (format nil "key: ~a, value: ~a" entry-key entry-value)))
    (print formatted-output)))

(defun is-my-operator (candidate-operator)
  (member candidate-operator 
          '("OP_PLUS" "OP_MINUS" "OP_MULT" "OP_DIV") 
          :test #'string=))

(defun evaluate-res (flag token-list)
  (if flag 
      (cons flag (list (cdr token-list))) 
      (cons flag (list token-list))))

(defun next-token (token-hierarchy)
  (second (first token-hierarchy)))

(defun next-value (token-hierarchy)
  (first (first token-hierarchy)))

(defun format-valuef (numeric-fraction)
  (with-output-to-string (output-stream)
    (format output-stream "~ab~a" 
            (numerator numeric-fraction)
            (denominator numeric-fraction))))

(setf keywords 
      (list( list "and" "KW_AND") (list "or" "KW_OR") (list "not" "KW_NOT") (list "equal" "KW_EQUAL")
        (list "for" "KW_FOR") (list "while" "KW_WHILE") (list "less" "KW_LESS") (list "nil" "KW_NIL")
        (list "list" "KW_LIST") (list "append" "KW_APPEND") (list "concat" "KW_CONCAT") (list "set" "KW_SET")
        (list "def" "KW_DEF") (list "for" "KW_FOR") (list "if" "KW_IF") (list "while" "KW_WHILE")
        (list "exit" "KW_EXIT") (list "load" "KW_LOAD") (list "disp" "KW_DISP") (list "true" "KW_TRUE") (list "false" "KW_FALSE")))

(setf operators-alist 
      (list (list "+" "OP_PLUS") (list "-" "OP_MINUS") (list "/" "OP_DIV") (list "*" "OP_MULT")
      (list "(" "OP_OP") (list ")" "OP_CP") (list "," "OP_COMMA")))

;; Fonksiyonel erişim için helper fonksiyonlar ekledik
(defun get-keyword (keyword)
  (cdr (assoc keyword keywords)))

(defun get-operator (operator)
  (cdr (assoc operator operators-alist)))

(setf terminate-program nil)
(setf token-list())  ; our tokens
(setf unmached-token "") ; unmached token during tokenization process

(defun gpp-lexer (&optional filename)
  (if filename
      (process-file filename)
      (process-command-line))
  (setf tokens (reverse token-list)))

(defun process-file (filename)
  "Dosya okuma işlemi."
  (with-open-file (stream filename)
    (read-and-tokenize stream)))

(defun process-command-line ()
  (format t "> ")
  (loop
    (let ((current-char (read-char nil)))
      (when (or terminate-program (char= current-char #\Newline))
        (handle-command-line-termination current-char))
      (tokenize current-char))))

(defun handle-command-line-termination (current-char)
  (if terminate-program
      (return-from process-command-line nil)
      (progn
        (format t "> ")
        (clear-input))))

(defun read-and-tokenize (stream)
  (loop
     :for curr-char := (read-char stream nil) :while curr-char :do
     (tokenize curr-char)))

(defun tokenize (curr-char)
  (if (or
       (char= curr-char #\Space)       ; Space kontrolü
       (char= curr-char #\Tab)         ; Tab kontrolü
       (char= curr-char #\Newline)     ; Newline kontrolü
       (string= curr-char "(")         ; Parantez kontrolü
       (string= curr-char ")"))        ; Parantez kontrolü
      (progn
        (when (> (length unmached-token) 0)
          (let ((result nil))
            (cond
              ;; KEYWORD
              ((setf result (map-keyword unmached-token))
               (when (string= unmached-token "exit")
                 (setf terminate-program t))
               (push result token-list))

              ;; OPERATOR
              ((setf result (map-operator unmached-token))
               (push result token-list))

              ((is-valuef unmached-token)             ;; VALUEF (Rational parsing here instead of using parse-rational)
               (let* ((split-pos (position #\b unmached-token))  ; Find position of 'b' to split
                      (numerator nil)
                      (denominator nil))
                 (if split-pos
                     (progn
                       (setf numerator (parse-integer (subseq unmached-token 0 split-pos)))
                       (setf denominator (parse-integer (subseq unmached-token (1+ split-pos))))
                       (push (list (/ numerator denominator) "VALUEF") token-list)  ; Directly push the parsed value
                     ))))

              ((is-identifier unmached-token)   ;; IDENTIFIER
               (push (list unmached-token "IDENTIFIER") token-list))
              (t nil))  ;; SYNTAX-ERROR
            (setf unmached-token "")))
        (when (or (string= curr-char "(") (string= curr-char ")"))
          (push (map-operator curr-char) token-list)))
  (setf unmached-token (concatenate 'string unmached-token (string curr-char)))))

(defun is-legal-token (sample)
    (let ((curr 0)
          (end (length sample))
          (result t))
        (loop while (< curr end) do
            (let ((c (char sample curr)))
                (if (or (is-digit c) (is-alpha c) (char= c #\_) (map-operator c))
                    (setq curr (1+ curr)) ; Geçerli bir karakter ise sonraki karaktere geç
                    (progn 
                        (setq result nil) ; Geçersiz bir karakter bulundu
                        (return)))))  result)) 

(defun is-digit (sample)
  (let ((ascii (char-code sample)))
    (cond ((and (>= ascii (char-code #\0)) (<= ascii (char-code #\9))) t)
          (t nil))))
(defun is-alpha (sample)
    (setf ascii (char-code sample))
    (or 
        (and (>= ascii (char-code #\a)) (<= ascii (char-code #\z))) 
        (and (>= ascii (char-code #\A)) (<= ascii (char-code #\Z)))))

(defun is-valuef (sample)
  (let ((curr 0)
        (end (length sample))
        (temp-valuef nil))

    (let ((first-char (char sample 0)))
      (if (char= first-char #\0)
          (if (eq (length sample) 1)
              nil  t)))  
    (loop
      (cond
        ((eq curr end) (return t))
        ((is-digit (char sample curr))
         (setf curr (1+ curr)))
        ((char= (char sample curr) #\b)
         (setf temp-valuef t)
         (setf curr (1+ curr)))
        (t (return nil))))))

(defun is-identifier (sample)
  (let ((firstLetter (char sample 0))
        (curr 1)
        (end (length sample)))
    (if (or (is-alpha firstLetter) (char= firstLetter #\_))
        (progn
          (loop while (< curr end) do
            (let ((c (char sample curr)))
              (if (not (or (is-alpha c) (is-digit c) (char= c #\_)))
                  (return-from is-identifier nil)))
            (setq curr (1+ curr)))
          t) ;; Döngü bittiğinde sorun yoksa true döndür
        nil)))

(defun map-operator (key)
  (dolist (pair operators-alist)
    (when (string= key (car pair))
      (return pair))))

(defun map-keyword (key)
  (loop for pair in keywords
        if (string= key (car pair))
        return pair))

(gppinterpreter *args*)