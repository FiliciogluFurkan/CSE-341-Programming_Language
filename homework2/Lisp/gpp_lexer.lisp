(defun is-alphanumeric-char (char)
  (alphanumericp char)
)

;; controls the number
(defun is-number (str)
  (and (not (string= str ""))        ;; not empty
       (every #'digit-char-p str)))  ;; all characters should be number

;; number+f+number (example: 123f456)
(defun is-number-with-letter (str)
  (let ((len (length str)))
    (and (> len 2)  ;; Must be at least one number + letter + one number
         (some #'digit-char-p str) ;; Part of the string must be numbers
         (let ((f-index (position #\f str))) ;; 'f' find position
           (and f-index  ;; 'f'exists
                (> f-index 0)  ;; 'f', should come after first character
                (< f-index (- len 1)) ;; 'f', son karakterden önce olmalı
                (every #'digit-char-p (subseq str 0 f-index)) ;; There should be only numbers before 'f'
                (every #'digit-char-p (subseq str (+ f-index 1) len))))))) ;; There should be only numbers afyer 'f'


;; ID check (such as: abc123 or ABC123)
(defun is-identifier (str)
  (and (> (length str) 0)         ;; at least one character
       (alpha-char-p (elt str 0)) ;; first character should be letter
       (every #'(lambda (ch) (or (alpha-char-p ch) (digit-char-p ch))) str))) ;; remainings one can be letter or nummber


(defun classify-token (token)
 ;type of the token 
  (cond ;controls token type
    ((equal token "+") 'OP_PLUS)
    ((equal token "-") 'OP_MINUS)
    ((equal token "*") 'OP_MULT)
    ((equal token "/") 'OP_DIV)
    ((equal token "(") 'OP_OP)
    ((equal token ")") 'OP_CP)
    ((equal token ",") 'OP_COMMA)
    ((equal token "true") 'KW_TRUE)
    ((equal token "COMMENT") 'COMMENT)
    ((equal token "false") 'KW_FALSE)
    ((equal token "equal") 'KW_EQUAL)
    ((equal token "and") 'KW_AND)
    ((equal token "or") 'KW_OR)
    ((equal token "not") 'KW_NOT)
    ((equal token "less") 'KW_LESS)
    ((equal token "nil") 'KW_NIL)
    ((equal token "list") 'KW_LIST)
    ((equal token "append") 'KW_APPEND)
    ((equal token "concat") 'KW_CONCAT)
    ((equal token "deffun") 'KW_DEFFUN)
    ((equal token "set") 'KW_SET)
    ((equal token "for") 'KW_FOR)
    ((equal token "if") 'KW_IF)
    ((equal token "load") 'KW_LOAD)
    ((equal token "disp") 'KW_DISP)
    ((equal token "exit") 'KW_EXIT)
    ((is-number token) 'VALUEI)  ;; number
    ((is-number-with-letter token) 'VALUEF)  ;; number+f+number
    ((is-identifier token) 'IDENTIFIER)  ;; identifier
  ))


(defun lex-token (input)
  (loop for char across input
        with tokens = '()
        with current-token = ""
        with in-comment = nil ;; contorls comment lines
        do
        (progn
          (cond
            ;; comment line control
            ((and (not in-comment) (char= char #\;)) ;; beginning of the comment line
             (setq in-comment t)  ;; start comment line
            (push "COMMENT" tokens))
            (in-comment
             (if (char= char #\Newline)  ;; end of the comment line
                 (progn
                   (setq in-comment nil)  ;; out from comment line
                  )
                )) 
             
           
            ;; Space characters (space, tab, newline), but except 'n'!
            ((and (find char " \t\n") (not (char= char #\n)) (not (char= char #\t)))
             (when (and current-token (not (string= current-token "")))
               (let ((classified-token (classify-token (string-trim " " current-token))))
                 (print (format nil "Token completed on space: ~A" current-token))
                 (push classified-token tokens)
                 (setq current-token "")))) ;; reset current token

            ((find char "+-*/(),") ;; operators ang pharantesies
             (when (not (string= current-token "")) ;; if current token not empty
               (push (classify-token (string-trim " " current-token)) tokens)
               (setq current-token "")) ;; process previous token

             ;; add character operator as token
             (push (classify-token (string char)) tokens)
            )

            ((is-alphanumeric-char char)
             (setq current-token (concatenate 'string current-token (string char))))
            (t (format t "Syntax Error ~a~% cannot be tokenized" char))
            ))
        finally
        (progn
          (when current-token
            (push (classify-token (string-trim " " current-token)) tokens))
          (print (format nil "Final tokens: ~A" (reverse tokens))) ;; write final tokens
          (return (reverse tokens)))))


(defun analyze-tokens (tokens)
        (format t "~%")
    (loop for token in tokens do
        (format t "~a~%" token)
    )
)

(defun read-file (filename)
  "Verilen dosya adını alıp dosyadaki tüm metni bir string olarak döndüren fonksiyon."
  (with-open-file (stream filename :direction :input)
    (let ((content "")) 
      (loop for line = (read-line stream nil)
            while line do
            (setq content (concatenate 'string 
                                     content 
                                     line 
                                     (string #\Newline)))) 
      content)))

(defun gppinterpreter (&optional file-name)
  (if file-name
      (let ((input (read-file file-name)))   ;; read data from file
        (let ((tokens (lex-token input)))    ;; analayze the tokens
          (analyze-tokens tokens)))          ;; write results
    (progn
      (force-output)
      (format t "Please enter code: ")      ;; message for input from user
      (read_from_user))))                   ;; if not called with filename,gets input

(defun read_from_user ()
  (let ((input (read-line)))             ;; readline from user
    (format t "You entered: ~A~%" input)  ;; write what it wrote
    (let ((tokens (lex-token input)))     ;; analayze tokens
      (format t "Coming token here is: ~A~%" tokens)  ;; write tokens
      (analyze-tokens tokens)             ;; analayze tokens
      (format t "Final tokens: ~A~%" tokens)))) ;;write last tokens

;when read from the file,it writes the output twice.You can see in both format