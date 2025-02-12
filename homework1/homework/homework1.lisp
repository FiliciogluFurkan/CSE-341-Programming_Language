(let ((quicklisp-file "~/quicklisp/setup.lisp"))
  (when (not (probe-file quicklisp-file))
    (error "Quicklisp not found. Please install Quicklisp."))
  (load quicklisp-file))
;;here, we need to load cl-ppcre and split-sequence packages

(ql:quickload "cl-ppcre")
(ql:quickload "split-sequence")
;;here, we need to load the homework package 

(defpackage :homework
  (:use :cl :cl-ppcre :split-sequence))

(in-package :homework)

(defun line-type (line)
  (cond
    ((cl-ppcre:scan "if\\s*\\(" line) 'if-statement) ;if line type equals if-statement,if statemens becomes line type name
    ((cl-ppcre:scan "while\\s*\\(.*\\)\\s*{" line) 'while-statement) ;if line type equals while-statement,while statements becomes line type name
    ((cl-ppcre:scan "for\\s*\\(" line) 'for-statement); if line type equals for-statement,calls for-statement function,for statements becomes line type name
    ((cl-ppcre:scan "\\w+\\s*\\w+\\s*=\\s*\\w+\\(\.*\\);" line) 'variable-assignment-by-function) ;if line type equals variable-assignment-by-function, variable assignment by function becomes line type name
    ((cl-ppcre:scan "return\.*" line) 'return) ;if line type equals return,return becomes line type name,return statement becomes line type name
    ((cl-ppcre:scan "\\w+\\s*(\\&\\&|\\|\\|)\\s*\\w+" line) 'logical-operation) ;if line type equals logical-operation,logical-operation becomes line type name
    ((cl-ppcre:scan "\\w+\\s*(==|!=|<=|>=|<|>)\\s*\\w+" line) 'comparison-operation); if line type equals comparison,comparison-operation becomes line type name
    ((cl-ppcre:scan "\\w+\\s*[-+*/%]\\s*\\w+" line) 'arithmetical-operation); if line type equals arithmetical-operation, arithmetical-operation becomes line type name
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*=\\s*\\w+\\s*;" line)  'variable-declaration); if line type equals variable-declaration, variable-declaration becomes line type name
    ((cl-ppcre:scan "\\w+\\s*=\\s*\\w+\\s*" line)  'variable-assignment); if line type equals variable-assignment,  variable-assignment becomes line type name
    ((cl-ppcre:scan "printf\\s*\\(\"\.*\",\\s*\.*\\s*\\);" line) 'printf-variable); if line type equals printf,printf-variable becomes line type name
    ((cl-ppcre:scan "printf\\s*\\(\"\.*\"\\s*\\);" line) 'printf-literal); if line type equals printf-literal,
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(\.*\\)\\s*;" line) 'function-declaration); if line type equals function-
    ((cl-ppcre:scan "^\\s*$" line) 'empty-line);  if line type equals empty-line, empty-line becomes line type name
    ((cl-ppcre:scan "\\w{1}\\s*\\(.*\\)\\s*;" line) 'function-call); if line type equals function-call, function-call becomes line type name
    ((cl-ppcre:scan "\\w+\\s+\\w+\\s*\\(\.*\\)\\s*{" line) 'function-definition); if line type equals function-definition, function-definition becomes line type name
    ((cl-ppcre:scan "\\}" line) 'close-brace); if line type equals close-brace, close-brace becomes line type name
    ((cl-ppcre:scan "\\{" line) 'open-brace);  if line type equals open-brace, open-brace becomes line type name
    ((cl-ppcre:scan "\\w+;" line) 'return-value); if line type equals return-value, return-value becomes line type name
    (t 'unknown))
)

;;we define a function that takes a line as an argument and returns a lisp code as a string
(defun convert-line (line) 
  (let ((type (line-type line))) ; call line-type function to get line type
    (cond
      ((eq type 'if-statement) (convert-if line)) ; if the line type is if-statement, call convert-if
      ((eq type 'for-statement) (convert-for-loop line)) ; if the line type is for-statement, call convert-for-loop
      ((eq type 'while-statement) (convert-while-loop line)) ; if the line type is while-statement, call convert-while-loop
      ((eq type 'return) (convert-return line)) ; if the line type is return, call convert-return
      ((eq type 'arithmetical-operation) (convert-arithmetical-operation line)) ; if arithmetical-operation, call convert-arithmetical-operation
      ((eq type 'variable-declaration) (convert-variable-declaration line)) ; if variable-declaration, call convert-variable-declaration
      ((eq type 'variable-assignment) (convert-variable-assignment line)) ; if variable-assignment, call convert-variable-assignment
      ((eq type 'function-declaration) (convert-function-declaration line)) ; if function-declaration, call convert-function-declaration
      ((eq type 'function-definition) (convert-function-definition line)) ; if function-definition, call convert-function-definition
      ((eq type 'close-brace) (convert-close-bracket line)) ; if close-brace, call convert-close-bracket
      ((eq type 'open-brace) nil) ; if open-brace, return nil
      ((eq type 'printf-literal) (printfToLispLiteral line)) ; if printf-literal, call printfToLispLiteral
      ((eq type 'printf-variable) (printfToLispVariable line)) ; if printf-variable, call printfToLispVariable
      ((eq type 'variable-assignment-by-function) (convert-variable-assignment-by-function line)) ; if variable-assignment-by-function, call convert-variable-assignment-by-function
      ((eq type 'function-call) (convert-function-call line)) ; if function-call, call convert-function-call
      ((eq type 'return-value) (convert-return-value line)) ; if return-value, call convert-return-value
      ((eq type 'empty-line) (convert-empty-line line)) ; if empty-line, call convert-empty-line
      (t (format nil "Unknown type: ~A" type))))) ; if unknown type, return error message



(defun convert-function-definition (line)
  (if (string= line "int main() {") ;if line is int main, we return a string that defines a function named main
      "(defun main () (let (" ;we define a function named main
      (let* ((parts (cl-ppcre:split "^\\w+ " line)) ;split line into parts
             (return-type (first parts)) 
             (function (second parts))
             (function-name (first (cl-ppcre:split "\\(" function)))
             (formatted-params (convert-parameters-with-type function)))
        (format nil "(defun ~a ~a " function-name formatted-params))));return a string that defines a function named function-name



(defun convert-function-declaration (line) ;function declaration is a function that takes no arguments and returns a value of type return-type
  (let* ((parts (cl-ppcre:split " " line :limit 2)) ;split line into parts
         (return-type (first parts)) 
         (print return-type)
         (function (second parts))
         (function-name (first (cl-ppcre:split "\\(" function)))
         (return-type-lisp
          (let ((lowercase-type (string-downcase return-type))) ;convert return-type to lowercase
            (cond ;if return-type is double, float, int, or void, return a lisp type that corresponds to return-type
             ((string= lowercase-type "double") 'double-float)
             ((string= lowercase-type "char") 'character)
             ((string= lowercase-type "void") 'void)
             ((string= lowercase-type "int") 'integer)
             ((string= lowercase-type "float") 'float)
             (t (error "Unknown type: ~a" return-type)))))
         (params (parse-parameters-from-function function)) ;parse function parameters
         (types (parse-types params)) ;parse function parameters types
         (converted-types '()))


    (dolist (type types) ;iterate over types
      (let ((converted-type
             (let ((lowercase-type (string-downcase type)))
               (cond ;if type is double, char, void, int, or float, return a lisp type that corresponds to type
                ((string= lowercase-type "double") 'double-float)
                ((string= lowercase-type "char") 'character)
                ((string= lowercase-type "void") 'void)
                ((string= lowercase-type "int") 'integer)
                ((string= lowercase-type "float") 'float)
                (t (error "Unknown type: ~a" type))))))
        (push converted-type converted-types)))
    (format nil "(declaim (ftype (function (~a) ~a) ~a))"
            (reduce (lambda (a b)
                      (if (string= a "")
                          b
                          (format nil "~a ~a~a" a " " b)))
                    converted-types
                    :initial-value "")
            return-type-lisp
            function-name)))




(defun convert-variable-assignment (line) ;variable assignment is a variable that takes a value
  (let* ((assignment (cl-ppcre:split "\\s*=\\s*" line)) ;split line into assignment
         (var (string-trim '(#\Newline #\Return #\Space #\Tab) (first assignment)))
         (value (first ( cl-ppcre:split ";" (string-trim '(#\Newline #\Return #\Space #\Tab) (second assignment))))))
    (format nil "(setf ~a ~a)" var value))) ;return a string that assigns a value to a variable and writes it to the console with format setf
 

(defun convert-function-call (line) ;function call is a function that takes arguments and returns a value
  (let* ((split-line (cl-ppcre:split "\\s*\\(" line)) ;split line into function name and arguments
         (function-name (first split-line))
         (arguments (second split-line)))
    (format nil "(~a ~a)" function-name (string-trim ")" arguments)))) 
 


(defun printfToLispLiteral (line) ;print function literal is a function that takes a string as an argument and returns void
  (let* ((parts (cl-ppcre:split "\"" line))  
         (literal (second parts)))           
    (when literal  
      (format nil "(format t \"~a\")" literal)))) ;return a string that prints a string literal to the console with format t



(defun printfToLispVariable (line) ;print function variable is a function that takes a string as an argument and returns void
  (let* ((parts (cl-ppcre:split "\"" line))
         (literal (second parts))
         (after-literal (third parts)) ;split literal into format string and variable
         (variable (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "," after-literal))))) ;split literal into variable and format string
         (clean-literal (if (cl-ppcre:scan "\\);$" literal) ; if literal ends with ), remove ;
                            (first (cl-ppcre:split "\\);" literal)) ;split literal into variable and format string
                            literal))) 
          (print clean-literal) ;print literal
          (setf after-literal (redefine-percent-to-tilde clean-literal)) 
          (print after-literal) ;print literal with tildes
    
    (when (and variable (cl-ppcre:scan "\%[dfi]" clean-literal)) ;if variable is a format string, return a string that prints a variable to the console with format
      (let ((formatted-literal (cl-ppcre:split "\%" variable))) ;split variable into format string and variable
        (format nil "(format t \"~a\" ~a)" after-literal variable))))) 
 
 ;redefine-percent-to-tilde function takes a string as an argument and returns a string with tildes instead of percents
(defun redefine-percent-to-tilde (input-string)
  (let ((result (make-string (length input-string))))
    (loop for i from 0 below (length input-string)
          do (setf (char result i)
                   (if (char= (char input-string i) #\%)
                       #\~ ;if char is %, replace it with ~
                       (char input-string i)))) result))

;;convert-parameters function takes a string as an argument and returns a string with the parameters converted to lisp
(defun convert-parameters (params)
  (print params)
  (let* ((params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" params))))) ;split params-str into parameters and function name
         (arguments (cl-ppcre:split "," params)) ;split parameters into arguments
         (cleaned-args '())) ;cleaned-args is a list of cleaned arguments

    (dolist (parameter arguments)
      (let ((trimmed (string-trim " " parameter)))
        (push trimmed cleaned-args)))
    (format nil "~a"
            (reduce (lambda (a b) ;reduce cleaned-args into a string
                      (if (string= a "") ; if a is empty, return b
                          b
                          (format nil "~a ~a~a" a " " b))) ; else, add a space and b
                    cleaned-args ; cleaned-args is a list of cleaned arguments
                    :initial-value "")))) ; return the first element of cleaned-args



;; convert-parameters-with-type takes a string of parameters and returns a string of the same parameters with the type of each variable
(defun convert-parameters-with-type (params-str)
  (print params-str)
  (let* ((params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" params-str)))))
         (arguments (cl-ppcre:split "," params))
         (cleaned-args '())) ; cleaned-args is a list of cleaned arguments

    (dolist (parameter arguments) ;for each argument in arguments
      (let ((variable-name (second (cl-ppcre:split " " (string-trim " " parameter)))))
        (push variable-name cleaned-args))) ; push the variable name to cleaned-args

    (print cleaned-args)
    (format nil "(~a)" ; return a string of the form (a b c)
            (reduce (lambda (a b)
                      (if (string= a "") ; if a is empty, return b
                          b ; else, add a space and b
                          (format nil "~a ~a~a" a " " b)))
                    cleaned-args
                    :initial-value ""))))


;; parse-parameters-from-function takes a string of a function and returns a string of the same function with the parameters cleaned
(defun parse-parameters-from-function (function)
  (let* ( ; split the function into the parameters and arguments
    (params (first (cl-ppcre:split "\\)" (second (cl-ppcre:split "\\(" function))))) ; split the parameters into the parameters according to the ( character
    (arguments (cl-ppcre:split "," params)) ; split the arguments into the arguments according to the , character
    (cleaned-args '()))
   
   (dolist (parameter arguments)
    (let ((param  (string-trim " " parameter)))
      (push param cleaned-args)
    )
   )
     (nreverse cleaned-args)
  )
)
;; parse-types takes a list of parameters and returns a list of the types of each parameter
(defun parse-types (params) ; params is a list of parameters
  (let* (
    (types '())) ; types is a list of the types of each parameter
   
   (dolist (param params) ; for each parameter in params
    (let ((param  (first (cl-ppcre:split " " (string-trim " " param))))) ; split the parameter into the variable name and the type
      (push param types) ; push the variable name to types
    )
   )
     (nreverse types); return the list of types
  )
)


;; convert-variable-assignment-by-function takes a string of a variable assignment by a function and returns a string of the same variable assignment with the parameters cleaned
(defun convert-variable-assignment-by-function (line)
  (let* ((split-line (cl-ppcre:split "=" line)) ; split the line into the variable name and the function call
         (variable-split (cl-ppcre:split " " (first split-line))) ; split the variable name into the variable name and the type
         (variable-name (string-trim " " (second variable-split))) ; split the variable name into the variable name and the type
         (function-call (string-trim " " (second split-line))); split the function call into the function call
         (function-call-without-sc (string-trim " " (first (cl-ppcre:split ";" function-call)))) ; split the function call into the function call without the ;
         (function-name (first (cl-ppcre:split "\\(" function-call-without-sc))) ; split the function call into the function name and the parameters
         (cleaned-args (convert-parameters function-call))) ; convert the parameters into a string of the same parameters with the type of each variable
    (format nil "(~a (~a ~a))" variable-name function-name cleaned-args)
  )
)
; convert-variable-declaration takes a string of a variable declaration and returns a string of the same variable declaration with the parameters cleaned
(defun convert-variable-declaration (line) ; line is a string of a variable declaration
  (let* ((split-line (cl-ppcre:split "=" line)) ; split the line into the variable name and the assigned value
         (variable-split (cl-ppcre:split " " (first split-line))) ; split the variable name into the variable name and the type
         (variable-name (string-trim " " (second variable-split))) ; split the variable name into the variable name and the type
         (assigned-value (first (cl-ppcre:split ";" (string-trim " " (second split-line)))))) ; split the assigned value into the assigned value and the parameters
    (format nil "(~a ~a)" variable-name assigned-value)
  )
)
; convert-return takes a string of a return statement and returns a string of the same return statement with the parameters cleaned
(defun convert-return (line) ; line is a string of a return statement
  (let ((value (second (cl-ppcre:split " " line :limit 2)))) ; split the line into the return value and the parameters 
    (if (string= line "return 0;") ; if the return value is 0, return the return statement with the parameters cleaned
        (format nil ")~a)" (convert-line value)) ; adds additional parentheses to the return statement with the parameters cleaned
        (format nil "~a" (convert-line value)))))  ; return the return statement with the parameters cleaned

(defun convert-return-value (line) 
  (let ((value (first (cl-ppcre:split ";" line))))
    (format nil "(~a)" value)))
;;
;;   "Preserve empty lines in the Lisp code."
(defun convert-empty-line (line)
  "Preserve empty lines in the Lisp code."
  (format nil "~%"))
 
 
(defun convert-if (line) ; line is a string of an if statement
  (let* ((condition (second (cl-ppcre:split "\\s*if\\s*\\(" line))) ; split the line into the condition
         (cleaned-condition (string-trim ")" condition)) ; trim the ) character from the condition
         (parts (cl-ppcre:split "\\s+" cleaned-condition)))  
    (setf cleaned-condition (format nil "~a ~a ~a" (second parts) (first parts) (third parts)))
    (format nil ")(if( ~a ~%  (progn ~%    " cleaned-condition)))  ; return a string of the form (if( condition (progn but i couldn't close the pharantesis both if and progn at the same time
 
 ;; convert-unknown takes a string of an unknown line and returns a string of the same unknown line
(defun convert-unknown (line)
  (format nil ";; Unknown line: ~a~%" line))

;; convert-close-bracket takes a string of a closing bracket and returns a string of the same closing bracket
(defun convert-close-bracket (line)
  (if (cl-ppcre:scan "\\s*}\\s*" line)
      (let ((modified-line (cl-ppcre:regex-replace-all "\\s*}\\s*" "" line))) 
        (concatenate 'string modified-line ")");convert } to )
      )
      line))

;; convert-while-loop takes a string of a while loop and returns a string of the same while loop
(defun convert-while-loop (line)

  (let* ((trimmed-line (string-trim " " line))
         (start (search "(" trimmed-line)) ; search for the first ( character
         (end (search ")" trimmed-line)) ; search for the first ) character
         (condition (if (and start end) (subseq trimmed-line (1+ start) end) nil)) ; if the start and end are not nil, return the substring between the first ( and the first ) characters
         (clean-condition (string-trim " " condition)) ; trim the spaces from the condition
         (loop-variable (first (cl-ppcre:split "\\s+" clean-condition))) ; split the condition into the loop variable and the limit
         (limit (nth 2 (cl-ppcre:split "\\s+" clean-condition))) ; split the condition into the loop variable and the limit
         (pseudo-for-line (format nil "for (int ~a = ~a; ~a < ~a; ~a++)"
                                  loop-variable
                                  loop-variable
                                  loop-variable
                                  limit          
                                  loop-variable)))
    (convert-for-loop pseudo-for-line))) 
 
 
 
 ;; convert-for-loop takes a string of a for loop and returns a string of the same for loop
(defun convert-for-loop (line) ; line is a string of a for loop
  (let* ((parts (cl-ppcre:split "[();]" line)) ; split the line into the initialization, condition, and increment
         (initialization (string-trim " " (nth 1 parts)))   ; trim the spaces from the initialization
         (condition (string-trim " " (nth 2 parts)))      ; trim the spaces from the condition
         (increment (string-trim " " (nth 3 parts))))      ; trim the spaces from the increment
    (let* ((init-parts (cl-ppcre:split "\\s+" initialization)) ; split the initialization into the initialization and the limit
           (var (second init-parts))          ; split the initialization into the initialization and the limit
           (init-value (fourth init-parts)))  
      (let ((limit (second (cl-ppcre:split "\\s*<\\s*" condition))))  ; split the condition into the loop variable and the limit
        (if (cl-ppcre:scan "\\+\\+" increment)
            (format nil "(loop for ~a from ~a below ~a do" var init-value limit) ; return a string of the form (loop for var from init-value below limit do
            (error "Unsupported increment format: ~a" increment))))))

;; convert-arithmetical-operation takes a string of an arithmetical operation and returns a string of the same arithmetical operation
(defun convert-arithmetical-operation (line)
  (print line)
  (let* ((parts (cl-ppcre:split "\\s*[-+*/%]\\s*" (first (cl-ppcre:split ";" line)))) ; split the line into the operator and the operands
         (operator (second (cl-ppcre:split " " line)))
         (first-operand (first parts))
         (second-operand (second parts)))
    (format nil "(~a ~a ~a)" operator first-operand second-operand))) ; return a string of the form (operator first-operand second-operand)

 
(defun write-to-output (output-line) ; output-line is a string of a line that has been converted to Lisp
  (with-open-file (out "output.lisp" :direction :output :if-does-not-exist :create :if-exists :append) ; open the output file in the current directory and append the output-line to the file
    (format out "~A~%" output-line))) ; write the output-line to the file

; process-file-recursively takes a stream of a file and recursively processes the file
(defun process-file-recursively (stream) ; stream is a stream of a file
  (let ((line (read-line stream nil 'eof))) ; read a line from the stream
    (when (not (eq line 'eof)); if the line is not eof, do the following
      (let ((cleaned-line (clean-line line)))  ; clean the line
        (when (not (string= cleaned-line "")) ; if the cleaned-line is not empty, do the following
          (let ((converted-line (convert-line cleaned-line))) ; convert the cleaned-line to Lisp
            (when converted-line ; if the converted-line is not nil, do the following
              (write-to-output converted-line) ; write the converted-line to the output file
              )))))
    (process-file-recursively stream))) ; recursively process the file
 
 
(defun process-file (filename) ; filename is a string of the name of a file
  (with-open-file (stream filename :direction :input) ; open the file in the current directory
    (process-file-recursively stream)))  ; recursively process the file
 
 ;; clean-line takes a string of a line and returns a string of the same line with all whitespace removed
(defun clean-line (line)
  (string-trim '(#\Newline #\Return #\Space) line))
 
 ;; process-file takes a string of the name of a file and processes the file
(process-file "input.c")

;; quit the lisp interpreter
(quit)