(defun is-operator (in-str operators) ;check given string if it matches any operator in the list, return true, else return false
	(if (null (first operators) )
		(= 1 2)
		(if (string= in-str (first operators))
			(= 1 1)
			(is-operator in-str (rest operators))
		)
	)
)

(defun is-keyword (in-str keywords);check given string if it matches any keyword in the list, return true, else return false
	(if (null (first keywords) )
		(= 1 2)
		(if (string= in-str (first keywords))
			(= 1 1)
			(is-keyword in-str (rest keywords))
		)
	)
)

(defun is-boolean (in-str);check given string if it is a bool value, return true, else return false
	(if (string= in-str "true")
		(= 1 1)
		(if (string= in-str "false")
			(= 1 1)
			(= 1 2)
		)
	)
)

(defun is-id (in-str index);check given string if it is an identifier, return true, else return false
	(if (= index (length in-str) )
		(= 1 1)
		(if (alpha-char-p (char in-str index))
			(is-id in-str (+ 1 index))
			(= 1 2)
		)
	)
)

(defun is-integer (in-str index) ;check given string if it is an integer, return true, else return false
	(if (= index (length in-str) )
		(= 1 1)
		(if (and (= 0 index) (string= (char in-str index) "-"))
			(is-integer in-str (+ 1 index))
			(if (digit-char-p (char in-str index))
			(is-integer in-str (+ 1 index))
			(= 1 2)
			)
		)	
	)
)

(defun parse-string (in-str index operators) ;Parses the given string to terminals
	(if (= index (- (length in-str) 1) ) ;Check of last character of the given string
		(if (is-operator (char in-str index) operators) ;Operator check
			(cons "" (cons (string (char in-str index)) '()) ) ;if the last character is an operator, return a list with empty string and operator  
			( if (string= " " (string (char in-str index))) ;Last characters whitespace check 
				nil ;if its whitespace character return nil
				(cons (string (char in-str index)) '()) ; if the last character is a letter or a digit, return character in a list  
			) 
		)
		(if (string= (char in-str index) " ") ; Character whitespace check
			(cons "" (parse-string in-str (+ index 1) operators)) ;If the character is a white space , get next index, returns the returned list with adding empty string.
			(if (and (string= (char in-str index) "*") (string= (char in-str (+ 1 index)) "*") ) ;checks '**' operator
				(if (= 1 (- (- (length in-str) 1) index));if it finds '**' in end of string returns list with empty string and operator,else save operator and continue parse. 
					(cons "" (cons "**" '()))
					(cons "" (cons "**" (parse-string in-str (+ index 2) operators)))
				)	
				(if (and (string= (char in-str index) "-") (digit-char-p (char in-str (+ 1 index))) ); Check '-', if it is part of an integer value or an operator. 
					(cons (concatenate 'string (string (char in-str index)) (first (setq temp(parse-string in-str (+ index 1) operators))) ) (rest temp));Add negative sign of integer
					(if (is-operator (char in-str index) operators);operator check
						(cons "" (cons (string (char in-str index)) (parse-string in-str (+ index 1) operators) )); Adds empty string and the operator to list 
						(cons (concatenate 'string (string (char in-str index)) (first (setq temp (parse-string in-str (+ index 1) operators))) ) (rest temp)) ;Add digit or letter 
					)
				)
			)
		)
	)
)

(defun set-token (str-list operators keywords) ; sets the headers
	(if (null str-list)
		nil
		(cond 
			((is-operator (first str-list) operators) (cons (list "operator" (first str-list)) (set-token (rest str-list) operators keywords ) ) ) 
			((is-keyword (first str-list) keywords) (cons (list "keyword" (first str-list)) (set-token (rest str-list) operators keywords ) ) )
			((is-boolean (first str-list)) (cons (list "boolean" (first str-list)) (set-token (rest str-list) operators keywords ) ) )
			((is-id (first str-list) 0) (cons (list "identifier" (first str-list)) (set-token (rest str-list) operators keywords ) ) )
			((is-integer (first str-list) 0) (cons (list "integer" (first str-list)) (set-token (rest str-list) operators keywords ) ) )
			(1 (cons (list "unknown" (first str-list)) (set-token (rest str-list) operators keywords ) ))
		)
	)
)

(defun remove-emptys (str-list) ; removes empty string elements from given string list
	(if (null str-list)
		nil
		(progn
			(setq temp (remove-emptys (rest str-list)))
			(if (string/= (first str-list) "")
				(cons (first str-list) temp)
				temp
			)
		)
	)
)

(defun get-file (fp) ; Gets file pointer and returns all file as a string
	(setq line (read-line fp nil))
	(if (null line)
		nil
		(concatenate 'string line (get-file fp))  
	)
)

(defun lexer (filename)
	(setq operators '(")" "+" "-" "/" "*" "(" "**")) ; operator list
	(setq keywords '("and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit")) ;keyword list
	
	;Get file as a string and parse it ,and tokenize it.
	(set-token (remove-emptys (parse-string (get-file (setq in (open filename :if-does-not-exist nil))) 0 operators)) operators keywords )
)
