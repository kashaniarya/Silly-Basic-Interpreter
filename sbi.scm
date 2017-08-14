#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; Kaio Barbosa    ksbarbos
;; Arya Kashani    akashani

;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

;label hash table
(define *label-ht* (make-hash))

;variable hash table
(define *variable-ht* (make-hash))

;function hash table
(define *function-ht* (make-hash))

;function to get from label table
(define (label-get key)
    (hash-ref *label-ht* key))

;function to put into label hash table
(define (label-put! key value)
    (hash-set! *label-ht* key value))

;function to get from variable table
(define (variable-get key)
    (hash-ref *variable-ht* key))

;function to put into variable hash table
(define (variable-put! key value)
    (hash-set! *variable-ht* key value))

;function to get from function table
(define (function-get key)
    (hash-ref *function-ht* key))

;function to put into function hash table
(define (function-put! key value)
    (hash-set! *function-ht* key value))

;store into variable hash table pi & e before main begins
(for-each
    (lambda (pair)
        (variable-put! (car pair) (cadr pair)))
    `(
    (e       2.718281828459045235360287471352662497757247093)
    (pi      3.141592653589793238462643383279502884197169399)
    ))

;store necessary functions into function hash table before main begins
(for-each
    (lambda (pair)
        (function-put! (car pair) (cadr pair)))
    `(
    (abs    ,abs)
    (acos    ,acos)
    (asin    ,asin)
    (atan    ,atan)
    (ceil    ,ceiling)
    (cos    ,cos)
    (exp    ,exp)
    (floor    ,floor)
    (log    ,log)
    (log10   ,(lambda (x) (/ (log x) (log 10.0))))
    (log2    ,(lambda (x) (/ (log x) (log 2.0))))
    (round    ,round)
    (sin    ,sin)
    (sqrt    ,sqrt)
    (tan    ,tan)
    (trunc    ,truncate)
    (+    ,+)
    (-    ,-)
    (/    ,(lambda (x y) (/ x (+ y 0.0))))
    (*    ,*)
    (log10_2 0.301029995663981195213738894724493026768189881)
    (sqrt_2  1.414213562373095048801688724209698078569671875)
    (e       2.718281828459045235360287471352662497757247093)
    (pi      3.141592653589793238462643383279502884197169399)
    (div     ,(lambda (x y) (floor (/ x y))))
    (quot    ,(lambda (x y) (truncate (/ x y))))
    (mod     ,(lambda (x y) (- x (* (div x y) y))))
    (rem     ,(lambda (x y) (- x (* (quot x y) y))))
    (^       ,expt)
    (=       ,=)
    (<       ,<)
    (>       ,>)
    (>=      ,>=)
    (<=      ,<=)
    (<>        ,(lambda (x y) (not (equal? x y))))
    (print    ,print)
    (length    ,length)
    )
)

;helper function to show two things
(define (show label it)
    (display label)
    (display " = ")
    (display it)
    (newline)
)

;helper function to check if x is not null and is a symbol
(define (label-check? z)
    (and (not (null? z))
        (symbol? z)))

;function to execute in main before main program,
;going through program and storing labels into the label hash table
(define (find-labels program)
    (map (lambda (line)
        (when (not (null? (cdr line)))
            (when (label-check? (cadr line))
                (label-put! (cadr line) line)))
        ) program)
)

;if keyword is dim, 
(define (dim-func input)
    ;make an array of the given name
    (let ((arrayName (caar input)))
        ;check to see if size is number or array
        (cond
            ;if number, 
            ((number? (cadar input))
                (let ((arraySize (cadar input)))
                    (define v (make-vector (+ 1 arraySize)))
                    ;and store it in the variable table
                    (variable-put! arrayName v)
                )
            )
            (else
                ;else? then its a variable, get it from the var-ht
                (let ((arraySize (variable-get (cadar input))))
                    (define v (make-vector (+ 1 arraySize)))
                    ;and store it in the variable table
                    (variable-put! arrayName v)

                )
            )
        )
    )
)

;if the keyword is if
(define (if-func input)
    (cond
        ;if the two things being compared are both not numbers
        ((and (not (number? (cadar input))) (not (number? (caddar input))))
            (cond
                ;if they're both lists
                ((and (list? (cadar input)) (>= (length (caddar input)) 2))
                    (cond
                        ;if they're both expressions,
                        ((and (not(= 2 (length (cadar input)))) (not(= 2 (length (caddar input)))))
                            (if ((function-get (caar input)) (evaluate-Expression (cadar input)) (evaluate-Expression (caddar input)))
                                (goto-func (cdr input))
                                '()
                            )
                        )
                        ;if the first is an expression, and the second is an array
                        ((and (not(= 2 (length (cadar input)))) (= 2 (length (caddar input))))
                            (let ((arrayExpression (caddar input)))
                                (cond
                                    ;if the second arg of the array is a number,
                                    ((number? (cadr arrayExpression))
                                        (if ((function-get (caar input)) (evaluate-Expression (cadar input)) (vector-ref (variable-get (car arrayExpression) (cadr arrayExpression))))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                    ;else, if its a variable
                                    (else
                                        (if ((function-get (caar input)) (evaluate-Expression (cadar input)) (vector-ref (variable-get (car arrayExpression) (variable-get (cadr arrayExpression)))))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                )
                            )
                        )
                        ;if the first is an array, and the second is an expression
                        ((and (= 2 (length (cadar input))) (not (= 2 (length (caddar input)))))
                            (let ((arrayExpression (cadar input)))
                                (cond
                                    ;if the second arg of the array is a number,
                                    ((number? (cadr arrayExpression))
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression) (cadr arrayExpression))) (evaluate-Expression (caddar input)))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                    ;else, if its a variable
                                    (else
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression) (variable-get (cadr arrayExpression)))) (evaluate-Expression (caddar input)))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                )
                            )
                        )
                        ;if both arguments are arrays
                        ((and (= 2 (length (cadar input))) (= 2 (length (caddar input))))
                        	(let* ((foo (caddar input)) (arr (cadar input)))
		                    	(cond
		                    		((> (length (cadr foo)) 1)
		                    			(let ((evalexpress (evaluate-Expression (cadr foo))))
		                    				(if ((function-get (caar input)) (vector-ref (variable-get (car arr)) (variable-get (cadr arr))) (vector-ref (variable-get (car foo)) evalexpress))
			                                            (goto-func (cdr input))
			                                            '()
			                                )
		                    			)
		                    		)
		                    		(else
		                    			(let* ((arrayExpression1 (cadar input)) (arrayExpression2 (caddar input)))
			                                (cond
			                                    ;if the second arg of both arrays ARE numbers,
			                                    ((and (number? (cadr arrayExpression1)) (number? (cadr arrayExpression2)))
			                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression1) (cadr arrayExpression1))) (vector-ref (variable-get (car arrayExpression2) (cadr arrayExpression2))))
			                                            (goto-func (cdr input))
			                                            '()
			                                        )
			                                    )
			                                    ;if the second arg of both arrays are NOT numbers
			                                    ((and (not (number? (cadr arrayExpression1))) (not (number? (cadr arrayExpression2))))
			                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression1) (variable-get (cadr arrayExpression1)))) (vector-ref (variable-get (car arrayExpression2) (variable-get (cadr arrayExpression2)))))
			                                            (goto-func (cdr input))
			                                            '()
			                                        )
			                                    )
			                                    ;if the second arg of the first array is a variable and 
			                                    ;the second arg of the second array is a number
			                                    ((and (not (number? (cadr arrayExpression1))) (number? (cadr arrayExpression2)))
			                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression1) (variable-get (cadr arrayExpression1)))) (vector-ref (variable-get (car arrayExpression2) (cadr arrayExpression2))))
			                                            (goto-func (cdr input))
			                                            '()
			                                        )
			                                    )
			                                    ;if the second arg of the first array is a number and 
			                                    ;the second arg of the second array is a variable
			                                    ((and (number? (cadr arrayExpression1)) (not (number? (cadr arrayExpression2))))
			                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression1) (cadr arrayExpression1))) (vector-ref (variable-get (car arrayExpression2) (variable-get (cadr arrayExpression2)))))
			                                            (goto-func (cdr input))
			                                            '()
			                                        )
			                                    )
			                                )
			                            )
		                    		)
		                    	)
                        	)
                        )
                    )
                )
                ;checks if the second argument is a list/expression
                ((and (list? (caddar input)) (not(= 2 (length (caddar input)))))
                    ;if it is, evaluate the expression and see if it meets the if-statment
                    (if ((function-get (caar input)) (variable-get (cadar input)) (evaluate-Expression (caddar input)))
                        (goto-func (cdr input))
                        '()
                    )
                )
                ;if second argument is array,
                ((and (list? (caddar input)) (= 2 (length (caddar input))))
                    (let ((arrayExpression (caddar input)))
                        (cond
                        	((equal? (car arrayExpression) '-)
                        		(let ((negative (- 0 (cadr arrayExpression))))
                        			(if ((function-get (caar input)) (variable-get (cadar input)) negative)
                                    	(goto-func (cdr input))
                                    	'()
                                	)
                        		)
                        	)
                            ;if second arg of array is number, 
                            ((number? (cadr arrayExpression))
                                (if ((function-get (caar input)) (variable-get (cadar input)) (vector-ref (variable-get (car arrayExpression)) (cadr arrayExpression)))
                                    (goto-func (cdr input))
                                    '()
                                )
                            )
                            (else
                                ;else, if second argument of array is a variable, 
                                (if ((function-get (caar input)) (variable-get (cadar input)) (vector-ref (variable-get (car arrayExpression)) (variable-get (cadr arrayExpression))))
                                    (goto-func (cdr input))
                                    '()
                                )
                            )
                        )
                    )
                )
                ;else, its not the case that both arguments arent numbers
                (else
                    (cond
                        ;checks if the first argument is a list/expression,
                        ((and (list? (cadar input)) (not(= 2 (length (cadar input)))))
                            ;if it is, evaluate the expression and see if it meets the if-statment
                            (if ((function-get (caar input)) (evaluate-Expression (cadar input)) (variable-get (caddar input)))
                                (goto-func (cdr input))
                                '()
                            )        
                        )
                        ;if first argument is array,
                        ((and (list? (cadar input)) (= 2 (length (cadar input))))
                            (let ((arrayExpression (cadar input)))
                                (cond
                                    ;if second arg of array is number,
                                    ((number? (cadr arrayExpression))
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression)) (cadr arrayExpression)) (variable-get (caddar input)))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                    ;else, second arg of array is variable,
                                    (else
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression)) (variable-get (cadr arrayExpression))) (variable-get (caddar input)))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                )
                            )
                        )
                        (else
                            ;the second argument wasn't a list and is just a variable
                            (let* ((arg1 (variable-get (cadar input))) (arg2 (variable-get (caddar input))))
                                (if ((function-get (caar input)) arg1 arg2)
                                    (goto-func (cdr input))
                                    '()
                                )    
                            )
                        )
                    )
                )
            )
        )
        ;else, ONE OF THE ARGUMENTS IS A NUMBER
        (else
            (cond
                ;if first arg of if is not an number (therefore second arg is),
                (not(number? (cadar input))
                    (cond
                        ;if the first arg is an expression
                        ((and (list? (cadar input)) (not(= 2 (length (cadar input)))))
                            (if ((function-get (caar input)) (evaluate-Expression (cadar input)) (caddar input))
                                (goto-func (cdr input))
                                '()
                            )
                        )
                        ;else, its an array
                        ((and (list? (cadar input)) (= 2 (length (cadar input))))
                            (let ((arrayExpression (cadar input)))
                                (cond
                                    ;if the second arg of array is a number, 
                                    ((number? (cadr arrayExpression))
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression)) (cadar arrayExpression)) (caddar input))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                    ;else its a variable
                                    (else
                                        (if ((function-get (caar input)) (vector-ref (variable-get (car arrayExpression)) (variable-get (cadr arrayExpression))) (caddar input))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                )
                            )
                        )
                        (else
                            ;else, first arg is variable, second arg is number
                            (if ((function-get (caar input)) (variable-get (cadar input)) (caddar input))
                                (goto-func (cdr input))
                                '()
                            )
                        )
                    )
                )
                ;first arg is a number and second argument isn't,
                (else
                    (cond
                        ;if second argument is an expression,
                        ((and (list? (caddar)) (not(= 2 (length (caddar input)))))
                            (if ((function-get (caar input)) (cadar input) (evaluate-Expression (caddar input)))
                                (goto-func (cdr input))
                                '()
                            )
                        )
                        ;if second argument is an array,
                        ((and (list? (caddar)) (= 2 (length (caddar input))))
                            (let ((arrayExpression (caddar input)))
                                (cond
                                    ;if second arg of array is a number, 
                                    ((number? (cadr arrayExpression))
                                        (if ((function-get (caar input)) (cadar input) (vector-ref (variable-get (car arrayExpression)) (cadr arrayExpression)))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                    ;else if its a variable, 
                                    (else
                                        (if ((function-get (caar input)) (cadar input) (vector-ref (variable-get (car arrayExpression)) (variable-get (cadr arrayExpression))))
                                            (goto-func (cdr input))
                                            '()
                                        )
                                    )
                                )
                            )
                        )
                        ;else, if second arg is a variable, 
                        (else
                            (if ((function-get (caar input)) (cadar input) (variable-get (caddar input)))
                                (goto-func (cdr input))
                                '()
                            )
                        )
                    )
                )
            )
        )
    )
)


{define (readnumber)
    (let ((object (read)))
        (cond [(eof-object? object) object]
            [(number? object) (+ object 0.0)]
            [else (begin (printf "invalid number: ~a~n" object)
                (readnumber))] )) 
}

{define (fetch-input var)
    (let ((number (readnumber)))
        (if (eof-object? number)
            ; then
            (variable-put! 'inputcount -1)
            ; else
            (if (list? var)
                ; then
                (vector-set! (variable-get (car var)) (cadr var) number)
                ; else
                (when #t
                    (variable-put! var number)
                )
            )         
        )
    )
}


(define (input-func input)
    (define (recur in)
        (when (and (> (length in) 0) (not(null? (car in))))
            (fetch-input (car in))
            (recur (cdr in))
            )
        )
        (when (not(null? input))
            ; then
            (when #t
            (variable-put! 'inputcount (length input))
            (recur input)
            )
        )
)


;if keyword is goto, 
(define (goto-func input)
    ;addy is the value of the label which is the line
    (let ((addy (label-get (car input))) )
        ;and set the new line number to be executed to be the one right before
        ;becasue when we finish here, the line number will be incremented
        (set! *lineNum* (- (car addy) 1))
        ;do what that line is suppose to do
        (interpret-func addy)
    )
)

;helper function to help with the infs and nans
(define (expression-handler op a1 a2)
    (cond 
        ((and(eq? 0 (cadr a1)) (eq? 0 (cadr a2)))
        'nan.0)
        ((and (eq? '+ (car a1)) (eq? '+ (car a2)))
        '+inf.0)
        ((and (eq? '- (car a1)) (eq? '+ (car a2)))
        '-inf.0)
        ((and (eq? '+ (car a1)) (eq? '- (car a2)))
        '+inf.0)
        ((and (eq? '- (car a1)) (eq? '- (car a2)))
        '-inf.0)
    )
)

;if keyword is let,
(define (let-func input)
    ;var is what the variable thats going to be set is
    (let ((var (car input)))
        ;checks to see if var is a pair because that means its an array
        (if (pair? var)
            (cond 
                ;if so, check to see if the second arg is a number, 
                ((and (number? (cadar input)) (number? (cadr input)))
                	;if its a number, check the bounds and then just vector-set!
                	(if (>= (vector-length (variable-get (caar input))) (cadar input))
                		(vector-set! (variable-get (caar input)) (cadar input) (cadr input))
                		'()
                	)
                )
                ;if 2nd arg of array is a number and value isnt a number,
                ((and (number? (cadar input)) (not (number? (cadr input))))
                	(if (>= (vector-length (variable-get (caar input))) (variable-get (cadar input)))
                		(vector-set! (variable-get (caar input)) (cadar input) (variable-get (cadr input)))
                		'()
                	)
                )
                ;if 2nd arg of array isnt a number and value isnt a number
                ((and (not (number? (cadar input))) (not (number? (cadr input))) (list? (cadr input)))
                	(if (>= (vector-length (variable-get (caar input))) (variable-get (cadar input)))
                		(vector-set! (variable-get (caar input)) (variable-get (cadar input)) (vector-ref (variable-get (caadr input)) (evaluate-Expression (cadadr input))))
                		'()
                	)
                )
                ((and (not (number? (cadar input))) (not (number? (cadr input))) (not(list? (cadr input))))
                	(cond
                		((list? (cadar input))
                			(if (>= (vector-length (variable-get (caar input))) (evaluate-Expression (cadar input)))
                				(vector-set! (variable-get (caar input)) (evaluate-Expression (cadar input)) (variable-get (cadr input)))
                				'()
                			)
                		)
                		(else
                			(if (>= (vector-length (variable-get (caar input))) (variable-get (cadar input)))
                				(vector-set! (variable-get (caar input)) (variable-get (cadar input)) (variable-get (cadr input)))
                				'()
                			)
                		)
                	)
                )	
                ;else, if 2nd arg is a variable and value is an number
                (else
                	;if its a variable, check the bounds and get it from the var ht and then vector-set!
                	(if (>= (vector-length (variable-get (caar input))) (variable-get (cadar input)))
                		(vector-set! (variable-get (caar input)) (variable-get (cadar input)) (cadr input))
                		'()
                	)
                )
            )
            ;else part of if-statement
            (cond 
                ;checks to see if the arg is a number, and puts it in
                ((number? (cadr input))
                    (variable-put! var (cadr input))
                )
                ;if its a list, execute the expression and put it in
                ((list? (cadr input))
                    (let ((n (evaluate-Expression (cadr input))))
                        (variable-put! var n)
                    )
                )
                (else 
                    ;else, its a variable, so it gets the var, and then puts it in
                    (variable-put! var (variable-get (cadr input)))
                )
            )
        )
    )
)

;recursive helper function to evaluate expressions 
(define (evaluate-Expression e)
    (cond
        ;if the expression is length of 3
        ((= (length e) 3)
            ;let the oper be the first/car of the expression,
            ;arg1 = the first argument and arg2 = the second argument
            (let* ((oper (car e)) (arg1 (cadr e)) (arg2 (caddr e)))
                ;if the oper is divide and arg2 isnt a number or isnt a variable and the cadr of arg2 isnt 0,
                ;then its a special case
                (if (and (eq? oper '/) (not(or(number? arg2) (hash-has-key? *variable-ht* arg2))) (eq? 0 (cadr arg2)))
                    (expression-handler oper arg1 arg2)
                    (cond
                        ;if arg1 and arg2 are numbers
                        ((and (number? arg1) (number? arg2))
                                ((function-get oper) arg1 arg2)
                        )
                        ;if arg1 and arg2 are not comments, 
                        ((and (not(number? arg1)) (not(number? arg2)))
                            (cond 
                                ;are they both variables?
                                ((and (hash-has-key? *variable-ht* arg1) (hash-has-key? *variable-ht* arg2))
                                    ((function-get oper) (variable-get arg1) (variable-get arg2))
                                )
                                ;is just arg2 a variable? and arg1 another expression?
                                ((and (not(hash-has-key? *variable-ht* arg1)) (hash-has-key? *variable-ht* arg2))
                                    ((function-get oper) (evaluate-Expression arg1) (variable-get arg2))
                                )
                                ;is arg1 a variable and arg2 another expression?
                                ((and (hash-has-key? *variable-ht* arg1) (not(hash-has-key? *variable-ht* arg2)))
                                    ((function-get oper) (variable-get arg1) (evaluate-Expression arg2))
                                )
                                (else 
                                    ;are they both another expression? recurse!
                                    ((function-get oper) (evaluate-Expression arg1) (evaluate-Expression arg2))
                                )
                            )
                        )
                        ;if arg1 isnt a number,
                        ((and (not(number? arg1)) (number? arg2))
                            (cond 
                                ;is it a variable?
                                ((hash-has-key? *variable-ht* arg1)
                                    ((function-get oper) (variable-get arg1) arg2)
                                )
                                (else 
                                    ;if not, an expression. recurse!
                                    ((function-get oper) (evaluate-Expression arg1) arg2)
                                )
                            )
                        )
                        ;if arg2 isnt an number,
                        ((and (number? arg1) (not(number? arg2)))
                            (cond 
                                ;is it a variable?
                                ((hash-has-key? *variable-ht* arg2)
                                    ((function-get oper) arg1 (variable-get arg2))
                                )
                                (else 
                                    ;if not, then an expression. recurse!
                                    ((function-get oper) arg1 (evaluate-Expression arg2))
                                )
                            )
                        )
                    )
                )
            )
        )
        ;if expression is length of 2
        ((= (length e) 2)
            ;if the first arg of the expression isnt a list, 
            (if (not(list?(cadr e)))
                ; then
                (let* ((oper (car e)) (arg1 (cadr e)))
                	(cond
                		((and (hash-has-key? *variable-ht* oper) (not (number? arg1)))
                			(vector-ref (variable-get oper) (variable-get arg1))
                		)
                		(else
                			(if (number? arg1)    
                        		; then
                        		((function-get oper) (+ arg1 0.0))
                        		; else
                        		((function-get oper) (+ (variable-get arg1) 0.0))
                    		)
                		)
                	)
                )
                ; else
                (cond
                    ;is the second arg of the expression a list?
                    ((list? (cdr e))
                        ((function-get (car e)) (evaluate-Expression (cadr e)))
                    )
                    ;if not, 
                    (else
                    	(let* ((oper (car e)) (arg1 (* -1 (cadadr e)) ))
                            ((function-get oper) arg1)
                        )
                    )
                )
            )
        )
    )
)

;if keywrod print,
(define (print-func L)
    ;map what needs to be printed
    (map {lambda (input) 
        (cond
            ;if its a number, 
            ((number? input)                
                (printf "~a" input)
            )
            ;if its a string,
            ((string? input)
                (printf "~a" input)
            )
            ;if its a variable,
            ((hash-has-key? *variable-ht* input)
                (printf "~a" (variable-get input))
            )
            ;if its a list,
            ((list? input)
                (cond
                    ;if the first thing in the list is a variable, then its an array
                    ((hash-has-key? *variable-ht* (car input))
                        (cond
                            ;if the second argument is a number, print
                            ((number? (cadr input))
                                (printf "~a" (vector-ref (variable-get (car input)) (cadr input)))
                            )
                            (else
                            	#|
                            	(newline)
                            	(printf "~s~n" input)
                            	(printf "~s~n" (car input))
                            	(printf "~s~n" (cadr input))
                            	(newline)
                            	|#
                                ;else, the second is a variable, so get it, and then print
                                (printf "~a" (vector-ref (variable-get (car input)) (variable-get (cadr input))))
                            )
                        )
                    )
                    ;if the first arg is a function and the length of input is two
                    ( (and (hash-has-key? *function-ht* (car input)) (= 2 (length input)))
                        (cond
                            ;if the second arg is a number
                            ((number? (cadr input))
                                ;if its log 0
                                (if (and(eq? (car input) 'log) (eq? 0 (cadr input)))
                                    ;then
                                    (printf "~a" '-inf.0)
                                    ;else
                                    (printf "~a"((function-get (car input)) (cadr input)))
                                )
                            )
                            ;if the second arg is a list,
                            ((list? (cadr input))
                                ;then, evaluate the expression/list
                                (printf "~a" (evaluate-Expression input))
                            )
                            (else 
                                ;else, the second arg is a variable
                                (printf "~a" ((function-get (car input)) (variable-get (cadr input))))
                            )
                        )
                    )
                    ;else, its an expression
                    (else
                        (printf "~a" (evaluate-Expression input))
                    )
                )
            )
            ;else, its a variable
            (else
                ((printf "~a"(variable-get input)))
            )
        )
    } L)
    ;print a newline
    (newline)
)

;function to interpret a line
(define (interpret-func line)
    (cond 
        ;case 1: line# label statement
        ((= 3 (length line))
                (let ((*statement* (caddr line))) 
                    (let ((keyword (car *statement*)))
                    ;whatever the keyword is, execute that function
                        (when (eq? keyword 'dim)
                            (dim-func (cdr *statement*))
                        )
                        (when (eq? keyword 'goto)
                            (goto-func (cdr *statement*))
                        )
                        
                        (when (eq? keyword 'if)
                            (if-func (cdr *statement*))
                        )
                        
                        (when (eq? keyword 'input)
                            (input-func (cdr *statement*))
                        )
                        
                        (when (eq? keyword 'let)
                            (let-func (cdr *statement*))
                        )
                        (when (eq? keyword 'print)
                            ;if the word print is alone, its just a newline
                            (if (not(null? (cdr *statement*)))
                                (print-func (cdr *statement*))
                                (newline)
                            )
                        )
                    )
                )
        )

        ;case 2: line# statement
        ((= 2 (length line))
                (when (not(symbol? (cadr line)))
                    (let ((*statement* (cadr line))) 
                        (let ((keyword (car *statement*)))
                        ;whatever the keyword is, execute that function
                            (when (eq? keyword 'dim)
                                (dim-func (cdr *statement*))
                            )
                            (when (eq? keyword 'goto)
                                (goto-func (cdr *statement*))
                            )
                            
                            (when (eq? keyword 'if)
                                (if-func (cdr *statement*))
                            )
                            
                            (when (eq? keyword 'input)
                                (input-func (cdr *statement*))
                            )
                            
                            (when (eq? keyword 'let)
                                (let-func (cdr *statement*))
                            )
                            ;if the word print is alone, its just a newline
                            (when (eq? keyword 'print)
                                (if (not(null? (cdr *statement*)))
                                    (print-func (cdr *statement*))
                                    (newline)
                                )
                            )
                        )
                    )
                )
        )
    )
)

;global variable for line number
(define *lineNum* 0)

;main function that calls interpret-func on each line and parses by line number
(define (interpret-program program ln)
    (when (> (length program) ln)
        (let ((line (list-ref program ln)))
            (interpret-func line)
            (set! *lineNum* (+ *lineNum* 1))
            (interpret-program program *lineNum*)
        )
    )
)

;the main that calls the main func, interpret program
;but first it finds labels
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (find-labels program)
              (interpret-program program *lineNum*)
        )
    )
)

(main (vector->list (current-command-line-arguments)))


