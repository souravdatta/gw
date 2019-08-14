#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

(define prog-test-format
  '(program
    (10 (let x (op* 2 3)) "LET X = 2 * 3")
    (20 (let y 20) "LET Y = 20")
    (22 (goto 40) "GOTO 40")
    (30 (input "name" name) "INPUT \"name\"; NAME")
    (40 (print (+ x y)) "PRINT X+Y")
    (8 (print "x" space "=" space x tab "!") "PRINT \"X\";\"=\";X,\"!\"")
    (9 (print x) "PRINT X")))

(define GOTOLINE "__goto__line__")

(define (program? prog)
  (and (eq? (car prog) 'program)
       (not (null? (cdr prog)))
       (list? (cadr prog))))

(define (program-sort prog)
  (sort (cdr prog)
        (λ (x y)
          (< (car x)
             (car y)))))

(define (program-hash sorted-prog)
  (make-hash (map (λ (p)
                    (cons (car p)
                          (cdr p))) sorted-prog)))

(define (program-listing prog)
  (if (program? prog)
      (let* ([pdata (program-data prog)]
             [keys (sort (hash-keys (first pdata)) <)])
        (for ([k keys])
          (displayln (format "~a" (second (hash-ref (first pdata) k))))))
      (displayln " ")))

(define (program-data prog)
  (let* ([sorted (program-sort prog)]
         [hashed (program-hash sorted)]
         [min-line (caar sorted)]
         [max-line (car (last sorted))])
    (list hashed min-line max-line)))

(define (program-data-exec env prog-data #:line [line #f])
  (let* ([minline (second prog-data)]
         [maxline (third prog-data)]
         [phash (first prog-data)]
         [exec-line (if line line minline)])
    (when (<= exec-line maxline)
      (when (hash-has-key? phash exec-line)
        ;(displayln (second (hash-ref phash exec-line)))
        (gw-eval env (first (hash-ref phash exec-line)))
        (let ((gotoline (get-env env GOTOLINE)))
          (set-env! env GOTOLINE (nil))
          (when (not (nil? gotoline))
            (set! exec-line (- gotoline 1)))))
      (program-data-exec env prog-data #:line (+ 1 exec-line)))))

(define (program-exec env prog)
  (program-data-exec env (program-data prog)))

(struct nil ())

(define (make-env #:defaults [defaults '()])
  (make-hash defaults))

(define (get-env env sym)
  (if (hash-has-key? env sym)
      (hash-ref env sym)
      (nil)))

(define (set-env! env sym val)
  (hash-set! env sym val))

(define (make-defaults)
  (list (cons 'print (λ (args)
                       (for ([x (cdr args)])
                         (display (gw-eval (car args) x))
                         (display " "))
                       (newline)))
        (cons '+ (λ (args)
                   (apply + (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '- (λ (args)
                   (apply - (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '* (λ (args)
                   (apply * (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '/ (λ (args)
                   (apply / (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '< (λ (args)
                   (apply < (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '> (λ (args)
                   (apply > (map (λ (a) (gw-eval (car args) a))
                                 (cdr args)))))
        (cons '<= (λ (args)
                    (apply <= (map (λ (a) (gw-eval (car args) a))
                                   (cdr args)))))
        (cons '>= (λ (args)
                    (apply >= (map (λ (a) (gw-eval (car args) a))
                                   (cdr args)))))
        (cons '= (λ (args)
                   (apply equal? (map (λ (a) (gw-eval (car args) a))
                                      (cdr args)))))
        (cons '^ (λ (args)
                   (let ([env (car args)])
                     (expt (gw-eval env (second args))
                           (gw-eval env (third args))))))                                    
        (cons 'end (λ (args) (nil)))
        (cons 'pconcat (λ (args)
                         (let* ([env (car args)]
                                [oargs (map (λ (a)
                                              (gw-eval env a))
                                            (cdr args))])
                           (string-join (map (λ (x)
                                               (format "~a" x))
                                             oargs)
                                        " "))))))

(define (gw-eval env expr)
  (cond
    ((nil? expr) expr)
    ((number? expr) expr)
    ((string? expr) expr)
    ((symbol? expr) (get-env env expr))
    ((boolean? expr) expr)
    ((list? expr)
     (cond
       ((eq? (car expr) 'let)
        (let-eval env expr))
       ((eq? (car expr) 'goto)
        (goto-eval env expr))
       ((eq? (car expr) 'if)
        (if-eval env expr))
       (else (gw-apply env
                       (car expr)
                       (cdr expr)))))
    (else (error (format "~a" expr)))))

(define (let-eval env expr)
  (let ([sym (second expr)]
        [val (gw-eval env (third expr))])
    (when (not (string=? (symbol->string sym)
                         GOTOLINE))
      (set-env! env sym val))))

(define (goto-eval env expr)
  (let ([line (second expr)])
    (when (not (number? line))
      (set! line (nil)))
    (set-env! env GOTOLINE line)))

(define (if-eval env expr)
  (let ([cond-e (second expr)]
        [true-e (third expr)]
        [false-e (fourth expr)])
    (if (gw-eval env cond-e)
        (gw-eval env true-e)
        (gw-eval env false-e))))

(define (gw-apply env fnsym args)
  (let ([fn (get-env env fnsym)]
        [fnargs (map (λ (a) (gw-eval env a)) args)])
    (fn (cons env fnargs))))

(define-tokens Tok [SYMBOL NUMBER STRING])
(define-empty-tokens Tok* [LET PRINT GOTO END IF THEN ELSE
                               PLUS MINUS MULT DIV POW
                               LT LE GT GE EQ
                               OPAREN CPAREN NEWLINE COMMA EOF])

(define gw-lexer
  (lexer
   [(eof) (token-EOF)]
   [(:or whitespace blank iso-control) (gw-lexer input-port)]
   [(:or (:: #\l #\e #\t)
         (:: #\L #\E #\T)) (token-LET)]
   [(:or (:: #\p #\r #\i #\n #\t)
         (:: #\P #\R #\I #\N #\T)) (token-PRINT)]
   [(:or (:: #\g #\o #\t #\o)
         (:: #\G #\O #\T #\O)) (token-GOTO)]
   [(:or (:: #\e #\n #\d)
         (:: #\E #\N #\D)) (token-END)]
   [(:or (:: #\i #\f)
         (:: #\I #\F)) (token-IF)]
   [(:or (:: #\e #\l #\s #\e)
         (:: #\E #\L #\S #\E)) (token-ELSE)]
   [(:or (:: #\t #\h #\e #\n)
         (:: #\T #\H #\E #\N)) (token-THEN)]
   [#\+ (token-PLUS)]
   [#\- (token-MINUS)]
   [#\* (token-MULT)]
   [#\/ (token-DIV)]
   [#\^ (token-POW)]
   [#\( (token-OPAREN)]
   [#\) (token-CPAREN)]
   [#\< (token-LT)]
   [(:: #\< #\=) (token-LE)]
   [#\> (token-GT)]
   [(:: #\> #\=) (token-GE)]
   [#\= (token-EQ)]
   [#\, (token-COMMA)]
   [(:: #\" any-string #\") (token-STRING (second (regexp-split #px"\"" lexeme)))]
   [(:: alphabetic (:* (:or alphabetic numeric)))
    (token-SYMBOL (string->symbol lexeme))]
   [(:+ numeric) (token-NUMBER (string->number lexeme))]))

(define gw-parser
  (parser
   (tokens Tok Tok*)
   (start lined-statement)
   (end EOF)
   (error (λ (tok tname tval)
            (format "Error in parsing at '~a', token ~a" tval tname)))
   (precs (left PLUS MINUS LT LE GT GE EQ) (right MULT DIV POW))
   (grammar
    (expr ((NUMBER) $1)
          ((SYMBOL) $1)
          ((STRING) $1)
          ((expr PLUS expr) (list '+ $1 $3))
          ((expr MINUS expr) (list '- $1 $3))
          ((expr MULT expr) (list '* $1 $3))
          ((expr DIV expr) (list '/ $1 $3))
          ((expr POW expr) (list '^ $1 $3))
          ((expr LT expr) (list '< $1 $3))
          ((expr LE expr) (list '<= $1 $3))
          ((expr GT expr) (list '> $1 $3))
          ((expr GE expr) (list '>= $1 $3))
          ((expr EQ expr) (list '= $1 $3))
          ((OPAREN expr CPAREN) $2))
    
    (statement
     ((let-statement) $1)
     ((print-statement) $1)
     ((goto-statement) $1)
     ((if-statement) $1)
     ((END) (list 'end)))

    (let-statement
     ((LET SYMBOL EQ expr) (list 'let $2 $4)))

    (print-statement
     ((PRINT expr-list) (list 'print (cons 'pconcat $2))))

    (goto-statement
     ((GOTO NUMBER) (list 'goto $2)))

    (if-statement
     ((IF expr THEN NUMBER) (list 'if $2 (list 'goto $4) '(end)))
     ((IF expr THEN NUMBER ELSE NUMBER) (list 'if $2 (list 'goto $4) (list 'goto $6))))

    (expr-list
     ((expr) (list $1))
     ((expr COMMA expr-list) (append (if (list? $1) $1 (list $1)) $3)))

    (lined-statement
     ((NUMBER statement) (list $1 $2))))))

(define gw-line-parser
  (parser
   (tokens Tok Tok*)
   (start statement)
   (end EOF)
   (error (λ (tok tname tval)
            (format "Error in parsing at '~a', token ~a" tval tname)))
   (precs (left PLUS MINUS LT LE GT GE EQ) (right MULT DIV POW))
   (grammar
    (expr ((NUMBER) $1)
          ((SYMBOL) $1)
          ((STRING) $1)
          ((expr PLUS expr) (list '+ $1 $3))
          ((expr MINUS expr) (list '- $1 $3))
          ((expr MULT expr) (list '* $1 $3))
          ((expr DIV expr) (list '/ $1 $3))
          ((expr POW expr) (list '^ $1 $3))
          ((expr LT expr) (list '< $1 $3))
          ((expr LE expr) (list '<= $1 $3))
          ((expr GT expr) (list '> $1 $3))
          ((expr GE expr) (list '>= $1 $3))
          ((expr EQ expr) (list '= $1 $3))
          ((OPAREN expr CPAREN) $2))
    
    (statement
     ((let-statement) $1)
     ((print-statement) $1)
     ((goto-statement) $1)
     ((if-statement) $1)
     ((END) (list 'end)))

    (let-statement
     ((LET SYMBOL EQ expr) (list 'let $2 $4)))

    (print-statement
     ((PRINT expr-list) (list 'print (cons 'pconcat $2))))

    (goto-statement
     ((GOTO NUMBER) (list 'goto $2)))

    (if-statement
     ((IF expr THEN NUMBER) (list 'if $2 (list 'goto $4) '(end)))
     ((IF expr THEN NUMBER ELSE NUMBER) (list 'if $2 (list 'goto $4) (list 'goto $6))))

    (expr-list
     ((expr) (list $1))
     ((expr COMMA expr-list) (append (if (list? $1) $1 (list $1)) $3)))

    (lined-statement
     ((NUMBER statement) (list $1 $2))))))

(define (gw-repl-parse s)
  (let ([is (open-input-string s)])
    (gw-line-parser (λ () (gw-lexer is)))))

(define (gw-repl-exec env s)
  (gw-eval env (gw-repl-parse s)))

(define (gw-parse-line s)
  (let ([is (open-input-string s)])
    (append (gw-parser (λ () (gw-lexer is))) (list s))))

(define (gw-parse chunk)
  (cons 'program
        (map gw-parse-line
             (filter (λ (x)
                       (not (string=? x "")))
                     (regexp-split #px"\n" chunk)))))

(define (gw chunk #:env [env #f])
  (let ([e (if env env (make-env #:defaults (make-defaults)))])
    (program-exec e (gw-parse chunk))))

(define global-env (make-env #:defaults (make-defaults)))

(define global-program-string "")

(define (repl) 
  (display "OK ")
  (let ([line (read-line)])
    (cond
      ((eof-object? line) (displayln "BYE"))
      ((string=? line "")
       (repl))
      ((string=? line "list")
       (begin
         (program-listing (gw-parse  global-program-string))
         (repl)))
      ((string=? line "run")
       (begin
         (with-handlers ([exn:fail? (λ (e)
                                      (displayln " "))])
           (gw global-program-string #:env global-env))
         (repl)))
      ((string=? line "exit")
       (displayln "BYE"))
      ((string=? line "new")
       (set!  global-program-string "")
       (repl))
      (else
       (with-handlers ([exn:fail? (λ (e1)
                                    (with-handlers ([exn:fail?
                                                     (λ (e2)
                                                       (displayln " ")
                                                       (repl))])
                                      (gw-repl-exec global-env line)
                                      (repl)))])
         (gw-parse-line line)
         (set! global-program-string (string-append
                                       global-program-string
                                       "\n"
                                       line))
         (repl))))))

(repl)

;; All tests

(define (test1)
  (define e (make-env #:defaults (make-defaults)))
  (gw-eval e '(let x 10))
  (gw-eval e '(let y "hello"))
  (gw-eval e '(print x y)))

(define (test2)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (10 (let x 10) "let x = 10")
              (20 (let y "hello") "let y = \"hello\"")
              (30 (print x) "print x")
              (40 (print y) "print y")
              (50 (print x y) "print x, y")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test3)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (10 (let x 10) "let x = 10")
              (20 (let y "hello") "let y = \"hello\"")
              (22 (goto 50))
              (30 (print x) "print x")
              (40 (print y) "print y")
              (50 (print x y) "print x, y")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test4)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (10 (let x 10) "let x = 10")
              (20 (let y "hello") "let y = \"hello\"")
              (30 (print x) "print x")
              (32 (goto 50))
              (40 (print y) "print y")
              (50 (print x y) "print x, y")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test5)
  ;; INFINITE loop
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (10 (let x 10) "let x = 10")
              (20 (let y "hello") "let y = \"hello\"")
              (30 (print x) "print x")
              (40 (print y) "print y")
              (50 (print x y) "print x, y")
              (60 (goto 30))))
  (displayln (program-listing p))
  (program-exec e p))

(define (test6)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (10 (let x 10) "let x = 10")
              (20 (let y 20) "let y = 20")
              (30 (let r1 (+ x y)) "let r1 = x + y")
              (40 (let r2 (- x y)) "let r2 = x - y")
              (50 (let r3 (* x 20)) "let r3 = x * 20")
              (60 (let r4 (/ y 10)) "let r4 = y / 10")
              (70 (print r1 r2 r3 r4) "print r1, r2, r3, r4")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test7)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (8 (print "minimum calculation") "print \"minimum calculation\"")
              (10 (let x 40) "let x = 40")
              (20 (let y 20) "let y = 20")
              (30 (if (< x y) (goto 40) (goto 60)) "if x < y then 40 else 60")
              (40 (print x) "print x")
              (50 (goto 70) "goto 70")
              (60 (print y) "print y")
              (70 (end) "end")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test8)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (8 (print "minimum calculation") "print \"minimum calculation\"")
              (10 (let x 10) "let x = 10")
              (20 (let y 20) "let y = 20")
              (30 (if (< x y) (goto 40) (goto 60)) "if x < y then 40 else 60")
              (40 (print x) "print x")
              (50 (goto 70) "goto 70")
              (60 (print y) "print y")
              (70 (end) "end")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test9)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (8 (print "loop test") "print \"loop test\"")
              (10 (let x 1) "let x = 1")
              (20 (if (<= x 10) (goto 30) (goto 60)) "if x <= 10 then 30 else 60")
              (30 (print x) "print x")
              (40 (let x (+ x 1)) "let x = x + 1")
              (50 (goto 20) "goto 20")
              (60 (print "DONE!") "print \"DONE!\"")
              (70 (end) "end")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test10)
  (define e (make-env #:defaults (make-defaults)))
  (define p '(program
              (8 (print "loop test") "print \"loop test\"")
              (10 (let x 10) "let x = 10")
              (20 (if (>= x 1) (goto 30) (goto 60)) "if x >= 1 then 30 else 60")
              (30 (print x) "print x")
              (40 (let x (- x 1)) "let x = x - 1")
              (50 (goto 20) "goto 20")
              (60 (print "DONE!") "print \"DONE!\"")
              (70 (end) "end")))
  (displayln (program-listing p))
  (program-exec e p))

(define (test11)
  (define program "
8 print 1, \"loop test\"
10 let x = 10
20 if x >= 1 then 30 else 60
30 print 2, x
40 let x = x - 1
50 goto 20
60 print 3, \"DONE!\"
70 end")
  (gw-parse program))

(define (test12)
  (define program "
8 print 1, \"loop test\"
10 let x = 10
20 if x >= 1 then 30 else 60
30 print \">>\", x
32 if x = 5 then 60
40 let x = x - 1
50 goto 20
60 print 3, \"DONE!\"
70 end")
  (gw program))

