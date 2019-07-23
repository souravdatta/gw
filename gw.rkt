#lang racket


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
      (map (λ (x) (displayln (third x))) (program-sort prog))
      (displayln "...")))

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
                       (newline)))))

(define (gw-eval env expr)
  (cond
    ((number? expr) expr)
    ((string? expr) expr)
    ((symbol? expr) (get-env env expr))
    ((list? expr)
     (cond
       ((eq? (car expr) 'let)
        (let-eval env expr))
       ((eq? (car expr) 'goto)
        (goto-eval env expr))
       (else (gw-apply env
                       (car expr)
                       (cdr expr)))))
    (else (error "WAT??"))))

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

(define (gw-apply env fnsym args)
  (let ([fn (get-env env fnsym)]
        [fnargs (map (λ (a) (gw-eval env a)) args)])
    (fn (cons env fnargs))))

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
  (program-exec e p))

  
