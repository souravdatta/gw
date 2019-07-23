#lang racket


(define prog '(program
               (10 (let x (op* 2 3)) "LET X = 2 * 3")
               (20 (let y 20) "LET Y = 20")
               (22 (goto 40) "GOTO 40")
               (30 (input "name" name) "INPUT \"name\"; NAME")
               (40 (print (op+ x y)) "PRINT X+Y")
               (8 (print "x") "PRINT \"X\"")
               (9 (print x) "PRINT X")))

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
    (list hashed min-line max-line sorted)))
