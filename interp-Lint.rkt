#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp_Lint interp-Lint-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for Lint: integer arithmetic

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code. This code does not use
;;   the match 'app' feature because the book doesn't introduce
;;   that until a later.

(define (interp_exp e)
    (match e
      [(Int n) n]
      [(Prim 'read '())
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp_exp "expected an integer" r)])]
      [(Prim '- (list e))
       (define v (interp_exp e))
       (fx- 0 v)]
      [(Prim '+ (list e1 e2))
       (define v1 (interp_exp e1))
       (define v2 (interp_exp e2))
       (fx+ v1 v2)]
      [(Prim '- (list e1 e2))
       (define v1 (interp_exp e1))
       (define v2 (interp_exp e2))
       (fx- v1 v2)]
      ))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]
    ))


;; This version of the interpreter for Lint is the base class
;; for interp-Rvar-class in interp-Rvar.rkt.

(define interp-Lint-class
  (class object%
    (super-new)
    
    (define/public ((interp_exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp_exp "expected an integer" r)])]
        [(Prim '- (list e))
         (define v ((interp_exp env) e))
         (fx- 0 v)]
        [(Prim '+ (list e1 e2))
         (define v1 ((interp_exp env) e1))
         (define v2 ((interp_exp env) e2))
         (fx+ v1 v2)]
        [(Prim '- (list e1 e2))
         (define v1 ((interp_exp env) e1))
         (define v2 ((interp_exp env) e2))
         (fx- v1 v2)]
        ))

    (define/public (interp-program p)
      (match p
        [(Program '() e) ((interp_exp '()) e)]
        ))
    ))

(define (pe_neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(define (pe_add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r1))]))

(define (pe_sub r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))

(define (pe_exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe_neg (pe_exp e1))]
    [(Prim '+ (list e1 e2)) (pe_add (pe_exp e1) (pe_exp e2))]
    [(Prim '- (list e1 e2)) (pe_sub (pe_exp e1) (pe_exp e2))]))

(define (pe_Lint p)
  (match p
    [(Program '() e) (Program '() (pe_exp e))]))


(define (test_pe p)
  (assert "testing pe_Lint"
    (equal? (interp-Lint p) (interp-Lint (pe_Lint p)))))


(test_pe (parse-program `(program () (+ 10 (- (+ 5 3))))))
(test_pe (parse-program `(program () (+ 10 (read)))))

