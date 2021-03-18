#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Rany.rkt")
(provide interp-Rwhile interp-Rwhile-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Rwhile-class
  (class interp-Rany-class
    (super-new)

    (define/override ((interp-exp env) e)
      (verbose "Rwhile/interp-exp" e)
      (define recur (interp-exp env))
      (define result
      (match e
        [(SetBang x rhs)
         (set-box! (lookup x env) (recur rhs))]
        [(WhileLoop cnd body)
         (define (loop)
           (cond [(recur cnd)  (recur body) (loop)]
                 [else         (void)]))
         (loop)]
        [(Begin es body)
         (for ([e es]) (recur e))
         (recur body)]
        [else ((super interp-exp env) e)]))
      (verbose "Rwhile/interp-exp" e result)
      result)
    ))

(define (interp-Rwhile p)
  (send (new interp-Rwhile-class) interp-program p))

