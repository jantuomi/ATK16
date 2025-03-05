;; Assume that core.scm is loaded

(define *unique-counter* 0)
(define (next-unique)
  (set! *unique-counter* (+ 1 *unique-counter*))
  *unique-counter*)
(define *macro-scratch-reg* R12)

(define (%packed-string s)
  ;; emit length
  (define sl (string-length s))
  (u16 sl)			     ; cast to u16 to get bounds check
  (emit-word sl)

  ;; compute packed words
  (for-each (@ emit-byte) (string->ascii-list s)))

(define-syntax emit-test-eq
  (syntax-rules ()
    ((_ lhs rhs dest set)
     (begin (sub lhs rhs)
	    (br flag-zero (label dest) set: set)))))

(define-syntax emit-test-lt
  (syntax-rules ()
    ((_ lhs rhs dest set)
     (begin (sub lhs rhs)
	    (br flag-sign (label dest) set: set)))))

(define-syntax emit-test-lte
  (syntax-rules ()
    ((_ lhs rhs dest set)
     (begin (sub lhs rhs)
	    (sub lhs (u16 1))
	    (br flag-sign (label dest) set: set)))))

(define-syntax emit-inverted-test
  ;; jump to dest if <lhs op rhs> is NOT true.
  ;; as such the behaviour of this macro can be considered to
  ;; be inverted.
  ;; the reason for this is to allow having the true branch
  ;; before the false branch in memory (arbitrary decision).
  (syntax-rules ()
    ((_ lhs op rhs dest)
     (cond
      ((eq? '== op) (emit-test-eq  lhs rhs dest #f))
      ((eq? '!= op) (emit-test-eq  lhs rhs dest #t))
      ((eq? '<  op) (emit-test-lt  lhs rhs dest #f))
      ((eq? '>= op) (emit-test-lt  lhs rhs dest #t))
      ((eq? '<= op) (emit-test-lte lhs rhs dest #f))
      ((eq? '>  op) (emit-test-lte lhs rhs dest #t))
      (else (error "unsupported operator" op))))))

(define-syntax %if-else
  (syntax-rules ()
    ((_ pred tb fb)
     (let* ((lhs  (eval (car 'pred)))
	    (op   (cadr 'pred))
	    (rhs  (eval (caddr 'pred)))
	    (n    (next-unique))
	    (sym-false (string->symbol (format "~A-false" n)))
	    (sym-end   (string->symbol (format "~A-end"   n))))
       (ld *macro-scratch-reg* lhs)

       (emit-inverted-test *macro-scratch-reg* op rhs sym-false)

       tb
       (ld PC (label sym-end))
       (def-label sym-false)
       fb
       (def-label sym-end)))))

(define-syntax %if
  (syntax-rules ()
    ((_ pred tb)
     (let* ((lhs  (eval (car 'pred)))
	    (op   (cadr 'pred))
	    (rhs  (eval (caddr 'pred)))
	    (n    (next-unique))
	    (sym-end   (string->symbol (format "~A-end"   n))))
       (ld *macro-scratch-reg* lhs)

       (emit-inverted-test *macro-scratch-reg* op rhs sym-end)

       tb
       (def-label sym-end)))))

