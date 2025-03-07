;; Assume that core.scm is loaded

(define *unique-counter* 0)
(define (next-unique)
  (inc! *unique-counter*)
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
     (let* ((lhs  (eval (car `pred)))
	    (op   (cadr 'pred))
	    (rhs  (eval (caddr `pred)))
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

(define-syntax %when
  (syntax-rules ()
    ((_ pred body body* ...)
     (let* ((lhs  (eval (car `pred)))
	    (op   (cadr 'pred))
	    (rhs  (eval (caddr `pred)))
	    (n    (next-unique))
	    (sym-end   (string->symbol (format "~A-end"   n))))
       (ld *macro-scratch-reg* lhs)

       (emit-inverted-test *macro-scratch-reg* op rhs sym-end)

       body body* ...

       (def-label sym-end)))))

(define-syntax %while
  (syntax-rules ()
    ((_ pred body body* ...)
     (let* ((lhs (eval (car `pred)))
	    (op  (cadr 'pred))
	    (rhs (eval (caddr `pred)))
	    (n   (next-unique))
	    (sym-test (string->symbol (format "~A-test" n)))
	    (sym-end  (string->symbol (format "~A-end"  n))))
       (ld *macro-scratch-reg* lhs)

       (def-label sym-test)
       (emit-inverted-test *macro-scratch-reg* op rhs sym-end)

       body body* ...

       (ld PC (label sym-test))
       (def-label sym-end)))))

(define *procedures* '())

(define-syntax %decl-proc
  (syntax-rules ()
    ((_ signature)
     (set! *procedures*
       (cons `signature *procedures*))
     )))

(define *proc-scope* (make-parameter #f))

(define (param pname)
  (let* ((scope (or (*proc-scope*)
		    (error "cannot use params outside a procedure context"))))
    (or (assocdr pname (*proc-scope*))
	(error "no such param" pname))))

(define-syntax %param
  (syntax-rules ()
    ((_ pname)
     (param 'pname))))

(define-syntax %def-proc
  (syntax-rules ()
    ((_ signature body* ...)
     (let* ((name     (car `signature))
	    (params   (cdr `signature))
	    (bindings (zip params
                           (map (@ reg) (iota (length params) 0))))
	    (n-max-params 12))
       (unless (<= (length params) n-max-params)
	 (error (format "proc has too many params (~A > ~A)" (length params) n-max-params)))
       (%decl-proc signature)
       (def-label name)
       (parameterize ((*proc-scope* bindings))
	 body* ...)
       ))))

(define-syntax %call
  (syntax-rules ()
    ((_ name args* ...)
     (let* ((params  (or (assocdr `name *procedures*)
			 (error "proc not defined" `name)))
	    (args    (list args* ...))
	    (n       (next-unique))
	    (sym-ret (string->symbol (format "~A-ret" n))))

       (unless (= (length params) (length args))
	 (error (format "incorrect args to ~A, expected ~A, got ~A" `name params args)))

       (let loop ((i 0))
	 (when (< i (length args))
	   ;; check that there are no reg arguments in later positions
	   ;; that would be clobered/invalidated by a move
	   (let loop ((j (+ i 1)))
	     (when (< j (length args))
	       (let* ((jarg  (list-ref args j))
		      (jtype (type-of jarg))
		      (jval  (val-of  jarg)))
		 (when (and (eq? 'reg jtype)
			    (eq? i    jval))
		   (error "proc call would clobber register before it is moved" jarg '(name args* ...))))
	       (loop (+ j 1))))

	   ;; move each argument to its designated register
	   ;; first arg to R0, etc.
	   (let* ((rn (reg i)))
	     (ld rn (list-ref args i)))
	   (loop (+ 1 i))))

       (st SP (label sym-ret) push: #t)
       (ld PC (label `name))
       (def-label sym-ret)
       ))))

(define (spush datum)
  (cond
   ((eq? 'reg (type-of datum))
    (st datum SP push: #t))
   (else
    (ld *macro-scratch-reg* datum)
    (st *macro-scratch-reg* SP push: #t indirect: 1))))

(define (spop reg)
  (unless (eq? 'reg (type-of reg))
    (error "not a register" reg))
  (ld reg SP pop: #t indirect: 1))
