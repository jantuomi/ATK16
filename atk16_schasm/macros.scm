;; Assume that core.scm is loaded

(define *unique-counter* 0)
(define (next-unique)
  (inc! *unique-counter*)
  *unique-counter*)

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
     (begin (%sub lhs rhs)
	    (%br flag-zero (label dest) #:asserted set)))))

(define-syntax emit-test-lt
  (syntax-rules ()
    ((_ lhs rhs dest set)
     (begin (%sub lhs rhs)
	    (%br flag-sign (label dest) #:asserted set)))))

(define-syntax emit-test-lte
  (syntax-rules ()
    ((_ lhs rhs dest set)
     (begin (%sub lhs rhs)
	    (%sub lhs ZR (u16 1))
	    (%br flag-sign (label dest) #:asserted set)))))

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
       (%ld TMP lhs)

       (emit-inverted-test TMP op rhs sym-false)

       tb
       (%ld PC (label sym-end))
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
       (%ld TMP lhs)

       (emit-inverted-test TMP op rhs sym-end)

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
       (%ld TMP lhs)

       (def-label sym-test)
       (emit-inverted-test TMP op rhs sym-end)

       body body* ...

       (%ld PC ZR (label sym-test))
       (def-label sym-end)))))

;; Stack push and pop

(define (%spush word)
  (cond
   ((eq? 'reg (type-of word))
    (%st word SP #:postdec #t))
   (else
    (%ld TMP word)
    (%st TMP SP #:postdec #t #:indirect 1))))

(define (%spop reg)
  (unless (eq? 'reg (type-of reg))
    (error "not a register" reg))
  (%ld reg SP #:preinc #t #:indirect 1))

;; Stack variables and parameter registers

(define *params*   '())
(define *soffsets* '())
(define *ssizes*   '())
(define *proc?*    #f)

(define (get-param name)
  (or (assocdr name *params*)
      (error "no such param" name)))

(define (get-soffset name)
  (or (assocdr name *soffsets*)
      (error "no such svar" name)))

(define (get-ssize name)
  (or (assocdr name *ssizes*)
      (error "no such svar" name)))

(define-syntax param
  (syntax-rules ()
    ((_ name)
     (get-param `name))))

(define-syntax soffset
  (syntax-rules ()
    ((_ name)
     (get-soffset `name))))

(define-syntax ssize
  (syntax-rules ()
    ((_ name)
     (get-ssize `name))))

(define (svars-size)
  (apply + (map (@ cdr) *ssizes*)))

(define (def-svar name size)
  (set! *soffsets* (cons `(,name . ,(svars-size)) *soffsets*))
  (set! *ssizes*   (cons `(,name . ,size)         *ssizes*))
  (%sub SP (u16 size)))

(define-syntax %svar
  (syntax-rules ()
    ((_ name size)
     (def-svar `name size))))

;; Procedure definitions and calls

(define *procedures* '())

(define-syntax decl-proc
  (syntax-rules ()
    ((_ signature)
     (set! *procedures*
       (cons `signature *procedures*))
     )))

(define (%return #!optional word)
  ;; Load word into R0 (return value register)
  (when word
    (%ld R0 word))

  ;; Free up stack memory that was used in this proc
  ;; by setting SP to base pointer
  (%ld SP BP)

  ;; Pop the base pointer and return address from the stack
  (%spop BP)
  (%spop PC))

(define-syntax %proc
  (syntax-rules ()
    ((_ signature body* ...)
     (let* ((name     (car `signature))
	    (params   (cdr `signature))
	    (bindings (zip params
                           (map (@ reg) (iota (length params) 0))))
	    (n-max-params 12))
       (unless (<= (length params) n-max-params)
	 (error (format "proc has too many params (~A > ~A)" (length params) n-max-params)))

       (unless (not *proc?*)
	 (error "nested procs not supported" name))

       (decl-proc signature)
       (def-label name)

       (define old-offsets *soffsets*)
       (define old-sizes   *ssizes*)
       (set! *soffsets* '())
       (set! *ssizes*   '())
       (set! *proc?*    #t)
       (set! *params*   bindings)

       body* ...

       (%return)

       (set! *soffsets* old-offsets)
       (set! *ssizes*   old-sizes)
       (set! *proc?* #f)
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
	   (let* ((rn    (reg i))
		  (argi  (list-ref args i))
		  (itype (type-of argi)))
	     (cond
	      ((eq? 'reg itype)
	       (%ld rn argi))
	      ((or (eq? 'imm itype)
		   (eq? 'label itype))
	       (%ld rn ZR argi))
	      (else (error "invalid argument" argi))))
	   (loop (+ 1 i))))

       ;; Push the return address and base pointer to the stack
       (%spush (label sym-ret))
       (%spush BP)

       ;; Update base pointer to current stack pointer
       (%ld BP SP)

       ;; Jump to the procedure address
       (%ld PC (label `name))

       ;; Define the return address
       (def-label sym-ret)
       ))))
