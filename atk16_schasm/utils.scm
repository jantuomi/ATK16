;; Utils

(define-syntax comment
  (syntax-rules ()
    ((_ expr* ...)
     (begin))))

(define-syntax @
  (syntax-rules ()
    ((_ fn-body expr ...)
     (lambda (x) (fn-body expr ... x)))))

(define (zip alist blist)
  (if (null? alist)
      '()
      ;; else, cons and recurse
      (cons (cons (car alist) (car blist)) (zip (cdr alist) (cdr blist)))))

(define (type-of pair)
  (if (pair? pair) (car pair) (error "not a pair" pair)))
(define (val-of pair)
  (if (pair? pair) (cdr pair) (error "not a pair" pair)))

(define (parse-rhs-offset rhs1 rhs2)
  (define t1 (and rhs1 (type-of rhs1)))
  (define t2 (and rhs2 (type-of rhs2)))

  (define regs '())
  (when (eq? 'reg t1)
    (set! regs (cons rhs1 regs)))
  (when (eq? 'reg t2)
    (set! regs (cons rhs2 regs)))
  (unless (<= (length regs) 1)
    (error "only one register can be supplied as rhs"))

  (define offsets '())
  (when (or (eq? 'imm t1) (eq? 'label t1))
    (set! offsets (cons rhs1 offsets)))
  (when (or (eq? 'imm t2) (eq? 'label t2))
    (set! offsets (cons rhs2 offsets)))
  (unless (<= (length offsets) 1)
    (error "only one imm or label can be supplied as rhs"))

  (define rhs    (if (null? regs)    ZR      (car regs)))
  (define offset (if (null? offsets) (imm 0) (car offsets)))

  (cons rhs offset))

(define (assocar k alist)
  (let ((v (assoc k alist)))
    (and v (car v))))
(define (assocdr k alist)
  (let ((v (assoc k alist)))
    (and v (cdr v))))

(define-syntax inc!
  (syntax-rules ()
    ((_ n)
     (set! n (+ n 1)))
    ((_ n m)
     (set! n (+ n m)))))
(define-syntax sub!
  (syntax-rules ()
    ((_ n)
     (set! n (- n 1)))
    ((_ n m)
     (set! n (- n m)))))

(define (string->ascii-list s)
  (let* ((len (string-length s))
         (dummy (cons #f '()))
         (tail dummy))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (set-cdr! tail (cons (char->integer (string-ref s i)) '()))
      (set! tail (cdr tail)))
    (cdr dummy)))

(define (rest-args args)
  (define pos '())
  (define kws '())

  (let loop ((as args))
    (when (not (null? as))
      (let ((head (car as))
	    (tail (cdr as)))
	(cond
	 ((keyword? head)
	  (set! kws (cons (cons head (car tail)) kws))
	  (loop (cdr tail)))
	 (else
	  (set! pos (append pos (list head)))
	  (loop tail))))))

  (values pos kws))
