(import (chicken base)
	(chicken bitwise)
	(chicken format)
	(chicken io))

(define *buffer* (make-vector (* 2 (expt 2 16)) #f))
(define *cursor* 0)
(define *labels* '())

;; Meta

(define (write-image-to filepath)
  (eval-*buffer*)

  (let ((port (open-output-file filepath)))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length *buffer*)))
      (write-byte (vector-ref *buffer* i) port))
    (close-output-port port)))

(define (eval-*buffer*)
  (let loop ((i 0))
    (if (< i (vector-length *buffer*))
	(let* ((elem   (vector-ref *buffer* i))
	       (evaled (eval-*buffer*-elem elem)))
	  (vector-set! *buffer* i evaled)
	  (loop (+ i 1))))))

(define (eval-*buffer*-elem elem)
  (cond
   ((number? elem) elem)
   ((eq? #f elem)  0)
   ((eq? 'label (type-of elem))
    (let* ((elem-val (val-of elem))
	   (qualif   (car elem-val))
	   (label    (cadr elem-val))
	   (addr     (or (assocdr label *labels*)
			 (error "deferred label ref was never defined" label))))
      (cond
       ((eq? 'hi qualif)  (arithmetic-shift addr -8))
       ((eq? 'lo qualif)  (bitwise-and #xFF addr))
       ((eq? 'rel qualif) (- addr (caddr elem-val)))
       (else            (error "invalid qualifier in elem" elem)))))
   (else (error "invalid elem" elem))))

;; Directives

(define (def-label sym . exprs)
  (set! *labels* (cons (cons sym *cursor*) *labels*))
  ;; don't do anything with exprs, it's there for allowing
  ;; a nice appearance for labeled blocks
  )

(define (at-addr addr)
  (unless (and (number? addr)
	       (>= addr 0)
	       (< addr (expt 2 16)))
    (error "invalid addr" addr))

  (set! *cursor* addr))

;; Value constructors and references

(define (label sym)
  (cons 'label sym))

(define (reg n)
  (cond
   ((or (< n 0) (>= n 16)) (error "invalid arg to reg" n))
   (else (cons 'reg n))))

(define R0  (reg 0))
(define R1  (reg 1))
(define R2  (reg 2))
(define R3  (reg 3))
(define R4  (reg 4))
(define R5  (reg 5))
(define R6  (reg 6))
(define R7  (reg 7))
(define R8  (reg 8))
(define R9  (reg 9))
(define R10 (reg 10))
(define R11 (reg 11))
(define R12 (reg 12))
(define R13 (reg 13))
(define R14 (reg 14))
(define R15 (reg 15))

(define FL (reg 13))
(define PC (reg 14))
(define SP (reg 15))

(define (imm n)
  (cons 'imm n))

(define (i16 n)
  (unless (and (<  n (expt 2 15))
	       (>= n (- (expt 2 15))))
    (error "n does not fit in bounds" n))
  (imm (modulo n (expt 2 16))))

(define (u16 n)
  (unless (and (<  n (expt 2 16))
	       (>= n 0))
    (error "n does not fit in bounds" n))
  (imm n))

(define (i8 n)
  (unless (and (<  n (expt 2 7))
	       (>= n (- (expt 2 7))))
    (error "n does not fit in bounds" n))
  (imm (modulo n (expt 2 16))))

(define (u8 n)
  (unless (and (<  n (expt 2 8))
	       (>= n 0))
    (error "n does not fit in bounds" n))
  (imm n))

(define (alu-op n)
  (unless (and (>= n 0)
	       (<  n 8))
    (error "invalid arg to alu-op" n))
  (cons 'alu-op n))

(define alu-plus  (alu-op 0))
(define alu-minus (alu-op 1))
(define alu-and   (alu-op 2))
(define alu-or    (alu-op 3))
(define alu-xor   (alu-op 4))
(define alu-shl   (alu-op 5))
(define alu-shr   (alu-op 6))
(define alu-sar   (alu-op 7))

(define (flag n)
  (unless (and (>= n 0)
	       (<  n 4))
    (error "invalid arg to flag" n))
  (cons 'flag n))

(define flag-carry    (flag 0))
(define flag-overflow (flag 1))
(define flag-zero     (flag 2))
(define flag-sign     (flag 3))

;; Encode and emit

(define (encode-byte . chunks)
  (let ((total-bits (apply + (map car chunks))))
    (unless (= total-bits 8)
      (error "emit: total bits in chunks must equal 8, got" total-bits)))

  (let loop ((chs chunks) (acc 0))
    (if (null? chs)
        acc
        (let* ((chunk (car chs))
               (n     (car chunk))
               (val   (cdr chunk)))

          (when (>= val (expt 2 n))
            (error "emit: value" val "does not fit in" n "bits"))

          ;; shift acc left by n bits and combine with val
          (loop (cdr chs)
                (bitwise-ior (arithmetic-shift acc n) val))))))

(define (emit-byte-chunks . chunks)
  (unless (eq? (vector-ref *buffer* *cursor*) #f)
    (error "overwriting already written memory at" *cursor*))

  (define byte (apply encode-byte chunks))

  (vector-set! *buffer* *cursor* byte)
  (set! *cursor* (+ *cursor* 1)))

(define (emit-byte b)
  (emit-byte-chunks `(8 . ,b)))

(define (emit-word w)
  ;; emit a word as two bytes
  (emit-byte-chunks `(8 . ,(arithmetic-shift w -8)))
  (emit-byte-chunks `(8 . ,(bitwise-and #xFF w))))

(define (emit-deferred-sexpr sexpr)
  (unless (pair? sexpr)
    (error "not a pair" sexpr))
  (vector-set! *buffer* *cursor* sexpr)
  (set! *cursor* (+ *cursor* 1)))

;; Instructions

(define (hlt)
  ;; just full zeros
  (emit-word 0))

(define (alu op lhs rhs)
  (unless (eq? 'alu-op (type-of op)) (error "invalid op" op))
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))

  (define rhs-type (type-of rhs))
  (define m-mode
    (cond ((eq? 'reg rhs-type)   0)
	  ((eq? 'imm rhs-type)   1)
	  ((eq? 'label rhs-type) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= m-mode 0)
      ;; reg mode
      (begin
	(emit-byte-chunks `(4 . 1)
			  `(3 . ,(val-of op))
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
	                  `(4 . ,(val-of rhs))))

      ;; immediate mode
      (begin
	(emit-byte-chunks `(4 . 1)
			  `(3 . ,(val-of op))
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
			  `(4 . 0))

	(if (eq? 'label rhs-type)
	    ;; if label, emit a label reference
	    (begin (emit-deferred-sexpr `(label . (hi ,(val-of rhs))))
		   (emit-deferred-sexpr `(label . (lo ,(val-of rhs)))))
	    ;; otherwise, emit the value
	    (emit-word (val-of rhs))))))

(define (add lhs rhs) (alu (alu-op 0) lhs rhs))
(define (sub lhs rhs) (alu (alu-op 1) lhs rhs))
(define (and lhs rhs) (alu (alu-op 2) lhs rhs))
(define (or  lhs rhs) (alu (alu-op 3) lhs rhs))
(define (xor lhs rhs) (alu (alu-op 4) lhs rhs))
(define (shl lhs rhs) (alu (alu-op 5) lhs rhs))
(define (shr lhs rhs) (alu (alu-op 6) lhs rhs))
(define (sar lhs rhs) (alu (alu-op 7) lhs rhs))

(define (ld lhs rhs #!key (indirect #f) (pop #f))
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))

  (define d-mode (if indirect 1 0))
  (define p-mode (if pop 1 0))

  (define rhs-type (type-of rhs))
  (define m-mode
    (cond ((eq? 'reg rhs-type)   0)
	  ((eq? 'imm rhs-type)   1)
	  ((eq? 'label rhs-type) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= m-mode 0)
      ;; reg mode
      (begin
	(emit-byte-chunks `(4 . 2)
			  `(1 . ,d-mode)
			  `(1 . ,p-mode)
			  `(1 . 0)
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
			  `(4 . ,(val-of rhs))))

      ;; immediate mode
      (begin
	(emit-byte-chunks `(4 . 2)
			  `(1 . ,d-mode)
			  `(1 . ,p-mode)
			  `(1 . 0)
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
			  `(4 . 0))

	(if (eq? 'label rhs-type)
	    ;; if label, emit a label reference
	    (begin (emit-deferred-sexpr `(label . (hi ,(val-of rhs))))
		   (emit-deferred-sexpr `(label . (lo ,(val-of rhs)))))
	    ;; otherwise, emit the value
	    (emit-word (val-of rhs))))))

(define (mov lhs rhs)
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))
  (unless (eq? 'reg (type-of rhs)) (error "invalid rhs" rhs))

  (emit-byte-chunks `(4 . 3)
		    `(4 . 0))
  (emit-byte-chunks `(4 . ,(val-of lhs))
		    `(4 . ,(val-of rhs))))

(define (st lhs rhs #!key (indirect #f) (push #f))
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))

  (define d-mode (if indirect 1 0))
  (define p-mode (if push 1 0))

  (define rhs-type (type-of rhs))
  (define m-mode
    (cond ((eq? 'reg rhs-type)   0)
	  ((eq? 'imm rhs-type)   1)
	  ((eq? 'label rhs-type) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= m-mode 0)
      ;; reg mode
      (begin
	(emit-byte-chunks `(4 . 4)
			  `(1 . ,d-mode)
			  `(1 . ,p-mode)
			  `(1 . 0)
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
			  `(4 . ,(val-of rhs))))

      ;; immediate mode
      (begin
	(emit-byte-chunks `(4 . 4)
			  `(1 . ,d-mode)
			  `(1 . ,p-mode)
			  `(1 . 0)
			  `(1 . ,m-mode))
	(emit-byte-chunks `(4 . ,(val-of lhs))
			  `(4 . 0))

	(if (eq? 'label rhs-type)
	    ;; if label, emit a label reference
	    (begin (emit-deferred-sexpr `(label . (hi ,(val-of rhs))))
		   (emit-deferred-sexpr `(label . (lo ,(val-of rhs)))))
	    ;; otherwise, emit the value
	    (emit-word (val-of rhs))))))

(define (br flag offset #!key (asserted #t))
  (unless (eq? 'flag (type-of flag)) (error "invalid flag selector" flag))

  (define set
    (cond ((eq? #t asserted) 1)
	  ((eq? #f asserted) 0)
	  (else (error "invalid asserted bool" asserted))))

  (define offset-type (type-of offset))
  (cond
   ((eq? 'imm offset-type)
    (let* ((imm (val-of offset)))
      (unless (< imm (expt 2 8)) (error "offset too large" imm))

      (emit-byte-chunks `(4 . 5)
			`(2 . ,(val-of flag))
			`(1 . 0)
			`(1 . ,set))
      (emit `(8 . ,imm))))
   ((eq? 'label offset-type)
    (emit-byte-chunks `(4 . 5)
		      `(2 . ,(val-of flag))
		      `(1 . 0)
		      `(1 . ,set))
    (emit-deferred-sexpr `(label . (rel ,(val-of offset) ,(+ *cursor* 1)))))
   (else (error "invalid offset" offset))))

;; "Macros"

(define (emit-packed-string s)
  ;; emit length
  (define sl (string-length s))
  (u16 sl) ; cast to u16 to get bounds check
  (emit-word sl)

  ;; compute packed words
  (emit-bytes (string->ascii-list s)))

(define (emit-bytes bytes)
  (let loop ((bs bytes))
    (if (null? bs)
	;; if done, return total number of bytes emitted
	(length bytes)
	;; if not, emit the byte and loop
	(begin (emit-byte (car bs))
	       (loop (cdr bs))))))

;; Utils

(define (type-of pair)
  (if (pair? pair) (car pair) (error "not a pair" pair)))
(define (val-of pair)
  (if (pair? pair) (cdr pair) (error "not a pair" pair)))

(define (assocar k alist)
  (let ((v (assoc k alist)))
    (and v (car v))))
(define (assocdr k alist)
  (let ((v (assoc k alist)))
    (and v (cdr v))))

(define (string->ascii-list s)
  (let* ((len (string-length s))
         (dummy (cons #f '()))
         (tail dummy))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (set-cdr! tail (cons (char->integer (string-ref s i)) '()))
      (set! tail (cdr tail)))
    (cdr dummy)))

(define (pack-bytes-to-words bytes)
  (cond
   ((>= (length bytes) 2)
    (let* ((hi   (car bytes))
	   (lo   (cadr bytes))
	   (word (+ (arithmetic-shift hi 8) lo)))

      (unless (and (>= word 0)
		   (<  word (expt 2 16)))
	(error "packing of two bytes produced an invalid 16 bit word" word))

      (cons word (pack-bytes-to-words (cddr bytes)))))
   ((= (length bytes) 1)
    (let* ((hi   (car bytes))
	   (word (arithmetic-shift hi 8)))

      (unless (and (>= word 0)
		   (<  word (expt 2 16)))
	(error "packing of one byte produced an invalid 16 bit word" word))

      (list word)))
   ((= (length bytes) 0)
    '())))
