(import (chicken base)
	(chicken bitwise)
	(chicken format)
	(chicken io)
	(chicken keyword)
	(only srfi-1 iota))

(load "utils.scm")

;; State

(define *buffer* (make-vector (* 2 (expt 2 16)) #f))
(define *cursor* 0)
(define *labels* '())

;; Buffer write and eval

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

;; Encode and emit

(define (foobar x y . rest) #f)

(define (encode-byte . chunks)
  (let ((total-bits (apply + (map car chunks))))
    (unless (= total-bits 8)
      (error "total bits in chunks must equal 8, got" total-bits)))

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
  (unless (and (>= b 0)
	       (<  b (expt 2 8)))
    (error "value out of bounds" b))

  (emit-byte-chunks `(8 . ,b)))

(define (emit-word w)
  (unless (and (>= w 0)
	       (<  w (expt 2 16)))
    (error "value out of bounds" w))

  ;; emit a word as two bytes
  (emit-byte-chunks `(8 . ,(arithmetic-shift w -8)))
  (emit-byte-chunks `(8 . ,(bitwise-and #xFF w))))

(define (emit-deferred-sexpr sexpr)
  (unless (pair? sexpr)
    (error "not a pair" sexpr))
  (vector-set! *buffer* *cursor* sexpr)
  (set! *cursor* (+ *cursor* 1)))

;; Directives

(begin
  (define (def-label-fn sym)
    (when (assoc sym *labels*)
      (error "label already defined" sym))

    (set! *labels* (cons (cons sym *cursor*) *labels*))
    )
  (define-syntax def-label
    (syntax-rules ()
      ((_ sym exprs* ...)
       (begin
	 (def-label-fn sym)
	 exprs* ...))))

  )

(define-syntax at-addr
  (syntax-rules ()
    ((_ addr exprs* ...)
     (begin
       (unless (and (number? addr)
		    (>= addr 0)
		    (< addr (expt 2 16)))
	 (error "invalid addr" addr))

       (set! *cursor* addr)

       exprs* ...))))

;; Value constructors and references

(define (label sym)
  (cons 'label sym))

(define (reg n)
  (cond
   ((or (< n 0) (>= n 16)) (error "invalid arg to reg" n))
   (else (cons 'reg n))))

;; R0..R5 caller saved arguments, generic
;; R0 return value register
(define R0  (reg 0))
(define R1  (reg 1))
(define R2  (reg 2))
(define R3  (reg 3))
(define R4  (reg 4))
(define R5  (reg 5))
;; R6..R11 callee saved arguments, generic
(define R6  (reg 6))
(define R7  (reg 7))
(define R8  (reg 8))
(define R9  (reg 9))
(define R10 (reg 10))
(define R11 (reg 11))
;; Special (see below)
(define R12 (reg 12))
(define R13 (reg 13))
(define R14 (reg 14))
(define R15 (reg 15))

;; Zero register (hardwired to 0)
(define ZR (reg 12))
;; Flag register (only lowest 4 bits set by ALU)
(define FL (reg 13))
;; Program counter
(define PC (reg 14))
;; Stack pointer
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

;; Instructions

(define (%hlt)
  ;; just full zeros
  (emit-word 0))

(define (%alu op lhs rhs1 #!optional rhs2)
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs register" lhs))
  (define rhs-offset-pair (parse-rhs-offset rhs1 rhs2))
  (define rhs    (car rhs-offset-pair))
  (define offset (cdr rhs-offset-pair))

  ;; Addressing modes
  ;; 0: reg        (1 word)
  ;; 1: reg + word (2 words

  (define m-mode (if offset 1 0))

  (emit-byte-chunks `(4 . 1)
		    `(3 . ,(val-of op))
		    `(1 . ,m-mode))
  (emit-byte-chunks `(4 . ,(val-of lhs))
	            `(4 . ,(val-of rhs)))

  (cond
   ;; in reg+word mode, if label, emit a label reference
   ((and offset (eq? 'label (type-of offset)))
    (emit-deferred-sexpr `(label . (hi ,(val-of offset))))
    (emit-deferred-sexpr `(label . (lo ,(val-of offset)))))
   ;; in reg+word mode, if imm, emit the value
   ((and offset (eq? 'imm (type-of offset)))
    (emit-word (val-of offset)))
   (offset
    (error "invalid offset in reg+word addressing mode" offset))))

(define (%add lhs rhs1 #!optional rhs2) (%alu (alu-op 0) lhs rhs1 rhs2))
(define (%sub lhs rhs1 #!optional rhs2) (%alu (alu-op 1) lhs rhs1 rhs2))
(define (%and lhs rhs1 #!optional rhs2) (%alu (alu-op 2) lhs rhs1 rhs2))
(define (%or  lhs rhs1 #!optional rhs2) (%alu (alu-op 3) lhs rhs1 rhs2))
(define (%xor lhs rhs1 #!optional rhs2) (%alu (alu-op 4) lhs rhs1 rhs2))
(define (%shl lhs rhs1 #!optional rhs2) (%alu (alu-op 5) lhs rhs1 rhs2))
(define (%shr lhs rhs1 #!optional rhs2) (%alu (alu-op 6) lhs rhs1 rhs2))
(define (%sar lhs rhs1 #!optional rhs2) (%alu (alu-op 7) lhs rhs1 rhs2))

;; Load data to to-reg from memory pointed by from-reg + offset.
;; With indirect = 0, copies the value from-reg + offset to to-reg without memory access.

;; TODO tää ei toimi niinkun kuvittelis. key arg menee optionaalin paikalle.

(define (%ld lhs rhs1 . rest)
  (define-values (pos kws) (rest-args rest))
  (define rhs2 (if (> (length pos) 0) (car pos) #f))
  (define indirect (or (assocdr #:indirect kws) 0))
  (define preinc   (or (assocdr #:preinc   kws) #f))

  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs register" lhs))
  (define rhs-offset-pair (parse-rhs-offset rhs1 rhs2))
  (define rhs    (car rhs-offset-pair))
  (define offset (cdr rhs-offset-pair))

  (unless (and (number? indirect)
	       (>= indirect 0)
	       (<  indirect 4))
    (error "invalid indirect" indirect))
  (unless (boolean? preinc) (error "invalid preinc" preinc))

  ;; Addressing modes
  ;; 0: reg                      (1 word)
  ;; 1: reg, preincrement        (1 word)
  ;; 2: reg + word               (2 words)
  ;; 3: reg + word, preincrement (2 words)
  (define m-mode (if offset 1 0))
  (define p-mode (if preinc 1 0))

  (emit-byte-chunks `(4 . 2)
		    `(2 . ,indirect)
		    `(1 . ,m-mode)
		    `(1 . ,p-mode))
  (emit-byte-chunks `(4 . ,(val-of lhs))
		    `(4 . ,(val-of rhs)))

  (cond
   ;; in reg+word mode, if label, emit a label reference
   ((and offset (eq? 'label (type-of offset)))
    (emit-deferred-sexpr `(label . (hi ,(val-of offset))))
    (emit-deferred-sexpr `(label . (lo ,(val-of offset)))))
   ;; in reg+word mode, if imm, emit the value
   ((and offset (eq? 'imm (type-of offset)))
    (emit-word (val-of offset)))
   (offset
    (error "invalid offset in reg+word addressing mode" offset))))

;; Store data in from-reg to memory pointed by to-reg + offset.
(define (%st lhs rhs1 . rest)
  (define-values (pos kws) (rest-args rest))
  (define rhs2 (if (> (length pos) 0) (car pos) #f))
  (define indirect (or (assocdr #:indirect kws) 1))
  (define postdec  (or (assocdr #:postdec  kws) #f))

  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs register" lhs))
  (define rhs-offset-pair (parse-rhs-offset rhs1 rhs2))
  (define rhs    (car rhs-offset-pair))
  (define offset (cdr rhs-offset-pair))

  (unless (and (number? indirect)
	       (>= indirect 1)
	       (<  indirect 4))
    (error "invalid indirect" indirect))
  (unless (boolean? postdec) (error "invalid postdec" postdec))

  ;; Addressing modes
  ;; 0: reg                       (1 word)
  ;; 1: reg, postdecrement        (1 word)
  ;; 2: reg + word                (2 words)
  ;; 3: reg + word, postdecrement (2 words)
  (define m-mode (if offset  1 0))
  (define p-mode (if postdec 1 0))

  (emit-byte-chunks `(4 . 4)
		    `(2 . ,indirect)
		    `(1 . ,m-mode)
		    `(1 . ,p-mode))
  (emit-byte-chunks `(4 . ,(val-of lhs))
		    `(4 . ,(val-of rhs)))

  (cond
   ;; in reg+word mode, if label, emit a label reference
   ((and offset (eq? 'label (type-of offset)))
    (emit-deferred-sexpr `(label . (hi ,(val-of offset))))
    (emit-deferred-sexpr `(label . (lo ,(val-of offset)))))
   ;; in reg+word mode, if imm, emit the value
   ((and offset (eq? 'imm (type-of offset)))
    (emit-word (val-of offset)))
   (offset
    (error "invalid offset in reg+word addressing mode" offset))))

(define (%br flag offset #!key (asserted #t))
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
      (emit-byte `(8 . ,imm))))
   ((eq? 'label offset-type)
    (emit-byte-chunks `(4 . 5)
		      `(2 . ,(val-of flag))
		      `(1 . 0)
		      `(1 . ,set))
    (emit-deferred-sexpr `(label . (rel ,(val-of offset) ,(- *cursor* 1)))))
   (else (error "invalid offset" offset))))
