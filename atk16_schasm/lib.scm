(import (chicken base)
	(chicken bitwise)
	(chicken format)
	(chicken io))

(define *buffer* (make-vector (expt 2 16) #f))
(define *cursor* 0)
(define *labels* '())

;; Meta

(define (write-image-to filepath)
  (let ((port (open-output-file filepath)))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length *buffer*)))
      (let* ((word (or (vector-ref *buffer* i) 0))
             (high (arithmetic-shift word -8))  ; high byte: shift right by 8 bits
             (low (bitwise-and word #xFF)))     ; low byte: mask with 0xFF
        (write-byte high port)
        (write-byte low port)))
    (close-output-port port)))

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
  (assocdr sym *labels*))

(define (reg n)
  (cond
   ((or (< n 0) (>= n 16)) (error "invalid arg to reg" n))
   (else `(reg ,n))))

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
  `(imm ,n))

(define (i16 n)
  (unless (and (<  n (expt 2 15))
	       (>= n (- (expt 2 15))))
    (error "n does not fit in bounds" n))
  `(imm ,(modulo n (expt 2 16))))

(define (u16 n)
  (unless (and (<  n (expt 2 16))
	       (>= n 0))
    (error "n does not fit in bounds" n))
  `(imm ,n))

(define (i8 n)
  (unless (and (<  n (expt 2 7))
	       (>= n (- (expt 2 7))))
    (error "n does not fit in bounds" n))
  `(imm ,(modulo n (expt 2 16))))

(define (u8 n)
  (unless (and (<  n (expt 2 8))
	       (>= n 0))
    (error "n does not fit in bounds" n))
  `(imm ,n))

(define (alu-op n)
  (unless (and (>= n 0)
	       (<  n 8))
    (error "invalid arg to alu-op" n))
  `(alu-op ,n))

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
  `(flag ,n))

(define flag-carry    (flag 0))
(define flag-overflow (flag 1))
(define flag-zero     (flag 2))
(define flag-sign     (flag 3))

;; Encode and emit

(define (emit . chunks)
  (unless (eq? (vector-ref *buffer* *cursor*) #f)
    (error "overwriting already written memory at" *cursor*))
  (define word
    (if (and (= (length chunks) 1)
	     (number? (car chunks)))
	;; single argument
	(car chunks)
	;; multiple (bits . value) pairs
	(apply encode chunks)))

  (vector-set! *buffer* *cursor* word)
  (set! *cursor* (+ *cursor* 1)))

(define (encode . chunks)
  (let ((total-bits (apply + (map car chunks))))
    (unless (= total-bits 16)
      (error "emit: total bits in chunks must equal 16, got" total-bits)))

  (let loop ((chs chunks) (acc 0))
    (if (null? chs)
        acc
        (let* ((chunk (car chs))
               (n (car chunk))
               (val (cdr chunk)))

          (when (>= val (expt 2 n))
            (error "emit: value" val "does not fit in" n "bits"))

          ;; shift acc left by n bits and combine with val
          (loop (cdr chs)
                (bitwise-ior (arithmetic-shift acc n) val))))))

;; Instructions

(define (hlt)
  ;; just full zeros
  (emit 0))

(define (alu op lhs rhs)
  (unless (eq? 'alu-op (type-of op)) (error "invalid op" op))
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))

  (define mode
    (cond ((eq? 'reg (type-of rhs)) 0)
	  ((eq? 'imm (type-of rhs)) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= mode 0)
      ;; reg mode
      (emit `(4 . 1)			; opcode = 1
	    `(3 . ,(cadr op))		; alu op
	    `(1 . ,mode)		; reg/imm mode
	    `(4 . ,(cadr lhs))		; lhs
	    `(4 . ,(cadr rhs)))		; rhs

      ;; immediate mode
      (begin
	(emit `(4 . 1)	       	        ; opcode = 1
	      `(3 . ,(cadr op))      	; alu op
	      `(1 . ,mode)	       	; reg/imm mode
	      `(4 . ,(cadr lhs))	; lhs
	      `(4 . 0))		        ; unused
	(emit `(16 . ,(cadr rhs))))     ; rhs
      ))

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

  (define m-mode
    (cond ((eq? 'reg (type-of rhs)) 0)
	  ((eq? 'imm (type-of rhs)) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= m-mode 0)
      ;; reg mode
      (emit `(4 . 2)			; opcode = 2
	    `(1 . ,d-mode)		; indirect mode
	    `(1 . ,p-mode)		; pop mode
	    `(1 . 0)                    ; unused
	    `(1 . ,m-mode)              ; reg/imm mode
	    `(4 . ,(cadr lhs)) 		; lhs
	    `(4 . ,(cadr rhs)))		; rhs

      ;; immediate mode
      (begin
	(emit `(4 . 2)			; opcode = 2
	      `(1 . ,d-mode)		; indirect mode
	      `(1 . ,p-mode)		; pop mode
	      `(1 . 0)                  ; unused
	      `(1 . ,m-mode)            ; reg/imm mode
       	      `(4 . ,(cadr lhs))       	; lhs
	      `(4 . 0))		        ; unused
	(emit `(16 . ,(cadr rhs))))     ; rhs
      ))

(define (mov lhs rhs)
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))
  (unless (eq? 'reg (type-of rhs)) (error "invalid rhs" rhs))

  (emit `(4 . 3)             ; opcode = 3
	`(4 . 0)             ; unused
	`(4 . ,(cadr lhs))   ; lhs
	`(4 . ,(cadr rhs)))) ; rhs

(define (st lhs rhs #!key (indirect #f) (push #f))
  (unless (eq? 'reg (type-of lhs)) (error "invalid lhs" lhs))

  (define d-mode (if indirect 1 0))
  (define p-mode (if push 1 0))

  (define m-mode
    (cond ((eq? 'reg (type-of rhs)) 0)
	  ((eq? 'imm (type-of rhs)) 1)
	  (else (error "invalid rhs" rhs))))

  (if (= m-mode 0)
      ;; reg mode
      (emit `(4 . 4)			; opcode = 4
	    `(1 . ,d-mode)		; indirect mode
	    `(1 . ,p-mode)		; pop mode
	    `(1 . 0)                    ; unused
	    `(1 . ,m-mode)              ; reg/imm mode
	    `(4 . ,(cadr lhs)) 		; lhs
	    `(4 . ,(cadr rhs)))		; rhs

      ;; immediate mode
      (begin
	(emit `(4 . 4)			; opcode = 4
	      `(1 . ,d-mode)		; indirect mode
	      `(1 . ,p-mode)		; pop mode
	      `(1 . 0)                  ; unused
	      `(1 . ,m-mode)            ; reg/imm mode
       	      `(4 . ,(cadr lhs))       	; lhs
	      `(4 . 0))		        ; unused
	(emit `(16 . ,(cadr rhs))))     ; rhs
      ))

(define (br flag offset #!key (asserted #t))
  (unless (eq? 'flag (type-of flag)) (error "invalid flag selector" flag))

  (define set
    (cond ((eq? #t asserted) 1)
	  ((eq? #f asserted) 0)
	  (else (error "invalid asserted bool" asserted))))

  (unless (eq? 'imm (type-of offset)) (error "invalid offset" offset))
  (define imm (cadr offset))
  (unless (< imm (expt 2 8)) (error "offset too large" imm))

  (emit `(4 . 5)            ; opcode = 5
	`(2 . ,(cadr flag)) ; flag selector
	`(1 . 0)            ; unused
	`(1 . ,set)         ; asserted (set / not set)
	`(8 . ,imm))        ; offset
  )

;; "Macros"

(define (emit-packed-string s)
  ;; emit length
  (define sl (string-length s))
  (u16 sl) ; cast to u16 to get bounds check
  (emit sl)

  ;; compute packed words
  (emit-bytes (string->ascii-list s)))

(define (emit-words words)
  (let loop ((ws words))
    (if (null? ws)
	;; if done, return total number of words emitted
	(length words)
	;; if not, emit the word and loop
	(begin (emit (car ws))
	       (loop (cdr ws))))))

(define (emit-bytes bytes)
  (emit-words (pack-bytes-to-words bytes)))

;; TODO: figure out a system for jumping to a non-declared label 'program here

(define-syntax defer
  (syntax-rules ()
    ((_ exp exp* ...)
     (emit (lambda () (exp exp* ...))))))

;; Utils

(define (type-of v)
  (if (and (list? v)
	   (> (length v) 0))
      (car v)
      #f))

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
