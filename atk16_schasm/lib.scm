(import (chicken base)
	(chicken bitwise)
	(chicken format))

(define *buffer* (make-vector (expt 2 16) #f))
(define *cursor* 0)
(define *labels* '())

;; Directives

(define (def-label sym)
  (set! *labels* (cons (cons sym *cursor*) *labels*)))

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

(define (imm n)
  (let* ((n16 (modulo n (expt 2 16))))
    `(imm ,n16)))

(define (alu-op n)
  (cond
   ((or (< n 0) (>= n 8)) (error "invalid arg to alu-op" n))
   (else `(alu-op ,n))))

(define alu-plus  (alu-op 0))
(define alu-minus (alu-op 1))
(define alu-and   (alu-op 2))
(define alu-or    (alu-op 3))
(define alu-xor   (alu-op 4))
(define alu-shl   (alu-op 5))
(define alu-shr   (alu-op 6))
(define alu-sar   (alu-op 7))

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
  (define asserted
    (cond (#t 1)
	  (#f 0)
	  (else (error "invalid asserted bool" asserted))))

  (unless (eq? 'imm (type-of offset)) (error "invalid offset" offset))
  (define offset (cadr offset))
  (unless (< offset (expt 2 8)) (error "offset too large" offset))

  (emit `(4 . 5)            ; opcode = 5
	`(2 . ,(cadr flag)) ; flag selector
	`(1 . 0)            ; unused
	`(1 . ,asserted)    ; asserted (set / not set)
	`(8 . ,offset))     ; offset
  )

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
