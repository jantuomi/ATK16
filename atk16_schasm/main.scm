(load "core.scm")
(load "macros.scm")

;; load asm modules
(load "bootstrap.scm")

;; you can just write a raw word
(emit-word #x1234)

;; compile instructions
(ld SP (u16 0))

;; set labels
(def-label 'some-data
  (emit-word #xF800)
  (emit-word #xF801))

;; set the current emit address
(at-addr #x20)
(def-label 'main
  (add R1 R2)
  (add R3 (u16 #xFF)))

;; abs jump
(ld PC (label 'main))

;; rel jump
(add PC (i16 10))

;; jump forward to a currently undefined label
(ld PC (label 'forward))
(def-label 'forward
  (hlt))

;; store string data in memory
(def-label 'text-data
  (%packed-string "hölynpöly"))

(at-addr #x50)

;; branch based on an ALU flag
(br flag-carry (label 'branch-true) set: #t)
(def-label 'branch-false
  (emit-word 1))
(def-label 'branch-true
  (emit-word 2))

;; use macros like %if-else, %when, %while for convenience
(let ((x #x5678))
  (%if-else (R1 == R2)
	    (emit-word #x1234)
	    (emit-word x)))

(%when (R1 <= (u16 #xFF))
     (emit-word #xbeef))

;; procedures
(%def-proc (println *str)
  (param '*str) ;; => R0
  (ld PC SP pop: #t indirect: 1))

;; procedure calls
(ld R1 (u16 10))
(%while (R1 > (u16 0))
	(%call println (label 'text-data))
	(sub R1 (u16 1)))

;; compile to a 128KB image file
(write-image-to "out.bin")
