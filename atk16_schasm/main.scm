(import (chicken base))

(load "lib.scm")

;; you can just write a raw word
(emit #x1234)

;; compile instructions
(ld SP (u16 0))

;; set the current emit address
(at-addr #x10)

;; set labels
(def-label 'reset-vector
  (emit #xF800)
  (emit #xF801))

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

;; copy to register
(mov R0 R1)

;; store string data in memory
(def-label 'data
  (emit-packed-string "hölynpöly"))

;; compile to a 128KB image file
(write-image-to "out.bin")
