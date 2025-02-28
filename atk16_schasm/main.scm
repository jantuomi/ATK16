(import (chicken base))

(load "lib.scm")

(define FL (reg 13))
(define PC (reg 14))
(define SP (reg 15))

(ld SP (imm 0))

(at-addr #x10)
(def-label 'reset-vector)
(emit #xF800)
(emit #xF801)

(at-addr #x20)
(def-label 'main)
(add (reg 1) (reg 2))
(add (reg 3) (imm #xFF))

;; abs jump
(ld PC (imm (label 'main)))

;; rel jump
(add PC (imm 10))

(mov (reg 0) (reg 1))
