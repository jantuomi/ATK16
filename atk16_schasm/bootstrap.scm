(define addr-segment-heap       #x8000) ; grows up until SP
(define addr-segment-mmio       #xE7F0)
(define addr-segment-sprite     #xE800)
(define addr-segment-text       #xF800)
;; note: sprite memory & text memory can overlap
;; since they are never used at the same time
(define addr-mmio-terminal      (+ addr-segment-mmio 0))
(define addr-mmio-keyboard      (+ addr-segment-mmio 1))
(define addr-mmio-graphics-mode (+ addr-segment-mmio 2))

(define graphics-disabled 0)
(define graphics-text     1)
(define graphics-sprite   2)

(at-addr #x0)
(def-label 'entrypoint
  ;; Set stack pointer to end of heap segment, i.e. one below addr-segment-mmio.
  ;; Stack grows down.
  ;; Heap grows up from addr-segment-heap until it meets the stack.
  (%ld SP (u16 (- addr-segment-mmio 1)))
  ;; Set graphics mode to disabled.
  (%ld R0 (u16 addr-mmio-graphics-mode))
  (%st R0 (u16 graphics-disabled)))
