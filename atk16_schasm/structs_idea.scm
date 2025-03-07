# This buffer is for notes you don’t want to save, and for Chicken Scheme code.

;; define structs
(%def-struct point
	     (x  u16)
	     (y  u16))

;; static struct instances
(%static global-point
	 point (u16 10) (u16 20))

;; use static address as load offset
(ld R0 ZR (global-point:x))

;; stack allocated instances
(%def-proc (main)
	   ;; alloc some space on the stack
	   (%svar my-point (point:size))
	   ;; use stack offset as load offset
	   (ld R0 SP (my-point:x)))

;; a vector of static instances
(let loop ((i 0))
  (when (< i 10)
    (%static (string->symbol (format "global-points/~A" i))
	     point (u16 i) (u16 i))))

(ld R0 ZR (global-points/0:x))
