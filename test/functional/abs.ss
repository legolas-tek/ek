;;
;; RUN: glados < %s | FileCheck -v %s
;;

(define abs (lambda (x)
	(if (< x 0)
		(- 0 x)
		x)))

; CHECK: 50
(abs 50)

; CHECK-NEXT: 42
(abs -42)

; CHECK-NEXT: 12345678987654321
(abs 12345678987654321)

; CHECK-NEXT: 12345678987654321
(abs -12345678987654321)