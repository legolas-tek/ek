;;
;; RUN: glados < %s | FileCheck -v %s
;;

(define sqsum (lambda (a)
	(define aa (* a a))
	(lambda (b)
		(define bb (* b b))
		(+ aa bb))))

; CHECK: <lambda>
(sqsum 57)

; CHECK-NEXT: Error
(sqsum 2 3)

; CHECK-NEXT: 25
((sqsum 3) 4)
