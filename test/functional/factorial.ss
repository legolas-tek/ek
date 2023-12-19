;;
;; RUN: glados < %s | FileCheck -v %s
;;

(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

; CHECK: 120
(factorial 5)

; CHECK-NEXT: 720
(factorial 6)

; CHECK-NEXT: 5040
(factorial 7)

; CHECK-NEXT: 1
(factorial 0)

; CHECK-NEXT: 1
(factorial 1)
