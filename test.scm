(use gauche.test)
(use gauche.uvector)

(test-start "rscode")

(load "./rscode")
(import rscode)

(use srfi-27)

(test-module 'rscode)

(define rscode (make-rscode 26 19))

(test-section "rs-encode / rs-decode")

(dotimes (i 100)
  (let ((input (map (^_ (random-integer 256)) (iota 19))))
    (test "rs-encode & rs-decode (no errors)"
          input
          (lambda() (let ((encoded (rs-encode rscode input)))
                      ;(print "\n" "input  ; " input)
                      ;(print "encoded; " encoded)
                      (rs-decode rscode encoded))))))

(define (add-errors r n)
  (let ((r (list-copy r)))
    (dotimes (i n)
      (set! (~ r (random-integer (length r))) (random-integer 256)))
    r))



(dotimes (i 100)
  (let ((input (map (^_ (random-integer 256)) (iota 19))))
    (test "rs-encode & rs-decode (with errors)"
          input
          (lambda() (let ((encoded (rs-encode rscode input)))
                      ;(print "\n" "input  ; " input)
                      ;(print "encoded; " encoded)
                      (rs-decode rscode (add-errors encoded (random-integer 4))))))))

(test-end :exit-on-failure #t)
