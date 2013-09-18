(use gauche.test)
(use gauche.uvector)

(test-start "rscode")

(load "./rscode")
(import rscode)

(use srfi-27)

(test-module 'rscode)

(define rscode (make-rscode 26 19))

(test-section "well-known rs-encode / rs-decode pattern")

(let ((input '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100)))
  (test "rs-encode (well known)"
        '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100 57 115 92 47 14 44 132)
        (lambda() (rs-encode rscode input))))

(let ((input '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100 57 115 92 47 14 44 132)))
  (test "rs-decode (well known)"
        '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100)
        (lambda() (rs-decode rscode input))))

(let ((input '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 0 224 235 100 57 115 92 47 14 44 132)))
  (test "rs-decode (well known + 1 errors)"
        '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100)
        (lambda() (rs-decode rscode input))))

(let ((input '(208 251 75 42 44 27 126 95 0 50 221 125 52 86 157 0 224 235 100 57 115 92 47 14 44 132)))
  (test "rs-decode (well known + 2 errors)"
        '(208 251 75 42 44 27 126 95 50 50 221 125 52 86 157 243 224 235 100)
        (lambda() (rs-decode rscode input))))

(test-end :exit-on-failure #t)

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
