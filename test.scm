(use gauche.test)
(use gauche.uvector)

(test-start "rscode")

(load "./rscode")
(import rscode)
(test-module 'rscode)

(define rscode (make-rscode 26 19))

(test-section "rs-encode / rs-decode")

; 適当なリストを変換
(test "rs-encode"
  '(18 203 17 239 28 192 169 236 17 236 17 236 17 236 80 247 150 5 82 102 247 198 4 146 164 64)
  (lambda() (rs-encode rscode '(6 149 153 2 164 232 126 148 50 180 232 54 122 100 122 100 122 100 122))))

(test "rs-decode"
  '(6 149 153 2 164 232 126 148 50 180 232 54 122 100 122 100 122 100 122)
  (lambda() (rs-decode rscode '(18 203 17 239 28 192 169 236 17 236 17 236 17 236 80 247 150 5 82 102 247 198 4 146 164 64))))

(test "rs-decode (with noise)"
  '(6 149 153 2 164 232 126 148 50 180 232 54 122 100 122 100 122 100 122)
  (lambda() (rs-decode rscode '(19 203 17 239 28 192 169 0 17 236 17 236 17 236 80 247 150 5 82 102 247 198 4 146 164 64))))

(test-end :exit-on-failure #t)

;; 複号してみる
;(print (rs-decode rscode '(18 203 17 239 28 192 169 236 17 236 17 236 17 236 80 247 150 5 82 102 247 198 4 146 164 64)))
;
;; 改竄してもok
;(print (rs-decode rscode '(19 203 17 239 28 192 169 0 17 236 17 236 17 236 80 247 150 5 82 102 247 198 4 146 164 64)))
;
;
;; 文字列を変換する関数
;(define (encode str)
;  (rs-encode rscode (u8vector->list (string->u8vector str))))
;
;(define (decode lst)
;  (u8vector->string (list->u8vector (rs-decode rscode lst))))
;
;; やってみる
;(encode "ABCDE")
;
;; 化けてても…
;(decode '(3 41 88 61 80 143 63 10 153 194 97 190))
;
