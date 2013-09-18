(define-module rscode
  (use gauche.uvector)
  (use gauche.collection)
  (use gauche.sequence)
  (export
    make-rscode
    rs-encode
    rs-decode
    rs-encode-string
    rs-decode-string

    make-bch-code-over-2
    make-rs-code-over-2
  ))

(select-module rscode)

;;; Polynomial

(define (make-poly deg)
  (make-list (+ deg 1) 0))

(define (poly-ref poly i)
  (ref poly (- (poly-degree poly) i)))

(define (poly-zero? poly)
  (null? (poly-shrink poly)))

(define (poly-shrink poly)
  (define (remove-first-zeros list)
    (if (null? list)
	list
	(if (zero? (car list))
	    (remove-first-zeros (cdr list))
	    list)))
  (reverse (remove-first-zeros (reverse poly))))

(define (poly-expand poly n)
  (if (poly-zero? poly)
      (make-list n 0)
      (cons (car poly)
	    (poly-expand (cdr poly) (- n 1)))))

(define (poly-degree poly)
  (- (length (poly-shrink poly)) 1))

(define (poly-elevate-degree poly offset)
  (cond
    ((null? poly) (list 0))
    ((zero? offset) poly)
    ((positive? offset) (poly-elevate-degree (cons 0 poly) (- offset 1)))
    (else (poly-elevate-degree (cdr poly) (+ offset 1)))))
#|
  (if (zero? offset)
      poly
      (poly-elevate-degree (cons 0 poly) (- offset 1))))
  |#

(define (poly= poly1 poly2)
  (equal? (poly-shrink poly1)
	  (poly-shrink poly2)))

(define (poly-find-min . args)
  (find-min args :key poly-degree))

(define (poly-find-max . args)
  (find-max args :key poly-degree))



;;; Misc

(define (max-length-polys lists)
  (apply max (map length lists)))

(define (expand-polys lists)
  (if (null? lists) '(())
    (let ((len (max-length-polys lists)))
      (map (lambda (lst) (poly-expand lst len)) lists))))



;;; GF(2)

;;; 位数が2の有限体の値は0,1の２つ. それにn次の原始多項式の根8個を添加して拡大体としているので
;;; 値は a[7]*A^7+a[6]*A^6+a[5]*A^5+a[4]*A^4+a[3]*A^3+a[2]*A^2+a[1]*A^1+a[0]*A^0
;;; となる(Aは根の一つ). a[0]~a[7]はもちろん0か1. だからnbitの整数として表せる.
;;; ...という認識. 

(define-class <galois-field-2> ()
  ((size :init-keyword :size)
   (exp-table :init-keyword :exp-table) 
   (log-table :init-keyword :log-table)))

(define (get-poly-from-n n)
  (case n
    ([3] #x03)
    ([4] #x03)
    ([5] #x05)
    ([6] #x03)
    ([8] #x1d)
    (else (error "Sorry, the specified field is not supported yet:" n))))

(define (max-exp gf2)
  (- (~ gf2 'size) 1))

;; prim-poly: primitive polynomial. (e.g., x^8 + x^4 + x^3 + x^2 + 1 = 0)
(define (init-galois-field-2 gf2 prim-poly)
  (let ([x 1]
	[maximum (max-exp gf2)])
    (dotimes (i maximum)
      (set! (~ gf2 'exp-table i) x)
      (set! (~ gf2 'log-table x) i)
      (set! x (logxor (logand (ash x 1) maximum)
		      (if (zero? (logand x (ash (~ gf2 'size) -1)))
			  0
			  prim-poly))))
    (set! (~ gf2 'log-table 0) #f)
    (set! (~ gf2 'exp-table maximum) #f)))

(define (make-galois-field-2 n prim-poly)
  (let* ((size (expt 2 n))
	 (gf2 (make <galois-field-2>
		:size size
		:exp-table (make-vector size 0)
		:log-table (make-vector size 0))))
    (init-galois-field-2 gf2 prim-poly)
    gf2))

(define-macro (with-gf2-table gf2 . rest)
  `(let ((ref-exp-table (pa$ ~ ,gf2 'exp-table))
	 (ref-log-table (pa$ ~ ,gf2 'log-table))
	 (maximum (max-exp gf2)))
     ,@rest))

(define (gf2-add gf2 . rest)
  (apply logxor rest))

(define (gf2-mul gf2 a b)
  (with-gf2-table gf2
   (if (or (zero? a) (zero? b))
       0
       (let ((index (mod (+ (ref-log-table a) (ref-log-table b)) maximum)))
	 (ref-exp-table index)))))

(define (gf2-pow gf2 a exp)
  (with-gf2-table gf2
   (if (zero? a)
       0
       (let ((index (mod (* (ref-log-table a) exp) maximum)))
	 (ref-exp-table index)))))

(define (gf2-div gf2 a b)
  (with-gf2-table gf2
   (if (zero? a)
       0
       (if (zero? b)
	   (error "divide by zero")
	   (let ((index (mod (- (ref-log-table a) (ref-log-table b)) maximum)))
	     (ref-exp-table index))))))

(define (gf2-alpha gf2 exp)
  (with-gf2-table gf2
   (ref-exp-table (mod exp maximum))))

(define (gf2-log-alpha gf2 val)
  (with-gf2-table gf2
   (ref-log-table val)))

(define (gf2-mul-poly gf2 a b)
  (apply map (pa$ gf2-add gf2)
         (expand-polys
           (map (lambda (j offset)
                  (poly-elevate-degree (map (lambda (i) (gf2-mul gf2 i j)) a) offset))
                b
                (iota (length b))))))

(define (gf2-divmod-poly gf2 a b)
  (define (poly-leading-coefficient poly)
    (fold (^(a b) (if (> a 0) a b)) 0 poly))
  (let ((max-deg-denom (poly-leading-coefficient b)))
    (when (<= max-deg-denom 0)
          (error "divide by zero poly" a "/" b))
    (let loop ((q '())
               (r a))
      (let ((offset (- (poly-degree r) (poly-degree b))))
        (if (< offset 0)
          (values (poly-shrink q) (poly-shrink r))
          (let* ((tmp-q (poly-elevate-degree (list (gf2-div gf2 (poly-leading-coefficient r) max-deg-denom)) offset))
                 (nq (gf2-add-poly gf2 q tmp-q))
                 (nr (gf2-add-poly gf2 r (gf2-mul-poly gf2 tmp-q b))))
            (loop nq nr)))))))


(define (gf2-div-poly gf2 a b)
  (receive (q r) (gf2-divmod-poly gf2 a b)
	   q))

(define (gf2-mod-poly gf2 a b)
  (receive (q r) (gf2-divmod-poly gf2 a b)
	   r))

(define (gf2-add-poly gf2 . rest)
  (apply map (lambda args (apply gf2-add gf2 args)) (expand-polys rest)))

(define (gf2-dif-poly gf2 a)
  (map (^(x i) (if (even? i) x 0)) (cdr a) (iota (length a))))

(define (gf2-calc-poly gf2 a b)
  (apply gf2-add gf2
	 (map (lambda (c i) (gf2-mul gf2 c (gf2-pow gf2 b i))) a (iota (+ (poly-degree a) 1)))))


; dmin : minimum (design) distance
(define (get-generator-polynomial-for-bch gf2 dmin :optional (b 0))
  ; b = 1, narrow sense
  (define (get-conjugates s q m)
    ; conjugates = cyclotomic cosets ?
    (delete-duplicates (map
                         (lambda (i)
                           (mod (* s (expt q i)) (- (expt q m) 1)))
                         (iota m))))
  (define (minimal-polynomial-terms s)
    (let1 terms (map (lambda (e)
                       (list (gf2-alpha gf2 e) (gf2-alpha gf2 0)))
                     (get-conjugates s q m))
      terms))
  (fold (lambda (elm knil) (gf2-mul-poly gf2 knil elm))
	(list 1)
	(delete-duplicates (fold append '() (map minimal-polynomial-terms (iota (- dmin 1) b))))))

(define (get-generator-polynomial-for-rs gf2 error-words :optional (b 0))
  ; multiply all (x - a^i) to calc G(x) of RS
  (fold (lambda (i s)
	  (gf2-mul-poly gf2 s (list (gf2-alpha gf2 i)
				    (gf2-alpha gf2 0))))
	(list 1)
	(iota error-words b))) ; error-words = 2t

(define (gf2-solve-key-equation gf2 a b)
  (let loop ((m (poly-find-max a b))
             (n (poly-find-min a b))
             (x (list 0))
             (y (list 1)))
    (if (and (not (poly-zero? n))
             (>= (poly-degree n) (poly-degree y)))
      (receive (q r) (gf2-divmod-poly gf2 m n)
        (let ((z (gf2-add-poly gf2 (gf2-mul-poly gf2 q y) x)))
          (loop n r y z)))
      (let ((h (list (car y))))
        (values (gf2-div-poly gf2 y h)
                (gf2-div-poly gf2 n h))))))


(define-class <rscode> ()
  ((gf2         :init-keyword :gf2)
   (g           :init-keyword :g)
   (num-total-words :init-keyword :num-total-words)
   (num-data-words  :init-keyword :num-data-words)
   (num-error-words :init-keyword :num-error-words)
   (channel-decoder :init-value identity)
   (channel-encoder :init-value identity)))

(define (make-bchcode n k d :optional (gf2-channel-exp 8) (gf2-decoder-exp 8) (gf2-prim-poly #f))
  (let* ((gf2 (make-galois-field-2 gf2-exp (or gf2-prim-poly (get-poly-from-n gf2-exp))))
         (num-error-words (- num-total-words num-data-words))
         (g   (get-generator-polynomial-for-rs gf2 num-error-words)))
    (make <rscode>
          :gf2 gf2
          :g g
          :num-total-words num-total-words
          :num-data-words  num-data-words
          :num-error-words num-error-words)))


(define (make-rscode num-total-words num-data-words :optional (gf2-exp 8) (gf2-prim-poly #f))
  (unless (< num-total-words (expt 2 gf2-exp))
    ; due to the Reed-Solomon limit
    (error #`"num-total-words must be less than 2^,|gf2-exp|"))
  (let* ((gf2 (make-galois-field-2 gf2-exp (or gf2-prim-poly (get-poly-from-n gf2-exp))))
         (num-error-words (- num-total-words num-data-words))
         (g   (get-generator-polynomial-for-rs gf2 num-error-words)))
    (make <rscode>
          :gf2 gf2
          :g g
          :num-total-words num-total-words
          :num-data-words  num-data-words
          :num-error-words num-error-words)))


(define (rs-encode rscode data-words)
  ;; TODO: check data-words size
  (let* ((gf2 (~ rscode 'gf2))
         (channel-decode (~ rscode 'channel-decoder))
         (channel-encode (~ rscode 'channel-encoder))
         (I (poly-elevate-degree (reverse (channel-decode data-words)) (~ rscode 'num-error-words)))
         (g (~ rscode 'g)))
    (channel-encode (gf2-add-poly gf2 (gf2-mod-poly gf2 I g) I))))


(define (rs-decode rscode encoded-words)
  ;;
  (let* ((gf2 (~ rscode 'gf2))
         (channel-decode (~ rscode 'channel-decoder))
         (channel-encode (~ rscode 'channel-encoder))
         (r (channel-decode encoded-words))
         (num-total-words (~ rscode 'num-total-words))
         (num-error-words (~ rscode 'num-error-words))
         (b (~ rscode 'b))
         (dmin (~ rscode 'dmin))
         (dmin-1 (- dmin 1))
         (s (map (lambda (i) (gf2-calc-poly gf2 r (gf2-alpha gf2 i)))
                 (iota dmin-1 b))) ; syndrome vector
         (z (poly-elevate-degree '(1) dmin-1))) ; z = x^{dmin-1}
    ;(print "Syndrome: " s)
    (channel-encode
      (reverse
        (drop
          ; Solve Key Equation: Omega(x) = Sigma(x) * S(x) mod x^(2t)
          (receive (sigma omega) (gf2-solve-key-equation gf2 z s)
            (let* ((denom (gf2-dif-poly gf2 sigma))
                   ; Forney algorithm
                   ; $$ e_k = - \frac{\alpha^{i_k}\omega(\alpha^{-i_k})}{\alpha^{b \cdot i_k}\sigma'(\alpha^{-i_k})} $$
                   (e* (map (^i
                              (let ((v (gf2-alpha gf2 (- i))))
                                (if (zero? (gf2-calc-poly gf2 sigma v))
                                  (gf2-div gf2
                                           (gf2-mul gf2 (gf2-alpha gf2      i ) (gf2-calc-poly gf2 omega v))
                                           (gf2-mul gf2 (gf2-alpha gf2 (* b i)) (gf2-calc-poly gf2 denom v)))
                                  0)))
                            (iota num-total-words))))
              (map (pa$ gf2-add gf2) e* r)))
          num-error-words)))))

(define (rs-encode-string rscode str)
  (when (> 8 (~ rscode r))
    (error "Not supported on the field. Use rs-encode directly instead."))
  (rs-encode rscode (u8vector->list (string->u8vector str))))

(define (rs-decode-string rscode lst)
  (when (> 8 (~ rscode r))
    (error "Not supported on the field. Use rs-decode directly instead."))
  (u8vector->string (list->u8vector (rs-decode rscode lst))))


(define (gf2-format-poly gf2 poly)
  (let ((deg (poly-degree poly)))
    (string-join
      (reverse
        (filter-map
          (lambda (val idx)
            (and (not (zero? val))
              (format "a^~A x^~A" (gf2-log-alpha gf2 val) idx)))
          poly
          (iota (length poly))))
      " + ")))


;;; Test

;; (use srfi-27)

;; (define (randomize c)
;;   (let ((r (make-poly 25))
;; 	(e (make-poly 25)))
;;     (do ((i (+ 1 (random-integer 3)) (- i 1)))
;;     	((<= i 0))
;;       (set! (~ r (random-integer (polynomial-deg r))) (random-integer 256)))
;;     (do ((l 0 (+ l 1)))
;;     	((> l 25))
;;       (unless (= (~ r l) (~ c l))
;;     	      (set! (~ e l) (logxor (~ r l) (~ c l)))))
;;     e))

;; (define (test)
;;   (let ((rscode (make-rscode 26 19))
;; 	(sample-data '(122 100 122 100 122 100 122 54 232 180 50 148 126 232 164 2 153 149 6)))
;;     (dotimes (i 100)
;;       (if (poly= sample-data
;; 		 (rs-decode rscode #?=(randomize (rs-encode rscode sample-data))))
;; 	  (print i "GOOD")
;; 	  (print i "BAD")))))




(define-class <bch-code> ()
  ((r           :init-keyword :r :init-value 1)
   (m           :init-keyword :m :init-value 8)
   (t           :init-keyword :t :init-value 1)
   (b           :init-keyword :b :init-value 1)
   (gf2-prim-poly :init-keyword :gf2-prim-poly :init-value #f)

   (gf2)
   (q)
   (n)
   (dmin)
   (g-roots)
   (g)

   (num-total-words)
   (num-data-words )
   (num-error-words)

   (channel-decoder :init-value identity)
   (channel-encoder :init-value identity)

   (channel-decoder-table)
   (channel-encoder-table)))

(define-macro (slot-copy! obj symbols)
  `(begin
     ,@(map (lambda (sym) `(slot-set! ,obj ',sym ,sym))
            symbols)))

(define-method initialize ((self <bch-code>) initargs)
  (next-method)
  (let* ((r    (slot-ref self 'r))
         (m    (slot-ref self 'm))
         (t    (slot-ref self 't))
         (b    (slot-ref self 'b))

         (gf2-exp (* r m))
         (gf2-prim-poly (or (slot-ref self 'gf2-prim-poly) (get-poly-from-n gf2-exp)))
         (gf2  (make-galois-field-2 gf2-exp gf2-prim-poly))

         (q (expt 2 r))
         (n (- (expt q m) 1))
         (dmin (+ (* 2 t) 1))

         (get-conjugates (lambda (s q m)
                           ; conjugates = cyclotomic cosets ?
                           (delete-duplicates (map
                                                (lambda (i)
                                                  (mod (* s (expt q i)) (- (expt q m) 1)))
                                                (iota m)))))

         (g-roots (delete-duplicates (fold append '() (map (cut get-conjugates <> q m) (iota (- dmin 1) b)))))
         (g (fold (lambda (e knil) (gf2-mul-poly gf2 knil (list (gf2-alpha gf2 e) (gf2-alpha gf2 0)))) (list 1) g-roots))

         (num-error-words (length g-roots))
         (num-total-words n)
         (num-data-words  (- num-total-words num-error-words))

         (channel-alphabet-list (iota (- q 1) 0 (/ n (- q 1))))

         (channel-decoder-table (make-hash-table))
         (channel-encoder-table (make-hash-table))
         
         (channel-decoder (lambda (l)
                             (map (pa$ hash-table-get channel-decoder-table) l)))
         (channel-encoder (lambda (l)
                             (map (pa$ hash-table-get channel-encoder-table) l)))
         )

    (hash-table-put! channel-decoder-table 0 0)
    (hash-table-put! channel-encoder-table 0 0)
    (for-each-with-index 
      (lambda (channel-value decoder-value)
        (hash-table-put! channel-decoder-table (+ 1 channel-value) decoder-value)
        (hash-table-put! channel-encoder-table decoder-value (+ 1 channel-value)))
      (sort (map (pa$ gf2-alpha gf2) channel-alphabet-list)))
    (slot-copy! self (gf2 gf2-prim-poly
                          q
                          n
                          dmin
                          g-roots
                          g
                          num-error-words
                          num-total-words
                          num-data-words
                          channel-decoder-table channel-encoder-table
                          channel-decoder
                          channel-encoder
                          ))))





(define (make-rs-code-over-2 r t :optional (b 1) (poly #f))
  (make-bch-code-over-2 r 1 t b poly))

; Make BCH over GF(2^r), with n = q^m  (It is Reed-Solomon if m == 1)
(define (make-bch-code-over-2 r m t :optional (b 1) (poly #f))
  (make <bch-code>
        :r r
        :m m
        :t t
        :b b
        :gf2-prim-poly poly))


