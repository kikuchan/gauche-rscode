(define-module rscode
  (use gauche.uvector)
  (export
    make-rscode
    rs-encode
    rs-decode
    rs-encode-string
    rs-decode-string
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

(define (poly-elevate-order poly offset)
  (if (zero? offset)
      poly
      (poly-elevate-order (cons 0 poly) (- offset 1))))

(define (poly= poly1 poly2)
  (equal? (poly-shrink poly1)
	  (poly-shrink poly2)))


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
    ([8] #x1d)
    (else (error "Sorry, the specified field is not supported yet."))))

(define (max-exp gf2)
  (- (~ gf2 'size) 1))

(define (init-galois-field-2 gf2 prim-poly)
  (let ([x 1]
	[maximum (max-exp gf2)])
    (do ((i 0 (+ i 1)))	((>= i maximum))
      (set! (~ gf2 'exp-table i) x)
      (set! (~ gf2 'log-table x) i)
      (set! x (logxor (logand (ash x 1) maximum)
		      (if (zero? (logand x (ash (~ gf2 'size) -1)))
			  0
			  prim-poly))))
    (set! (~ gf2 'log-table 0) maximum)))

(define (make-galois-field-2 n prim-poly)
  (let* ((size (expt 2 n))
	 (gf2 (make <galois-field-2>
		:size size
		:exp-table (make-vector size 0)
		:log-table (make-vector size 0))))
    (init-galois-field-2 gf2 prim-poly)
    gf2))

(define-macro (with-gf2-table . rest)
  `(let ((exp-table (~ gf2 'exp-table))
	 (log-table (~ gf2 'log-table))
	 (maximum (max-exp gf2)))
     ,@rest))

(define (gf2-add gf2 . rest)
  (apply logxor rest))

(define (gf2-mul gf2 a b)
  (with-gf2-table
   (if (or (zero? a) (zero? b))
       0
       (let ((index (mod (+ (~ log-table a) (~ log-table b)) maximum)))
	 (~ exp-table index)))))

(define (gf2-pow gf2 a exp)
  (with-gf2-table
   (if (zero? a)
       0
       (let ((index (mod (* (~ log-table a) exp) maximum)))
	 (~ exp-table index)))))

(define (gf2-div gf2 a b)
  (with-gf2-table
   (if (zero? a)
       0
       (if (zero? b)
	   (error "divide by zero")
	   (let ((index (mod (- (~ log-table a) (~ log-table b)) maximum)))
	     (~ exp-table index))))))

(define (gf2-alpha gf2 exp)
  (with-gf2-table
   (~ exp-table (mod exp maximum))))

(define (gf2-log-alpha gf2 val)
  (with-gf2-table
   (~ log-table val)))

(define (gf2-mul-poly gf2 a b)
  (apply map (pa$ gf2-add gf2)
         (expand-polys
           (map (lambda (j offset)
                  (poly-elevate-order (map (lambda (i) (gf2-mul gf2 i j)) a) offset))
                b
                (iota (length b))))))

(define (gf2-divmod-poly gf2 a b)
  (define (poly-leading-coefficient poly)
    (fold (^(a b) (if (> a 0) a b)) 0 poly))
  (let ((max-deg-denom (poly-leading-coefficient b)))
    (when (<= max-deg-denom 0)
          (error "devide by zero poly"))
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
;  (poly-expand (map (^(x i) (if (even? i) x 0)) (cdr a) (iota (length a))) (length a)))
  (map (^(x i) (if (even? i) x 0)) (cdr a) (iota (length a))))

(define (gf2-calc-poly gf2 a b)
  (apply gf2-add gf2
	 (map (lambda (c i) (gf2-mul gf2 c (gf2-pow gf2 b i))) a (iota (+ (poly-degree a) 1)))))

(define (get-generator-polynomial-for-rs gf2 error-words)
  (fold (lambda (i s)
	  (gf2-mul-poly gf2 s (list (gf2-alpha gf2 i)
				    (gf2-alpha gf2 0))))
	(list 1)
	(iota error-words)))

(define (gf2-solve-key-equation gf2 a b)
  (let loop ((m (if (< (poly-degree a) (poly-degree b)) b a))
             (n (if (< (poly-degree a) (poly-degree b)) a b))
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
   (num-error-words :init-keyword :num-error-words)))

(define (make-rscode num-total-words num-data-words :optional (gf2-exp 8) (gf2-prim-poly #f))
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
         (I (poly-elevate-order (map (cut gf2-alpha gf2 <>) (reverse data-words)) (~ rscode 'num-error-words)))
         (g (~ rscode 'g)))
    (gf2-add-poly gf2 (gf2-mod-poly gf2 I g) I)))


(define (rs-decode rscode encoded-words)
  ;;
  (let* ((gf2 (~ rscode 'gf2))
         (r encoded-words)
         (num-total-words (~ rscode 'num-total-words))
         (num-error-words (~ rscode 'num-error-words))
         (s (map (lambda (i) (gf2-calc-poly gf2 r (gf2-alpha gf2 i)))
                 (iota num-error-words)))
         (z (poly-elevate-order '(1) num-error-words)))
    (map (pa$ gf2-log-alpha gf2)
         (reverse
           (drop
             (receive (sigma omega) (gf2-solve-key-equation gf2 z s)
               (let* ((x (list 0 1))
                      (denom (gf2-mul-poly gf2 x (gf2-dif-poly gf2 sigma))))
                 (map (pa$ gf2-add gf2)
                      (map (^i
                             (let ((v (gf2-alpha gf2 (- (max-exp gf2) i))))
                               (if (zero? (gf2-calc-poly gf2 sigma v))
                                 (gf2-div gf2
                                          (gf2-calc-poly gf2 omega v)
                                          (gf2-calc-poly gf2 denom v))
                                 0)))
                           (iota num-total-words))
                      r)))
             num-error-words)))))






(define (rs-encode-string rscode str)
  (rs-encode rscode (u8vector->list (string->u8vector str))))

(define (rs-decode-string rscode lst)
  (u8vector->string (list->u8vector (rs-decode rscode lst))))


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
