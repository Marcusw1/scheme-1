; 2_sigma
(define (sumlist l) (if (null? l) 0 (+ (car l)(sumlist(cdr l)))))
(define (listlen l) (if (null? l) 0 (+ 1 (listlen (cdr l)))))
(define (mean l)(/ (sumlist l) (listlen l)))
(define square (lambda(n)(* n n)))
(define (squareList l)(map square l))
(define (meanSquared l) (square (mean l)))
(define (squareListMean l) (mean (squareList l)))
(define (sigma . n)(sqrt(- (squareListMean n) (meanSquared n))))

; 3_line
(define (line n) (if (> n 1) (line (- n 1)) 0) (display "*"))
(define (line1 n)(cond ((< n 1) (newline))(else (display "*") (line1 (- n 1)))))
(define (histogram list)(if (null? list) (newline)(begin (line1 (car list))(histogram (cdr list)))))

; 4_f(x)_max
(define (fmax X Y funct) (let* ((trisect (/ (- Y X) 3.))(xtri (+ X trisect))(ytri (- Y trisect)))
   (cond ((< (abs (- (funct X) (funct Y))) .0000001) (/ (+ Y X) 2.)) ((> (funct xtri) (funct ytri)) (fmax X ytri funct)) (else (fmax xtri Y funct)))))

; 5_scalar
(define vectorslens=? (lambda (l1 l2) (= (vector-length l1)(vector-length l2))))
(define (error-msg . n) (write "ERROR: Different sizes of vectors!"))
(define mul (lambda (x y)(* x y)))

; a) scalar recursive
(define scalar-product-R (lambda (l1 l2) (if (vectorslens=? l1 l2) (compute-product-R (vector->list l1) (vector->list l2)) (error-msg))))
(define compute-product-R (lambda (l1 l2) (if (null? l1) 0 (+ (mul (car l1)(car l2)) (compute-product-R (cdr l1)(cdr l2))))))

; b) scalar iterative
(define scalar-product-I (lambda (l1 l2) (if (vectorslens=? l1 l2) (compute-product-I l1 l2) (error-msg))))
(define compute-product-I (lambda (l1 l2) (do ( (i 0 (+ i 1)) (sum 0)) ((= i (vector-length l1)) (display sum)) (set! sum( + sum (* (vector-ref l1 i)(vector-ref l2 i)))) )))


