
; 6_matrix
(define rows 0)
(define cols 0)
(define mat '#())
(define (create-mat f) (let* ((inport (open-input-file f))) (init-matrix inport)))
(define (init-matrix port) (set! rows (read port)) (set! cols (read port)) (set! mat (make-vector (* cols rows))) 
(do ((i 0 (+ i 1)))((= i (vector-length mat)))(vector-set! mat i (read port))))

; a) row
(define (row f r) (create-mat f)  (display-row r))
(define (display-row r) (do ((i 0 (+ i 1)) (j (offset-row r) (+ j 1))) ((= i cols)) (display (vector-ref mat j)) (display " ")))
(define (offset-row r) (- (* cols r) cols))

; b) col
(define (col f c) (create-mat f) (display-col c))
(define (display-col c) (do ((i 0 (+ i 1)) (j (- c 1) (+ j cols)) ) ((= i rows)) (display (vector-ref mat j)) (display " ")))

; c)mmul
(define mat1 '#())
(define mat2 '#())
(define mat3 '#()) ;mat3 length = col2*row1

(define row1 0)
(define row2 0)
(define row3 0)

(define col1 0)
(define col2 0)
(define col3 0)


(define (mmul f1 f2 f3) (make-matrix1 f1) (make-matrix2 f2)
                        (if (col=row?) (make-store-matrix3 f3)(display "ERROR: Different Column and Row"))
                        (compute-mmul) (store-matrix mat3 f3) (print-matrix mat3 col3))

;;; make matrix1
(define (make-matrix1 f) (let* ((p (open-input-file f))) (init-matrix1 p)))
(define (init-matrix1 port) (set! row1 (read port)) (set! col1 (read port)) (set! mat1 (make-vector (* col1 row1)))
(do ((i 0 (+ i 1)))((= i (vector-length mat1)))(vector-set! mat1 i (read port))) (close-input-port port))

;;; make matrix2
(define (make-matrix2 f)(let* ((p (open-input-file f))) (init-matrix2 p)))
(define (init-matrix2 port) (set! row2 (read port)) (set! col2 (read port)) (set! mat2 (make-vector (* col2 row2)))
(do ((i 0 (+ i 1)))((= i (vector-length mat2)))(vector-set! mat2 i (read port)))(close-input-port port))

;;; make matrix3
(define (make-store-matrix3 f) (set! col3 col2)(set! row3 row1)(set! mat3(make-vector (* col2 row1))))

(define (col=row?) (eq? col1 row2))

(define (compute-mmul) (do ((i 0(+ i row1)) (j 0 (+ j (- col1 1 ))))  ((= i (vector-length mat1))) (dot-product i j)))
(define (dot-product indx indx2) (do ((i indx (+ i 1))) ((eq? i col2))
                                  (do ((j indx2 (+ j 1))) ((eq? j (+ j row1)))
                                    (vector-set! mat3 i
                                      (+ (vector-ref mat3 i) (*(vector-ref mat1 j) (vector-ref mat2 (+ j col2))))))))


(define (store-matrix m f) (let* ((p (open-output-file f)))
                            (display row3 p) (display " " p) (display col3 p) (display #\newline p)
                             (do ((i 0 (+ i 1)) (j 0 (+ j 1))) ((= i (vector-length m)) (display""))
                             (if(eq? j col3) (display #\newline p))
                             (if(eq? j col3) (set! j 0))
                             (display (vector-ref m i) p))
                             (display #\newline p)
                            (close-output-port p)))   
                                  
(define (print-matrix v c) (display #\newline) (do ((i 0 (+ i 1)) (j 0 (+ j 1))) ((= i (vector-length v)) (display""))
                             (if(eq? j c) (display #\newline))
                             (if(eq? j c) (set! j 0))
                             (display (vector-ref v i))))


(define (display-m1) (display mat1))
(define (display-m2) (display mat2))
(define (display-m3) (display mat3))
(define (display-r1) (display row1))
(define (display-r2) (display row2))
(define (display-c1) (display col1))
(define (display-c2) (display col2))




