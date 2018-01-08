; ********** Task 3 **********

(define (aprox-zero x eps)
  (if (< (abs x) eps) #true #false))

(define (middle a b)
  (/ (+ a b) 2))

(define (check-sign? x a)
  (if (or(and(positive? x)(positive? a)) (and(negative? x)(negative? a))) #t #f ))

(define (find-root fn a b eps)
  (define mid (middle a b))
  (cond ((aprox-zero (fn mid) eps) mid)
        ((check-sign? (fn mid) (fn a)) (find-root fn mid b eps))
        (else (find-root fn a mid eps))))