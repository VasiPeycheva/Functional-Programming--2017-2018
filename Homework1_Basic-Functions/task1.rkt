; ********** Task 1 **********

(define (argmin fn list)
  (cond ((null? (cdr list)) (car list))
        ((or (< (fn (car list)) (fn (argmin fn (cdr list)))) (= (fn (car list)) (fn (argmin fn (cdr list))))) (car list))
        (else (argmin fn (cdr list)))))

(define (argmax fn list)
  (cond ((null? (cdr list)) (car list))
        ((or(> (fn (car list)) (fn (argmax fn (cdr list)))) (= (fn (car list)) (fn (argmax fn (cdr list))))) (car list))
        (else (argmax fn (cdr list)))))