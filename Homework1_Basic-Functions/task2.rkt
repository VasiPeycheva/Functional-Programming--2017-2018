; ********** Task 2 **********

(define (max-list list)
  (cond ((null? (cdr list)) (car list))
         ((> (car list) (max-list (cdr list))) (car list))
         (else (max-list (cdr list)))))

(define (remove* element list)
  (cond ((null? list) '())
        ((= element (car list)) (cdr list))
        (else (cons (car list) (remove* element (cdr list))))))

(define (numberToList number)
  (if (< number 10) (list number)
      (append (numberToList (quotient number 10)) (list (remainder number 10)))))


(define (listToNumber list)
  (define (listToNumberRec list result)
    (if (= (length list) 1)
        (+ result (car list))
        (listToNumberRec (cdr list) (+ result (* (car list) (expt 10 (- (length list) 1)))))))
  (listToNumberRec list 0))

(define (helper n)
  (define max (max-list (numberToList n)))
  (* (listToNumber (remove* max (numberToList n))) max))


(define (reduce n)
 (cond ((< n 10) n)
       (else (reduce (helper n)))))