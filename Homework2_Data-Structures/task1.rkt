; help functions
(define (first-row matrix) (car matrix))
(define (rest-row matrix) (cdr matrix))
(define (first-col matrix) (map car matrix))

(define testList '((1 5 2 )
                   (2 3 8 )
                   (-2 0 4)))

; ****************** TASK 1 ******************

; main function
(define (row-reduce matrix)
  (row-reduce-rec '() matrix))

; main function-helper
; at first, the variable "row-eliminator" is the empty list, until we find a row in the matrix, with first element != 0
(define (row-reduce-rec row-eliminator matrix)
  (cond ((null? matrix) '())
        ((= (car (first-row matrix)) 0) (cons (first-row matrix) (row-reduce-rec row-eliminator (rest-row matrix))))
        ((and(not(= (car (first-row matrix)) 0))(null? row-eliminator)) (cons (first-row matrix) (row-reduce-rec (first-row matrix) (rest-row matrix))))
        (else (cons (eliminate-row row-eliminator (first-row matrix)) (row-reduce-rec row-eliminator (rest-row matrix))))))

;sum two rows, making the first element of row 2 equal to zero
(define (eliminate-row row1 row2)
  (define eliminator (car row1))
  (define target (car row2))
  (sum-list (my-map row1 (-(/ target eliminator))) row2))

;miltiply every element in the list with k
(define (my-map list k)
  (if (null? list) '()
      (cons (* (car list) k) (my-map (cdr list) k))))

;sum two lists
(define (sum-list list1 list2)
  (if (null? list1) '()
      (cons (+ (car list1) (car list2)) (sum-list (cdr list1) (cdr list2)))))
