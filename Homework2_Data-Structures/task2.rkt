; help functions
(define (first-row matrix) (car matrix))
(define (rest-row matrix) (cdr matrix))
(define (first-col matrix) (map car matrix))

(define testList '((1 5 2 )
                   (2 3 8 )
                   (-2 0 4)))

; ****************** TASK 2 ******************

; main function
(define (cross-out matrix)
  (row-rec 1 matrix))

; iterate through the matrix by rows
(define (row-rec row matrix)
  (define maxRow (length (first-col matrix)))
  (if(= row maxRow)(col-rec row 1 matrix)
     (cons (col-rec row 1 matrix) (row-rec (+ row 1) matrix))))

; iterate through the matrix by columns
(define (col-rec row col matrix)
  (define maxCol (length (first-row matrix)))
  (if (= col maxCol) (remove-row-col row col matrix)
      (cons (remove-row-col row col matrix) (col-rec row (+ col 1) matrix))))

; generate matrix with removed k-row and p-column
(define (remove-row-col row col matrix)
  (remove-row row (remove-col col matrix)))

; remove the k-th row in given matrix 
(define (remove-row k matrix)
  (define (remove iter k matrix)
    (if (= iter k) (rest-row matrix)
        (cons (first-row matrix) (remove (+ iter 1) k (rest-row matrix)))))
  (remove 1 k matrix))

; remove the k-th column in given matrix
; iterate through each row and remove the k-th element in each row
(define (remove-col k matrix)
  (if (null? matrix) '()
      (cons (remove-list k (first-row matrix)) (remove-col k (rest-row matrix)))))

; remove the k-th element in given list
(define (remove-list k list)
  (if(= k 1)(cdr list)
  (cons (car list)(remove-list (- k 1) (cdr list)))))