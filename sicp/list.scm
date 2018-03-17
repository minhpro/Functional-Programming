;:get nth element of a list, n <= number of elements of the list
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

;: Iterative version of length
(define (length items)
    (define (length-iter a count)
        (if (null? a)
            count
            (length-iter (cdr a) (+ 1 count))))
        (length-iter items 0))

;: append two list form a list
;: (append (list 1 3) (list 5 7)) => (1 3 5 7)
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

;: return the list of the last element
(define (last-pair items)
    (if (or (null? items) (null? (cdr items)))
        items
        (last-pair (cdr items)))) 

;: Reverse a list
(define (reverse-iter items answer)
    (if (null? items)
    answer
    (reverse-iter (cdr items)
        (cons (car items)))))
          
(define (reverse items)
    (reverse-iter items nil))

;: map function
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;: filter function
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;: accumulate function
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;: