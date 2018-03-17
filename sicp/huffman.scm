(load "list.scm") ;load list operations

(load "set.scm") ;load set operations (unordered)

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; EXERCISE 2.67

(define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

;: (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; EXERCISE 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (branch-correct? symbol branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))))

(define (encode-symbol symbol tree)
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
      (cond ((branch-correct? symbol lb)
              (if (leaf? lb) '(0) (cons 0 (encode-symbol symbol lb))))
            ((branch-correct? symbol rb)
              (if (leaf? rb) '(1) (cons 1 (encode-symbol symbol rb))))
            (else (error "bad symbol --ENCODE-SYMBOL" symbol)))))
  

;; EXERCISE 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;: merge leaves form a tree
(define (successive-merge leaves)
    (let ((sub-tree (make-code-tree (car leaves) (cadr leaves)))
          (rest (cddr leaves)))
        (if (null? rest)
            sub-tree
            (successive-merge (adjoin-set sub-tree rest)))))