;   4
; 2   7
;1 3 6 8

(define make-node
  (lambda (n l r)
    (list n l r)))

(define left
  (lambda (n)
    (ref n 1)))

(define right 
  (lambda (n)
    (ref n 2)))

(define node
  (lambda (n)
    (car n)))

(define member
  (lambda (n v)
    (if (eq? n #f)
        #f
        (let ((nv (node n)))
          (cond  ((eq? nv v) #t)
                 ((< nv v) (member (right n) v))
                 (else (member (left n) v)))))))

(define n (make-node 4
                     (make-node 2
                                (make-node 1 #f #f)
                                (make-node 3 #f #f))
                     (make-node 7
                                (make-node 6 #f #f)
                                (make-node 8 #f #f))))

(display n)
(member n 5)
