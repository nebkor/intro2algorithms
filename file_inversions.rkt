#lang racket

(define (count-inversions lst)
  (define (cnt l len)
    (cond
     [(= len 0) 0]
     [(= len 1) 0]
     [(= len 2) (if (< (car l) (cadr l))
                    0
                    1)]
     [else
      (if (even? len)
          (let* ([half-len (/ len 2)]
                 [right (take-right l half-len)]
                 [left (drop-right l half-len)])
            (+ (cnt left half-len)
               (cnt right half-len)))
          (+ (cnt (take-right l (sub1 len)) (sub1 len))
             (cnt (drop-right l 1) (sub1 len))))]))
  (cnt lst (length lst)))

(define (merge l1 l2)
  (define (help l1 l2 m)
    (cond
     [(and (null? l1) (null? l2)) m]
     [(null? l1) (append m l2)]
     [(null? l2) (append m l1)]
     [else
      (let ([c1 (car l1)]
            [c2 (car l2)])
        (if (> c1 c2)
            (help l1 (cdr l2) (append m (list c2)))
            (help (cdr l1) l2 (append m (list c1)))))]))
  (help l1 l2 '()))

