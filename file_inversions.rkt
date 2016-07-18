#lang racket

(define (msort lst)
  ;; called by the main helper function
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

  ;; now to recursively split and merge
  (define (main-help lst len)
    (if (< len 2)
        lst
        (let* ([right-len (if (even? len)
                              (/ len 2)
                              (/ (sub1 len) 2))]
               [left-len (- len right-len)]
               [right (take-right lst right-len)]
               [left (drop-right lst right-len)])
          (merge (main-help left left-len)
                 (main-help right right-len)))))
  ;; returns the sorted list
  (main-help lst (length lst)))

;; read in integer list like:
;;
;; (define nums
;;   (map (lambda (x) (string->number x))
;;        (call-with-input-file numfile
;;          (lambda (in) (port->lines in)))))

(define (count-inversions lst)
  ;; called by the main helper function
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

  ;; now to recursively split and merge
  (define (main-help lst len)
    (if (< len 2)
        lst
        (let* ([right-len (if (even? len)
                              (/ len 2)
                              (/ (sub1 len) 2))]
               [left-len (- len right-len)]
               [right (take-right lst right-len)]
               [left (drop-right lst right-len)])
          (merge (main-help left left-len)
                 (main-help right right-len)))))
  ;; returns the sorted list
  (main-help lst (length lst)))

(module+ main
  (let* ([args (current-command-line-arguments)]
         [numfile (vector-ref args 0)]
         [nums (map (lambda (x) (string->number x))
                    (call-with-input-file numfile
                      (lambda (in) (port->lines in))))])
    (msort nums)))
