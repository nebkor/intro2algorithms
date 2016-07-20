#lang racket

(define (msort lst)

  (define invs 0)

  ;; called by the main helper function
  (define (merge left right)
    (define (help left right m cnt)
      (let ([mid (length right)]
            [end-index (length m)])
        (cond
         [(and (null? left) (null? right)) (begin (set! invs (+ invs cnt))
                                                  (reverse m))]
         [(null? left) (begin (set! invs (+ invs cnt))
                              (append (reverse m) right))]
         [(null? right) (begin (set! invs (+ invs cnt (length m)))
                               (append (reverse m) left))]
         [else
          (let ([cl (car left)]
                [cr (car right)])
            (if (> cl cr)
                (help left (cdr right) (cons cr m) (+ (sub1 (length left)) cnt))
                (help (cdr left) right (cons cl m) cnt)))])))
    (help left right '() 0))

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
  (let ([res (main-help lst (length lst))])
    (cons res invs)))

(define (cmerge l1 l2)
    (define (help l1 l2 m cnt)
      (cond
       [(and (null? l1) (null? l2)) (values (reverse m) cnt)]
       [(null? l1) (values (append (reverse m) l2) (+ cnt (length l2)))]
       [(null? l2) (values (append (reverse m) l1) (+ cnt (length l1)))]
       [else
        (let ([c1 (car l1)]
              [c2 (car l2)])
          (if (> c1 c2)
              (help l1 (cdr l2) (cons c2 m) (add1 cnt))
              (help (cdr l1) l2 (cons c1 m) cnt)))]))
    (help l1 l2 '() 0))

;; read in integer list like:
;;
;; (define nums
;;   (map (lambda (x) (string->number x))
;;        (call-with-input-file numfile
;;          (lambda (in) (port->lines in)))))



(module+ main
  (let* ([args (current-command-line-arguments)]
         [numfile (vector-ref args 0)]
         [nums (map (lambda (x) (string->number x))
                    (call-with-input-file numfile
                      (lambda (in) (port->lines in))))]
         [snums (msort nums)]
         [count (cdr snums)]
         [tail (take-right (car snums) 10)])
    (displayln (~a "Last ten items: " tail))
    (displayln (~a "inversions: " count))))
