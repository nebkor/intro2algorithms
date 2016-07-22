#lang racket

(define (count-inversions lst)

  (define invs 0)

  ;; called by the main helper function
  (define (rmerge left right)
    (cond
     [(null? right) left]
     [(null? left) right]
     [else
      (let ([cl (car left)]
            [cr (car right)])
        (if (> cl cr)
            (begin
              (set! invs (+ invs (length left)))
              (cons cr (rmerge left (cdr right))))
            (cons cl (rmerge (cdr left) right))))]))

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
          (rmerge (main-help left left-len)
                  (main-help right right-len)))))

  ;; function entry point:
  (begin
    (main-help lst (length lst))
    invs))

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
         [invs (count-inversions nums)])
    (displayln (~a "Inversions in " numfile ": " invs))))
