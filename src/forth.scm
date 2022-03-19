(define data-stack
  (make-vector 16))

(define data-sp -1)

(define push-data
  (lambda (value)
    (set! data-sp (+ data-sp 1))
    (vector-set! data-stack data-sp value)))

(define pop-data
  (lambda ()
    (if (> data-sp -1)
     (let ((value (vector-ref data-stack data-sp)))
       (set! data-sp (- data-sp 1))
       value)
     data-sp)))

(define parse-token
  (lambda ()
    #f))

(define execute
  (lambda (input)
    (print input)))

