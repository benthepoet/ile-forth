(define getp
  (lambda (p l)
    (let loop ((a l))
      (if (null? a)
          #f
          (if (eq? p (car a))
              (cadr a)
              (loop (cddr a)))))))

(define data-stack-size 16)

(define data-stack
  (make-vector data-stack-size))

(define data-sp -1) 

(define push-data
  (lambda (value)
    (if (= data-sp (- data-stack-size 1))
        (raise ':stack-overflow)
        (begin
          (set! data-sp (+ data-sp 1))
          (vector-set! data-stack data-sp value)))))

(define pop-data
  (lambda ()
    (if (> data-sp -1)
     (let ((value (vector-ref data-stack data-sp)))
       (vector-set! data-stack data-sp 0)
       (set! data-sp (- data-sp 1))
       value)
     (raise ':stack-underflow))))

(define word-dict (list))
 
(define add-word
  (lambda (name code)
    (set! word-dict
          (cons (list ':name name ':code code) word-dict))))

(add-word "drop"
          (lambda ()
            (let ((a (pop-data)))
              (println a))))

(add-word "dup"
          (lambda ()
            (if (< data-sp 0)
                (raise ':stack-underflow))
            (let ((a (vector-ref data-stack data-sp)))
              (push-data a))))

(add-word "+"
          (lambda ()
            (let ((a (pop-data))
                  (b (pop-data)))
              (push-data (+ a b)))))

(add-word "-"
          (lambda ()
            (let ((a (pop-data))
                  (b (pop-data)))
              (push-data (- a b)))))

(add-word "="
          (lambda ()
            (let ((a (pop-data))
                  (b (pop-data)))
              (if (= a b)
                  (push-data -1)
                  (push-data 0)))))

(add-word "bye"
          (lambda ()
            (exit)))

(define call-word
  (lambda (name)
    (let loop ((head (car word-dict))
               (tail (cdr word-dict)))
      (if (string=? name (getp ':name head))
          (let ((code (getp ':code head)))
            (code)
            'ok)
          (if (pair? tail)
              (loop (car tail) (cdr tail))
              '?)))))
       

(define parse-token
  (lambda ()
    #f))

(define execute
  (lambda (input)
    (call-word input)))

(let loop ((input (read-line (current-input-port))))
  (with-exception-catcher
    (lambda (e) (println e))
    (lambda ()
      (println (execute input))))
  (loop (read-line (current-input-port))))
