(define getp
  (lambda (p l)
    (let loop ((a l))
      (if (null? a)
          #f
          (if (eq? p (car a))
              (cadr a)
              (loop (cddr a)))))))

(define p-stack-size 16)

(define p-stack
  (make-vector p-stack-size))

(define p-sp -1) 

(define push-p
  (lambda (value)
    (if (= p-sp (- p-stack-size 1))
        (raise ':stack-overflow)
        (begin
          (set! p-sp (+ p-sp 1))
          (vector-set! p-stack p-sp value)))))

(define pop-p
  (lambda ()
    (if (> p-sp -1)
     (let ((value (vector-ref p-stack p-sp)))
       (vector-set! p-stack p-sp 0)
       (set! p-sp (- p-sp 1))
       value)
     (raise ':stack-underflow))))

(define word-dict (list))
 
(define add-word
  (lambda (name code)
    (set! word-dict
          (cons (list ':name name ':code code) word-dict))))

(add-word "drop"
          (lambda ()
            (let ((a (p-data)))
              (println a))))

(add-word "dup"
          (lambda ()
            (if (< p-sp 0)
                (raise ':stack-underflow))
            (let ((a (vector-ref p-stack p-sp)))
              (push-p a))))

(add-word "+"
          (lambda ()
            (let ((a (pop-p))
                  (b (pop-p)))
              (push-p (+ a b)))))

(add-word "-"
          (lambda ()
            (let ((a (pop-p))
                  (b (pop-p)))
              (push-p (- a b)))))

(add-word "="
          (lambda ()
            (let ((a (pop-p))
                  (b (pop-p)))
              (if (= a b)
                  (push-p -1)
                  (push-p 0)))))

(add-word "bye"
          (lambda ()
            (exit)))

(define find-word
  (lambda (name)
    (let loop ((head (car word-dict))
               (tail (cdr word-dict)))
      (if (string=? name (getp ':name head))
          head 
          (if (pair? tail)
              (loop (car tail) (cdr tail))
              '())))))

(define parse-token
  (lambda (token)
    (let ((word (find-word token)))
      (if (pair? word)
          (let ((code (getp ':code word)))
            (println 'ok)
            (code))
          (println '?)))))          

(define parse-input
  (lambda (input)
    (let loop ((i 0)
               (k -1)
               (l (string-length input)))
      (if (< i l)
          (if (and (= k -1)
                   ()))
              
          (let ((c (string-ref input c)))
            (if (char-whitespace? c)
              (println (substring input k i)))    
            (loop (+ i 1) l))))
    (print input)))

(let loop ((input (read-line (current-input-port))))
  (with-exception-catcher
    (lambda (e) (println e))
    (lambda ()
      (parse-token input)))
  (loop (read-line (current-input-port))))
