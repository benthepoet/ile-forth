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
            (let ((a (pop-p)))
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
              (push-p (- b a)))))

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
          (let ((n (string->number token)))
            (if n
              (push-p n)
              (println '?)))))))          

(define parse-input
  (lambda (input)
    (let ((cursor 0)
          (token (make-string 32))
          (length (string-length input)))
      (let loop ((i 0))
          (if (or (= i length)
                  (char-whitespace? (string-ref input i)))
              (begin
                (if (> (- i cursor) 0)
                    (begin
                      (parse-token
                       (substring input cursor i))
                      (set! cursor i)))
                (set! cursor (+ cursor 1))))
        (if (< i length)
            (loop (+ i 1)))))))

(let loop ((input (read-line (current-input-port))))
  (with-exception-catcher
   (lambda (e)
    (println e))
   (lambda ()
     (parse-input input)))
  (loop (read-line (current-input-port))))
