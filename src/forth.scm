(define getp
  (lambda (p l)
    (let loop ((a l))
      (if (null? a)
          #f
          (if (eq? p (car a))
              (cadr a)
              (loop (cddr a)))))))

(define +memory-size+ 1024)

(define +memory+
  (make-vector +memory-size+))

(define +here+ 0)

(define +latest+ (list))

(define +p-stack-size+ 64)

(define +p-stack+
  (make-vector +p-stack-size+))

(define +p-sp+ -1) 

(define p-push
  (lambda (value)
    (if (= +p-sp+ (- +p-stack-size+ 1))
        (raise ':stack-overflow)
        (begin
          (set! +p-sp+ (+ +p-sp+ 1))
          (vector-set! +p-stack+ +p-sp+ value)))))

(define p-pop
  (lambda ()
    (if (> +p-sp+ -1)
     (let ((value (vector-ref +p-stack+ +p-sp+)))
       (vector-set! +p-stack+ +p-sp+ 0)
       (set! +p-sp+ (- +p-sp+ 1))
       value)
     (raise ':stack-underflow))))

(define +latest+ (list))
 
(define define-primitive
  (lambda (name code)
    (set! +latest+
          (cons (list ':name name ':address +here+) +latest+))
    (vector-set! +memory+ +here+ code)
    (set! +here+ (+ +here+ 1))))

(define-primitive
  "drop"
  (lambda ()
    (let ((a (p-pop)))
      (println a))))

(define-primitive
  "dup"
  (lambda ()
     (if (< +p-sp+ 0)
         (raise ':stack-underflow))
     (let ((a (vector-ref +p-stack+ +p-sp+)))
       (p-push a))))

(define-primitive
  "+"
  (lambda ()
     (let ((a (p-pop))
           (b (p-pop)))
       (p-push (+ a b)))))

(define-primitive
  "-"
  (lambda ()
     (let ((a (p-pop))
           (b (p-pop)))
       (p-push (- b a)))))

(define-primitive
  "="
  (lambda ()
    (let ((a (p-pop))
          (b (p-pop)))
      (if (= a b)
          (p-push -1)
          (p-push 0)))))

(define-primitive
  "bye"
  (lambda ()
    (exit)))

(define find-definition
  (lambda (name)
    (let loop ((head (car +latest+))
               (tail (cdr +latest+)))
      (if (string=? name (getp ':name head))
          head 
          (if (pair? tail)
              (loop (car tail) (cdr tail))
              '())))))

(define parse-token
  (lambda (token)
    (let ((definition (find-definition token)))
      (if (pair? definition)
          (let* ((address (getp ':address definition))
                 (code (vector-ref +memory+ address)))
            (println 'ok)
            (code))
          (let ((n (string->number token)))
            (if n
              (p-push n)
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
