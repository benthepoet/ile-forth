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

(define +memory-cursor+ 0)

(define +memory-iptr+ 0)

(define +latest+ (list))

(define +param-stack+ (list))

(define param-push
  (lambda (x)
    (set! +param-stack+ (cons x +param-stack+))))

(define param-pop
  (lambda ()
    (if (pair? +param-stack+)
        (let ((head (car +param-stack+))
              (tail (cdr +param-stack+)))
          (set! +param-stack+ tail)
          head)
        (raise 'stack-underflow))))

(define +return-stack+ (list))

(define return-push
  (lambda (x)
    (set! +return-stack+ (cons x +return-stack+))))

(define return-pop
  (lambda ()
    (if (pair? +return-stack+)
        (let ((head (car +return-stack+))
              (tail (cdr +return-stack+)))
          (set! +return-stack+ tail)
          head)
        (raise 'stack-underflow))))

(define define-primitive
  (lambda (name code)
    (set! +latest+
          (cons (list ':name name ':address +memory-cursor+) +latest+))
    (vector-set! +memory+ +memory-cursor+ code)
    (set! +memory-cursor+ (+ +memory-cursor+ 1))))

(define-primitive
  "drop"
  (lambda ()
    (let ((a (param-pop)))
      (println a))))

(define-primitive
  "dup"
  (lambda ()
     (if (not (pair? +param-stack+))
         (raise ':stack-underflow))
     (let ((head (car +param-stack+)))
       (param-push head))))

(define-primitive
  "+"
  (lambda ()
     (let ((a (param-pop))
           (b (param-pop)))
       (param-push (+ a b)))))

(define-primitive
  "-"
  (lambda ()
     (let ((a (p-pop))
           (b (p-pop)))
       (p-push (- b a)))))

(define-primitive
  "="
  (lambda ()
    (let ((a (param-pop))
          (b (param-pop)))
      (if (= a b)
          (param-push -1)
          (param-push 0)))))

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
              (param-push n)
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
