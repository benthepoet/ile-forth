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

(define +input-buffer+ "")
(define +input-cursor+ 0)

(define define-primitive
  (lambda (name instr)
    (set! +latest+
          (cons (list ':name name ':address +memory-cursor+) +latest+))
    (vector-set! +memory+ +memory-cursor+ instr)
    (set! +memory-cursor+ (+ +memory-cursor+ 1))))

(define-primitive
  "quit"
  (lambda ()
    (set! +return-stack+ (list))))

(define-primitive
  "interpret"
  (lambda ()
    (set! +memory-iptr+ 1)
    (read-word)))

(define-primitive
  "drop"
  (lambda ()
    (param-pop)))    

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
     (let ((a (param-pop))
           (b (param-pop)))
       (param-push (- b a)))))

(define-primitive
  "="
  (lambda ()
    (let ((a (param-pop))
          (b (param-pop)))
      (if (= a b)
          (param-push -1)
          (param-push 0)))))

(define-primitive
  "."
  (lambda ()
    (println (param-pop))))

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
              (list))))))

(define process-word
  (lambda (word)
    (let ((definition (find-definition word)))
      (if (pair? definition)
          (let* ((address (getp ':address definition))
                 (instr (vector-ref +memory+ address)))
            (instr))
          (let ((n (string->number word)))
            (if n
                (param-push n)
                (raise '?)))))))          

(define read-word
  (lambda ()
    (let ((word "")
          (length (string-length +input-buffer+)))
      (let loop ((i +input-cursor+))
          (if (or (= i length)
                  (char-whitespace? (string-ref +input-buffer+ i)))
              (begin
                (if (> (- i +input-cursor+) 0)
                    (begin
                      (set! word (substring +input-buffer+ +input-cursor+ i))
                      (process-word word)
                      (set! +input-cursor+ i)))
                (set! +input-cursor+ (+ +input-cursor+ 1))))
          (if (and (< i length)
                   (string=? word ""))
              (loop (+ i 1)))))))

(let input-loop ((input (read-line (current-input-port))))
  (with-exception-catcher
   (lambda (e)
    (println e))
   (lambda ()
     (set! +input-buffer+ input)
     (set! +input-cursor+ 0)
     (set! +memory-iptr+ 0)
     
     (if (= 0 (string-length +input-buffer+))
        (raise '?))
    
     (let next ()
        (let ((instr (vector-ref +memory+ +memory-iptr+)))
         (set! +memory-iptr+ (+ +memory-iptr+ 1))
         (instr))
        (if (< +input-cursor+ (string-length +input-buffer+))
            (next)
            (println 'ok)))))
  
  (input-loop (read-line (current-input-port))))
