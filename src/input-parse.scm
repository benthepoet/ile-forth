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
                      (println
                       (substring input cursor i))
                      (set! cursor i)))
                (set! cursor (+ cursor 1))))
        (if (< i length)
            (loop (+ i 1)))))))
              
   
             
