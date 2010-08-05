#lang racket

(require (file "http/request.rkt")
         (file "private/connection-manager.rkt")
         web-server/http/request-structs)
(require (planet soegaard/digest:1:2/digest))

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (let ([cust (make-custodian)])
    (custodian-limit-memory cust (* 50 1024 1024))
    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))
      (define conn (new-connection in out cust))
      (thread (lambda ()
                (handle conn)
                (kill-connection! conn))))))

(define (write-handshake origin key1 key2 key3 out)
  (define challenge-solution (solve-challenge key1 key2 key3))
  
  (display "HTTP/1.1 101 WebSocket Protocol Handshake\r\n" out)
  (display "Upgrade: WebSocket\r\n" out)
  (display "Connection: Upgrade\r\n" out)
  (display (format "Sec-WebSocket-Origin: ~a\r\n" origin) out)
  (display "Sec-WebSocket-Location: ws://localhost:9999/\r\n" out)
  (display "\r\n" out)
  (display challenge-solution out)
  (flush-output out))  

(define (handle conn)
  (define out (connection-o-port conn))
  (define in (connection-i-port conn))
  (define req (read-request conn))
  (define headers (simple-request-headers req))
  (define origin (header-value (headers-assq* #"Origin" headers)))
  (define key1 (header-value (headers-assq* #"Sec-WebSocket-Key1" headers)))
  (define key2 (header-value (headers-assq* #"Sec-WebSocket-Key2" headers)))
  (define key3 (read-bytes 8 in))
  
  (write-handshake origin key1 key2 key3 out)

  (write-message "welcome" out)
  
  (let loop ()
    (cond [(regexp-match #rx#"\0([^\377]*)\377" in)
           => (match-lambda [(list _ data)
                             (write-message (bytes->string/utf-8 data) out)])])
    (loop)))

(define (write-message msg out)
  (write-bytes #"\0" out)
  (write-bytes (string->bytes/utf-8 msg) out)
  (write-bytes #"\377" out)
  (flush-output out))

(define (solve-challenge key1 key2 key3)
  (define num1 (/ (extract-nums key1)
                  (count-spaces key1)))
  (define num2 (/ (extract-nums key2)
                  (count-spaces key2)))
  
  (md5-bytes (bytes-append (integer->integer-bytes num1 4 #f #t)
                           (integer->integer-bytes num2 4 #f #t)
                           key3)))

(define (extract-nums key)
  (string->number 
   (bytes->string/utf-8 
    (apply bytes-append 
           (regexp-match* #"[0-9]" key)))))

(define (count-spaces key)
  (length (regexp-match* #" " key)))
