#lang racket
(require net/url
         web-server/private/util
         (file "../private/connection-manager.rkt")
         web-server/http/request-structs)

(provide (all-defined-out))

(define-struct simple-request (method uri headers))

(define (ext:read-request conn)
  (with-handlers ([exn:fail? (lambda (exn)
                               (kill-connection! conn)
                               (raise exn))])
    (read-request conn)))

;; **************************************************
;; read-request: connection -> request
;; read the request line, and the headers
(define (read-request conn)
  (define ip 
    (connection-i-port conn))
  (define-values (method uri major minor)
    (read-request-line ip))
  (define headers 
    (read-headers ip))
  (make-simple-request method uri headers))


;; **************************************************
;; read-request-line  
(define match-method
  (let ([rx (byte-regexp #"^([^ ]+) (.+) HTTP/([0-9]+)\\.([0-9]+)$")])
    (lambda (a) (regexp-match rx a))))

; read-request-line : iport -> bytes url number number
; to read in the first line of an http request, AKA the "request line"
; effect: in case of errors, complain [MF: where] and close the ports
(define (read-request-line ip)
  (define line (read-bytes-line ip 'any))
  (if (eof-object? line)
      (network-error 'read-request "http input closed abruptly")
      (cond
        [(match-method line)
         => (match-lambda
              [(list _ method url major minor)
               (values method
                       (string->url (bytes->string/utf-8 url))
                       (string->number (bytes->string/utf-8 major))
                       (string->number (bytes->string/utf-8 minor)))])]
        [else (network-error 'read-request "malformed request ~a" line)])))

;; **************************************************
;; read-headers  
(define match-colon
  (let ([rx (byte-regexp (bytes-append #"^([^:]*):[ " (bytes 9) #"]*(.*)"))])
    (lambda (a) (regexp-match rx a))))

; read-headers : iport -> (listof header?)
(define (read-headers in)
  (let read-header ()
    (define l (read-bytes-line in 'any))
    (cond
      [(eof-object? l) null]
      [(zero? (bytes-length l)) null]
      [(match-colon l) 
       => (match-lambda
            [(list _ field value)
             (list* (make-header field (read-one-head in value))
                    (read-header))])]
      [else (network-error 'read-headers "malformed header")])))

; read-one-head : iport bytes -> bytes
(define (read-one-head in rhs)
  (match (peek-byte in)
    [(or 32 9) ;(or (eq? c #\space) (eq? c #\tab))
     ; (read-bytes-line in 'any) can't return eof
     ; because we just checked with peek-char
     ; Spidey: FLOW
     (read-one-head in (bytes-append rhs (read-bytes-line in 'any)))]
    [_ rhs]))