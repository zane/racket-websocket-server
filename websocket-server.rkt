#lang racket
;;; websocket-server.rkt
;;; 
;;; Implements a WebSocket server as described in version 76 of the
;;; web socket protocol as outlined by the document at the following
;;; URL:
;;; 
;;; http://www.whatwg.org/specs/web-socket-protocol/
(require (file "http/request.rkt")
         (file "private/connection-manager.rkt")
         (planet soegaard/digest:1:2/digest)
         web-server/http/request-structs)

;; message-handler : string output-port -> any
;;
;; Function used to handle messages from the client. Defaults to echo
;; all messages back to the client.
(define message-handler (make-parameter write-message))

;; serve : number -> (-> any)
;;
;; Starts the server which will begin listening for connections on the
;; provided port. Evaluates to a function which can in turn be
;; evaluated to stop the server.
(define (serve #:port [port 80]
               #:message-handler [handler (message-handler)])
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust]
                 [message-handler handler])
    (define listener (tcp-listen port 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

;; accept-and-handle : tcp-listener -> any
;;
;; Accepts a tcp-listener managing a connection, creates a custodian
;; for resources used by that connection and calls a handler for that
;; connection.
(define (accept-and-handle listener)
  (let ([cust (make-custodian)])
    (custodian-limit-memory cust (* 50 1024 1024)) ; 50MB
    (parameterize ([current-custodian cust])
      (let*-values ([(in out) (tcp-accept listener)]
                    [(host client) (tcp-addresses in)])
        (log-info (format "C: ~a" client))
        (let ([conn (new-connection in out cust)])
          (thread (lambda ()
                    (handle conn)
                    (log-info (format "D: ~a" client))
                    (kill-connection! conn))))))))

;; write-handshake : byte-string byte-string byte-string byte-string output-port -> any
;;
;; Given the three keys specified in the WebSocket protocol and a
;; given output port a valid WebSocket handshake and challenge
;; solution will be written to the provided output port.
(define (write-handshake origin key1 key2 key3 out)
  (fprintf out "HTTP/1.1 101 WebSocket Protocol Handshake\r\n")
  (write-headers (list (make-header #"Upgrade" #"WebSocket")
                       (make-header #"Connection" #"Upgrade")
                       (make-header #"Sec-WebSocket-Origin" origin)
                       (make-header #"Sec-WebSocket-Location" #"ws://localhost:9999/"))
                 out)
  (let ([challenge-solution (solve-challenge key1 key2 key3)])
    (display challenge-solution out))
  (flush-output out))

;; write-headers : (Listof header) -> any
;;
;; Writes a given list of headers to the provided output port.
(define (write-headers headers out)
  (for ([header headers])
     (fprintf out
              "~a: ~a\r\n"
              (header-field header)
              (header-value header)))
  (fprintf out "\r\n"))

;; handle : connection -> any
;;
;; Handles a given connection by writing the server hanshake and then
;; initiating handling of any received messages from the client. Note
;; that the default message handler echoes all messages back to the
;; client.
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

  ;; connection is now open
  (let loop ([message (read-message in)])
    (unless (string=? message "")
      ((message-handler) message out)
      (loop (read-message in)))))

;; read-message : input-port -> string
;;
;; Reads a message from the given WebSocket client.
(define (read-message in)
  (cond [(regexp-match #rx#"\0([^\377]*)\377" in)
         => (match-lambda [(list _ data)
                           (bytes->string/utf-8 data)])]))

;; write-message : string output-port -> any
;;
;; Writes a message to the given WebSocket output port.
(define (write-message msg out)
  (write-bytes #"\0" out)
  (write-bytes (string->bytes/utf-8 msg) out)
  (write-bytes #"\377" out)
  (flush-output out))

;; solve-challenge : byte-string byte-string byte-string -> byte-string
;;
;; Produces the server challenge response when given the three keys
;; provided by the client in the client's handshake.
(define (solve-challenge key1 key2 key3)
  (define num1 (/ (extract-nums key1)
                  (count-spaces key1)))
  (define num2 (/ (extract-nums key2)
                  (count-spaces key2)))
  
  (md5-bytes (bytes-append (integer->integer-bytes num1 4 #f #t)
                           (integer->integer-bytes num2 4 #f #t)
                           key3)))

;; extract-nums : byte-string -> number
;;
;; Given a byte string containing digits interspersed with other
;; characters, evaluates to the number represented by those digits
;; concatenated together. Used by 'solve-challenge' to determine the
;; challenge response for the server handshake.
(define (extract-nums key)
  (string->number 
   (bytes->string/utf-8 
    (apply bytes-append 
           (regexp-match* #"[0-9]" key)))))

;; count-spaces : byte-string -> number
;;
;; Counts the number of space characters in a given byte string. Used
;; by 'solve-challenge' to determine challenge response for the server
;; handshake.
(define (count-spaces key)
  (length (regexp-match* #" " key)))
