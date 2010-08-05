#lang racket/base
(require racket/contract)

(define-struct connection (id i-port o-port custodian)
  #:mutable)

(provide/contract
 [struct connection
         ([id integer?]
          [i-port input-port?]
          [o-port output-port?]
          [custodian custodian?])]
 [new-connection (input-port? output-port? custodian? . -> . connection?)]
 [kill-connection! (connection? . -> . void)])

;; new-connection : number input-port output-port custodian -> connection
;;
;; Ask the connection manager for a new connection.
(define i (box 0))
(define (new-connection i-port o-port cust)
  (make-connection
     (begin0 (unbox i) (set-box! i (add1 (unbox i))))
     i-port o-port cust))

;; kill-connection!: connection -> void

;; Closes all the ports associated with a given connection and
;; shutdowns all resources associated with it.
(define (kill-connection! conn)
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-o-port conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-i-port conn)))
  (custodian-shutdown-all (connection-custodian conn)))
