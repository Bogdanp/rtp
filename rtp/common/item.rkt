#lang racket/base

(require racket/contract
         racket/serialize
         racket/string)

(provide
 entry/c

 make-item
 item?
 item-name
 item-entries
 item-created-at
 item-modified-at)

(serializable-struct item (name entries created-at modified-at)
  #:transparent)

(define entry/c
  (cons/c non-empty-string? (or/c bytes? string?)))

(define/contract (make-item name [entries null])
  (->* (non-empty-string?) ((listof entry/c)) item?)
  (item name entries (current-seconds) (current-seconds)))
