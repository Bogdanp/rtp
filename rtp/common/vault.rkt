#lang racket/base

(require crypto
         crypto/libcrypto
         racket/contract
         racket/fasl
         racket/serialize
         racket/string
         "item.rkt")

(provide
 make-vault
 vault?
 vault-name
 vault-encrypt!
 vault-decrypt
 write-vault
 read-vault)

(serializable-struct vault
  (name kdf-spec kdf-params [salt #:mutable] cipher-spec [iv #:mutable] [data #:mutable]))

(define DEFAULT-KDF-SPEC '(pbkdf2 hmac sha256))
(define DEFAULT-KDF-PARAMS '((iterations 200000)))
(define DEFAULT-CIPHER-SPEC '(aes gcm))

(define (generate-salt)
  (crypto-random-bytes 32))

(define/contract (make-vault name
                             [kdf-spec DEFAULT-KDF-SPEC]
                             [kdf-params DEFAULT-KDF-PARAMS]
                             [cipher-spec DEFAULT-CIPHER-SPEC])
  (->* (non-empty-string?) (kdf-spec? cipher-spec?) vault?)
  (vault name kdf-spec kdf-params #f cipher-spec #f #""))

(define/contract (write-vault v [out (current-output-port)])
  (->* (vault?) (output-port?) void?)
  (s-exp->fasl (serialize v) out))

(define/contract (read-vault [in (current-input-port)])
  (->* () (input-port?) vault?)
  (deserialize (fasl->s-exp in)))

(define/contract (vault-encrypt! vault passphrase items)
  (-> vault? non-empty-string? (listof item?) void?)
  (parameterize ([crypto-factories (list libcrypto-factory)])
    (define cipher (get-cipher (vault-cipher-spec vault)))
    (define salt (generate-salt))
    (define key (kdf (vault-kdf-spec vault)
                     (string->bytes/utf-8 passphrase)
                     salt
                     (vault-kdf-params vault)))
    (define iv (generate-cipher-iv cipher))
    (define data (s-exp->fasl (serialize items)))
    (set-vault-salt! vault salt)
    (set-vault-iv! vault iv)
    (set-vault-data! vault (encrypt cipher key iv data))))

(define/contract (vault-decrypt vault passphrase)
  (-> vault? non-empty-string? (listof item?))
  (define data
    (parameterize ([crypto-factories (list libcrypto-factory)])
      (define cipher (get-cipher (vault-cipher-spec vault)))
      (define key (kdf (vault-kdf-spec vault)
                       (string->bytes/utf-8 passphrase)
                       (vault-salt vault)
                       (vault-kdf-params vault)))
      (decrypt cipher key (vault-iv vault) (vault-data vault))))

  (deserialize (fasl->s-exp data)))
