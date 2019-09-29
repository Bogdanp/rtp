#lang at-exp racket/base

(require racket/cmdline
         racket/file
         racket/format
         racket/match
         raco/command-name
         "common/item.rkt"
         "common/vault.rkt")

(define VAULTS-PATH
  (expand-user-path "~/.config/rtp/vaults"))

(define current-program-name
  (make-parameter (short-program+command-name)))

(define current-vault
  (make-parameter #f))

(define (prompt [message ""])
  (display message)
  (read-line))

(define (find-vaults)
  (for/list ([filename (in-list (directory-list VAULTS-PATH))])
    (call-with-input-file (build-path VAULTS-PATH filename)
      read-vault)))

(define (find-vault name)
  (for/first ([vault (in-list (find-vaults))]
              #:when (string=? (vault-name vault) name))
    vault))

(define (exit-with-errors! . messages)
  (parameterize ([current-output-port (current-error-port)])
    (for ([message messages])
      (displayln message)))
  (exit 1))

(define (handle-help)
  (exit-with-errors!
   "usage: raco rtp <command> <option> ... <arg> ..."
   ""
   "available commands:"
   "  create   create a new vault"
   "  insert   insert a new item into a vault"
   "  items    list all the items in the current vault"
   "  help     print this message and exit"
   "  vaults   list all available vaults"))

(define (handle-create)
  (define name (prompt "Name: "))
  (define passphrase (prompt "Passphrase: "))
  (define vault (make-vault name))
  (vault-encrypt! vault passphrase '())
  (call-with-output-file (build-path VAULTS-PATH "default.rtpvault")
    #:exists 'truncate/replace
    (lambda (out)
      (write-vault vault out))))

(define (handle-insert)
  (current-vault (find-vault "default"))
  (command-line
   #:program (current-program-name)
   #:once-each
   [("-v" "--vault")
    name
    "the name of the vault into which the password should be inserted"
    (current-vault (find-vault name))]
   #:args (name)
   (define vault (current-vault))
   (define passphrase (prompt "Passphrase for vault: "))
   (define item-passphrase (prompt @~a{Passphrase for '@name': }))
   (define items (cons (make-item name `(("password" . ,item-passphrase)))
                       (vault-decrypt vault passphrase)))
   (vault-encrypt! vault passphrase items)
   (call-with-output-file (build-path VAULTS-PATH "default.rtpvault")
     #:exists 'truncate/replace
     (lambda (out)
       (write-vault vault out)))))

(define (handle-items)
  (current-vault (find-vault "default"))
  (command-line
   #:program (current-program-name)
   #:once-each
   [("-v" "--vault")
    name
    "the name of the vault to use"
    (current-vault (find-vault name))]
   #:args ()
   (define vault (current-vault))
   (define passphrase (prompt "Passphrase for vault: "))
   (for ([item (in-list (vault-decrypt vault passphrase))])
     (displayln (item-name item)))))

(define (handle-vaults)
  (for ([vault (in-list (find-vaults))])
    (displayln (vault-name vault))))

(define ((handle-unknown command))
  (exit-with-errors! @~a{error: unrecognized command '@command'}))

(define all-commands
  (hasheq 'create handle-create
          'insert handle-insert
          'items  handle-items
          'help   handle-help
          'vaults handle-vaults))

(define-values (command handler args)
  (match (current-command-line-arguments)
    [(vector command args ...)
     (values command (hash-ref all-commands (string->symbol command) (handle-unknown command)) args)]

    [_
     (values "help" handle-help null)]))

(module+ main
  (make-directory* VAULTS-PATH)
  (parameterize ([current-command-line-arguments (list->vector args)]
                 [current-program-name (~a (current-program-name) " " command)])
    (handler)))
