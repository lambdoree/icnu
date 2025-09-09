(define-module (icnu literals)
  #:use-module ((icnu ic) #:prefix ic:)
  #:use-module (icnu utils internal)
  #:use-module (ice-9 match)
  #:export (ic-literal? ic-literal-value ic-make-literal-node!))

;;; Literal helpers live here. This module is intentionally a thin layer on top of
;;; the pure IC core (imported with prefix ic:) so that IC stays focused on core
;;; IR manipulation and icnu provides higher-level concerns.

(define (ic-literal? net name)
  "Return #t if NAME has a literal tag in NET."
  (let ((tag (ic:node-tag net name)))
    (memq tag '(lit/bool lit/num lit/str))))

(define (ic-literal-value net name)
  "Return the value represented by literal node NAME in NET.
If NAME is not a literal node, return NAME (the symbol) as a fallback."
  (if (ic-literal? net name)
      (let ((meta (ic:node-meta net name)))
        (if (eq? meta #f)
            name
            (let* ((v-pair (and (pair? meta) (list? meta) (assq 'value meta)))
                   (val (cond
                          (v-pair (cdr v-pair))
                          ((and (pair? meta) (eq? (car meta) 'value)) (cdr meta))
                          ((list? meta)
                           (let ((vp (assq 'value meta)))
                             (if vp (cdr vp) meta)))
                          (else meta))))
              (cond
               ((and (pair? val) (eq? (car val) 'quote)) (cadr val))
               ((list? val) name)
               (else val)))))
      name))

(define (ic-make-literal-node! net name lit-tag lit-val)
  "Create a literal node NAME in NET with tag LIT-TAG and metadata LIT-VAL.
This forwards to the core add/set functions so the operation remains a thin wrapper."
  (ic:add-node! net name 'A)
  (ic:set-node-tag! net name lit-tag)
  (ic:set-node-meta! net name lit-val)
  net)
