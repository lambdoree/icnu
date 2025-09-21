;;; Aggregator module: re-export pure IC and non-pure (Scheme-semantics) rewrites

(define-module (icnu rewrite)
  #:use-module ((icnu ic-rewrite) #:prefix icr:)
  #:use-module ((icnu icnu-rewrite) #:prefix icnur:)
  #:export (rewrite-pass-AC!
            rewrite-pass-AE!
            rewrite-pass-if-fold!
            rewrite-pass-const-fold!
            rewrite-pass-wire-cleanup!
            rewrite-pass-AA-merge!
            rewrite-pass-CE-annihilation!
            rewrite-pass-inpack-direct-wire!
            resolve-literal-ep
            is-literal-node?
            get-literal-value
            *unresolved*
            *resolve-literal-limit*))

;; Re-export bindings explicitly to avoid import name collisions
(define rewrite-pass-AC!              icr:rewrite-pass-AC!)
(define rewrite-pass-AE!              icr:rewrite-pass-AE!)
(define rewrite-pass-wire-cleanup!    icr:rewrite-pass-wire-cleanup!)
(define rewrite-pass-AA-merge!        icr:rewrite-pass-AA-merge!)
(define rewrite-pass-CE-annihilation! icr:rewrite-pass-CE-annihilation!)
(define rewrite-pass-inpack-direct-wire! icr:rewrite-pass-inpack-direct-wire!)

(define resolve-literal-ep           icnur:resolve-literal-ep)
(define rewrite-pass-if-fold!        icnur:rewrite-pass-if-fold!)
(define rewrite-pass-const-fold!     icnur:rewrite-pass-const-fold!)
(define is-literal-node?             icnur:is-literal-node?)
(define get-literal-value            icnur:get-literal-value)
(define *unresolved*                 icnur:*unresolved*)
(define *resolve-literal-limit*      icnur:*resolve-literal-limit*)

