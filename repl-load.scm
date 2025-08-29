(define-module (repl-load)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu eval)  
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (icnu tools icnu-inject)
  #:use-module (icnu tools icnu-validate)
  #:use-module (icnu tools icnu-proof)
  #:use-module (icnu utils assertions)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu utils strings)
  #:re-export (
	       mk-node mk-wire mk-par mk-nu
		       parse-net pretty-print
		       empty-net copy-net make-fresh-name all-names node-agent endpoint valid-port?
		       peer net-nodes net-links net-nu-set get-ports unlink-port!
		       rewire! delete-node! all-nodes-with-agent find-active-pairs
		       set-link-conflict-mode!
		       *link-conflict-mode*
		       mark-nu!
		       link-peers!
		       add-node!
		       <net>
		       net?

		       rewrite-pass-if-fold!
		       rewrite-pass-const-fold!
		       rewrite-pass-wire-cleanup!
		       rewrite-pass-AA-merge!
		       rewrite-pass-AC!
		       rewrite-pass-AE!
		       rewrite-pass-CE-annihilation!
		       resolve-literal-ep
		       is-literal-node?
		       get-literal-value
		       *unresolved*

		       IC_TRUE IC_FALSE IC_IF IC_Y
		       JOIN-REPLACE JOIN-MAX
		       IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		       IC_AND IC_OR IC_NOT IC_COPY
		       IC_PRIM_ADD IC_PRIM_ADD_RUNTIME IC_APPLY
		       IC_MK_TRUE IC_MK_FALSE
		       IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD

		       generate-injection-form

		       validate-ir

		       assert-eq assert-true assert-false

		       format-string

		       debug-level? set-debug-level! set-debug-log! debugf warnf debugf-limited debug-once

		       string-contains? string-join-list string-split-char

		       small-step-string big-step-string small-step-net big-step-net small-step-sequence-net

		       eval-icnu-string eval-net reduce-net-to-normal-form *default-reduction-passes* read-sexpr-from-string
		       ))
