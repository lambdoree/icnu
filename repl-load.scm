(define-module (repl-load)
  #:use-module (icnu icnu)
  #:use-module (icnu rewrite)
  #:use-module (icnu eval)  
  #:use-module (icnu stdlib icnu-lib)
  #:use-module (icnu tools icnu-inject)
  #:use-module (icnu tools icnu-validate)
  #:use-module (icnu utils assertions)
  #:use-module (icnu utils format)
  #:use-module (icnu utils log)
  #:use-module (icnu utils strings)
  #:re-export (
	       ;; From (icnu icnu)
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

		       ;; From (icnu rewrite)
		       rewrite-pass-copy-fold!
		       rewrite-pass-if-fold!
		       rewrite-pass-const-fold!
		       rewrite-pass-wire-cleanup!
		       rewrite-pass-AA-merge!
		       resolve-literal-ep
		       is-literal-node?
		       get-literal-value
		       *unresolved*

		       ;; From (icnu stdlib icnu-lib)
		       IC_TRUE IC_FALSE IC_IF IC_Y
		       IC_CHURCH-ENCODE
		       JOIN-REPLACE JOIN-MAX
		       IC_LITERAL IC_EQ_CONST IC_LT_CONST IC_GT_CONST
		       IC_AND IC_OR IC_NOT IC_COPY
		       IC_PRIM_ADD IC_PRIM_ADD_RUNTIME IC_APPLY
		       IC_MK_TRUE IC_MK_FALSE
		       IC_CHURCH-RUN IC_CHURCH-APPLY IC_CONS IC_NIL IC_FIRST IC_REST IC_FOLD

		       ;; From (icnu tools icnu-inject)
		       generate-injection-form

		       ;; From (icnu tools icnu-validate)
		       validate-ir

		       ;; From (icnu utils assertions)
		       assert-eq assert-true assert-false

		       ;; From (icnu utils format)
		       format-string

		       ;; From (icnu utils log)
		       debug-level? set-debug-level! set-debug-log! debugf warnf debugf-limited debug-once

		       ;; From (icnu utils strings)
		       string-contains? string-join-list string-split-char

		       eval-icnu-string eval-net reduce-net-to-normal-form *default-reduction-passes*
		       ))
