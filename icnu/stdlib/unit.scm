(define-module (icnu stdlib unit)
  #:use-module (icnu stdlib ic-lib)
  #:use-module (icnu utils helpers)
  #:use-module (icnu utils format)
  #:use-module (icnu utils internal)
  #:use-module (icnu utils compat)
  #:export (IC_UNIT IC_CALL_UNIT))

;; IC_UNIT : Unit 정의 전개자
;;   name        : 심볼 — 유닛의 공개 이름(A 에이전트 노드로 생성)
;;   in-names    : (listof 심볼) — (선언적) 입력 파라미터 심볼들 (본문에서 사용)
;;   out-names   : (listof 심볼) — (선언적) 출력 파라미터 심볼들 (본문에서 사용)
;;   ret-name    : 심볼 — 내부 결과(A 에이전트)
;;   body        : S-식 — (par ...) 내부에 전개될 본체 배선(로컬 변수 포함)
;; 반환값: (par ...) S-식
(define (IC_UNIT name in-names out-names ret-name body)
  (let* ((body-forms (if (and (pair? body) (eq? (car body) 'par)) (cdr body) (if body (list body) '())))
         (collect
           (lambda (forms)
             (letrec ((walk
                       (lambda (f acc)
                         (cond
                           ((null? f) acc)
                           ((and (pair? f) (eq? (car f) 'node) (symbol? (cadr f)))
                            (walk (cdr f) (cons (cadr f) acc)))
                           ((pair? f)
                            (let ((acc2 (walk (car f) acc)))
                              (walk (cdr f) acc2)))
                           (else acc)))))
               (walk forms '()))))
         (decls (collect body-forms))
         (raw (cons ret-name decls))
         (uniq
           (lambda (lst)
             (let loop ((l lst) (acc '()))
               (if (null? l) (reverse acc)
                   (let ((x (car l)))
                     (if (memq x acc) (loop (cdr l) acc) (loop (cdr l) (cons x acc))))))))
         (internal-candidates (uniq raw))
         (internal (icnu-filter
                    (lambda (x) (and (symbol? x) (not (eq? x name))))
                    internal-candidates))
         (forms
           (list
            `(node ,name C)
            `(nu (,@internal)
                 (par
                   ,@body-forms
                   (node ,ret-name C)))
            `(wire (,ret-name l) (,name p))))) 
    forms))

(define (IC_CALL_UNIT unit-name in-pack out-pack result)
  (let ((frame (string->symbol (format-string #f "~a-frame" unit-name))))
    (let* ((in-ep  (if (symbol? in-pack)  `(,in-pack r)  in-pack))
           (out-ep (if (symbol? out-pack) `(,out-pack r) out-pack)))
      (append
       (list `(wire (,unit-name l) (,result p)))
       (IC_PURE_PAIR in-ep out-ep frame)
       (list `(wire (,frame l) (,unit-name r)))))))
