#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)

(struct exception (exn) #:transparent)
(struct triggered (exception) #:transparent)

(struct trigger (e) #:transparent)
(struct handle (e1 e2 e3) #:transparent)

(struct ?int (n) #:transparent)
(struct ?bool (b) #:transparent)
(struct ?.. (l) #:transparent)
(struct ?seq (s) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (exn) #:transparent)


(struct if-then-else (ifcond istrue isfalse) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)

(struct ?= (e1 e2) #:transparent)
(struct head (l) #:transparent)
(struct tail (l) #:transparent)

(struct ~ (e) #:transparent)

(struct ?any (l) #:transparent)
(struct ?all (l) #:transparent)

(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

(struct get-env () #:transparent)


(define (fri e env) 
  (cond [(true? e) e]
        
        [(false? e) e]
        
        [(int? e) e]
        
        [(..? e)
         (let
             ([v1 (fri (..-e1 e) env)]
              [v2 (fri (..-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [#t (.. v1 v2)]))]
            
        [(exception? e) e]
        
        [(empty? e) e]
        
        [(handle? e) (let ([e1 (fri (handle-e1 e) env)]
                           [e2 (fri (handle-e2 e) env)]
                           [e3 (fri (handle-e3 e) env)])
                       (if (triggered? e1)
                           e1
                           (if (exception? e1)
                               (if (and (triggered? e2) (equal? (exception-exn e1) (exception-exn (triggered-exception e2))))
                                   e3
                                   e2)
                               (triggered (exception "handle: wrong argument type")))))]
        
        [(trigger? e) (let ([v (fri (trigger-e e) env)]
                            )
                      (cond [(triggered? v) v]
                             [(exception? v) (triggered v)]
                             [#t (triggered (exception "trigger: wrong argument type"))]))]
                   
        
        [(?int? e) (let ([n (fri (?int-n e) env)])
                     (if (triggered? n)
                         n
                         (if (int? n)
                             (true)
                             (false))))]
        
        [(?bool? e) (let ([b (fri (?bool-b e) env)])
                     (if (triggered? b)
                         b
                         (if (or (true? b) (false? b))
                             (true)
                             (false))))]
        [(?..? e) (let ([l (fri (?..-l e) env)])
                     (cond [(triggered? l) l]
                            [(..? l) (true)]
                            [#t (false)]))]
        [(?seq? e) (let ([s (fri (?seq-s e) env)])
                     (cond [(triggered? s) s]
                           [(empty? s) (true)]
                           [(..? s)
                            (let ([v1 (..-e1 s)]
                                  [v2 (..-e2 s)])
                               (if (empty? v2)
                                   (true)
                                   (fri (?seq v2) env)))]
                           [#t (false)]))]
                   
        [(?empty? e) (let ([em (fri (?empty-e e) env)])
                     (if (triggered? em)
                         em
                         (if (empty? em)
                             (true)
                             (false))))]
        [(if-then-else? e) 
         (let ([ifcond (fri (if-then-else-ifcond e) env)])
           (cond [(triggered? ifcond) ifcond]
                 [(false? (fri (?bool ifcond) env)) (fri (if-then-else-istrue e) env)]
                 [#t (if (true? ifcond)
                         (fri (if-then-else-istrue e) env)
                         (fri (if-then-else-isfalse e) env))]))]
        [(add? e) 
         (let ([v1 (fri (add-e1 e) env)]
               [v2 (fri (add-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (or (true? v1) ( true? v2))
                      (true)
                      (false))]
                 [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2)))]
                 [(and (or (..? v1) (empty? v1)) (or (..? v2) (empty? v2)))
                  (if (and (true? (fri (?seq v1) env)) (true? (fri (?seq v2) env)))
                      (if (empty? v1)
                          v2
                          (let ([v1a (..-e1 v1)]
                                [v1b (..-e2 v1)])
                            (if (empty? v1b)
                                (.. v1a v2)
                                (.. v1a (fri (add v1b v2) env )))))
                      (triggered (exception "add: wrong argument type")))]
                 [#t (triggered (exception "add: wrong argument type"))]))]
        [(mul? e) 
         (let ([v1 (fri (mul-e1 e) env)]
               [v2 (fri (mul-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (and (true? v1) ( true? v2)) (true) (false))]
                 [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2)))]
                 [(and (empty? v1) (empty? v2)) (empty)]
                 [#t (triggered (exception "mul: wrong argument type"))]))]
        [(?leq? e) 
         (let ([v1 (fri (?leq-e1 e) env)]
               [v2 (fri (?leq-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (implies (true? v1) ( true? v2))
                      (true)
                      (false))]
                 [(and (int? v1) (int? v2)) (if (<= (int-n v1) (int-n v2))
                                                (true)
                                                (false))]
                 [(and (or (..? v1) (empty? v1)) (or (..? v2) (empty? v2)))
                  (if (and (true? (fri (?seq v1) env)) (true? (fri (?seq v2) env)))
                      (cond [(or (and (empty? v1) (not (empty? v2))) (and (empty? v1) (empty? v2))) (true)]
                            [(and (not (empty? v1)) (empty? v2)) (false)]
                            [#t (let ([v1a (..-e1 v1)]
                                      [v1b (..-e2 v1)]
                                      [v2a (..-e1 v2)]
                                      [v2b (..-e2 v2)])
                                  (cond [(or (and (empty? v1b) (empty? v2b)) (and (empty? v1b) (not (empty? v2b)) )) (true)]
                                        [(and (not (empty? v1b)) (empty? v2b) ) (false)]
                                        [#t (fri (?leq v1b v2b) null)]))]) (false))]
                 [#t (triggered (exception "?leq: wrong argument type"))]))]
        [(?=? e) 
         (let ([v1 (fri (?=-e1 e) env)]
               [v2 (fri (?=-e2 e) env)])
           (cond [(triggered? v1) v1]
                 [(triggered? v2) v2]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (not (xor (true? v1) ( true? v2)))
                      (true)
                      (false))]
                 [(and (int? v1) (int? v2)) (if (= (int-n v1) (int-n v2))
                                                (true)
                                                (false))]
                 [(and (empty? v1) (empty? v2)) (true)]
                 [(and (..? v1) (..? v2))
                  (let ([v1a (..-e1 v1)]
                        [v1b (..-e2 v1)]
                        [v2a (..-e1 v2)]
                        [v2b (..-e2 v2)])
                    (if (fri (?= v1a v2a) env) (fri (?= v1b v2b) env) (false)))]
                 [#t (false)]))]

        [(head? e) 
         (let ([v (fri (head-l e) env)])
           (cond [(triggered? v) v]
                 [(..? v) (..-e1 v)]
                 [#t (triggered (exception "head: wrong argument type"))]))]
        [(tail? e) 
         (let ([v (fri (tail-l e) env)])
           (cond [(triggered? v) v]
                 [(..? v) (..-e2 v)]
                 [#t (triggered (exception "tail: wrong argument type"))]))]
        [(~? e) 
         (let ([v (fri (~-e e) env)])
           (cond [(triggered? v) v]
                 [(int? v) (int (* -1 (int-n v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (triggered (exception "~: wrong argument type"))]))]
        
        [(?all? e) (let ([v (fri (?all-l e) env)])
                     (cond [(triggered? v) v]
                           [(..? v) (if (true? (fri (?seq v) env))
                                        (if (false? (..-e1 v))
                                            (false)
                                            (fri (?all (..-e2 v)) env))
                                        (triggered (exception "?all: wrong argument type")))]
                           [(empty? v) (true)]
                           [#t (triggered (exception "?all: wrong argument type"))]))]
        [(?any? e) (let ([v (fri (?any-l e) env)])
                     (cond [(triggered? v) v]
                           [(..? v) (if (true? (fri (?seq v) env))
                                        (if (true? (..-e1 v))
                                            (true)
                                            (fri (?any (..-e2 v)) env))
                                        (triggered (exception "?any: wrong argument type")))]
                           
                           [(empty? v) (false)]
                           [#t (triggered (exception "?any: wrong argument type"))]))]
        [(vars? e) (let ([s (vars-s e)]
                         [e1 (vars-e1 e)]
                         [e2 (vars-e2 e)])
                     (if (and (list? s) (list? e1))
                         (if (null? env)
                             (fri e2 (map (lambda (i j) (cons i j)) s (map (lambda (val) (fri val env)) e1)))
                             (fri e2 (append (map (lambda (i j) (cons i j)) s (map (lambda (val) (fri val env)) e1)) env)))
                         (if (null? env)
                             (fri e2 (list (cons s (fri e1 env))))
                             (fri e2 (append (list (fri e1 env)) env)))))]
        
        [(valof? e) ( begin null (letrec ([to-find (valof-s e)]
                             [find-e-rec (lambda (var-list)
                                            (if (null? var-list)
                                                (triggered (exception "valof: undefined variable"))
                                                (let ([v (car (car var-list))])
                                                  (if (string=? v to-find)
                                                      (cdr (car var-list))
                                                      (find-e-rec (cdr var-list))))))])
                      (find-e-rec env)))]
        [(fun? e) (closure env e)]
        [(proc? e) (closure env e)]
        [(call? e) (begin null (let ([cl (fri (call-e e) env)]
                         [args (map (lambda (val) (fri val env)) (call-args e))])    
                      (if (closure? cl)
                          (let ([f (closure-f cl)]
                                [en (closure-env cl)])
                            (cond [(fun? f)
                                   (let ([name (fun-name f)]
                                         [farg (fun-farg f)]
                                         [body (fun-body f)])
                                     (fri body (append (list (cons name cl)) (append (map (lambda (i j) (cons i j)) farg args) en))))]
                                  [(proc? f)
                                   (let ([name (proc-name f)]
                                         [body (proc-body f)])
                                     (fri body (append (list (cons name cl)) env)))]
                                  [#t (triggered (exception "call: wrong argument type"))]))
                            (triggered (exception "call: wrong argument type")))))]
        [(get-env? e) env]
        [#t (triggered (exception "syntax error"))]
        ))

(define (greater e1 e2) (fri (call
                              (fun "my-greater" (list "e1" "e2")
                                   (if-then-else (?leq (valof "e1") (valof "e2")) (false) (true)))
                              (list e1 e2)) null))

(define (rev e) (fri (call
                      (fun "my-rev" (list "e" "acc")
                           (if-then-else (?empty (valof "e"))
                                         (valof "acc")
                                         (call (valof "my-rev") (list (tail (valof "e")) (.. (head (valof "e")) (valof "acc"))))))
                      (list e (empty))) null))

(define (binary e1) (fri (call
                              (fun "my-binary" (list "e1" )
                                   (triggered (exception "not implemented")))
                              (list e1)) null))

(define (mapping f seq) (fri (call
                              (fun "my-mapping" (list "f" "seq")
                                   (if-then-else (?empty (valof "seq"))
                                                 (empty)
                                                 (.. (call (valof "f") (list (head (valof "seq"))))
                                                     (call (valof "my-mapping") (list (valof "f") (tail (valof "seq")))))))
                              (list f seq)) null))


(define (filtering f seq) (fri (call
                              (fun "my-filtering" (list "f" "seq")
                                   (if-then-else (?empty (valof "seq"))
                                                 (empty)
                                                 (if-then-else (call (valof "f") (list (head (valof "seq"))))
                                                               (.. (head (valof "seq")) (call (valof "my-filtering") (list (valof "f") (tail (valof "seq")))))
                                                               (call (valof "my-filtering") (list (valof "f") (tail (valof "seq")))))))
                              (list f seq)) null))

(define (folding f init seq) (fri (call
                              (fun "my-folding" (list "f" "init" "seq")
                                   (if-then-else (?empty (valof "seq"))
                                                 (valof "init")
                                                 (call
                                                  (valof "my-folding") (list (valof "f") (call (valof "f") (list (head (valof "seq")) (valof "init"))) (tail (valof "seq"))))))
                              (list f init seq)) null))

