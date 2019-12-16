(load "pmatch.scm")

(define eval-expr
  (lambda (expr env)
    (pmatch expr
            [,n (guard (number? n)) n]
            [(zero? ,e) (zero? (eval-expr e env))]
            [(add1 ,e) (add1 (eval-expr e env))]
            [(sub1 ,e) (sub1 (eval-expr e env))]
            [(* ,e1 ,e2) (* (eval-expr e1 env) (eval-expr e2 env))]
            [(if ,t ,c ,a) (if (eval-expr t env) (eval-expr c env) (eval-expr a env))]

            [,x (guard (symbol? x))
             (env x)]
            [(lambda (,x) ,body)
             (lambda (arg)
               (eval-expr body (lambda (y)
                                 (if (eq? x y)
                                   arg
                                   (env y)))))]
            [(,rator ,rand)
             ((eval-expr rator env)
              (eval-expr rand env))])))

(define test
  (lambda (expr)
    (write expr)(newline)
    (write (eval-expr expr (lambda (y) (error 'lookup "unbound"))))
    (newline)(newline)(newline)))

(test '7)
(test '(add1 (add1 5)))
(test '(sub1 (add1 5)))
(test '(* (sub1 3) (add1 5)))
(test '(if 3 (add1 2) 1))
(test '(lambda (x) x))
(test '((lambda (x) x) 3))
(test '(((lambda (x) x) (lambda (y) y)) 3)) ; return identity function from lambda, apply to number
(test '(((lambda (!)        ; factorial of 5, recursive definition using Y-combinator
           (lambda (n)
             ((! !) n)))
         (lambda (!)
           (lambda (n)
             (if (zero? n)
               1
               (* n ((! !) (sub1 n)))))))
        5))

