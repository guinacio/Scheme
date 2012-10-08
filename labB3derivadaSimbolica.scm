#lang scheme
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es): Guilherme Silva Inácio
;;;   E-mail:
;;;

;; Função de derivação

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((symbol? exp)
         (cond ((eq? exp var) 1)
               (else 0)))
        ((soma? exp)
         (faz-soma (deriv (par1 exp) var)
                  (deriv (par2 exp) var)))
        ((prod? exp) 
         (faz-soma (faz-prod (ter1 exp) (deriv (ter2 exp) var))
                               (faz-prod (ter2 exp) (deriv (ter1 exp) var))))
        ((subtra? exp)
         (faz-sub (deriv (par1 exp) var)
                  (deriv (par2 exp) var)))
        ((div? exp)
         (faz-div (faz-sub (faz-prod (ter2 exp) (deriv (ter1 exp) var))
                           (faz-prod (ter1 exp) (deriv (ter2 exp) var))) (faz-prod (ter2 exp) (ter2 exp))))
        ((coseno? exp)
         (faz-prod -1 (faz-seno (cadr exp))))
        ((seno? exp)
         (faz-coseno (cadr exp))) 
        (else (list 'deriv exp var))))

;; Soma

; Construtor
(define (faz-soma e1 e2)
  (list '+ e1 e2))
(define (faz-prod e1 e2)
  (list '* e1 e2))
(define (faz-sub e1 e2)
  (list '- e1 e2))
(define (faz-div e1 e2)
  (list '/ e1 e2))
(define (faz-seno e1)
  (list 'seno e1))
(define (faz-coseno e1)
  (list 'coseno e1))

; Seletores
(define (soma? exp) (and (list? exp) (eq? (car exp) '+)))

(define (subtra? exp) (and (list? exp) (eq? (car exp) '-)))

(define (par1 exp) 
  (cond
    ((and (soma? exp) (cadr exp))) 
    (else (and (subtra? exp) (cadr exp)))))

(define (par2 exp)
  (cond 
       ((and (soma? exp)
           (cond ((= (length (cddr exp)) 1) (caddr exp))
	         (else (cons '+ (cddr exp))))))
       ((and (subtra? exp)
           (cond ((= (length (cddr exp)) 1) (caddr exp))
                 (else (cons '- (cddr exp))))))))

(define (prod? exp) (and (list? exp) (eq? (car exp) '*)))

(define (div? exp) (and (list? exp) (eq? (car exp) '/)))

(define (ter1 exp)
  (cond
     ((and (prod? exp) (cadr exp)))
     (else (and (div? exp) (cadr exp)))))

(define (ter2 exp)
  (cond
      ((and (prod? exp)
            (cond ((= (length (cddr exp)) 1) (caddr exp))
                  (else (cons '* (cddr exp))))))
      ((and (div? exp)
            (cond ((= (length (cddr exp)) 1) (caddr exp))
                  (else (cons '/ (cddr exp))))))))

(define (coseno? exp)
  (and (list? exp) (eq? (car exp) 'coseno)))

(define (seno? exp)
  (and (list exp) (eq? (car exp) 'seno)))

;; Exemplo

(deriv '(coseno x) 'x)

;; SUA TAREFA:
;;
;; 1. Entender e explicar a função par2.
;;
(display "\n A função par2 verifica se o (cdr (cdr exp)) for de tamanho 1, se for, isso significa que a lista tem apenas dois elementos, portanto retorna automaticamente o segundo valor \n")

;; 2. Programar as abstrações de dados para as seguintes regras:
;;
;;    produto, subtração, divisão, seno, cosseno, expt
;;
;;    extra: outras regras a gosto
;;
;; 3. Introduzir simplificações nas funções construtores.
;;
;;
;; Ponteiros:
;;  http://myhandbook.info/form_diff.html
;;  http://www.mathacademy.com/pr/prime/articles/dif_rule/index.asp
;;  http://paginas.unisul.br/eqm/download/der/index.html
;;  http://www.expertmathtutoring.com/Differentiation-Rules.php
;;  