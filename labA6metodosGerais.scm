#lang scheme
(require mzlib/trace)
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;   Linguagem: Essentials of Programming Languages (2nd ed.) costumizado.
;;;
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102
;;;
;;;   Programador(es): Guilherme Silva Inácio
;;;   Email:
;;;

;; Constantes

(define *erro* 0.001)


(define *dx* 0.001)

;; Funções auxiliares (reutilizadas !)

(define (perto? chute x)
  (< (abs (- chute x)) *erro*))


(define (media x y)
  (/ (+ x y) 2))


(define (quadrado x)
  (* x x))

;; Raiz quadrada: Método de Heron de Alexandria

(define (bom? chute x)
  (perto? (quadrado chute) x))


(define (melhora chute x)
  (media chute (/ x chute)))


(define (raiz-it chute x)
  (cond ((bom? chute x) chute)
	(else (raiz-it (melhora chute x) x))))

(define (raiz-hr x)
  (raiz-it 1.0 x))

;; Ponto-fixo

(define (pf f chute)
  (let ((prox (f chute)))
    (cond ((perto? chute prox) prox)
	  (else (pf f prox)))))

;; Abafamento por média

(define (ma f)
  (lambda (x)(media x (f x))))

;; Raiz quadrada: Método de ponto-fixo

(define (raiz-pf x)
  (pf (ma (lambda (y)(/ x y))) 1.0))

;; Método de Newton

(define (newt g)
  (lambda (x)(- x (/ (g x)
		     ((deriv g) x)))))


(define (deriv g)
  (lambda (x)(/ (- (g (+ x *dx*)) (g x))
		*dx*)))


(define (newmeth g chute)
  (pf (newt g) chute))

;; Raiz quadrada: Método de Newton

(define (raiz-mn x)
  (newmeth (lambda (y)(- (quadrado y) x)) 1.0))

;; SUA TAREFA:
;;
;; 1. Escrever uma função "pf-trans" que recebe uma função, uma
;;    transformada e um chute e calcula o ponto-fixo da transformada da
;;    função. Reescrever os métodos de ponto-fixo e de Newton para raiz
;;    quadrada utilizando esta função.
;;
    (define (pftrans g trans chute)
      (pf (trans g) chute))
;;
   (define (raiz-pft-ma x)
      (pftrans (lambda (y)(/ x y)) ma 1.0))
   
   (raiz-pft-ma 25)
;;
    (define (raiz-pft-mn x)
      (pftrans (lambda (y)(- (quadrado y) x)) newt 1.0))
    
   (raiz-pft-mn 25)
;;
;;
;; 2. Escrever funções para a raiz enésima usando os métodos de ponto-fixo
;;    e de Newton.
;;
    (define (raizn-pft-ma x n)
      (pftrans (lambda (y)(/ x (expt y (- n 1)))) ma 1.0))
(raizn-pft-ma 27 3)
;;
    (define (raizn-pft-mn x n)
      (pftrans (lambda (y)(- (expt y n) x)) newt 1.0))
(raizn-pft-mn 27 3)
;;
;;
;; 3. A expressão:
;;
;;        N(1)
;;    -----------
;;    D(1) + N(2)
;;           -----------
;;           D(2) + N(3)
;;                  -----------
;;                  D(3) + N(4)
;;                         ----------
;;                         ... + N(k)
;;                               ----
;;                               D(k)
;;
;;    é chamada de fração contínua truncada em k. Escreva uma função "fc"
;;    que calcule esta expressão recebendo como parâmetros as funções
;;    N(i) e D(i) e o limite k.
;;    Caso a função esteja correta o valor da chamada:
;;
     
;;
;;    com k um inteiro positivo deve retornar uma aproximação para o
;;    inverso da razão áurea: 1/phi=1/1,6180=0,6180.
;;
 (define (fc N D k)
      (fcaux N D k 1))
 
 (define (fcaux N D k cont)  
   (cond
        ((= cont k) 0)
        (else (/ (N cont) (+ (D cont) (fcaux N D k (+ cont 1)))))))
;;
    (fc (lambda (x) 1.0) (lambda (x) 1.0) 15)
;;
;; 4. Utilize a função "fc" para calcular a tangente usando a seguinte
;;    fração contínua, proposta por J.H. Lambert em 1770:
;;
;;                 x
;;    tag(x) = ---------
;;                    x^2
;;             1 - ---------
;;                        x^2
;;                 3 - ---------
;;                            x^2
;;                     5 - ---------
;;                            ...
;;
       
    (define (tag x k)
     (/ (fc (lambda (y) (* x x -1.0)) (lambda (y) (- (* 2 y) 1)) k)
      (* -1 x)))     
   (tag (/ pi 4) 20)