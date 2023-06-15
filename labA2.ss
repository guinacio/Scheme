#lang scheme
(require mzlib/trace)
;;; -*- Mode: Lisp; Syntax: Scheme -*-
;;;   Linguagem: Module
;;;  
;;;   Departamento de Automação e Sistemas (DAS)
;;;   Universidade Federal de Santa Catarina (UFSC)
;;;   88.040-900 Florianópolis - SC
;;;   Brasil
;;;
;;;   DAS5102 Fundamentos da Estrutura da Informação
;;;
;;;   Programador(es): Guilherme Silva Inácio
;;;
;;;
;;;   Laboratório A2: Iteração e Recursão Linear
;;;

;;; INTRODUÇÃO


;;
;; Fatorial recursivo

(define (fat-r n)
  (cond ((= n 0) 1)
        (else (* n (fat-r (- n 1))))))

;;
;; Fatorial iterativo

(define (fat-i n)
  (fat-ir 1 1 n))

(define (fat-ir cont p cmax)
  (cond ((> cont cmax) p)
        (else (fat-ir (+ cont 1) (* cont p) cmax))))

;;
;; Uso da função "trace"

(trace fat-r)
(fat-r 6)

(trace fat-ir)
(fat-i 6)

;;
;; SUA TAREFA:
;;
;; 1. Programar a função exponencial de maneira recursiva e
;;    iterativa. Use a função trace para comparar os processos
;;    gerados.
;;
;;    exp(b,n) = se n=0 então 1
;;               senão b*exp(b,n-1)
(display "---------------Tarefa 1---------------\n")

(define (exp-r b n)
  (cond ((= n 0) 1)
        (else (* b (exp-r b (- n 1))))))
(trace exp-r)
(display "Recursiva: \n")
(exp-r 3 4)

;---------------------------------
(define (exp-i b n)
  (exp-it 1 1 b n))


(define (exp-it cont total base exp)
  (cond ((< cont exp) (exp-it (+ cont 1) (* base total) base exp))
        (else (* base total))))

(trace exp-it)
(display "Iterativa: \n")
(exp-i 3 4)

;;
;;
;; 2. Programar a função de Fibonacci de maneira recursiva e
;;    iterativa. Use a função trace para comparar os processos
;;    gerados.
;;
;;    fib(n) = se n=0 então 0
;;             se n=1 então 1
;;             se n>1 então fib(n-1)+fib(n-2)
(display "---------------Tarefa 2---------------\n")

(define (fib-r n)
  (cond 
    ((= n 0) 0)
    ((= n 1) 1)
    ((> n 1)  (+ (fib-r (- n 1)) (fib-r (- n 2))))))

(trace fib-r)
(display "Recursiva: \n")
(fib-r 5)

;-------------------------------------------
(define (fib-i n)
  (fib-it 1 0 n))

(define (fib-it aux total cont)
  (cond 
    ((= cont 0) total)
    (else (fib-it (+ aux total) aux (- cont 1)))))

(trace fib-it)
(display "Iterativa: \n")
(fib-i 16)
;;
;;
;; 3. Considere a função f definida sobre os naturais como segue:
;;
;;    f(n) = n se n < 3
;;    f(n) = f(n - 1) + 2 f (n - 2) + 3 f (n - 3) se n >= 3
;;
;;    Programe a função f de maneira recursiva e iterativa. Use a 
;;    função trace para comparar os processos gerados.
(display "---------------Tarefa 3---------------\n")
(define (fun-r n) 
  (cond
    ((< n 3) n)
    ((>= n 3) (+ (fun-r (- n 1)) (* (fun-r (- n 2)) 2) (* (fun-r (- n 3)) 3)))))

(trace fun-r)
(display "Recursiva: \n")
(fun-r 5)

;-----------------
(define (fun-i n)
  (fun-it 0 1 2 4 3 n))

(define (fun-it antepenultimo penultimo ultimo total cont n)
  (cond 
    ((< n 3) n)
    ((< cont n) (fun-it penultimo ultimo total (+ total (* ultimo 2) (* penultimo 3)) (+ cont 1) n))
    ((= cont n) total)))
     

(trace fun-it)
(display "Iterativa: \n")
(fun-i 5)

;;
;;
;; 4. Programar a função de Fibonacci de maneira recursiva e
;;    iterativa na linguagem C.
;;
