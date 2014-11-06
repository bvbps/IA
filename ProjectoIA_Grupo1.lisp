(load "exemplos.fas")

;;estrutura que define o tipo restricao
(defstruct (restricao)
		variaveis 
		funcao-validacao)

		
;construtor que recebe uma lista das variaveis envolvidades na restricao e um predico e retorna a restricao correspondente
(defun cria-restricao(lista-variaveis predicado)
	(make-restricao :variaveis lista-variaveis :funcao-validacao predicado))

	
(defstruct (psr)
		lista-variaveis
		lista-dominios 
		lista-restricoes 
		hash-atribuicoes ;;hash table com os pares atribuicoes
)

(defun cria-psr (vars dominios restricoes)
	;;(setq hashtable-atribuicoes (make-hash-table))
	;;(dotimes (i 4) (setf (gethash i *hashtable-atribuicoes*) i))  ;;;;nao por isto a por valores ja
	(make-psr :lista-variaveis vars :lista-dominios dominios :lista-restricoes restricoes :hash-atribuicoes (make-hash-table)))

(defun psr-atribuicoes (p)
	(let ((lista-retornada NIL))
		(maphash #'(lambda (key value) 
					(setf lista-retornada(append lista-retornada(list(cons key value))))) 
					(psr-hash-atribuicoes p))
		lista-retornada))
	
	
(defun psr-variaveis-todas (p)
	(psr-lista-variaveis p))
	


(defun psr-variaveis-nao-atribuidas (p)
	(let ((lista-retornada NIL))
		(dolist (x (psr-lista-variaveis p))
			(if (gethash x (psr-hash-atribuicoes p))
				()
				(setf lista-retornada(append lista-retornada(list x)))
			)
		)
		lista-retornada))
		
(defun psr-variavel-valor (p var)
	(first (list(gethash var (psr-hash-atribuicoes p))))
)
	
(defun psr-variavel-dominio (p var)
	(let ((vars (psr-lista-variaveis p))
			(doms (psr-lista-dominios p))
			(i 0))
		(dotimes (x (length vars) i)	
			(if (equal var (nth i vars))
				(return)
				(incf i)
				
			) 
		)
	(nth i doms))
)

(defun psr-variavel-restricoes (p var)
	(let ((vars (psr-lista-variaveis p))
			(restrs (psr-lista-restricoes p))
			(i 0))
		(dotimes (x (length vars) i)	
			(if (equal var (nth i vars))
				(return)
				(incf i)
				
			) 
		)
	(nth i restrs))
)

	
(defun psr-adiciona-atribuicao! (p var val)
	(setf (gethash var (psr-hash-atribuicoes p)) val)
)

(defun psr-remove-atribuicao! (p var)
	(remhash var (psr-hash-atribuicoes p))
)

(defun psr-altera-dominio! (p var dom)
	(let ((vars (psr-lista-variaveis p))
		  (i 0))
		(dotimes (x (length vars) i)	
			(if (equal var (nth i vars))
				(return)
				(incf i)
				
			) 
		)
	(setf (nth i (psr-lista-dominios p)) dom))
)	

(defun psr-completo-p (p)  
	(null (psr-variaveis-nao-atribuidas p)) 
)
;(load "ProjectoIA_Grupo1.lisp")
; (setf p1 (cria-psr '(A B C X Y Z) '('(0 1) '(2 3) '(4 5) '(6 7) '(8 9) '(1 4)) NIL))
