


;;estrutura que define o tipo restricao
(defstruct (restricao )
		listaVariaveis 
		funcao-validacao)

		
;construtor que recebe uma lista das variaveis envolvidades na restricao e um predico e retorna a restricao correspondente
(defun cria-restricao(lista-variaveis predicado)
	(make-restricao :listaVariaveis lista-variaveis :funcao-validacao? predicado))

(defun restricao-variaveis (res)
	(restricao-lista-variaveis res))
	
(defun restricao-funcao-validacao (res)
	(restricao-funcao-validacao res))


	
(defstruct (psr)
		lista-variaveis
		lista-dominios 
		lista-restricoes 
		;;lista-atribuicoes	
		hash-atribuicoes ;;hash table com os pares atribuicoes
)

(defun cria-psr (vars dominios restricoes)
	(setq *meu-hash* (make-hash-table))
	(dotimes (i 10) (setf (gethash i *meu-hash*) i))
	(make-psr :lista-variaveis vars :lista-dominios dominios :lista-restricoes restricoes :hash-atribuicoes *meu-hash*))

(defun psr-atribuicoes (p)
	(let (lista_retornada '())
		(maphash #'(lambda (key value) 
					(setf lista_retornada(append lista_retornada(list(cons key value))))) 
					*meu-hash*)
		lista_retornada))
	
	
(defun psr-variaveis-todas (p)
	(psr-lista-variaveis p))
	


(defun psr-variaveis-nao-atribuidas (p)
	(car '(psr-lista-variaveis p))
	(maphash #'(lambda (key value) 
		(if (nth-value 1 (gethash key *meu-hash*)) (list(cons key NIL)) ()))))
	
	
	
	
	
