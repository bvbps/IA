;Grupo 1
;Daniel Amado 75629
;Beatriz Santos 75735
;Antonio Ferreira 75787

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Tipos Abstractos de Informacao;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tipo Restricao

(defvar variavel :string)	;ver isto

(defstruct (restricao (:constructor cria-restricao) (:type list))
		(variaveis :type list)
		(funcao-validacao?))



(defstruct (psr (:constructor cria-psr))
		lista-variaveis
		lista-dominios 
		lista-restricoes 
		lista-vars-atribuicoes		
		atribuicoes ;;Recebe um PSR, retorna a lista de atribuicoes
		variaveis-todas
		variaveis-nao-atribuidas
		variavel-valor
		variavel-dominio
		variavel-restricoes
		adiciona-atribuicao!
		remove-atribuicao!
		altera-dominio!
		completo-p
		consistente-p
		variavel-consistente-p
		atribuicao-consistente-p
		atribuicoes-consistentes-arco-p
)

; devolve a lista de todas as atribuicoes.
(defun atribuicoes1 (p) 
		(lista-vars-atribuicoes p)		
)

; devolve a lista de todas as variaveis
(defun variaveis-todas1 (p) 
		(psr-lista-variaveis p)
)

; devolve a lista de todas as variaveis nao atribuidas
(defun variaveis-nao-atribuidas (p)
	(setf vars-atribuidas (mapcar #'Car (lista-atribucoes p))
		

; Tipo PSR



; Funcoes
; Funcoes de conversao





; Procura Retrocesso Simples

