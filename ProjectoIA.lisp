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
		(lista-variaveis :type list)
		(lista-dominios :type list)
		(lista-restricoes :type list)		
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

(defun atribuicoes1 (p) 
		(mapcar #'cons 
			(psr-lista-variaveis p) (car(psr-lista-dominios p))
			
		)			
)

(defun variaveis-todas1 (p) 
		(lista-variaveis p)
)

(defun variaveis-nao-atribuidas (p)

; Tipo PSR



; Funcoes
; Funcoes de conversao





; Procura Retrocesso Simples

