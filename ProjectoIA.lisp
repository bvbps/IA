;Grupo 1
;Daniel Amado 75629
;Beatriz Santos 75735
;Antonio Ferreira 75787

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Tipos Abstractos de Informacao;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tipo Restricao

(defvar variavel :string)	;ver isto

(defstruct (restricao (:constructor cria-restricao)
						(:type list))
		(variaveis :type list)
		(funcao-validacao?))



(defstruct (psr (:constructor cria-psr))

		(lista-variareis :type list)
		(lista-dominios :type list)
		(lista-restricoes :type list)				
)


; Tipo PSR





; Funcoes
; Funcoes de conversao





; Procura Retrocesso Simples

