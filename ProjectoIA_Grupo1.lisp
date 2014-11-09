(load "exemplos.fas")

;Grupo 1
;Daniel Amado 75629
;Beatriz Santos 75735
;Antonio Ferreira 75787

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
	(make-psr :lista-variaveis vars :lista-dominios dominios :lista-restricoes restricoes :hash-atribuicoes (make-hash-table :test 'equal)))

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

;vamos iterar a lista das restricoes. para cada restricao vemos as variaveis nela
;envolvidas. Se uma delas for a que queremos, guardamos na lista, a devolver, essa restricao
(defun psr-variavel-restricoes (p var)
	(let ((lista-retornada NIL)
			(lista-vars NIL)
			(lista-restr (psr-lista-restricoes p)))
		(dotimes (el (length lista-restr)) 	
			(setf lista-vars (restricao-variaveis (nth el lista-restr)))
			(dolist (el2 lista-vars)
				(cond((equal var el2)
					(setf lista-retornada(append lista-retornada(list(nth el lista-restr)))))
				)
			)
		)
		
	lista-retornada))


	
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

(defun psr-consistente-p (p)  ;logico inteiro
	(let ( (i 0) 
			(bool T)
			(lista-restr (psr-lista-restricoes p)))
		(dotimes (el (length lista-restr) )
			(incf i)
			(cond ((not(funcall (restricao-funcao-validacao (nth el lista-restr)) p))
					(setq bool nil) (return))
					(T (setq bool T))
				
			)
		)
	(values bool i) 
	)
)

(defun psr-variavel-consistente-p (p var)
	(let ((listaRestr (psr-variavel-restricoes p var)) (i 0) (bool T))
		(dotimes (el (length listaRestr))
			(incf i)
			(cond ((not(funcall (restricao-funcao-validacao (nth el listaRestr)) p))
					(setq bool nil) (return))
					(T (setq bool T))
			)
		)
	(values bool i) 
	)
)	

(defun psr-atribuicao-consistente-p (p var val)
	(let ((oldval (psr-variavel-valor p var))
		  (res1 NIL)
		  (res2 0)
		)
		(psr-adiciona-atribuicao! p var val)
		(setf (values res1 res2)  (psr-variavel-consistente-p p var))
		(if (null oldval) (psr-remove-atribuicao! p var)
			(psr-adiciona-atribuicao! p var oldval)
		)
	(values res1 res2)
	)
)
(defun psr-atribuicoes-consistentes-arco-p (p var1 val1 var2 val2)
		(let ((oldval1 (psr-variavel-valor p var1))
			  (oldval2 (psr-variavel-valor p var2))
			  (listaRestrvar1 (psr-variavel-restricoes p var1))
			  (listaRestrvar2 (psr-variavel-restricoes p var2))
			  (i 0) (bool T) (inter NIL))
			(setf inter (intersection listaRestrvar1 listaRestrvar2))
			(psr-adiciona-atribuicao! p var1 val1)
			(psr-adiciona-atribuicao! p var2 val2)
			(dotimes (el (length inter))
				(incf i)
				(cond ((not(funcall (restricao-funcao-validacao (nth el inter)) p))
						(setq bool nil) (return))
						(T (setq bool T))
				)
			)
			(if (null oldval1) (psr-remove-atribuicao! p var1)
				(psr-adiciona-atribuicao! p var1 oldval1)
			)
			(if (null oldval2) (psr-remove-atribuicao! p var2)
				(psr-adiciona-atribuicao! p var2 oldval2)
			)
			(values bool i)
		)
)

(defun int-to-var (linha coluna)
	(concatenate 'string (write-to-string linha) ":" (write-to-string coluna))
)

(defun calculaAdjacentes (l c nlines ncol )
	(let ((listaAdj NIL))
		(cond( (and (= l 0) (= c 0)) (setf listaAdj (append listaAdj ;estar no canto superior esquerdo do tabuleiro
				(list (int-to-var l c) 
					(int-to-var (+ l 1) c) 
					(int-to-var l (+ c 1)) 
					(int-to-var (+ l 1) (+ c 1)) ))))
					
			( (and (= l 0) (= ncol c)) (setf listaAdj (append listaAdj ;estar no canto superior direito do tabuleiro
				(list (int-to-var l (- c 1)) 
					(int-to-var (+ l 1) (- c 1))
					(int-to-var l c) 
					(int-to-var (+ l 1) c) ))))	
					
			( (and (= l nlines) (= c 0)) (setf listaAdj (append listaAdj ;estar no canto inferior esquerdo
				(list (int-to-var (- l 1) c) 
					(int-to-var l c) 
					(int-to-var (- l 1) (+ c 1))
					(int-to-var l (+ c 1)) ) )))	
		
			( (and (= l nlines) (= c ncol)) (setf listaAdj (append listaAdj ;estar no canto inferior direito
				(list (int-to-var (- l 1) (- c 1))
					(int-to-var l (- c 1)) 
					(int-to-var (- l 1) c) 
					(int-to-var l c) ))))
	
			( (= c 0) (setf listaAdj (append listaAdj ;estar encostado ao lado esquerdo, nao estando num canto
				(list (int-to-var (- l 1) c) 
					(int-to-var l c) 
					(int-to-var (+ l 1) c) 
					(int-to-var (- l 1) (+ c 1)) 
					(int-to-var l (+ c 1))
					(int-to-var (+ l 1) (+ c 1))))))
			( (= c ncol) (setf listaAdj (append listaAdj ;estar encostado ao lado direito, nao estando num canto
				(list (int-to-var (- l 1) (- c 1)) 
					(int-to-var l (- c 1))
					(int-to-var (+ l 1) (- c 1)) 
					(int-to-var (- l 1) c) 
					(int-to-var l c) 
					(int-to-var (+ l 1) c) ))))
			( (= l 0) (setf listaAdj (append listaAdj ;estar encostado ao topo, nao estando num canto
				(list (int-to-var l (- c 1)) 
					(int-to-var (+ l 1) (- c 1)) 
					(int-to-var l c) 
					(int-to-var (+ l 1) c)
					(int-to-var l (+ c 1)) 
					(int-to-var (+ l 1) (+ c 1))))))
					
			( (= l nlines) (setf listaAdj (append listaAdj ;estar encostado a parte de baixo, nao estando num canto
				(list (int-to-var (- l 1) (- c 1)) 
					(int-to-var l (- c 1)) 
					(int-to-var (- l 1) c) 
					(int-to-var l c) 
					(int-to-var (- l 1) (+ c 1))
					(int-to-var l  (+ c 1))))))
					
			( t (setf listaAdj (append listaAdj ;nao estar nem encostado nem num canto
				(list (int-to-var (- l 1) (- c 1)) 
					(int-to-var l (- c 1)) 
					(int-to-var (+ l 1) (- c 1)) 
					(int-to-var (- l 1) c) 
					(int-to-var l c) 
					(int-to-var (+ l 1) c) 
					(int-to-var (- l 1) (+ c 1))
					(int-to-var l (+ c 1))
					(int-to-var (+ l 1) (+ c 1))))))
			
		)
	)

)


(defun cria-predicado (limit vars)
	(let ( (l limit) (v vars))	 
		 #'(lambda (psr)
				(let ((ltmp l)
					(NumZerosPermitidos (- 9 l))
					(NumZerosContados 0))
					(dotimes (x (length v)) 
						(cond ((equal (psr-variavel-valor psr (nth x v)) 1) 
								(decf ltmp))
							((equal (psr-variavel-valor psr (nth x v)) 0) 
								(incf NumZerosContados) 
								(cond((> NumZerosContados NumZerosPermitidos) (setf ltmp 1)(return))))
							((null (psr-variavel-valor psr (nth x v)))
								(setf ltmp 0)(return T))
						)
					) 
					(= ltmp 0)
				)
			)
	)
)


(defun 	fill-a-pix->psr (array)	
	(let( (restr-pred NIL) 
			(listadj NIL) 
			(listvars NIL)	
			(listdoms NIL) 
			(listrest NIL) 
			(dom (list 0 1)) 
			(nlinhas (first (array-dimensions array))) 
			(ncolunas (second (array-dimensions array))))
			(dotimes (c ncolunas)
				(dotimes (l nlinhas)
					(let ((limit (aref array l c))(var (concatenate 'string (write-to-string l) ":" (write-to-string c))))
					(setf listvars (append listvars (list var)))
					(setf listdoms (append listdoms (list dom)))
					(setf listadj (calculaAdjacentes l c (1- nlinhas) (1- ncolunas)))
					(cond( (not (null limit))
						(setf restr-pred (cria-predicado limit listadj))
												
						(setf listrest (append listrest 
							(list (cria-restricao listadj restr-pred)))))
						( t	(setf listrest (append listrest NIL))))
					
					)
				)
			)
			
			(cria-psr listvars listdoms listrest)
				
	)

)

(defun psr->fill-a-pix (psr linhas colunas)
		(let ((array (make-array (list linhas colunas))))
			(dotimes (c colunas)
					(dotimes (l linhas)
						(setf (aref array l c) (psr-variavel-valor psr (int-to-var l c)))
					)
			)
			array
		)
)

(defun procura-retrocesso-simples (psr)
	(retrocesso-simples psr)
)
(defun retrocesso-simples(p)
	(let ((num 0)(logic NIL)(var NIL)(domvar NIL) (bool NIL)(n 0))
		(cond ((psr-completo-p p) (values p n))
			(T (setf var (first (psr-variaveis-nao-atribuidas p)))
				(setf domvar (psr-variavel-dominio p var))
				(dotimes (x (length domvar))
					(setf (values logic num) (psr-atribuicao-consistente-p p var (nth x domvar)))
					
					;(multiple-value-bind (aux-psr testes-feitos) (psr-atribuicao-consistente-p p var (nth x domvar)) (setf logic aux-psr) testes-feitos)
					(cond  ( logic
							(psr-adiciona-atribuicao! p var (nth x domvar))
							;(setf (values p num) (retrocesso-simples p (+ n num)))

							(setf n (+ n (+ num (multiple-value-bind (aux-psr testes-feitos) (retrocesso-simples p) (setf bool aux-psr) testes-feitos))))
							(if (null bool)(psr-remove-atribuicao! p var))
						   )
						   (T (setf n (+ n num)))
					)
					(cond ((psr-completo-p p) (return)))
				)
				(values bool n)
			)
		)
	)
)
	

(defun resolve-simples (array)
	(let ((nlinhas (first (array-dimensions array))) 
		(ncolunas (second (array-dimensions array)))
		(res NIL))
		(setf res (procura-retrocesso-simples (fill-a-pix->psr array)))
		(cond ((null res) NIL)
			  (T (psr->fill-a-pix res nlinhas ncolunas))
		)
	)
	
)

;(load "ProjectoIA_Grupo1.lisp")
;(setf p1 (cria-psr '(A B C X Y Z) '('(0 1) '(2 3) '(4 5) '(6 7) '(8 9) '(1 4)) NIL))

;(setf array1 #2A((NIL NIL NIL NIL 6 NIL) (NIL 9 NIL NIL NIL NIL) (NIL NIL 8 NIL NIL NIL) (0 NIL NIL NIL 3 NIL) (NIL 5 NIL NIL NIL 0)))
