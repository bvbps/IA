(load "exemplos.fas")

;;Grupo 1
;;Daniel Amado 75629
;;Beatriz Santos 75735
;;Antonio Ferreira 75787

;;Estrutura que define o tipo restricao
(defstruct (restricao)
		variaveis 
		funcao-validacao)

		
;;Construtor que recebe uma lista das variaveis envolvidades na restricao e um predico e retorna a restricao correspondente
(defun cria-restricao(lista-variaveis predicado)
	(make-restricao :variaveis lista-variaveis :funcao-validacao predicado))

;;Estrutura que define o tipo psr
;;tem uma hash-table com os pares atribuicoes
(defstruct (psr)
		lista-variaveis
		lista-restricoes 
    hash-dominios
		hash-atribuicoes 
		hash-restricoes
)

;;Construtor que recebe uma lista de variaveis, 
;;uma lista de dominios e uma de restricoes e devolve o psr
(defun cria-psr (vars dominios restricoes)
	(let (
		(hashdominios (make-hash-table :test 'equal))
		(hashrestricoes (make-hash-table :test 'equal))
			)
		(dotimes (i (length vars)) 	
			(setf (gethash (nth i vars) hashdominios) (nth i dominios))
		)
		
		(dotimes (i (length restricoes)) 
			(dolist (el (restricao-variaveis (nth i restricoes)))
				(setf (gethash el hashrestricoes) (append (gethash el hashrestricoes) (list (nth i restricoes))))
			)
		)

    
		(make-psr :lista-variaveis vars 
				:lista-restricoes restricoes 
        :hash-dominios hashdominios
				:hash-restricoes hashrestricoes 
				:hash-atribuicoes (make-hash-table :test 'equal)
		)
	)
)


  
;;Para cada par da hash-table de atribuicoes,
;;mete-se na lista-retornada como um par e retorna-se a lista no final com os respectivos pares
(defun psr-atribuicoes (p)
	(let ((lista-retornada NIL))
		(maphash #'(lambda (key value) 
					(setf lista-retornada(append lista-retornada(list(cons key value))))) 
					(psr-hash-atribuicoes p))
		lista-retornada))
	
;;Devolve uma lista com todas as variaveis do psr
(defun psr-variaveis-todas (p)
	(psr-lista-variaveis p))

;;Devolve uma lista com as variaveis nao atribuidas
;;Para isso verificamos para todas as variaveis se estam dentro da 
;hash-table de atribuicoes e caso nao estejam metemo-las numa lista 
;;e por fim retornamos a lista
(defun psr-variaveis-nao-atribuidas (p)
	(let ((lista-retornada NIL))
		(dolist (x (psr-lista-variaveis p))
			(if (gethash x (psr-hash-atribuicoes p))
				()
				(setf lista-retornada(append lista-retornada(list x)))
			)
		)
		lista-retornada))

;;Pedimos a hash-table o valor atribuido a variavel e devolvemos
(defun psr-variavel-valor (p var)
	(first (list(gethash var (psr-hash-atribuicoes p))))
)

;;Verificamos em que posicao esta a variavel e depois
;; devolvemos o dominio da lista de dominios que esta na mesma posicao que a variavel
(defun psr-variavel-dominio (p var)
	(first (list(gethash var (psr-hash-dominios p))))
)

;vamos iterar a lista das restricoes. para cada restricao vemos as variaveis nela
;envolvida
;Se uma delas for a que queremos, guardamos na lista, a devolver, essa restricao
(defun psr-variavel-restricoes (p var)
	(first (list(gethash var (psr-hash-restricoes p))))
)


;;Adicionamos um par a hash-table de atribuicoes, com a variavel e o valor
(defun psr-adiciona-atribuicao! (p var val)
	(setf (gethash var (psr-hash-atribuicoes p)) val)
)

;;Retiramos da hash-table o par que corresponde a variavel
(defun psr-remove-atribuicao! (p var)
	(remhash var (psr-hash-atribuicoes p))
)

;;Vamos a lista de variaveis e verificamos em que posicao esta a variavel, depois
;;alteramos na mesma posicao da lista de dominios o dominio para o que nos e dado
(defun psr-altera-dominio! (p var dom)
  (setf (gethash var (psr-hash-dominios p)) dom)
)	

;;Verificamos se ha variaveis nao atribuidas
;;Caso haja, quer dizer que n e completo
(defun psr-completo-p (p)  
	(null (psr-variaveis-nao-atribuidas p)) 
)

;;Verificamos as restricoes ate nos aparecer uma que nao esteja satisfeita.
;;Caso estejam todas satisfeitas devolvemos true
(defun psr-consistente-p (p) 
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

;;Verificamos as restricoes que dependem da var, ate encontrarmos uma que nao seja satisfeita,
;;Caso sejam todas satisfeitas devolvemos True, que significa que a variavel e consistente
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

;;Atribuimos o valor a variavel e verificamos se e consistente 
;;Antes de devolver o resultado, voltamos a deixar o valor da variavel como estava
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

;;Atribuimos os dois valores as variaveis e depois testamos se sao consistentes
;;No final voltamos a atribuir os valores antigos das variaveis
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

;;Devolve o nome da variavel, dando a linha e a coluna onde pertencem
(defun int-to-var (linha coluna)
	;(concatenate 'string (write-to-string linha) ":" (write-to-string coluna))
  (cons linha coluna)
)

;;Calcula as variaveis adjacentes a outra variavel, 
;;dando o numero de linhas e de colunas da matriz e o numero e a linha da variavel
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


;;Cria a funcao de verificacao que recebe todas as variaveis adjacentes e o limite de 1 que podem haver, que e o numero da variavel central
;;O predicado conta o numero de variaveis pintadas e verifica se nao excede o limite de variaveis pintadas, caso exceda devolve falso.
;;O predicado conta o numero de variaveis brancas e verifica se nao excede o limite de variaveis brancas, caso exceda devolve falso.

(defun cria-predicado (limit vars)
	(let ( (l limit) (v vars))	 
		 #'(lambda (psr)
				(let ((bool t)
          (NumUnsContados 0)
					(NumZerosPermitidos (- (length v) l))
					(NumZerosContados 0))
					(dotimes (x (length v)) 
						(cond ((equal (psr-variavel-valor psr (nth x v)) 1) 
								(incf NumUnsContados))
							((equal (psr-variavel-valor psr (nth x v)) 0) 
								(incf NumZerosContados))
						)
            (cond((or(> NumZerosContados NumZerosPermitidos) (> NumUnsContados l)) (setf bool nil)(return)))
					) 
          bool
				)
			)
	)
)

;;Para cada variavel da lhes o dominio (0 1), e caso tenham uma condicao cria um predicado com as variaveis adjacentes e adiciona a lista de restricoes
;;Cria o psr com os argumentos necessarios, a lista de variaveis, a lista dos dominios e a lista das restricoes
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
					(let ((limit (aref array l c))
          (var (cons l c))
          )
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

;;Cria um array atraves dum psr dado, metendo o dominio de cada variavel no respectivo lugar
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

;;Chama a funcao recursiva retrocesso-simples de modo a resolver o psr
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
					(cond  ( logic
							(psr-adiciona-atribuicao! p var (nth x domvar))

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

(defun heuristica-grau (p)
  (let ((maxvalue 0)(maxkey nil))
    (dolist (var (psr-variaveis-nao-atribuidas p)) 
      (let ((value 0)(boolVarsNaoAtribuidas T))
        (dolist (restricao (psr-variavel-restricoes p var))
          (setf boolVarsNaoAtribuidas T)
          (dolist (restrvar (restricao-variaveis restricao))
            (if (and (not(psr-variavel-valor p restrvar)) (not(equal restrvar var)) boolVarsNaoAtribuidas) ;se boolVarsNaoAtribuidas ja for nil nao queremos por a nil outra vez
                (setf boolVarsNaoAtribuidas NIL)
            )
          )
          (if(null boolVarsNaoAtribuidas) (incf value))
        )
        (cond ((> value maxvalue)(setf maxkey var)(setf maxvalue value))
              ((and (not maxkey)(= value maxvalue))(setf maxkey var))
        )

      )
		)

  maxkey)
)

(defun procura-retrocesso-grau(p)
	(let ((num 0)(logic NIL)(var NIL)(domvar NIL) (bool NIL)(n 0))
		(cond ((psr-completo-p p) (values p n))
      (T (setf var  (heuristica-grau p))
				(setf domvar (psr-variavel-dominio p var))
				(dotimes (x (length domvar))
					(setf (values logic num) (psr-atribuicao-consistente-p p var (nth x domvar)))
        
					(cond  ( logic
							(psr-adiciona-atribuicao! p var (nth x domvar))


							(setf n (+ n (+ num (multiple-value-bind (aux-psr testes-feitos) (procura-retrocesso-grau p) (setf bool aux-psr) testes-feitos))))
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


(defun revise (psr x y inferencias)
  (let ((testesTotais 0)
        (revised nil)
        (dominio-x (gethash x inferencias))
        (dominio-y (list (psr-variavel-valor psr y)))
        (novo-dominio-x nil))
        (if(null dominio-x)
          (setf dominio-x (psr-variavel-dominio psr x))
          
        )
        (setf novo-dominio-x dominio-x)

        (cond((null (first dominio-y))
                (setf dominio-y (gethash y inferencias))
                (cond ((null dominio-y)
                  (setf dominio-y (psr-variavel-dominio psr y)) )
                )
              )
        )
        (dolist (valx dominio-x)
          (let ((foundConsistentValue nil) (consistente nil) (testes 0))
            (dolist (valy dominio-y)
              (setf (values consistente testes)  (psr-atribuicoes-consistentes-arco-p psr x valx y valy))
              (setf testesTotais (+ testesTotais testes))
              (cond (consistente (setf foundConsistentValue T) (return)))
            )
            (cond ((null foundConsistentValue) (setf revised T) (setf novo-dominio-x (remove valx novo-dominio-x :test #'equal))))
          )
        )
        (if revised (setf (gethash x inferencias) novo-dominio-x))
    (values revised testesTotais)  
  )
)

;funcao que verifica se var2 esta envolvida nalguma restricao com var1
(defun isIn (p var1 var2)
  (let((bool nil))
    (cond ((psr-variavel-valor p var2))
          (t (dolist (restricao (psr-variavel-restricoes p var1))
                (dolist (restrvar (restricao-variaveis restricao))
                  (cond((equal restrvar var2) (setf bool T)
                    (return T)))))
          )
    )
  bool
  )
)


(defun arcos-vizinhos-nao-atribuidos(p var)
  (let((lista-arcos nil) 
      )
      
  (dolist (var-natribuida (psr-variaveis-nao-atribuidas p)) 
    (cond((not(equal var var-natribuida))
            (if (isIn p var var-natribuida)
              (setf lista-arcos (append lista-arcos (list (cons var-natribuida var))))
            )
            
          )
    )
  )
  
  lista-arcos
 )
)


(defun forward-checking(p var)
  (let ((testesTotais 0) 
      (inferencias (make-hash-table :test 'equal)) 
      (lista-arcos (arcos-vizinhos-nao-atribuidos p var))
      (revised nil)
      (testes 0)
      (bool T))
      

      (dolist (arco lista-arcos)
       
        (setf (values revised testes)  (revise p (car arco) (cdr arco) inferencias))
        (setf testesTotais (+ testesTotais testes))
        (cond (revised (cond ((= (length (gethash (car arco) inferencias)) 0)
                              (setf bool nil) (return)
                            )
                      )
              )
        )
      )
    (if bool (values inferencias testesTotais)
              (values nil testesTotais)
    )
  )
)

(defun mrv (p)
  (let* ((var-n-atribuidas (psr-variaveis-nao-atribuidas p))
      (minvalue (length (psr-variavel-dominio p (first var-n-atribuidas))))
      (value 0)
      (minvar (first var-n-atribuidas))
      )
    (dolist (var var-n-atribuidas) 
        (setf value (length (psr-variavel-dominio p var)))
        (cond ((< value minvalue)(setf minvar var)(setf minvalue value))
        )
    )
  minvar 
  )
)

;copia as inferencias para o psr1
(defun copia-dominios (p inferencias)
   (maphash #'(lambda (key value) (psr-altera-dominio! p key value))  inferencias)
)

;copia dom2 para dom1 caso n seja 0
(defun backup (p inferencias)
  (let ((backup (make-hash-table :test 'equal)))
    (maphash #'(lambda (key value) (setf value value) (setf (gethash key backup) (psr-variavel-dominio p key)))  inferencias)
    backup    
  )
)

(defun procura-retrocesso-fc-mrv(p)
    (let ((testesTotais 0) (var nil)(resultadoFinal nil)(backup-dominios nil))
      (cond ((psr-completo-p p) (setf resultadoFinal p))
            (T (setf var (mrv p))
              (dolist (valor (psr-variavel-dominio p var))
                (let ((consistente nil)(testes 0)(inferencias nil)(resultado nil))
                  (setf (values consistente testes)  (psr-atribuicao-consistente-p p var valor))
                  (setf testesTotais (+ testesTotais testes))
                  (cond (consistente (psr-adiciona-atribuicao! p var valor) 
                                    (setf (values inferencias testes)  (forward-checking p var))
                                    (setf testesTotais (+ testesTotais testes))
                                    (cond (inferencias 
                                          (setf backup-dominios (backup p inferencias))
                                          (copia-dominios p inferencias)
                                          (setf (values resultado testes)  (procura-retrocesso-fc-mrv p))
                                          (setf testesTotais (+ testesTotais testes))
                                          (cond (resultado (setf resultadoFinal resultado)(return)))
                                          (copia-dominios p backup-dominios)
                                          )
                                          
                                    )
                                    (psr-remove-atribuicao! p var) 
                      )
                  )
                ) 
              )
            )
          
      )
  (values resultadoFinal testesTotais)
  )
)

(defun mac(p var)
  (let ((testesTotais 0) 
      (inferencias (make-hash-table :test 'equal)) 
      (lista-arcos (arcos-vizinhos-nao-atribuidos p var))
      (revised nil)
      (testes 0)
      (bool T))
      

      (dolist (arco lista-arcos)
        (let ( (v1 (cdr arco)) (v2 (car arco)) (novos-arcos nil))
        (setf (values revised testes)  (revise p v2 v1 inferencias))
        (setf testesTotais (+ testesTotais testes))
        (cond (revised (cond ((= (length (gethash v2 inferencias)) 0)
                              (setf bool nil) (return)
                            )
                      )
              (setf novos-arcos (arcos-vizinhos-nao-atribuidos p v2))
              (setf novos-arcos (remove (cons v1 v2) novos-arcos :test #'equal))
              (setf lista-arcos (nconc lista-arcos  novos-arcos))
              )
        )
        ) 
      )
    (if bool (values inferencias testesTotais)
              (values nil testesTotais)
    )
  )
)

(defun procura-retrocesso-mac-mrv (p)
    (let ((testesTotais 0) (var nil)(resultadoFinal nil)(backup-dominios nil))
      (cond ((psr-completo-p p) (setf resultadoFinal p))
            (T (setf var (mrv p))
              (dolist (valor (psr-variavel-dominio p var))
                (let ((consistente nil)(testes 0)(inferencias nil)(resultado nil))
                  (setf (values consistente testes)  (psr-atribuicao-consistente-p p var valor))
                  (setf testesTotais (+ testesTotais testes))
                  (cond (consistente (psr-adiciona-atribuicao! p var valor)
                                    (setf (values inferencias testes)  (mac p var))
                                    (setf testesTotais (+ testesTotais testes))
                                    (cond (inferencias 
                                          (setf backup-dominios (backup p inferencias))
                                          (copia-dominios p inferencias)
                                          (setf (values resultado testes)  (procura-retrocesso-mac-mrv p))
                                          (setf testesTotais (+ testesTotais testes))
                                          (cond (resultado (setf resultadoFinal resultado)(return)))
                                          (copia-dominios p backup-dominios)
                                          )
                                          
                                    )
                                    (psr-remove-atribuicao! p var) 
                      )
                  )
                ) 
              )
            )
          
      )
  (values resultadoFinal testesTotais)
  )
)

(defun 	fill-a-pix->psr-best (array)	
	(let( 
	(restr-pred NIL) 
	(listadj NIL)
	(listvars NIL)
	(listrest nil)
  (restr nil)
	(nlinhas (array-dimension array 0)) 
	(ncolunas (array-dimension array 1))
	(hash-atribuicoes (make-hash-table :test 'equal))
	(hash-restricoes (make-hash-table :test 'equal))
	(hash-doms (make-hash-table :test 'equal))
	(dom (list 0 1)) 
	(numAdjac 0)
	)
      
      
	(dotimes (c ncolunas)
	  (dotimes (l nlinhas)
	    (let (
	      (limit (aref array l c))
              (var (cons l c))
               )
            
            (setf listvars (append listvars (list var)))

            (setf listadj (calculaAdjacentes l c (1- nlinhas) (1- ncolunas)))
            
            
            (cond((not (null limit))
                  (setf numAdjac (length listadj))
                  ;para os dominios: (imediatos)
                  (cond ((= limit 9) 
                            (dolist (adj listadj)  
                                (setf (gethash adj hash-doms) (list 1))
                            )
                        )
                        ((= limit 0) 
                            (dolist (adj listadj)  
                                (setf (gethash adj hash-doms) (list 0))
                            )
                        )
                        ((and (= limit 6) (= limit numAdjac)) 
                            (dolist (adj listadj)  
                                (setf (gethash adj hash-doms) (list 1))
                          
                            )
                        )
                        ((and (= limit 4) (= limit numAdjac))
                            (dolist (adj listadj)  
                                (setf (gethash adj hash-doms) (list 1))

                            )
                        )
                        ((null (gethash var hash-doms)) (setf (gethash var hash-doms) dom))
                  )
                 
                  (setf restr-pred (cria-predicado limit listadj))
                  (setf restr (cria-restricao listadj restr-pred))
                  (dolist (adj listadj)  
                      (setf (gethash adj hash-restricoes) (append (gethash adj hash-restricoes) (list restr)))
                  )      
                  (setf listrest (append listrest (list (cria-restricao listadj restr-pred))))
                )
                (t (setf listrest (append listrest NIL)) (if (null (gethash var hash-doms)) (setf (gethash var hash-doms) dom)))
            )
            
					)
				)
			)
			 
			(cria-psr-best listvars listrest hash-doms hash-restricoes hash-atribuicoes)
	)

)

(defun cria-psr-best (vars listrest hash-dominios hash-restricoes hash-atribuicoes)

		(make-psr :lista-variaveis vars 
        :lista-restricoes listrest
				:hash-restricoes hash-restricoes 
				:hash-dominios hash-dominios 
				:hash-atribuicoes hash-atribuicoes
		)
    
)


(defun resolve-best (array)
	(let(
      (nlinhas (first (array-dimensions array))) 
      (ncolunas (second (array-dimensions array)))
      (res NIL)
      )
      
		(setf res (procura-retrocesso-fc-mrv-grau (fill-a-pix->psr-best array)))
		(cond ((null res) nil)
			  (T (psr->fill-a-pix res nlinhas ncolunas))
		)
	)
)

(defun procura-retrocesso-fc-mrv-grau(p)
    (let ((testesTotais 0) (var nil)(resultadoFinal nil)(backup-dominios nil))
      (cond ((psr-completo-p p) (setf resultadoFinal p))
            (T (setf var (mrv-grau p))
              (dolist (valor (psr-variavel-dominio p var))
                (let ((consistente nil)(testes 0)(inferencias nil)(resultado nil))
                  (setf (values consistente testes)  (psr-atribuicao-consistente-p p var valor))
                  (setf testesTotais (+ testesTotais testes))
                  (cond (consistente (psr-adiciona-atribuicao! p var valor) 
                                    (setf (values inferencias testes)  (forward-checking p var))
                                    (setf testesTotais (+ testesTotais testes))
                                    (cond (inferencias 
                                          (setf backup-dominios (backup p inferencias))
                                          (copia-dominios p inferencias)
                                          (setf (values resultado testes)  (procura-retrocesso-fc-mrv p))
                                          (setf testesTotais (+ testesTotais testes))
                                          (cond (resultado (setf resultadoFinal resultado)(return)))
                                          (copia-dominios p backup-dominios)
                                          )
                                          
                                    )
                                    (psr-remove-atribuicao! p var) 
                      )
                  )
                ) 
              )
            )
          
      )
  (values resultadoFinal testesTotais)
  )
)

(defun mrv-grau (p)
    (let* (
      (var-n-atribuidas (psr-variaveis-nao-atribuidas p))
      (minvalue (length (psr-variavel-dominio p (first var-n-atribuidas))))
      (value 0)
      (minvar (first var-n-atribuidas))
      )
    (dolist (var var-n-atribuidas) 
        (setf value (length (psr-variavel-dominio p var)))
        (cond ((< value minvalue)
                (setf minvar var)
                (setf minvalue value))
        )
    )
    (cond ((= minvalue 2)
            (setf minvar (heuristica-grau p))
          )
    )
  minvar 
  )
)

;(load "ProjectoIA_Grupo1.lisp")
;(setf p1 (cria-psr '(A B C X Y Z) '('(0 1) '(2 3) '(4 5) '(6 7) '(8 9) '(1 4)) NIL))

;(setf array1 #2A((NIL NIL NIL NIL 6 NIL) (NIL 9 NIL NIL NIL NIL) (NIL NIL 8 NIL NIL NIL) (0 NIL NIL NIL 3 NIL) (NIL 5 NIL NIL NIL 0)))
