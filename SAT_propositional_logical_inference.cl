;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;	
;;; SAT_propositional_logical_inference
;;; October 2021
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defconstant +bicond+ '<=>)
(defconstant +cond+ '=>)
(defconstant +and+ '^)  

(defconstant +or+ 'v)
(defconstant +not+ '¬)  


(defun truth-value-p (x)
  (or (eql x t) (eql x nil)))

(defun valor-T-NIL (x)
  (setf PPNIL 'PPNIL)
  (or (eql x t) (eql x PPNIL)))

(defun valor-NIL (x)
  (setf PPNIL 'PPNIL)
   (eql x PPNIL))

(defun valor-T (x)
   (eql x T))


(defun unary-connector-p (x)
  (eql x +cond+))

(defun binary-connector-p (x)
  (or (eql x +bicond+) (eql x +cond+)))

(defun binario-connector-p (x)
   (eql x +bicond+) )


(defun n-ary-connector-p (x)
  (or (eql x +and+) (eql x +or+)))

(defun connector-p (x)
  (or (unary-connector-p x)
    (binary-connector-p x)
      (n-ary-connector-p x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extrae-simbolos-proposicion (propos)
  (if (null propos) 
      ()
    ;;; modificado ya que no extraia en caso de listas dentro de la proposicion
    (if (listp (first propos))
               (extrae-simbolos-proposicion (first propos))
               
    (if (and  (atom(first propos))   ( not (connector-p(first propos))) ( not (eql (first propos) "¬")))
        
     (append (list (first propos)) (extrae-simbolos-proposicion (rest propos)))
      (extrae-simbolos-proposicion (rest propos)))
               )
 )

  )

(extrae-simbolos-proposicion '(<=> H ¬B))

(defun extrae-simbolos-proposiciones (Kb)
  (if (null Kb) 
      ()  
       (append (extrae-simbolos-proposicion (first kb)) (extrae-simbolos-proposiciones (rest kb)))
     )
 )

(extrae-simbolos-proposiciones '((<=> H ¬B) (<=> J ¬Q)))
( setf kb '(( <=> BB ( ^ P  ¬M))))
(extrae-simbolos-proposiciones kb)


(defun extrae-simbolos-proposiciones-sin-negados (lista_con_negados)
  (if (null  lista_con_negados )
      ()  
   ( if (string= (subseq (string (first lista_con_negados)) 0 1) "¬")
              (append (list (read-from-string (subseq (string (first lista_con_negados)) 1))) (extrae-simbolos-proposiciones-sin-negados(rest lista_con_negados)))
              (append  (list(first lista_con_negados)) (extrae-simbolos-proposiciones-sin-negados(rest lista_con_negados )))
                  
     )
    )
  )

(setf lista-sin-negados  '(H ¬B J ¬Q ))

(write (second lista-sin-negados) )
(string (second lista-sin-negados) )
;;; http://www.cs.rochester.edu/u/schubert/247-447/symbols-in-lisp.html
(subseq (string (second lista-sin-negados)) 0 1)
(read-from-string (subseq (string (second lista-sin-negados)) 1))
(read-from-string (string (second lista-sin-negados)) : start 1)
;;; http://www.reduce-algebra.com/lisp-docs/allman1se20.html
(eq (first(list (second  lista-sin-negados))) "¬")  
(eq (subseq (string (second lista-sin-negados)) 0 1) "¬")
;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node166.html
(string= (subseq (string (second lista-sin-negados)) 0 1) "¬")

(extrae-simbolos-proposiciones-sin-negados lista-sin-negados)

;;; remove duplicates from a list
;;; https://living-sun.com/es/common-lisp/102563-remove-duplicate-strings-from-a-list-common-lisp.html
;;; (remove-duplicates "("one" "two" "two" "three"))

(defun extrae-simbolos (kb)
  
      (if (not (listp (first kb)))
       kb    
 (remove-duplicates (extrae-simbolos-proposiciones-sin-negados (extrae-simbolos-proposiciones Kb)))
 
)   
   )
(time  (extrae-simbolos '((<=> H ¬B) (<=> J ¬Q) (<=> B ¬Q))))
;;; Outcome  (H J B Q)
(extrae-simbolos '(A))
;;; (A)
(extrae-simbolos '((v ¬A A B ¬B)))
;;; (A B)
(time (extrae-simbolos '((=> A ¬H) (<=> P (^ A ¬H)) (=> H P))))
;;; (A P H)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun reduce-T-NIL (lst-a-reducir  lst-reductor)
  ;;; in case the element inside first is a list 
  ;;; a recursive call is made
  

 (if (connector-p (first lst-a-reducir))
      ( append (first lst-a-reducir) (reduce-T-NIL (second lst-a-reducir) lst-reductor ))
    ()
    )
    
       
  
(if  (rest lst-a-reducir)
 
     (append ( list (substitute (second lst-reductor) (first lst-reductor) (first lst-a-reducir))) (reduce-T-NIL (rest lst-a-reducir)  lst-reductor))

     (append (list (substitute (second lst-reductor) (first lst-reductor)  (first lst-a-reducir))))
        
    
    )
 
   
  )

(setf lst-a-reducir '((=> A ¬H) (<=> P (^ A ¬H)) (=> H P)))
(setf lst-reductor '(A T))
;;; (setf lst-a-reducir '((=> A ¬H) (<=> A (^ A ¬H)) (=> H A)))

(reduce-T-NIL lst-a-reducir lst-reductor)

;;; https://www.google.es/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&ved=2ahUKEwin2bi29ePkAhWZgVwKHeq1DisQFjADegQIBRAB&url=http%3A%2F%2Fclhs.lisp.se%2FBody%2Ff_sbs_s.htm&usg=AOvVaw1ApiimfGFF85jLBSZDfmp6
(substitute 9 4 '(1 2 4 1 3 4 5))
;;;  (1 2 9 1 3 9 5)
(setf lst-reductor '(4 9))
(write (second lst-reductor))
(setf lst-a-reducir '((1 2 4 1 3 4 5)))
(reduce-T-NIL lst-a-reducir lst-reductor)

(setf lst-a-reducir '((=> A ¬H) (<=> P (^ A ¬H)) (=> H P)))
(setf lst-reductor '(A T))
(setf lst-a-reducir '((=> A ¬H) (<=> A (^ A ¬H)) (=> H A)))

(reduce-T-NIL lst-a-reducir lst-reductor)

;;; (setf cadena-lst-a-reducir  (string lst-a-reducir))
;;; (substitute (second lst-reductor) (first lst-reductor) (second lst-a-reducir))
;;; (reduce-T-NIL-listas lst-a-reducir  lst-reductor)


(defun Inversa (lst-reductor)
  ;;;CANNOT USE NIL AS IT IS CONFUSED WITH THE END OF LIST AND
  ;;; THE BROKEN LISTS APPEAR, INSTEAD A FICTITIONAL VALUE PPNIL IS USED
  ;;; THAT THE NIL TREATMENT WILL RECEIVE IN SUBSEQUENT REDUCTIONS
  
  (setf PPNIL 'PPNIL)
    (setf lsegundo T)
    (if( eql (second lst-reductor) T)
        (setf lsegundo PPNIL)
      (setf lsegundo T)
      )
    

  (append (list (read-from-string(concatenate 'string "¬" (string(first lst-reductor)))) lsegundo))
 
  )
(setf lst-reductor '(A T))
(Inversa lst-reductor)

(defun reduce-T-NIL-proposicion-Ant (propos lst-reductor)
           
  (if (eq (first propos) NIL) 
      
      ()
    ;;;
    ;;; cond better than if successive https://www.afralisp.net/autolisp/tutorials/cond-vs-if.php
    ;;; 
    (cond  
         ( (and  (atom(first propos))   ( not (connector-p(first propos)))            
                (string= (subseq (string ( first propos)) 0 1) "¬"))
         
            ( if (eql (read-from-string(subseq (string ( first propos)) 1 )) (first lst-reductor))
                 (append(list (second (Inversa lst-reductor))) (reduce-T-NIL-proposicion (rest propos)  lst-reductor))
         
                       (nconc (list (first propos)) (reduce-T-NIL-proposicion (rest propos) lst-reductor) )
            )

            );
           ( (and  (atom(first propos))   ( not (connector-p(first propos)))             ;;; ( not (connector-p(first propos))))
                  ( not (eql (first propos) "¬")))
            (append     (substitute (second lst-reductor) (first lst-reductor) (list (first propos))) (reduce-T-NIL-proposicion (rest propos)  lst-reductor))
        
            );
          ( (listp (first propos))
           (list (reduce-T-NIL-proposicion (first propos) lst-reductor))
          );
          (t
           (append (list (first propos)) (reduce-T-NIL-proposicion (rest propos) lst-reductor) )

       );
  ) ; fin del cond
 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reduce-T-NIL-proposicion (propos lst-reductor)
 
         
  (if (eq (first propos) NIL) 
      
      ()
    ;;;
    ;;; cond better than if successive https://www.afralisp.net/autolisp/tutorials/cond-vs-if.php
    ;;; 
    (cond  
         ( (and  (atom(first propos))   ( not (connector-p(first propos)))             
                (string= (subseq (string ( first propos)) 0 1) "¬"))
          
        
            ( if (eql (read-from-string(subseq (string ( first propos)) 1 )) (first lst-reductor))
                 (append(list (second (Inversa lst-reductor))) (reduce-T-NIL-proposicion (rest propos)  lst-reductor))
                        (nconc (list (first propos)) (reduce-T-NIL-proposicion (rest propos) lst-reductor) )
            )

            )
           ( (and  (atom(first propos))   ( not (connector-p(first propos)))             
                  ( not (eql (first propos) "¬")))
            (append     (substitute (second lst-reductor) (first lst-reductor) (list (first propos))) (reduce-T-NIL-proposicion (rest propos)  lst-reductor))
        
            )
          ( (listp (first propos))
           (append (list (reduce-T-NIL-proposicion (first propos) lst-reductor))(reduce-T-NIL-proposicion (rest propos) lst-reductor))

          

           );
          (t
           (append (list (first propos)) (reduce-T-NIL-proposicion (rest propos) lst-reductor) )

       );
  ) ; end of cond
 )
  )

(setf lst-reductor '(B T))
(setf propos '(<=> PP  (v ¬B (^ B  M)))  )
(reduce-T-NIL-proposicion propos lst-reductor)

(setf lst-reductor '(B T))
(setf propos '(<=> PP  (v (^ B  M)  ¬B))  )
(reduce-T-NIL-proposicion propos lst-reductor)


(setf lst-reductor '(H PPNIL))
(setf propos '(¬H))
(substitute (second lst-reductor) (first lst-reductor) (list (first propos)))
 ( if (string= (subseq (string ( first propos)) 0 1) "¬")

    (append (substitute (second (Inversa lst-reductor)) (first (Inversa lst-reductor)) (list (first propos)) :test 'equal))
   
   )





(setf lst-reductor '(H PPNIL))
(setf propos '(¬H))
(substitute (second lst-reductor) (first lst-reductor) (list (first propos)))
 ( if (string= (subseq (string ( first propos)) 0 1) "¬")

    (append (substitute (second (Inversa lst-reductor)) (first (Inversa lst-reductor)) (list (first propos)) :test 'equal))
   (write "no hace el if")
   )



(defun reduce-T-NIL-proposicion-todos-los-reductores (propos lst-reductor)
  (if (eq (first lst-reductor) NIL) 
      propos
 
        (reduce-T-NIL-proposicion-todos-los-reductores (reduce-T-NIL-proposicion propos  (first lst-reductor)) (rest lst-reductor)    )  


      )

  )


;;; (setf lst-a-reducir '((=> A ¬H) (<=> P (^ A ¬H)) (=> H P)))
(setf lst-reductor '((A T) (H T) (P PPNIL)))
(setf lst-a-reducir '(=> A H))
(setf lst-a-reducir '(=> H P))
(setf lst-a-reducir '(=> A ¬H)) 
(reduce-T-NIL-proposicion-todos-los-reductores lst-a-reducir lst-reductor)


(defun reduce-T-NIL-proposiciones-todos-los-reductores (proposiciones lst-reductor)
  (if (eq (first proposiciones) NIL) 
      ()
      
     (append (list (reduce-T-NIL-proposicion-todos-los-reductores (first proposiciones)  lst-reductor)  )  (reduce-T-NIL-proposiciones-todos-los-reductores (rest proposiciones) lst-reductor))



      )

  )

(setf lst-reductor '((A T) (H T) (P PPNIL)))
(setf proposiciones '((=> A H) (=> H P)))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

(setf proposiciones '((=> A H) (<=> P (^ A H)) (=> H P)))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

(setf proposiciones '((=> ¬H A) (<=> P (^ A ¬H)) (=> H P)))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

(setf proposiciones '((=> A ¬H  ) (<=> P (^ A ¬H)) (=> H P)))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

(setf proposiciones '((=> A ¬H  ) (<=> (^ A ¬H) P) (=> H P)))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

(setf lst-reductor '((B T)))
(setf proposiciones '((<=> PP  (v ¬B (^ B  M)  ))  ))
(reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCION PARA DETERMINAR SI UNA PROPOSICIÓN REDUCIDA A VALORES T PPNIL 
;;; ES CIERTA O FALSA 
(defun valora-proposicion-reducida-T-NIL (propos)
 

(setf PPNIL 'PPNIL)
         
     ;;
    ;;; ES MEJOR cond que if sucesivos https://www.afralisp.net/autolisp/tutorials/cond-vs-if.php
    ;;; 
(cond 
       
     ( (and (unary-connector-p (first propos)) (listp (second  propos)) (valor-T-NIL(third propos)) )     
                       
    
      (valora-proposicion-reducida-T-NIL (list (first propos) (valora-proposicion-reducida-T-NIL (second propos)) (third propos)))

      )

                 
     ( (and (unary-connector-p (first propos)) (valor-T-NIL(second propos)) (listp (third  propos)) )     
                       
    

       (valora-proposicion-reducida-T-NIL (list (first propos) (second propos) (valora-proposicion-reducida-T-NIL (third propos)) ))

      )

 
     ( (and (unary-connector-p (first propos)) (listp (second  propos)) (listp(third propos)) )     
                       
    
      (valora-proposicion-reducida-T-NIL (list (first propos) (valora-proposicion-reducida-T-NIL (second propos)) (valora-proposicion-reducida-T-NIL (second propos))))

      )

         
           ( (and (unary-connector-p (first propos)) (valor-T-NIL(second propos))       (valor-T-NIL(third propos)))     
                        (cond ( ( and (eql (second propos) T)  (eql (third propos) T))
                              
                      
                           T

                               ); 
                              ( ( and (eql (second propos) T)  (eql (third propos) PPNIL))
                                                    
                             PPNIL

                               );
                              (t
                             
                                 T


                               ); 

                              ); fin del cond segundo
            )
 ;;;;------------------------------------------------------------------------------------------------
 ;;;; BICONDICIONAL
   
     ( (and (binario-connector-p (first propos)) (listp (second  propos)) (valor-T-NIL(third propos)) )     
                       
    

      (valora-proposicion-reducida-T-NIL (list (first propos) (valora-proposicion-reducida-T-NIL (second propos)) (third propos)))

      )

     

            
     ( (and (binario-connector-p (first propos)) (valor-T-NIL(second propos)) (listp (third  propos)) )     
                       
    

       (valora-proposicion-reducida-T-NIL (list (first propos) (second propos) (valora-proposicion-reducida-T-NIL (third propos)) ))

      )

 
     ( (and (binario-connector-p (first propos)) (listp (second  propos)) (listp(third propos)) )     
                       
    

      (valora-proposicion-reducida-T-NIL (list (first propos) (valora-proposicion-reducida-T-NIL (second propos)) (valora-proposicion-reducida-T-NIL (second propos))))

      )

     

     
           ( (and (binario-connector-p (first propos)) (valor-T-NIL(second propos))       (valor-T-NIL(third propos)))     
                        (cond ( ( and (eql (second propos) T)  (eql (third propos) T))
                              
                      
                              
                                 T

                               ); 
                              ( ( and (eql (second propos) T)  (eql (third propos) PPNIL))
                                                    
                              
                                PPNIL

                               );
                              ( ( and (eql (second propos) PPNIL)  (eql (third propos) T))
                                                    
                             
                                PPNIL

                               );

                              (t
                           
                                 T


                               ); 

                              ); end of second cond
            )

 ;;;;************************************************************************************************
        ((and
          (n-ary-connector-p (car propos)))
         
          (setf listA (mapcar (lambda (x) (if (listp x) (valora-proposicion-reducida-T-NIL x) x) )propos))
             (if (eql (car listA) +or+)
                   (if (some #'valor-T (rest listA))
                       T
                       PPNIL
                     ) 
               ;;; caso and 
                    (if (every #'valor-T (rest listA))
                       T
                       PPNIL
                     ) 

               )
          )
           
         (t
               
      ;;;    
          PPNIL

             )
              
                      
          
         
     ) ; end of first cond

 
 
)
(setf PPNIL 'PPNIL)
(setf propos '(=> T PPNIL) )
(setf propos '(=> (=> PPNIL T) T ) )
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(=> (=> T T) PPNIL ) )
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(=> (=> T PPNIL) PPNIL ) )
(valora-proposicion-reducida-T-NIL propos)

(setf propos '(v (=> T T) T PPNIL ) )
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(v (=> T PPNIL) T PPNIL ) )
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(v (=> T PPNIL) PPNIL PPNIL ) )
(valora-proposicion-reducida-T-NIL propos)

(setf propos '(^ (=> T T) T T ) )
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(<=> T (^ T PPNIL)))
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(<=> T (^ T T)))
(valora-proposicion-reducida-T-NIL propos)
(setf propos '(<=> PPNIL (^ PPNIL PPNIL)))
(valora-proposicion-reducida-T-NIL propos)

;;;>>(interpretacion-modelo-p '((A t) (P nil) (H nil)) '((<=> A (¬ H)) (<=> P (^ A H))
;;; 

(setf lst-reductor '((A PPNIL) (P PPNIL) (H T)))
(setf proposiciones '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf lista-t-ppnil (reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor))
(valora-proposicion-reducida-T-NIL (first lista-t-ppnil))
(valora-proposicion-reducida-T-NIL (second lista-t-ppnil))
(valora-proposicion-reducida-T-NIL (third lista-t-ppnil))

(setf lst-reductor '((A T) (P PPNIL) (H PPNIL)))
(setf proposiciones '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf lista-t-ppnil (reduce-T-NIL-proposiciones-todos-los-reductores proposiciones lst-reductor))
(valora-proposicion-reducida-T-NIL (first lista-t-ppnil))
(valora-proposicion-reducida-T-NIL (second lista-t-ppnil))
(valora-proposicion-reducida-T-NIL (third lista-t-ppnil))

;;; An intermediate step for Exercise 4.4
;;; from propositions reduced to values T or PPNIL 
;;; It is evaluated if the proposition is fulfilled, if it is not fulfilled, it is warned 
;;; the position and the next proposition is not followed, otherwise it is valued
;;; the following proposition
(defun interpretacion-modelos-p ( kb conta)
  
  (cond ( (eq (first kb) NIL)                              
                      
                           
                                 T

                               ); 
                              
        ( (eq (valora-proposicion-reducida-T-NIL (first kb)) PPNIL)
                              
                      ;;; instruccion format https://en.wikipedia.org/wiki/Format_(Common_Lisp)
                           ;;;    (format t " KNOWLEDGE BASE PROPOSITION ~ D FAILS" conta)
                                 PPNIL

                               );                      
        
        (t
           
         (setf conta (+ conta 1))
         
         ( interpretacion-modelos-p  (rest kb) conta)
                                 


                               ); 

                              ); end of  cond 
  
  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun genera-lista-interpretaciones-paso-a-paso ( lst-simbolos)
  (setf PPNIL 'PPNIL)
 
  ;;; http://www.togores.net/vl/curso/lisp/bases/funciones/construccion-de-listas 
  
  
  (setq lista-interpretaciones (cons (list (first lst-simbolos) T)  NIL ))
  (setq lista-interpretaciones3 (list lista-interpretaciones))
    (setq lista-interpretaciones4 (cons (list (list (first lst-simbolos) PPNIL))  lista-interpretaciones3 ))

  (setq pp (cons (list (second lst-simbolos) T) (first lista-interpretaciones4 )))
  (setq pp1 (cons (list (second lst-simbolos) PPNIL) (first lista-interpretaciones4 )))
  (setq pp2 (cons (list (second lst-simbolos) T) (second lista-interpretaciones4 )))
  (setq pp3 (cons (list (second lst-simbolos) PPNIL) (second lista-interpretaciones4 )))
  (setq pptotal1 (cons pp (list pp1)))
  
  ( setq pptotal2 (append pptotal1  (list pp2)))
  (append pptotal2  (list pp3))
  )

(defun genera-lista-interpretaciones-un-simbolo (lst-lst-simbolo lista-interpretaciones lista-interpretaciones-acu)
  (setf PPNIL 'PPNIL)
 
  ;;; http://www.togores.net/vl/curso/lisp/bases/funciones/construccion-de-listas 
  (cond ( (null   lista-interpretaciones )
      

        lista-interpretaciones-acu 
    );   
         (t
    
    (setq pp (cons lst-lst-simbolo (first lista-interpretaciones )))
       (setq  lista-interpretaciones-acu    (append  lista-interpretaciones-acu (list pp)))
   
   (genera-lista-interpretaciones-un-simbolo lst-lst-simbolo (rest lista-interpretaciones) lista-interpretaciones-acu)

)
  
)

)

  

(defun genera-lista-interpretaciones-siguientes ( lista-interpretaciones lst-simbolos)
  (setf PPNIL 'PPNIL)
  

  ;;; http://www.togores.net/vl/curso/lisp/bases/funciones/construccion-de-listas 
   (cond ( (null   lst-simbolos )
        lista-interpretaciones 
    );   
  (t

   (setq lista-interpretaciones1 (genera-lista-interpretaciones-un-simbolo (list (first lst-simbolos) T) lista-interpretaciones '()))
   (setq lista-interpretaciones2 (genera-lista-interpretaciones-un-simbolo (list (first lst-simbolos) PPNIL) lista-interpretaciones '()))
   
   (setq lista-interpretaciones3 (append lista-interpretaciones1 lista-interpretaciones2))


   (genera-lista-interpretaciones-siguientes  lista-interpretaciones3 (rest lst-simbolos))
)
  
)

)

(defun genera-lista-interpretaciones ( lst-simbolos)
  (setf PPNIL 'PPNIL)
 
  ;;; http://www.togores.net/vl/curso/lisp/bases/funciones/construccion-de-listas 
  
  
  (setq lista-interpretaciones (cons (list (first lst-simbolos) T)  NIL ))
  (setq lista-interpretaciones3 (list lista-interpretaciones))
    (setq lista-interpretaciones4 (cons (list (list (first lst-simbolos) PPNIL))  lista-interpretaciones3 ))
  (genera-lista-interpretaciones-siguientes lista-interpretaciones4 (rest lst-simbolos))
    


  
)

(genera-lista-interpretaciones  '(P I L))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

(defun interpretacion-modelo-p (interp kb)
 ;;; the propositions are reduced to T PPNIL values 
 (setf listA (reduce-T-NIL-proposiciones-todos-los-reductores kb interp))
 ;;; propositions are valued, ending when one is false
 (setf conta 0)
 (interpretacion-modelos-p listA conta)

  )

;;; Tests
(setf PPNIL 'PPNIL)
(setf interp '((A PPNIL) (P PPNIL) (H T)))
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
( interpretacion-modelo-p interp kb) 

(setf interp '((A T) (P PPNIL) (H PPNIL)))
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
( interpretacion-modelo-p interp kb) 

(setf interp '((BB T) (PP T) (MM T) (P T) (M PPNIL)  (B PPNIL)))

(setf kb '((<=> BB  (^ P  ¬M))  (<=> PP  (v (^ B  M)  ¬B))  (<=> MM  (^ (v B  P)  ¬M))))
( interpretacion-modelo-p interp kb) 

(setf kb '((<=> BB  (^ P  ¬M)) ))
( interpretacion-modelo-p interp kb)

(setf kb '( (<=> PP  (v (^ B  M)  ¬B))  ))
( interpretacion-modelo-p interp kb)
;;;
;;;  Ejercicio 4.5
;;;
(defun explora-todas-interpretaciones (lst-interp kb lst-interp-encontradas)
 (setf PPNIL 'PPNIL)

  (cond ( (eq (first lst-interp) NIL)                              
                      
                               (format t " FINISH THE SEARCH FOR INTERPRETATIONS THAT ARE MODELS, ARE:" )
                                 lst-interp-encontradas
                               ); 
                              
                           
        
        (t
           
                   (cond ( (eq (interpretacion-modelo-p (first lst-interp) kb) PPNIL)                              
                      
                               ( explora-todas-interpretaciones (rest lst-interp) kb lst-interp-encontradas)
                               );                              
                        
        
                         (t
                   

                          (setq  lst-interp-encontradas    (append  lst-interp-encontradas (list (first lst-interp))))

                      
                          (explora-todas-interpretaciones (rest lst-interp) kb lst-interp-encontradas)
                           ); fin del t

                      ); end of cond 
           ); end of t

      ); end of cond 

  )
(defun explora-todas-interpretaciones-sin-Acumulado (lst-interp kb)
  (setf PPNIL 'PPNIL)
  (cond ( (eq (first lst-interp) NIL)                              
                      
                               (format t " FINISH THE SEARCH FOR INTERPRETATIONS THAT ARE MODELS " )
                                 
                               ); 
                              
                           
        
        (t
           
                   (cond ( (eq (interpretacion-modelo-p (first lst-interp) kb) PPNIL)                              
                      
                               ( explora-todas-interpretaciones (rest lst-interp) kb)
                               );                              
                        
        
                         (t
                          (format t "~%Found model interpretation of the base ~A~%" (first lst-interp))
                          (append(list (first lst-interp)) (explora-todas-interpretaciones (rest lst-interp) kb))


                    
                           ); fin del t

                      ); end of cond 
           ); end of t

      ); end of cond 

  )


(defun encuentra-modelos-p (kb)
  (setf listA (extrae-simbolos kb))
  (setf listB (genera-lista-interpretaciones listA))
  (explora-todas-interpretaciones listB kb '()) ;;; version with a list in which it is accumulating
 
  )
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
( time ( encuentra-modelos-p kb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ejercicio 4.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun consecuencia-p (prop kb)
  ;;; add the proposition that arises to the base and look for 
  ;;; if there is any model that fulfills the new base with the new proposition added
 ( setf listA (append (list prop) kb))
  (if (encuentra-modelos-p listA)
      T
      NIL)
  )
;;;
;;;   FOR THE MOMENT, CLAUSES WITH A SINGLE VARIABLE MUST PREcede them of ^ or the union sign
;;;  NEITHER CAN GO together ^ to the variable, you have to leave a space


;;; T
(setf prop '(^ A ))
(setf kb '((^ A)))
(consecuencia-p prop kb)


;;; NIL
(setf prop '(^ A ))
(setf kb '((^ ¬A)))
(consecuencia-p prop kb)


;;; gives T
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ ¬H ))
(time (consecuencia-p prop kb))


;;; gives T
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ ¬P ))
(consecuencia-p prop kb)

;;;  T 
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ ¬H ¬P))
(consecuencia-p prop kb)


;;; this is T although in the solutions it is shown how NIL fulfills it (A T) (H PPNIL) (PPNIL)
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ A  ¬H ¬P))
(consecuencia-p prop kb)

;;; This, on the other hand, does not comply with it and gives NIL
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ A  H ¬P))
(consecuencia-p prop kb)

;;; neither this 
(setf kb '((=> A ¬H  ) (<=> P (^ A H)) (=> H P)))
(setf prop '(^ A  ¬H P))
(consecuencia-p prop kb)


;;; T
( setf kb '(( <=> BB ( ^ P  ¬M))))
(setf interp '((BB T) (P T) (M PPNIL)))
( interpretacion-modelo-p interp kb) 

( setf kb '(( <=> BB ( ^ P  ¬M))))
(setf listA (extrae-simbolos kb))

(setf listB (genera-lista-interpretaciones listA))




( setf kb '(( <=> BB ( ^ P  ¬M))))
( encuentra-modelos-p kb) 
(defun extrae-simbolos (kb)
  
      (if (not (listp (first kb)))
       kb    
 (remove-duplicates (extrae-simbolos-proposiciones-sin-negados (extrae-simbolos-proposiciones Kb)))
 
)   
   )
(time  (extrae-simbolos '((<=> H ¬B) (<=> J ¬Q) (<=> B ¬Q))))
;;; resultado  (H J B Q)
(extrae-simbolos '(A))
;;; (A)
(extrae-simbolos '((v ¬A A B ¬B)))
;;; (A B)
(time (extrae-simbolos '((=> A ¬H) (<=> P (^ A ¬H)) (=> H P))))
;;; (A P H)

;******************************************************************************************
;  TESTS
;******************************************************************************************

;******************************************************************************************
;  Tests section 7 https://github.com/bertuccio/inferencia-logica-proposicional
;******************************************************************************************

;;; Knowledge Base
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM)))
(setf listA (extrae-simbolos kb))
(setf listB (genera-lista-interpretaciones listA))

;;; Is it possible that the three BB PP MM have told the truth?
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM)))
( encuentra-modelos-p kb)

;;; assuming that Blas has told the truth (BB), Pedro has told the truth (PP) and Manuel has told the truth (BB),
;;; Pedro (P) is guilty must give T
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM) (^ P)))
( encuentra-modelos-p kb)

;;; Manuel (M) is not guilty must give T
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM) (^ ¬M)))
( encuentra-modelos-p kb)


;;; Manuel (M) is guilty must give NIL
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM) (^ M)))
( encuentra-modelos-p kb)

;;; Blas (B) is not guilty must give T
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ BB) (^ PP)  (^ MM) (^ ¬B)))
( encuentra-modelos-p kb)


;;; Is it possible that all three are guilty? B, P, M gives T, so it is possible
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ B) (^ P)  (^ M)))
( encuentra-modelos-p kb)

;;; continuing with the assumption that all three are guilty. Did Manuel lie? (MM), evaluate to T

( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ B) (^ P)  (^ M) (^ ¬MM)))
( encuentra-modelos-p kb)

;;; continuing with the assumption that all three are guilty. Did Blas lie? (BB), evaluate T
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ B) (^ P)  (^ M) (^ ¬BB)))
( encuentra-modelos-p kb)

;;; continuing with the assumption that all three are guilty. Did Pedro tell the truth? (PP), evaluate T
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ B) (^ P)  (^ M) (^ PP)))
( encuentra-modelos-p kb)


;;;Is it possible that only one lied, should give NIL
;;;
( setf kb '(( <=> BB ( ^ P  ¬M))  ( <=> PP (v (^ B  M)  ¬B))  ( <=> MM  (^ (v B  P)  ¬M))  (^ ¬BB PP MM) (^ ¬PP BB MM)  (^ ¬MM PP MM) ))
( encuentra-modelos-p kb)

;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
; Test examples link to netlogo at http://www.cs.us.es/~fsancho/?e=120.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Example 0 of netlogo http://www.cs.us.es/~fsancho/?e=120.
;;;;;; It has a solution with Q=1
(setq cnfp0 '((v P Q) (v ¬P Q)))
(time (encuentra-modelos-p cnfp0))

;;;;;; Example 1 of  netlogo at http://www.cs.us.es/~fsancho/?e=120.
;;;;;; Has no solution
(setq cnfp1 '((v P Q) (v ¬P Q) (v ¬Q)))
(time (encuentra-modelos-p cnfp1))

;;;;;; Example 2 of netlogo at http://www.cs.us.es/~fsancho/?e=120.
;;;;;; Has no solution
(setq cnfp2 '((v P Q) (v ¬P Q) (v P ¬Q) (v ¬P ¬Q)))
(time (encuentra-modelos-p cnfp2))



;;;;;; Example 3 of netlogo at http://www.cs.us.es/~fsancho/?e=120.
;;; T=0 Q=0 and rest to 1 fulfills it
;;; T=0, S=1,R=0,P=1,Q=0 lo fulfills it
;;; Solutions from encuentra-modelos
;;; (((T T) (S T) (R T) (Q PPNIL) (P T)) ((T T) (S T) (R PPNIL) (Q PPNIL) (P PPNIL))
;;; ((T T) (S T) (R PPNIL) (Q PPNIL) (P T)) ((T PPNIL) (S T) (R T) (Q PPNIL) (P T))
;;; ((T PPNIL) (S T) (R PPNIL) (Q PPNIL) (P T)) ((T PPNIL) (S PPNIL) (R T) (Q PPNIL) (P T))
;;;  ((T PPNIL) (S PPNIL) (R PPNIL) (Q PPNIL) (P T)))

( setq cnfp3 '((v P ¬Q R ¬S ¬T) (v P  R  S  T ) (v P ¬R S) (v P R ¬S T) (v P ¬R ¬S) (v ¬P ¬Q) (v ¬Q R S ¬T) (v S ¬T)))
(time (encuentra-modelos-p cnfp3))


;;;;;; Example 4 of netlogo at http://www.cs.us.es/~fsancho/?e=120. Has no solution
(setq cnfp4 '((v P Q R) (v ¬P Q) (v ¬Q R) (v ¬R) (v P R)))
(time (encuentra-modelos-p cnfp4))

;;;;;; Example 5 of netlogo at http://www.cs.us.es/~fsancho/?e=120. Has no solution
(setq cnfp5 '((v ¬P ¬Q R) (v ¬S T) (v ¬T P) (v S) (v ¬S U) (v ¬U Q) (v ¬R)))
(time (encuentra-modelos-p cnfp5))

;;;;;; Example 6 of  netlogo at http://www.cs.us.es/~fsancho/?e=120. Has no solution
(setq cnfp6 '((v P Q) (v Q R) (v R W) (v ¬R ¬P) (v ¬W ¬Q) (v ¬Q ¬R)))
(time (encuentra-modelos-p cnfp6))

;;; Example 7 of netlogo at http://www.cs.us.es/~fsancho/?e=120
;;; Has no solution
(setq cnfp7 '( (v ¬P ¬Q R) (v ¬P ¬X W) (v ¬P ¬W U)  (v ¬X Q) (v X) (v P) (v ¬U)))
(time (encuentra-modelos-p cnfp7))
