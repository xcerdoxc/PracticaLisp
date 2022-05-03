(defun conversorGR (x)
    (/ (* x pi ) 180)
)

(defun inicia-patrons()
    (inicia-patronsCub)
    (iniciaEscena)
)

;Crea les figura del Cub
(defun inicia-patronsCub()
    (putprop 'cub '(
                    (0.5 0 0.5)   ;Punto 1 
                    (-0.5 0 0.5)  ;Punto 2
                    (0.5 0 -0.5)  ;Punto 3
                    (-0.5 0 -0.5) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (0.5 1 0.5)   ;Punto 5 
                    (-0.5 1 0.5)  ;Punto 6
                    (0.5 1 -0.5)  ;Punto 7
                    (-0.5 1 -0.5) ;Punto 8
                  ) 'puntsC  
    )
    (putprop 'cub '(
                        (1 2)
                        (1 5)
                        (1 3)
                        (2 4)
                        (2 6)
                        (3 4)
                        (3 7)
                        (4 8)
                        (5 6)
                        (5 7)
                        (6 8)
                        (7 8)
                    ) 'arestesC
    )
    (putprop 'cub '(
                        (1 2 3 4)
                        (1 2 6 5)
                        (1 3 5 7)
                        (2 4 6 8)
                        (3 4 7 8)
                        (5 6 7 8)

                  ) 'caresC
    )
)

(defun crea-figura (nom patro color)
    (putprop nom color 'colorF)
    (putprop nom patro 'patroFigura)
    (putprop nom '(
                    (1 0 0 0)
                    (0 1 0 0)
                    (0 0 1 0)
                    (0 0 0 1)
                )
                'transformacions
    )
    (afegeixFigura 'nom)
)

(defun afegeixFigura(f)
    (putprop 'escena (cons 'f (get 'escena 'figures) ) 'figures)
)

(defun iniciaEscena() 
    (putprop 'escena '() 'figura)
)


; elimina de la llista de figures posicio f
; sobreescribim el valor de figures a l'escena
(defun borra-figura (f)
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)
)

; repintam la figura f, amb el color del fons (blanc)
(defun cls-figura (f)
    (crea-figura (aux (get f 'puntos) (255 255 255)))                   ; cream la figura a base dels punts passats per f
    (pinta-figura aux)                                                  ; pintam la figura aux      
    (putprop 'escena  (delete aux (get 'escena 'figures)) 'figures)     ; eliminam la figura aux de l'escena
)

;borra tot el contingut de l'escena (i de la pantalla)
(defun borra-figures
    (cond ((null l) nil)
        (t  (borra_figura (car (get 'escena 'figures)))
            (borra-figures)
        )
    )
)

(defun pinta-figura (f)
    ;cridam a la funcio que pintara les cares recursivament de f (passem llista de cares per parametre i la seva posicio)
    (eval (cons 'color (get 'f 'color)))
    (pinta-cares((get 'f 'cares ) (1)))
)

(defun pintar-cares ('caresL)
    ;mentres hi hagi cares, cridar a pinta-cares
    (cond ((null caresL) nil)
        (pinta-arestes(car 'caresL)) ;passem llista arestes a quina es vol anar de la subllista
    )
)

(defun pinta-arestes('arestesL)        
    (cond ((null arestesL) nil) 
        ;hem de anar recorrer la lliste de les arestes de la figura, segons el que en diu la llista pasada per parametre
        ((equal (get 'f 'arestes) cdr('arestesL))          
            ;si es la posicio correcte. de la llista de arestes, passam la subllista en forma de llista passada per parametre 
            (pinta-aresta('punts)))
        )
        ; no es la subllista que volem, passam a la seguent
        (pinta-arestes(car 'arestesL))  
    )    
)

(defun pinta-punts('puntsL)
    (cond ((null puntsL) nil) 
        ;hem de anar recorrer la lliste dels punts de la figura, segons la posicio dels punts   
        ((equal (get 'f 'arestes) cdr('arestesL))  
            ;pintam els punts correctes, nomes els dos primers de cada subllista (x y)
            (move x y) 
            (draw x y)
        )    
        ; no es la subllista que volem, passam a la seguent
        (pinta-punts(car 'puntL))          
    )
)








(defun translacio (dx dy dz) 
    '((1 0 0 0) (0 1 0 0) (0 0 1 0) (dx dy dz 1))
)

(defun escalat (ex ey ez) 
    '((ex 0 0 0) (0 ey 0 0) (0 0 ez 0) (0 0 0 1))
)

(defun rotax (a) 
    '((1 0 0 0)
     (0 (cos a) (- 0 (sin a)) 0) 
     (0 (sin a) (cos a) 0) 
     (0 0 0 1))
)

(defun rotay (a) 
    '(((cos a) 0 (- 0 (sin a)) 0) 
    (0 1 0 0) 
    ((sin a) 0 (cos a) 0) 
    (0 0 0 1))
)

(defun rotaz (a) 
    '(((cos a) (- 0 (sin a)) 0 0) 
    ((sin a) (cos a) 0 0) 
    (0 0 1 0) 
    (0 0 0 1))
)

(defun trasllada-figura (f x y z) )

(defun rota-figura (f x y z) )

(defun escala-figura (f x y z) )

(defun inicia-figura (f) )

;---------------------------------------------
;-------------      PART 2      --------------
;---------------------------------------------

(defun animacio (f) )

(defun anima-rotacio (f) )

(defun anima-translacio (f) )

(defun anima-escalat (f) )

;---------------------------------------------
;-------       FUNCIONS AUXILIARS      -------
;---------------------------------------------

(defun pescalar (l1 l2)
    (cond ((null l1) nil)
        (t (+ (* (car l1) (car l2)) (pescalar (cdr l1) (cdr l2))))
    )
)
        
(defun transposta (m)
    (cond ((null (car m)) m)
        ()
    )
)
