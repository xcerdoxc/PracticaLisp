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

(defun crea-figura (nom patró color)
    (putprop 'nom 'color 'colorF)
    (putprop 'nom 'cub   'patro)
    (afegeixFigura (nom))
)
(defun afegeixFigura(f)
    (cons (get 'escena 'figura) f)
)
(defun iniciaEscena() 
    (putprop 'escena '() 'figura)
)


;pau
; elimina de la llista de figures posicio f
;sobreescribim el valor de figures a l'escena
(defun borra-figura (f)
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)
)

;pau
; repintam la figura f, amb el color del fons (blanc)
(defun cls-figura (f)
    ()
)

;pau
;borra tot el contingut de l’escena (i de la pantalla)
(defun borra-figures
    ;
    (colour 255 255 255) (cls)     ;fill blanc tot, aixo nomes porra la pantalla
)