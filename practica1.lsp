(defun conversorGR (x)
    (/ (* x pi ) 180)
)

(defun inicia-patrons()
    (inicia-patronsCub)
    (iniciaEscena)
)

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
