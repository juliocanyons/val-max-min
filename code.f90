PROGRAM maximo_minimo
  IMPLICIT NONE

  INTEGER, PARAMETER :: MAX_SIZE=10
  INTEGER, DIMENSION(10) :: input
  INTEGER :: nvals,j,temp,ilarge, ismall

  WRITE(*,101) 
101 FORMAT ("Ingresa el numero de datos a leer")
  READ (*,*) nvals

  principal: IF (nvals <= MAX_SIZE) THEN
     leer:     DO j=1,nvals
        WRITE(*,99) j
99      FORMAT("Ingresa valor: " , I2)
        READ(*,*) input(j)
     ENDDO leer

     temp= input(1)
     ilarge=1
     mayor: DO j=2, nvals
        IF (input(j) > temp) THEN
           temp= input(j)
           ilarge=j
     ENDIF
     
  END DO mayor

  temp= input(1)
  ismall=1
  menor: DO j=2, nvals
     IF (input(j) < temp) THEN
        temp=input (j)
        ismall = j
     ENDIF
     
  ENDDO menor
  
  DO j=1, nvals
     IF (ismall== j) THEN
        WRITE(*,105) ismall
105     FORMAT ("Input es : " , I2, " SMALLEST : ")
     ELSEIF (ilarge == j) THEN
        WRITE(*,106) ilarge
106     FORMAT ("Input es : " , I2, " LARGEST : " )
     ELSE
        WRITE (*,*) input(j)
     ENDIF
  ENDDO

  

  
     
  ELSE
     WRITE(*,102)
102  FORMAT ("Existen mucho valores")
  ENDIF principal

  

  
END PROGRAM maximo_minimo
