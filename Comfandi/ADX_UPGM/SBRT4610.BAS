!**************************************************
!Empresa       : Casa Informatica Unidad  Retail  *
!Programa      : SBRT4610.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Noviembre de 2001                *
!Observaciones : Modulo Impresion maquinas  4610  *
!						  *
!**************************************************
!* Modificaciones:                                                            

%ENVIRON T								    !Ambiente de terminal
%INCLUDE EAMP4VAJ.J86        				!4610 Printer
%INCLUDE EAMTRANS.J86        				!Transaction summary log
%INCLUDE EAMTSWKG.J86        				!Working Storage
!%INCLUDE JOURTSVA.011						! EJ
%INCLUDE EAMP4VGJ.J86	       				! Variables rutinas 4610

INTEGER*2 GLOBAL USER.SAVE(1) 				! Save area for subroutine calls
STRING GLOBAL    USER.SAVE$(1)				! Save area for subroutine calls
STRING GLOBAL    PRTLINE                   	! Variable EJ

SUB TSPREC01 EXTERNAL       				! print routine SMA
END SUB

SUB WRITE.TO.CR(PRINT.LINE$,FORMAT$) EXTERNAL
STRING PRINT.LINE$,FORMAT$
END SUB

FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL		! Rutina escritura EJ
  STRING QXL.TSUPEC21,PRTLINE				! Pilas Exito y Colsubsidio
END FUNCTION

FUNCTION Journal(X.PLINE$) PUBLIC
STRING Journal, X.PLINE$
  X.PLINE$ = QXL.TSUPEC21(X.PLINE$)
	Journal  = X.PLINE$
END FUNCTION 

SUB SALVAR.PRINT PUBLIC
      DIM USER.SAVE(12)
      USER.SAVE(1) = TS.LINETYPE
      USER.SAVE(2) = TS.LINEDATA
      USER.SAVE(3) = TS.LINEDATA2
      USER.SAVE(4) = TS.LINEDATA3
      USER.SAVE(5) = TS.PRT.PARM
      USER.SAVE(6) = TS.PRT.OPT
      USER.SAVE(7) = TS.PRT.SJDI
      USER.SAVE(8) = TS.SAVPRT.OPT
      USER.SAVE(9) = TS.XXMOD
      USER.SAVE(10) = TS.YYMOD
      USER.SAVE(11) = TS.ZMOD
      USER.SAVE(12) = TS.PRINTPRM
      DIM USER.SAVE$(5)
      USER.SAVE$(1) = TS.PRTBUF$
      USER.SAVE$(2) = TS.PRDATA$
      USER.SAVE$(3) = TS.FORMCR$
      USER.SAVE$(4) = TS.SJDATA$
      USER.SAVE$(5) = TS.SAVPRT$
END SUB
 
SUB RESTAURA.PRINT PUBLIC
      TS.LINETYPE   = USER.SAVE(1)
      TS.LINEDATA   = USER.SAVE(2)
      TS.LINEDATA2  = USER.SAVE(3)
      TS.LINEDATA3  = USER.SAVE(4)
      TS.PRT.PARM   = USER.SAVE(5)
      TS.PRT.OPT    = USER.SAVE(6)
      TS.PRT.SJDI   = USER.SAVE(7)
      TS.SAVPRT.OPT = USER.SAVE(8)
      TS.XXMOD      = USER.SAVE(9)
      TS.YYMOD      = USER.SAVE(10)
      TS.ZMOD       = USER.SAVE(11)
      TS.PRINTPRM   = USER.SAVE(12)
      TS.PRTBUF$    = USER.SAVE$(1)
      TS.PRDATA$    = USER.SAVE$(2)
      TS.FORMCR$    = USER.SAVE$(3)
      TS.SJDATA$    = USER.SAVE$(4)
      TS.SAVPRT$    = USER.SAVE$(5)
END SUB


!Funcion CISA.IMPRIMIR
!Parametros a pasar UE.CADENA$ = String a Imprimir
!  	            UE.STATION = Destino de la impresion
!                		4000H  - print on CR
!                		2000H  - print on SJ
!                		1000H  - print on DI
!                		0100H  - number of linefeeds (eg here = 1)
!

FUNCTION CISA.IMPRIMIR(UE.CADENA$,UE.STATION) PUBLIC
    STRING UE.CADENA$,LF$
    INTEGER*2 UE.STATION
    LF$ = CHR$(0DH)                                             					!
    IF UE.STATION = 1 THEN UE.STATION = 4100H
    IF UE.STATION = 2 THEN UE.STATION = 2100H
    IF UE.STATION = 3 THEN UE.STATION = 1100H
    IF PRT4610.ENABLE NE 0 THEN BEGIN									! Impresoras 4610
     If UE.CADENA$ = "" Then UE.CADENA$ = LF$								!
    ENDIF												!
    CALL SALVAR.PRINT    										! Salva variables impresion
    TS.LINETYPE   = 29											! Genera impresion al
    TS.SAVPRT$    = UE.CADENA$										! dispositivo requerido
    TS.SAVPRT.OPT = UE.STATION										!
    Call TSPREC01											!
    Call RESTAURA.PRINT    										! Restaura control aplicacion
END FUNCTION

FUNCTION UE.DIEJECT PUBLIC					        ! Expulsa papel en el 
    IF PRT4610.ENABLE NE 0 THEN \ 				    ! Document Insert
       WRITE #35;CHR$(1BH)+CHR$(6DH)\				!
    ELSE\							                !
       WRITE FORM "2C1 A0"; #35; CHR$(27), CHR$(12) !
END FUNCTION							!

FUNCTION UE.CRCUT PUBLIC					        ! Corta Papel en 
 STRING LF$                                         ! CR
    LF$ = CHR$(0DH)                                 !
    IF PRT4610.ENABLE NE 0 THEN BEGIN				! Impresoras 4610
        WRITE #34; CHR$(1BH)+CHR$(6DH)+LF$+LF$      !
    ENDIF ELSE \							        ! Impresoras Matriz
       WRITE FORM "2C1 A2"; #34; CHR$(27), CHR$(80)	!
END FUNCTION
