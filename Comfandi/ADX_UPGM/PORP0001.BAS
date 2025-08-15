!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PORP0001.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Portafolio de      *
!                productos			                  *
!**************************************************
! Modificaciones:

!--- Definicion de Variables de trabajo

%INCLUDE POSPVARI.BAS				  	        ! Variables programa
%INCLUDE EAMITEMR.J86				  	        ! Variables programa
%INCLUDE DMEXTR.J86          			      ! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS				  	        ! Rutinas Comunes

FUNCTION INICIO1
  CALL.ORDER% = 20                                           ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
  CALL DM.ERR(RET.ERR%,DISPD$)
  MSG$ = "MANTENIMIENTO PORTAFOLIO"
  CALL MSG.ERR(1,MSG$)				             ! Mensaje cabecera pantalla
  MSG$="Digite Codigo del Producto"
  CALL MSG.ERR(41,MSG$)				             ! Mensaje cabecera pantalla
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    CLOSE AREA2%					   !
    CALL SETF("0000000")				   !
    CALL CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

FUNCTION CAPTURA.HORA
   CALL MSG.ERR(41,"Digite Hora Formato 24 horas")
   DM.METPRA$= GET.DATOS
   IF (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT FUNCTION    ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT FUNCTION    ! Si presiona tecla ESC
   IF (VAL(DM.METPRA$) > 23 OR VAL(DM.METPRA$) < 0 ) THEN \
      DM.METPRA$ = " "
   WHILE DM.METPRA$ = " "
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.METPRA$ = GET.DATOS                               ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT FUNCTION ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT FUNCTION ! Si presiona tecla ESC
      IF (VAL(DM.METPRA$) > 24 OR VAL(DM.METPRA$) < 0 ) THEN \
         DM.METPRA$ = " "
   WEND							   !
   CALL MSG.ERR(41,"Digite Minuto del precio")
   DM.METPRB$= GET.DATOS
   IF (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT FUNCTION    ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT FUNCTION    ! Si presiona tecla ESC
   IF (VAL(DM.METPRB$) > 59 OR VAL(DM.METPRB$) < 0 ) THEN \
      DM.METPRB$ = " "
   WHILE DM.METPRB$ = " "                           
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.METPRB$ = GET.DATOS                               ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT FUNCTION ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT FUNCTION ! Si presiona tecla ESC
      IF (VAL(DM.METPRB$) > 59 OR VAL(DM.METPRB$) < 0 ) THEN \
         DM.METPRB$ = " "
   WEND							   !
   DM.METPRA$=RIGHT$("00"+DM.METPRA$,2)                    ! 
   DM.METPRB$=RIGHT$("00"+DM.METPRB$,2)                    ! 
END FUNCTION
!--- Fin procedimiento captura hora

FUNCTION CAPTURA.DATOS					      ! Funcion captura datos
   CALL MSG.ERR(41,"Digite Precio del Producto")
   DM.VLRCON$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE DM.VLRCON$ = " "
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.VLRCON$ = GET.DATOS                               ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   DM.VLRCON$=RIGHT$("000000000"+DM.VLRCON$,9)             ! 
!
! Dia Lunes
!*********** 
LUNES:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA01$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA01$,"10",1) = 0 OR DM.DIA01$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA01$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA1A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA1B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA1A$)                           ! Primera Hora
   J% = VAL(DM.HORA1B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO LUNES
   ENDIF 

!
! Dia Martes
!*********** 
MARTES:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA02$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA02$,"10",1) = 0 OR DM.DIA02$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA02$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA2A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA2B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA2A$)                           ! Primera Hora
   J% = VAL(DM.HORA2B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO MARTES
   ENDIF 
!
! Dia Miercoles
!*********** 
MIERCOLES:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA03$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA03$,"10",1) = 0 OR DM.DIA03$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA03$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA3A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA3B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA3A$)                           ! Primera Hora
   J% = VAL(DM.HORA3B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO MIERCOLES
   ENDIF 
!
! Dia Jueves
!*********** 
JUEVES:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA04$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA04$,"10",1) = 0 OR DM.DIA04$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA04$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA4A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA4B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA4A$)                           ! Primera Hora
   J% = VAL(DM.HORA4B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO JUEVES
   ENDIF 
!
! Dia Viernes
!*********** 
VIERNES:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA05$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA05$,"10",1) = 0 OR DM.DIA05$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA05$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA5A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA5B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA5A$)                           ! Primera Hora
   J% = VAL(DM.HORA5B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO VIERNES
   ENDIF 
!
! Dia Sabado
!*********** 
SABADO:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA06$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA06$,"10",1) = 0 OR DM.DIA06$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA06$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA6A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA6B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA6A$)                           ! Primera Hora
   J% = VAL(DM.HORA6B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO SABADO
   ENDIF 
!
! Dia Domingo
!*********** 
DOMINGO:
   CALL MSG.ERR(41,"Digite 1. Aplica 0. No Aplica")
   DM.DIA07$ = GET.DATOS
   IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
   IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA07$,"10",1) = 0 OR DM.DIA07$ = " ")
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
      DM.DIA07$ = GET.DATOS                                ! Asigna valor capturado
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              ! Si presiona tecla ESC
   WEND							   !
   BAN.PRG$ = "0"
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA7A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   CALL CAPTURA.HORA					   ! Capturo Fecha pantalla
   IF BAN.PRG$ = "1" THEN EXIT FUNCTION			   ! Si presiona ESC o F3
   DM.HORA7B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA7A$)                           ! Primera Hora
   J% = VAL(DM.HORA7B$)                           !
   IF J% < I% THEN BEGIN 
     CALL MSG.ERR(41,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      CALL DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO DOMINGO
   ENDIF 
!
! Grabacion del registro
!**********
   LEC$ = "C7 C9 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C3"  !
   WRITE FORM LEC$;#AREA1%;             \! Grabar Portafolio
     TMP.KEY$, DM.VLRCON$,              \
     DM.DIA01$, DM.HORA1A$, DM.HORA1B$, \
     DM.DIA02$, DM.HORA2A$, DM.HORA2B$, \
     DM.DIA03$, DM.HORA3A$, DM.HORA3B$, \
     DM.DIA04$, DM.HORA4A$, DM.HORA4B$, \
     DM.DIA05$, DM.HORA5A$, DM.HORA5B$, \
     DM.DIA06$, DM.HORA6A$, DM.HORA6B$, \
     DM.DIA07$, DM.HORA7A$, DM.HORA7B$, DM.ESTPOR$
END FUNCTION
!--- Fin de la funcion captura de datos

FUNCTION BUSQ.PLU
   BAN.PRG$ = "0"
   IR.ITEMCODE$ =PACK$(RIGHT$("000000000000"+DM.CODIGO$,12))        ! Creacion llave proyecto
   %INCLUDE EAMIRRD1.J86
   DM.NOMPRO$ = IR.ITEMNAME$
   IF BAN.PRG$ = "0" THEN CALL MSG.ERR(03,DM.NOMPRO$)      ! Mensaje
   IF BAN.PRG$ = "1" THEN CALL MSG.ERR(03,"Producto No Existe en EAMITEMR")
END FUNCTION
!--- Fin de la funcion busqueda de productos

FUNCTION BUSQ.POR
   BAN.PRG$ = "0"
   KEY$=PACK$(RIGHT$("000000000000"+DM.CODIGO$,12)+"01")   ! Creacion llave proyecto
   LEC$ = "C7 C9 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C3"  !
   READ FORM LEC$;#AREA1% KEY KEY$;                       \! Lee EAMITEMR
     TMP.KEY$, DM.VLRCON$,              \
     DM.DIA01$, DM.HORA1A$, DM.HORA1B$, \
     DM.DIA02$, DM.HORA2A$, DM.HORA2B$, \
     DM.DIA03$, DM.HORA3A$, DM.HORA3B$, \
     DM.DIA04$, DM.HORA4A$, DM.HORA4B$, \
     DM.DIA05$, DM.HORA5A$, DM.HORA5B$, \
     DM.DIA06$, DM.HORA6A$, DM.HORA6B$, \
     DM.DIA07$, DM.HORA7A$, DM.HORA7B$, DM.ESTPOR$
   IF BAN.PRG$ = "1" THEN BEGIN \
      CALL MSG.ERR(03,"Producto No Existe en Portafolio")
      WAIT;1800
!      CALL BUSQ.PLU
   ENDIF
   IF BAN.PRG$ = "0" THEN BEGIN \ 
      CALL BUSQ.PLU
      TMP.KEY$ = KEY$ + PACK$("01")
      CALL MSG.ERR(04,DM.VLRCON$)
      CALL MSG.ERR(06,DM.DIA01$)
      CALL MSG.ERR(07,LEFT$(DM.HORA1A$,2))
      CALL MSG.ERR(08,RIGHT$(DM.HORA1A$,2))
      CALL MSG.ERR(09,LEFT$(DM.HORA1B$,2))
      CALL MSG.ERR(10,RIGHT$(DM.HORA1B$,2))
      CALL MSG.ERR(11,DM.DIA02$)
      CALL MSG.ERR(12,LEFT$(DM.HORA2A$,2))
      CALL MSG.ERR(13,RIGHT$(DM.HORA2A$,2))
      CALL MSG.ERR(14,LEFT$(DM.HORA2B$,2))
      CALL MSG.ERR(15,RIGHT$(DM.HORA2B$,2))
      CALL MSG.ERR(16,DM.DIA03$)
      CALL MSG.ERR(17,LEFT$(DM.HORA3A$,2))
      CALL MSG.ERR(18,RIGHT$(DM.HORA3A$,2))
      CALL MSG.ERR(19,LEFT$(DM.HORA3B$,2))
      CALL MSG.ERR(20,RIGHT$(DM.HORA3B$,2))
      CALL MSG.ERR(21,DM.DIA04$)
      CALL MSG.ERR(22,LEFT$(DM.HORA4A$,2))
      CALL MSG.ERR(23,RIGHT$(DM.HORA4A$,2))
      CALL MSG.ERR(24,LEFT$(DM.HORA4B$,2))
      CALL MSG.ERR(25,RIGHT$(DM.HORA4B$,2))
      CALL MSG.ERR(26,DM.DIA05$)
      CALL MSG.ERR(27,LEFT$(DM.HORA5A$,2))
      CALL MSG.ERR(28,RIGHT$(DM.HORA5A$,2))
      CALL MSG.ERR(29,LEFT$(DM.HORA5B$,2))
      CALL MSG.ERR(30,RIGHT$(DM.HORA5B$,2))
      CALL MSG.ERR(31,DM.DIA06$)
      CALL MSG.ERR(32,LEFT$(DM.HORA6A$,2))
      CALL MSG.ERR(33,RIGHT$(DM.HORA6A$,2))
      CALL MSG.ERR(34,LEFT$(DM.HORA6B$,2))
      CALL MSG.ERR(35,RIGHT$(DM.HORA6B$,2))
      CALL MSG.ERR(36,DM.DIA07$)
      CALL MSG.ERR(37,LEFT$(DM.HORA7A$,2))
      CALL MSG.ERR(38,RIGHT$(DM.HORA7A$,2))
      CALL MSG.ERR(39,LEFT$(DM.HORA7B$,2))
      CALL MSG.ERR(40,RIGHT$(DM.HORA7B$,2))
      CALL CAPTURA.DATOS
   ENDIF
END FUNCTION
!--- Fin de la funcion busqueda de portafolio
                                            

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0001
CALL INICIADM 				                   									! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 4                                   						! Definicion area de trabajo archivo
ARCH1$="ADX_IDT1:PORTAFOL.DAT"               		           								! Asignar nombre archivo beneficiarios
ARCH2$="EAMITEMR"		                       	   								! Asignar nombre archivo Articulos
OPEN ARCH1$ KEYED RECL 82 AS AREA1%                        						! Abre archivo portafolio productos

OPEN ARCH2$ KEYED RECL 46 AS AREA2%               						! Abre archivo Eamitemr
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("C:\ADX_UPGM\PORTAFOL.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
MSG$ = "MANTENIMIENTO PORTAFOLIO"
CALL INICIO1 
WHILE (FIN$ = "0") 
      MSG$="Digite C¢digo del Producto"
      CALL MSG.ERR(41,MSG$)                                ! Mensaje
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.CODIGO$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION PORTAFOLIO    ","PORP0001.TXT") !
         MSG$ = "MANTENIMIENTO PORTAFOLIO"
         CALL INICIO1
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 ! Si presiona tecla ESC
      WHILE DM.CODIGO$ = " " OR VAL(DM.CODIGO$) = 0
        RET.ERR% = NXTF(-20)
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.CODIGO$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("DEFINICION PORTAFOLIO    ","PORP0001.TXT") !
           MSG$ = "MANTENIMIENTO PORTAFOLIO"
           CALL INICIO1
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      CALL BUSQ.POR                                        ! Busqueda portafolio
      CALL INICIO1
WEND

!----
!---- Fin del bloque principal
!----

E0001:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 7,,,3000 RECL 82  \! si no existe lo crea.
                        AS AREA1% 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$ = "Error: Archivo Productos EAMITEMR No Existe"   
            LOCATE 41,6:PRINT MSG$                         ! Presenta Error pantalla
            WAIT;2000
            STOP
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo              
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         STOP
!--- Fin despliegue de errores

!--- Fin del programa PORP0001.BAS
