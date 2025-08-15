!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PORP0002.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Descuento por      *
!                seccion                          *
!**************************************************
! ModIficaciones:


!--- Definicion de Variables de trabajo
String  Global IR.USERDATA$, DM.PROMO$, GR.PROMO$

%INCLUDE EAMITEMR.J86      	 ! Variables EAMITEMR
%INCLUDE EXIRTSVA.J86				 ! Expansion EAMITEMR

%INCLUDE POSPVARI.BAS				 ! Variables programa
%INCLUDE DMEXTR.J86          ! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS			   ! Rutinas Comunes

Function INICIO1
  Call.ORDER% = 23                                           ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                              ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
  MSG$ = "MANTEN. DSCTO POR DEPTO "
  Call MSG.ERR(1,MSG$)				             ! Mensaje cabecera pantalla
  MSG$="Digite Codigo del Departamento"
  Call MSG.ERR(42,MSG$)				             ! Mensaje cabecera pantalla
End Function
!--- Fin de la funcion de inicio

Function QUIT2     					   ! Funcion salida programa
    Close AREA1%					     !
    Close AREA3%					     !
    Call SETF("0000000")		   !
    Call CLRSCR						     !
    RET.ERR%= CLSDIS					 !
    Call DM.ERR(RET.ERR%,CLSDIS$)			   !
    Stop
End Function
!--- Fin de la funcion de salida

Function BORRA.SECCION(ITEM$)
   String ITEM$
   ITEM$=PACK$(RIGHT$("0000000000000000"+ITEM$,16))                ! Creacion llave proyecto
   DELREC AREA1%; ITEM$
End Function
!--- Fin del borrado de la seccion

Function GRABA.CUPON(ITEM$,DESC$,DPTO$)			
   String ITEM$, DESC$, DPTO$
   ITEMNAME$ = DM.NOMPRO$
   DESC$     = PACK$("00000"+"99"+RIGHT$("000"+STR$(VAL(DESC$)),3))
   ITEM$     = Right$("000000000000"+ITEM$,12)
   ITEM$     = Pack$(ITEM$)
   
   IR.ITEMNAME$ = DM.NOMPRO$
   IR.ITEMCODE$ = ITEM$
   IR.SALEPRIC$ = DESC$   
   IR.SALEQUAN$ = PACK$("91")	
   IR.DEPARTME$ = DPTO$
   IR.INDICAT2$ = PACK$("74")
   IR.FAMILYNU$ = PACK$("000000")
   IR.MPGROUP$  = PACK$("00")
   IR.LINKEDTO$ = PACK$("0000")
   IR.USERDATA$ = STRING$(31,"0")
   %INCLUDE EAMIRWR1.J86
End Function
!--- Fin de la funcion de grabacion de cupones

Function UE.CREA.CUPON(ITEM$)
   String ITEM$
   ITEM$     = Right$("0000"+ITEM$,4)
   ITEM$     = "77" + ITEM$
   ITEM$     = Right$("000000000000"+ITEM$,12)
   ITEM$     = Pack$(ITEM$)
   IR.ITEMCODE$ = ITEM$
   %INCLUDE EAMIRRD1.J86%
End Function
!--- Fin de la funcion creacion cupon de descuento

Function CAPTURA.HORA
   Call MSG.ERR(42,"Digite Hora Formato 24 horas ")
   DM.METPRA$= GET.DATOS
   If (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT Function    ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT Function    ! Si presiona tecla ESC
   If (VAL(DM.METPRA$) > 24 OR VAL(DM.METPRA$) < 0 ) THEN \
      DM.METPRA$ = " "
   WHILE DM.METPRA$ = " "
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.METPRA$ = GET.DATOS                               ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT Function ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT Function ! Si presiona tecla ESC
      If (VAL(DM.METPRA$) > 24 OR VAL(DM.METPRA$) < 0 ) THEN \
         DM.METPRA$ = " "
   WEnd							   !
   Call MSG.ERR(42,"Digite Minuto del Precio  ")
   DM.METPRB$= GET.DATOS
   If (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT Function    ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT Function    ! Si presiona tecla ESC
   If (VAL(DM.METPRB$) > 59 OR VAL(DM.METPRB$) < 0 ) THEN \
      DM.METPRB$ = " "
   WHILE DM.METPRB$ = " "                           
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.METPRB$ = GET.DATOS                               ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN BAN.PRG$="1":EXIT Function ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN BAN.PRG$="1":EXIT Function ! Si presiona tecla ESC
      If (VAL(DM.METPRB$) > 59 OR VAL(DM.METPRB$) < 0 ) THEN \
         DM.METPRB$ = " "
   WEnd							   !
   DM.METPRA$=RIGHT$("00"+DM.METPRA$,2)                    ! 
   DM.METPRB$=RIGHT$("00"+DM.METPRB$,2)                    ! 
End Function
!--- Fin procedimiento captura hora

Function CAPTURA.DATOS					      ! Funcion captura datos
   Call MSG.ERR(42,"Digite Codigo del cupon de descuento ")
   BAN.PRG$ = "0"
!   While BAN.PRG$ = "0"
!    CUPON:
!        DM.CUPON$ = GET.DATOS
!        If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
!        If (ENDF = F4.KEY)   THEN BEGIN
!          If DM.TPRE$ = "1" THEN \
!            Call BORRA.SECCION(DM.CODIGO$)
!            EXIT Function
!        EndIf
!        If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
!        If DM.CUPON$ = " " THEN BEGIN 
!           RET.ERR% = NXTF(-2)
!           Call DM.ERR(RET.ERR%,NXTF$)
!           GOTO CUPON 
!        EndIf
!        Call UE.CREA.CUPON(DM.CUPON$)
!        If BAN.PRG$ = "1" THEN BEGIN
!           Call MSG.ERR(42,"Cupon de descuento No definido ")
!           RET.ERR% = NXTF(-2)
!           Call DM.ERR(RET.ERR%,NXTF$)
!           BAN.PRG$ = "0"
!        EndIf Else BEGIN 
!        	Call MSG.ERR(05,IR.ITEMNAME$)
!        	BAN.PRG$ = "1"              ! Mantenimiento fechas
!        	DM.VLRCON$ = UNPACK$(IR.SALEPRIC$)
!        	DM.VLRCON$ = Right$(DM.VLRCON$,3)
!        	Call MSG.ERR(06,DM.VLRCON$)
!          RET.ERR% = NXTF(2)
!          Call DM.ERR(RET.ERR%,NXTF$)
!          RET.ERR% = NXTF(2)
!          Call DM.ERR(RET.ERR%,NXTF$)
!          
!        EndIf

!        If DM.TPRE$ = "1" THEN BEGIN 
!           BAN.PRG$ = "1"              ! Mantenimiento fechas
!           Call MSG.ERR(05,IR.ITEMNAME$)
!           RET.ERR% = NXTF(2)
!           Call DM.ERR(RET.ERR%,NXTF$)
!        EndIf
!
!  Wend 
   DM.CUPON$=Right$("0000"+DM.CUPON$,4) 
   DM.CUPON$="0000" 
!   Call MSG.ERR(42,"Digite descripcion del cupon de descuento ")
!   DM.NOMPRO$ = GET.DATOS
!   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
!   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
!   While DM.NOMPRO$ = " "
!      RET.ERR% = NXTF(-2)
!      Call DM.ERR(RET.ERR%,NXTF$)
!      DM.NOMPRO$ = GET.DATOS                               ! Asigna valor capturado
!      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
!      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
!   Wend							   !

   Call MSG.ERR(42,"Digite Porcentaje descuento de la seccion ")
   DM.VLRCON$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   If (ENDF = F4.KEY)   THEN BEGIN
      If DM.TPRE$ = "1" THEN \
         Call BORRA.SECCION(DM.CODIGO$)
         EXIT Function
   EndIf
   	
   While DM.VLRCON$ = " "
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.VLRCON$ = GET.DATOS                               ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   Wend							   !

   DM.VLRCON$=RIGHT$("000"+DM.VLRCON$,3)                   ! 

!
! Dia Lunes
!*********** 
LUNES:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA01$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA01$,"10",1) = 0 OR DM.DIA01$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA01$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA1A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA1B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA1A$)                           ! Primera Hora
   J% = VAL(DM.HORA1B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO LUNES
   EndIf 
!
! Dia Martes
!*********** 
MARTES:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA02$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA02$,"10",1) = 0 OR DM.DIA02$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA02$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA2A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA2B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA2A$)                           ! Primera Hora
   J% = VAL(DM.HORA2B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO MARTES
   EndIf 
!
! Dia Miercoles
!*********** 
MIERCOLES:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA03$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA03$,"10",1) = 0 OR DM.DIA03$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA03$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA3A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA3B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA3A$)                           ! Primera Hora
   J% = VAL(DM.HORA3B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO MIERCOLES
   EndIf 
!
! Dia Jueves
!*********** 
JUEVES:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA04$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA04$,"10",1) = 0 OR DM.DIA04$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA04$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA4A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA4B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA4A$)                           ! Primera Hora
   J% = VAL(DM.HORA4B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO JUEVES
   EndIf 

!
! Dia Viernes
!*********** 
VIERNES:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA05$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA05$,"10",1) = 0 OR DM.DIA05$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA05$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					                             ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			               ! Si presiona ESC o F3
   DM.HORA5A$=DM.METPRA$+DM.METPRB$ 			                 ! Arma hora
   Call CAPTURA.HORA					                             ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			               ! Si presiona ESC o F3
   DM.HORA5B$=DM.METPRA$+DM.METPRB$ 			                 ! Arma hora
   I% = VAL(DM.HORA5A$)                                    ! Primera Hora
   J% = VAL(DM.HORA5B$)                                    !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO VIERNES
   EndIf 
!
! Dia Sabado
!*********** 
SABADO:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA06$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA06$,"10",1) = 0 OR DM.DIA06$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA06$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA6A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA6B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA6A$)                           ! Primera Hora
   J% = VAL(DM.HORA6B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO SABADO
   EndIf 
!
! Dia Domingo
!*********** 
DOMINGO:
   Call MSG.ERR(42,"Digite 1. Aplica 0. No Aplica ")
   DM.DIA07$ = GET.DATOS
   If (ENDF = F3.SALIR) THEN EXIT Function                 ! Si presiona tecla F3
   If (ENDF = ESC.KEY)  THEN EXIT Function                 ! Si presiona tecla ESC
   WHILE (MATCH(DM.DIA07$,"10",1) = 0 OR DM.DIA07$ = " ")
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      DM.DIA07$ = GET.DATOS                                ! Asigna valor capturado
      If (ENDF = F3.SALIR) THEN EXIT Function              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN EXIT Function              ! Si presiona tecla ESC
   WEnd							   !
   BAN.PRG$ = "0"
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA7A$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   Call CAPTURA.HORA					   ! Capturo Fecha pantalla
   If BAN.PRG$ = "1" THEN EXIT Function			   ! Si presiona ESC o F3
   DM.HORA7B$=DM.METPRA$+DM.METPRB$ 			   ! Arma hora
   I% = VAL(DM.HORA7A$)                           ! Primera Hora
   J% = VAL(DM.HORA7B$)                           !
   If J% < I% THEN BEGIN 
     Call MSG.ERR(42,"Hora Final es Menor a la Inicial")
     WAIT;1800
     FOR K% = 1 TO 5
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
     NEXT K%
     GOTO DOMINGO
   EndIf 
!
! Grabacion del registro
!**********
   TMP.KEY$ = KEY$
   LEC$ = "C12 C4 C3 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C2"  !
   WRITE FORM LEC$;#AREA1%;             \! Grabar Portafolio
     TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
     DM.DIA01$, DM.HORA1A$, DM.HORA1B$, \
     DM.DIA02$, DM.HORA2A$, DM.HORA2B$, \
     DM.DIA03$, DM.HORA3A$, DM.HORA3B$, \
     DM.DIA04$, DM.HORA4A$, DM.HORA4B$, \
     DM.DIA05$, DM.HORA5A$, DM.HORA5B$, \
     DM.DIA06$, DM.HORA6A$, DM.HORA6B$, \
     DM.DIA07$, DM.HORA7A$, DM.HORA7B$, \
     DM.PROMO$
     
!     Call GRABA.CUPON(DM.CUPON$,DM.VLRCON$,KEY$)   ! Grabacion cupon de descuento

End Function
!--- Fin de la funcion captura de datos

Function BUSQ.SEC
   BAN.PRG$ = "0"
   DM.TPRE$ = "1"
   DM.NOMBRE$ = "DEFINICION DEPTO COMERCIAL"
   KEY$=PACK$(RIGHT$("0000000000000000"+DM.CODIGO$,16))  + Ucase$(GR.PROMO$)        ! Creacion llave proyecto
   
   LEC$ = "C12 C4 C3 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C2"  !
   READ FORM LEC$;#AREA1% KEY KEY$;                       \! LECTURA SECCION
     TMP.KEY$,  DM.CUPON$,  DM.VLRCON$, \
     DM.DIA01$, DM.HORA1A$, DM.HORA1B$, \
     DM.DIA02$, DM.HORA2A$, DM.HORA2B$, \
     DM.DIA03$, DM.HORA3A$, DM.HORA3B$, \
     DM.DIA04$, DM.HORA4A$, DM.HORA4B$, \
     DM.DIA05$, DM.HORA5A$, DM.HORA5B$, \
     DM.DIA06$, DM.HORA6A$, DM.HORA6B$, \
     DM.DIA07$, DM.HORA7A$, DM.HORA7B$, \
     DM.PROMO$
   If BAN.PRG$ = "1" THEN BEGIN 
      Call MSG.ERR(42,"Seccion No Definida para descuento")
      Call MSG.ERR(03,"SECCION NO DEFINIDA")
      Wait;1800
      DM.TPRE$ = "0"
   EndIf
   If BAN.PRG$ = "0" THEN BEGIN 
   	  Call MSG.ERR(03,DM.NOMBRE$) 
   	  
      !Call MSG.ERR(04,DM.CUPON$)
      
      Call MSG.ERR(05,DM.VLRCON$)
      
      Call MSG.ERR(07,DM.DIA01$)
      Call MSG.ERR(08,LEFT$(DM.HORA1A$,2))
      Call MSG.ERR(09,RIGHT$(DM.HORA1A$,2))
      Call MSG.ERR(10,LEFT$(DM.HORA1B$,2))
      Call MSG.ERR(11,RIGHT$(DM.HORA1B$,2))
      Call MSG.ERR(12,DM.DIA02$)
      Call MSG.ERR(13,LEFT$(DM.HORA2A$,2))
      Call MSG.ERR(14,RIGHT$(DM.HORA2A$,2))
      Call MSG.ERR(15,LEFT$(DM.HORA2B$,2))
      Call MSG.ERR(16,RIGHT$(DM.HORA2B$,2))
      Call MSG.ERR(17,DM.DIA03$)
      Call MSG.ERR(18,LEFT$(DM.HORA3A$,2))
      Call MSG.ERR(19,RIGHT$(DM.HORA3A$,2))
      Call MSG.ERR(20,LEFT$(DM.HORA3B$,2))
      Call MSG.ERR(21,RIGHT$(DM.HORA3B$,2))
      Call MSG.ERR(22,DM.DIA04$)
      Call MSG.ERR(23,LEFT$(DM.HORA4A$,2))
      Call MSG.ERR(24,RIGHT$(DM.HORA4A$,2))
      Call MSG.ERR(25,LEFT$(DM.HORA4B$,2))
      Call MSG.ERR(26,RIGHT$(DM.HORA4B$,2))
      Call MSG.ERR(27,DM.DIA05$)
      Call MSG.ERR(28,LEFT$(DM.HORA5A$,2))
      Call MSG.ERR(29,RIGHT$(DM.HORA5A$,2))
      Call MSG.ERR(30,LEFT$(DM.HORA5B$,2))
      Call MSG.ERR(31,RIGHT$(DM.HORA5B$,2))
      Call MSG.ERR(32,DM.DIA06$)
      Call MSG.ERR(33,LEFT$(DM.HORA6A$,2))
      Call MSG.ERR(34,RIGHT$(DM.HORA6A$,2))
      Call MSG.ERR(35,LEFT$(DM.HORA6B$,2))
      Call MSG.ERR(36,RIGHT$(DM.HORA6B$,2))
      Call MSG.ERR(37,DM.DIA07$)
      Call MSG.ERR(38,LEFT$(DM.HORA7A$,2))
      Call MSG.ERR(39,RIGHT$(DM.HORA7A$,2))
      Call MSG.ERR(40,LEFT$(DM.HORA7B$,2))
      Call MSG.ERR(41,RIGHT$(DM.HORA7B$,2))
   EndIf
   If DM.TPRE$ = "1" Then \
     Call CAPTURA.DATOS
End Function
!--- Fin de la funcion busqueda de portafolio

Function CARGAR.DEPTIVA
  OPEN "ADX_IDT1:DEPTIVA.DAT" AS 5 NOWRITE NODEL
  If End #5 THEN FIN
  DIM IVAXDEPT$(999,2)
  T% = 0
  WHILE (1)
	  READ #5; DM.PLAN$, DM.BASURA$, DM.NOMBRE$
	  T% = T% + 1    
	  IVAXDEPT$(T%,1) = DM.PLAN$
	  IVAXDEPT$(T%,2) = DM.NOMBRE$
  WEnd
 FIN:
   Close 5
End Function
!--- Fin procedimiento carja departamentos 


!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0002
Call INICIADM 				                   									! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 4                      ! Definicion area de trabajo archivo
ARCH1$="SECCION"		               								        ! Asignar nombre archivo secciones
ARCH3$="EAMITEMR"					   											        ! Maestro de productos
OPEN ARCH1$ KEYED RECL 84 AS AREA1%                       ! Abre archivo portafolio x seccion
Open ARCH3$ KEYED RECL 77 AS AREA3%							          ! Apertura Eamitemr
TERM$ = " "					           											      ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   										    !
Call DM.ERR(RET.ERR%,INITDM$)				   									  !
RET.ERR% = OPNDIS("C:\ADX_UPGM\PORTAFOL.PNT")	           	! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
MSG$ = "MANTEN. DSCTO POR DEPTO "
Call INICIO1 
Call CARGAR.DEPTIVA
While (FIN$ = "0") 
      DM.TPRE$ = "1"					   										! C¢digo Nuevo
      MSG$="Digite Codigo del Departamento"
      Call MSG.ERR(42,MSG$)                                						! Mensaje
      RET.ERR% = NXTF(-20) 			           									! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          						! Captura dato en pantalla
      DM.CODIGO$ = INP$                                    						! Asigna valor capturado
      If (ENDF = F1.AYUDA) THEN BEGIN \
         Call HELP("MANTEN. DSCTO POR DEPTO  ","PORP0002.TXT") !
         MSG$ = "MANTEN. DSCTO POR DEPTO "
         Call INICIO1
      EndIf
      If (ENDF = F3.SALIR) THEN Call QUIT2                 ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call QUIT2                 ! Si presiona tecla ESC
      While DM.CODIGO$ = " " OR VAL(DM.CODIGO$) = 0
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.CODIGO$ = INP$                                  ! Asigna valor capturado
        If (ENDF = F1.AYUDA) THEN BEGIN \
           Call HELP("MANTEN. DSCTO POR DEPTO  ","PORP0002.TXT") !
           MSG$ = "MANTEN. DSCTO X DEPTO  "
           Call INICIO1
        EndIf
        If (ENDF = F3.SALIR) THEN Call QUIT2               ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  THEN Call QUIT2               ! Si presiona tecla ESC
      Wend 
      Call MSG.ERR(42,"Codigo Promocion a consultar")
      GR.PROMO$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call QUIT2               ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call QUIT2               ! Si presiona tecla ESC
      DM.CODIGO$ = RIGHT$("0000000000000000"+DM.CODIGO$,16)
      GR.PROMO$  = Right$("0000"+GR.PROMO$,4)
      Call BUSQ.SEC                                        ! Busqueda seccion
      Call INICIO1
Wend 

!----
!---- Fin del bloque principal
!----

E0002:
         If ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
             CREATE POSFILE "MKTP:SECCION.DAT" KEYED 12,,,15000 RECL 84   \! si no existe lo crea.
                           AS Area1% compound perupdate 		   ! 
            RESUME                                         ! retorna ejecucion
         EndIf                                             ! del programa
         If ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         If ERRF% = AREA3% AND ERR = "OE" OR ERR = "FU" THEN BEGIN
            BAN.PRG$ = "1"
            RESUME
         EndIf
         If ERRF% = AREA3% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         EndIf
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         STOP
!--- Fin despliegue de errores

!--- Fin del programa PORP0002.BAS
