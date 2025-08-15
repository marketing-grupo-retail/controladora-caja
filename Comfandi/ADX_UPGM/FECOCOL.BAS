!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : FECOCOL.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion de Numeracion Fiscal  *
!                FECO - COLOMBIA                  *
!**************************************************
! Modificaciones
! Ver 1.0 - 2 Abr 2024

%ENVIRON C		                 																							! Ambiente de controlador

!--- Definicion de Variables de trabajo


STRING    Global U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, U.Aut2$,U.FECV$ !
INTEGER*4 Global U.INI%, U.FIN%, U.ACT%																			 !
String    Global U.Opt$, U.Numera$

%INCLUDE FISPVARI.BAS				  	   																					! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86          				   														! Inclucion Libreria Display Manager
%INCLUDE FISPRUTI.BAS				  	   																					! Rutinas Comunes

FUNCTION QUIT2     					   																							! Funcion salida programa
    CLOSE AREA1%																														!
    CALL SETF("0000000")				   																					!
    CALL CLRSCR						   																								!
    RET.ERR%= CLSDIS					   																						!
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   																		!
    Stop																																		!
END Function																																!
!--- Fin de la funcion de salida

FUNCTION INICIO01
   CALL.ORDER% = 60                                           							! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              							! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)																							!
END Function																																!
!--- Fin de la funcion de inicio

SUB CAPTURA.DATOS
String U.Key$
      U.KEY$ = Pack$(RIGHT$("0000"+DM.OPCION$,4))
      U.Numera$ = Str$(U.ACT%)																							! Numero actual de factura
      CALL MSG.ERR(27,"Numero Serial del Equipo")														!
      U.SERIAL$ = GET.DATOS																									!
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      IF (ENDF = F4.KEY) Then Begin
      	  Call MSG.ERR(27,"Eliminando Terminal "+DM.OPCION$)
      	  WAIT ; 2500
      	  Delrec AREA1% ; U.KEY$
      	  Exit Sub 
      EndIf

      WHILE U.SERIAL$ = " " 																								!
         CALL MSG.ERR(27,"Numero Serial del Equipo")												!
         RET.ERR% = NXTF(-2)																								!
         CALL DM.ERR(RET.ERR%,NXTF$)																				!
         U.SERIAL$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = F4.KEY) Then Begin
         	  Delrec AREA1% ; U.KEY$
         EndIf
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend																																	!

      CALL MSG.ERR(27,"Prefijo de Facturacion")															!
      U.PREF$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.PREF$ = " " 
         CALL MSG.ERR(27,"Prefijo de Facturacion")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.PREF$ = GET.DATOS                               									! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      NUM.INICIAL:
      CALL MSG.ERR(27,"Rango Inicial de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.TMP$ = " " OR VAL(U.TMP$) <= 0
         CALL MSG.ERR(27,"Rango Inicial de Facturacion")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      U.INI% = VAL(U.TMP$)

      CALL MSG.ERR(27,"Rango Final de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.TMP$ = " " OR VAL(U.TMP$) <= 0
         CALL MSG.ERR(27,"Rango Final de Facturacion")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      U.FIN% = VAL(U.TMP$)
      IF U.FIN% < U.INI% THEN BEGIN 
         CALL POSF(4)
         GOTO NUM.INICIAL
      ENDIF

      CALL MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
      U.AUT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.AUT$ = " " 
         CALL MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.AUT$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      WEND

      CALL MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
      U.FEC$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FEC$ = " " 
         CALL MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.FEC$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend

      CALL MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
      U.FECV$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FECV$ = " " 
         CALL MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         U.FECV$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      
      If Val(U.FECV$) < Val(U.FEC$) Then Begin
      	 	  CALL MSG.ERR(27,"Error definicion fechas autorizacion fiscal")
            wait ; 3000
      	 	  CALL MSG.ERR(27,"Numeracion no fue actualizada         ")
            wait ; 3000
            Exit Sub 
      EndIf
      
      If Ucase$(U.Opt$) <> "MOD" Then Begin
      	 If (Val(U.Numera$) < U.INI%) Or (Val(U.Numera$) > U.FIN%) Then Begin
      	 	  CALL MSG.ERR(27,"Error definicion de consecutivo fiscal")
            wait ; 3000
      	 	  CALL MSG.ERR(27,"Numeracion no fue actualizada         ")
            wait ; 3000
            Exit Sub 
      	 EndIf
      EndIf	
      	
      	
      If Ucase$(U.Opt$) = "MOD" Then Begin
      	NumAct:
         Call MSG.ERR(27,"Numero actual de factura")
         U.Numera$ = GET.DATOS
         If (ENDF = F3.SALIR) Then Exit SUB                              ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Exit SUB                      				 ! Si presiona tecla ESC
         If (Val(U.Numera$) < U.INI%) Or (Val(U.Numera$) > U.FIN%) Then Begin ! Control numeracion
       	 	  CALL MSG.ERR(27,"Error definicion de consecutivo fiscal")
            wait ; 3000
            RET.ERR% = NXTF(-2)
            CALL DM.ERR(RET.ERR%,NXTF$)
            GoTo NumAct
         EndIf
      EndIf
      
      DM.OPCION$ = Pack$(RIGHT$("0000"+DM.OPCION$,4))
      U.SERIAL$  = Left$(U.SERIAL$+"        ",8)
      U.PREF$    = Left$(U.PREF$+"    ",4)
      U.AUT2$     = Pack$(Left$(U.AUT2$+"000000000000",12))
      U.AUT$     = Pack$(Left$(U.AUT$+"              ",14))
      U.FEC$     = Pack$(Right$("000000"+U.FEC$,6))
      U.FECV$    = Pack$(Right$("000000"+U.FECV$,6))
      U.Act%     = Val(U.Numera$)
      Write Form "C2 C8 C7 C4 2I4 C6 2C3 I4"	;#AREA1% ;         \! Grabacion de registros
        DM.OPCION$,  \ Numero de terminal
        U.SERIAL$,   \ Numero serial
        U.AUT$,      \ Numero Autorizacion
        U.PREF$,     \ Prefijo de facturacion
        U.INI%,      \ Rango inicial
        U.FIN%,      \ Rango Final
        U.AUT2$,     \ Numero Autorizacion
        U.FEC$,      \ Fecha de autorizacion
        U.FECV$,     \ Fecha de vencimiento
        U.ACT%       ! Numero actual de registro
END SUB

SUB VALIDA.DATOS
STRING U.KEY$, LEC$

  U.KEY$ = RIGHT$("0000"+DM.OPCION$,4)
  U.KEY$ = PACK$(U.KEY$)
  BAN.PRG$ = "0"
  LEC$="C2 C8 C7 C4 2I4 C6 2C3 I4"	                           !
  READ FORM LEC$;#AREA1% KEY U.KEY$;                        \! Lee Reg Cabecera 
  U.KEY$,U.SERIAL$, U.Aut$, U.PREF$, U.INI%, U.FIN%, U.AUT2$, U.FEC$, U.FECV$, U.ACT%
  IF BAN.PRG$ = "0" THEN BEGIN
     Call MSG.ERR(03,U.SERIAL$)
     Call MSG.ERR(04,U.PREF$)
     Call MSG.ERR(05,Str$(U.INI%))
     Call MSG.ERR(06,Str$(U.FIN%))
     Call MSG.ERR(07,Unpack$(U.AUT$))
     Call MSG.ERR(08,Unpack$(U.FEC$))
     Call MSG.ERR(13,Unpack$(U.FECV$))
     Call MSG.ERR(09,Str$(U.ACT%))
  ENDIF  Else U.ACT% = 0
END SUB

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
U.Opt$ = Command$
If Ucase$(U.Opt$) = "VERSION" Then BEGIN
	 Print "Grupo Retail Ltda"
	 Print "Modulo Definicion Numeracion Fiscal - Colombia  Ver. 08/Abr/2019"
	 Stop 
EndIf
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 1
ARCH1$ = "EAMNMFIS" 																												! Archivo para Easy
ARCH1$ = "TF:NUMFIS"																												! Archivo para Comfandi
OPEN ARCH1$ KEYED RECL 45 AS AREA1%                        ! Abre archivo de control
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:FISCAL.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
WHILE (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      CALL INICIO01					   !
      MSG$="Digite Numero de la terminal"		   !
      CALL MSG.ERR(27,MSG$)				   ! Mensaje 
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("Numeracion Fiscal","NUMFIS.TXT") !
         CALL INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 ! Si presiona tecla ESC
      WHILE DM.OPCION$ = " " OR VAL(DM.OPCION$) = 0
        RET.ERR% = NXTF(-20)
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.OPCION$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("Numeracion Fiscal","NUMFIS.TXT") !
           CALL INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      CALL VALIDA.DATOS				           ! 
      CALL CAPTURA.DATOS                                   ! CAPTURA DATOS NUMFISCAL
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 2,,,5000 RECL 45 \! si no existe lo crea.
                        AS AREA1% compound perupdate 		   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa FECOCOL.BAS
