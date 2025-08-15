!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MNTOENTE.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Entes de Control   *
!**************************************************

!--- Definicion de Variables de trabajo


String    Global ENTE.NAME$, Ente.Dlg1$, Ente.Dlg2$, Ente.Dlg3$, Ente.Dlg4$ 
String    Global Ente.DlgX$, Ente.DlgY$, Ente.DlgZ$, Ente.DlgT$, Ente.Monto$

%INCLUDE POSPVARI.BAS				  	   																						! Variables programa
%INCLUDE DMEXTR.J86          				   																				! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS				  	   																						! Rutinas Comunes

FUNCTION QUIT2     					   																								! Funcion salida programa
    CLOSE AREA1%					   																									!
    CALL SETF("0000000")				   																						!
    CALL CLRSCR						   																									!
    RET.ERR%= CLSDIS					   																							!
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   																			!
    Stop																																			!
END FUNCTION
!--- Fin de la funcion de salida


FUNCTION BUSQ.ENTE					   																								! Busqueda de red prestadora
  BAN.PRG$ = "0"					   																									!
  DM.CONVEN$ = PACK$(RIGHT$("00"+DM.OPCION$,2))            										!
  LEC$="C1 C20 C1 C20 C1 C20 C1 C20 2C1 C5"                										!
  READ FORM LEC$;#AREA1% KEY DM.CONVEN$;                  							     \! Lee Reg Cabecera 
        DM.CONVEN$, ENTE.NAME$, ENTE.DLG1$,  ENTE.DLGX$, 										 \! 
        ENTE.DLG2$, ENTE.DLGY$, ENTE.DLG3$,  ENTE.DLGZ$,  									 \! 
        ENTE.DLG4$, ENTE.DLGT$, Ente.Monto$                										! 

  IF BAN.PRG$ = "0" THEN Begin 
  	 CALL MSG.ERR(02,ENTE.NAME$)
     CALL MSG.ERR(03,ENTE.DLG1$)
     CALL MSG.ERR(04,ENTE.DLGX$)
     CALL MSG.ERR(05,ENTE.DLG2$)
     CALL MSG.ERR(06,ENTE.DLGY$)
     CALL MSG.ERR(07,ENTE.DLG3$)
     CALL MSG.ERR(08,ENTE.DLGZ$)
     CALL MSG.ERR(09,ENTE.DLG4$)
     CALL MSG.ERR(10,UNPACK$(ENTE.DLGT$))
     Call MSG.ERR(12,UNPACK$(Ente.Monto$))
  ENDIF 																																			!
	
END FUNCTION						   																										!	
!--- Fin de la funcion busqueda de convenios

FUNCTION INICIO01
   CALL.ORDER% = 1                                            								! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              								! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
End FUNCTION
!--- Fin de la funcion de inicio

FUNCTION CAPTURA.DATOS					      																				! Funcion captura datos
STRING PLANES$
      CALL MSG.ERR(11,"Descripcion Ente de Control")
      ENTE.NAME$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE ENTE.NAME$ = " " 
         CALL MSG.ERR(11,"Descripcion Ente de Control")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.NAME$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      WEND

      CALL MSG.ERR(11,"Captura primer dialogo 1.Si 0.No")
      ENTE.Dlg1$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE (MATCH(ENTE.DLG1$,"01",1) = 0 OR ENTE.DLG1$ = " ")
         CALL MSG.ERR(11,"Captura primer dialogo 1.Si 0.No")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Dlg1$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      WEND
      If ENTE.DLG1$ = "1" Then Begin
        CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
        ENTE.DLGX$ = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
        WHILE ENTE.DLGX$ = " " 
         CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.DLGX$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
        WEND
      EndIf ELSE BEGIN
      	ENTE.DLGX$ = String$(20," ")
        RET.ERR% = NXTF(2)					   ! Salta un campo
        Call DM.ERR(RET.ERR%,NXTF$)
      EndIf

      CALL MSG.ERR(11,"Captura Segundo dialogo 1.Si 0.No")
      ENTE.Dlg2$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE (MATCH(ENTE.DLG2$,"01",1) = 0 OR ENTE.DLG2$ = " ")
         CALL MSG.ERR(11,"Captura Segundo dialogo 1.Si 0.No")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Dlg2$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      WEND
      If ENTE.DLG2$ = "1" Then Begin
        CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
        ENTE.DLGY$ = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
        WHILE ENTE.DLGY$ = " " 
         CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.DLGY$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
        WEND
      EndIf ELSE BEGIN
      	ENTE.DLGY$ = String$(20," ")
        RET.ERR% = NXTF(2)					   ! Salta un campo
        Call DM.ERR(RET.ERR%,NXTF$)
      EndIf

      CALL MSG.ERR(11,"Captura Tercer dialogo 1.Si 0.No")
      ENTE.Dlg3$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE (MATCH(ENTE.DLG3$,"01",1) = 0 OR ENTE.DLG2$ = " ")
         CALL MSG.ERR(11,"Captura Tercer dialogo 1.Si 0.No")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Dlg3$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      WEND
      If ENTE.DLG2$ = "1" Then Begin
        CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
        ENTE.DLGZ$ = GET.DATOS
        IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
        WHILE ENTE.DLGZ$ = " " 
         CALL MSG.ERR(11,"Ingrese Dialogo a solicitar")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.DLGZ$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
        WEND
      EndIf ELSE BEGIN
      	ENTE.DLGZ$ = String$(20," ")
        RET.ERR% = NXTF(2)					   ! Salta un campo
        Call DM.ERR(RET.ERR%,NXTF$)
      EndIf

      CALL MSG.ERR(11,"Genera Tirilla Mensaje 1.Si 0.No")
      ENTE.Dlg4$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE (MATCH(ENTE.DLG4$,"01",1) = 0 OR ENTE.DLG2$ = " ")
         CALL MSG.ERR(11,"Genera Tirilla Mensaje 1.Si 0.No")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Dlg4$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      WEND

      CALL MSG.ERR(11,"Ingrese Numero Mensaje a Imprimir")
      ENTE.DLGT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE ENTE.DLGT$ = " "  Or Val(ENTE.DLGT$) <= 0 
         CALL MSG.ERR(11,"Ingrese Numero Mensaje a Imprimir")
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.DLGT$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend
      
      CALL MSG.ERR(11,"Monto Minimo de compra generacion voucher")
      Ente.Monto$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE Ente.Monto$ = " "  Or Val(Ente.Monto$) <= 0 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Monto$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend
      ENTE.DLGT$ = PACK$(Right$("00"+ENTE.DLGT$,2))			   										! Empaqueta Llave 
      DM.OPCION$ = PACK$(Right$("00"+DM.OPCION$,2))			   										! Empaqueta Llave 
      Ente.Monto$ = PACK$(Right$("0000000000"+Ente.Monto$,10))                ! 
      LEC$="C1 C20 C1 C20 C1 C20 C1 C20 2C1 C5"                   						!
      WRITE FORM LEC$;#AREA1%;                            						       \! Grabacion registro 
        DM.OPCION$, ENTE.NAME$, ENTE.DLG1$,  ENTE.DLGX$,  								   \! 
        ENTE.DLG2$, ENTE.DLGY$, ENTE.DLG3$,  ENTE.DLGZ$,  									 \! 
        ENTE.DLG4$, ENTE.DLGT$, Ente.Monto$                										!	 
        
END FUNCTION
!--- Fin de la funcion captura de datos

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
CALL INICIADM 				                   																			! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         										! Definicion area de trabajo archivo
ARCH1$="TF:ENTES"		                       	   																! Asignar nombre archivo 
Open ARCH1$ KEYED RECL 91 AS AREA1%                       										! Abre archivo de Entes de control
TERM$ = " "					           																								! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   																						!
CALL DM.ERR(RET.ERR%,INITDM$)				   																				!
RET.ERR% = OPNDIS("ENTE.PNT")	           														          ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)																									!
FIN$="0"																																			!
WHILE (FIN$ = "0") 					   																								! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   																						!
      CALL INICIO01					   																								!
      MSG$="Digite Codigo Entre de Control"																		!
      CALL MSG.ERR(11,MSG$)				         																		! Mensaje 
      RET.ERR% = NXTF(-20) 			           																		! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			     																		!
      ATTR$ = SETF(PRM.ON$)                                										!
      INP$ = UPDF                                          										! Captura dato en pantalla
      DM.OPCION$ = INP$                                    										! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION ENTES CONTROL ","MNTOENTE.TXT") !
         CALL INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 										! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 										! Si presiona tecla ESC
      WHILE DM.OPCION$ = " " OR VAL(DM.OPCION$) = 0
        RET.ERR% = NXTF(-20)
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        										! Captura dato en pantalla
        DM.OPCION$ = INP$                                  										! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("DEFINICION ENTES CONTROL ","MNTOENTE.TXT") 							!
           CALL INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               										! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               										! Si presiona tecla ESC
      WEND
      CALL BUSQ.ENTE			                   																	! Busqueda convenio
      Call CAPTURA.DATOS	           																					! Captura datos 
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                              							   \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN            									! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 1,,,2000 RECL 91                     \! si no existe lo crea.
                        AS AREA1% compound perupdate 		                      ! 
            RESUME                                         										! retorna ejecucion
         ENDIF                                             										! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      									 \! Si encuentra fin de 
            BAN.PRG$ = "1"				   																					! archivo             
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  							 \! Valida la lectura
            BAN.PRG$ = "1"				       																			! del archivo de 
            RESUME					       																						! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR																										! Traducer Error aplicacion
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 24,1: Print String$(78," ")
         LOCATE 24,1:PRINT MSG$                          											! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa EPSP0101.BAS
