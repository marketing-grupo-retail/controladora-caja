!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMNTNIV.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Niveles cambio     *
!                de precios 
!**************************************************
! Version 1.0 - Agosto 12 2021


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

Sub Carga.Dato.Inicial																												! 
Integer*1 XI%, Xpos%
String Xlec$, Xdat1$, Xdat2$, Xdat3$

For XI% = 1 To 10
    Xdat1$ = Pack$(Right$("00"+Str$(XI%),2))
    Xdat2$ = Pack$("0000000000") 
    Xdat3$ = Pack$("0000000009")                               ! Monto inicial y final
    Write Form  "C1 C5 C5";#AREA1%; Xdat1$ , Xdat2$, Xdat3$
Next XI%

End Sub 

Sub LISTA.NIVELES
Integer*1 XI%, Xpos%
String Xkey$, Xdat1$, Xdat2$, Xdat3$
Xpos% = 4
For XI% = 1 To 10
  BAN.PRG$ = "0"					   																								!
  Xkey$ = Pack$(Right$("00"+Str$(XI%),2))
  READ FORM "C1 2C5";#AREA1% KEY Xkey$;                  							     \! Lee Reg Cabecera 
       Xdat1$, Xdat2$, Xdat3$
  If BAN.PRG$ = "0" Then Begin 																							!
   Xdat1$ = Str$(Val(unpack$(Xdat1$)))
   Xdat2$ = Str$(Val(unpack$(Xdat2$)))
   Xdat3$ = Str$(Val(unpack$(Xdat3$)))
   Call MSG.ERR(Xpos%,(Xdat1$))
   Call MSG.ERR(Xpos%+1,(Xdat2$))
   Call MSG.ERR(Xpos%+2,(Xdat3$))

  EndIf Else Begin
  	Call MSG.ERR(Xpos%,(Xkey$))
  	Call MSG.ERR(Xpos%+1,"UPS")
  EndIf
  Xpos% = Xpos% + 3
Next XI%

End Sub 

Function BUSQ.ENTE					   																								! Busqueda de red prestadora
  BAN.PRG$ = "0"					   																									!
  DM.CONVEN$ = PACK$(RIGHT$("00"+DM.OPCION$,2))            										!
  LEC$="C1 2C5"                										!
  READ FORM LEC$;#AREA1% KEY DM.CONVEN$;                  							     \! Lee Reg Cabecera 
        DM.CONVEN$, ENTE.DLGT$, Ente.Monto$                										! 
  IF BAN.PRG$ = "0" THEN Begin 
  	 ENTE.DLGT$  = Str$(Val(UNPACK$(ENTE.DLGT$)))
  	 ENTE.MONTO$ = Str$(Val(UNPACK$(ENTE.MONTO$)))
  	 CALL MSG.ERR(02,(ENTE.DLGT$))
     CALL MSG.ERR(03,(ENTE.MONTO$))
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
      
      CALL MSG.ERR(34,"Monto Minimo de control")
      Ente.Monto$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE Ente.Monto$ = " "  Or Val(Ente.Monto$) <  0 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Ente.Monto$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend

      CALL MSG.ERR(34,"Monto Maximo de control")
      ENTE.DLGT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE ENTE.DLGT$ = " "  Or Val(ENTE.DLGT$) <= Val(Ente.Monto$) 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         ENTE.DLGT$ = GET.DATOS                               								! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend
      DM.CONVEN$  = PACK$(RIGHT$("00"+DM.OPCION$,2))       										!
      Ente.Monto$ = PACK$(Right$("0000000000"+Ente.Monto$,10))                ! 
      Ente.DLGT$  = PACK$(Right$("0000000000"+Ente.Dlgt$,10))                 ! 
      
      Write FORM "C1 2C5";#AREA1%;                            						   \! Grabacion registro 
        DM.CONVEN$, Ente.Monto$, ENTE.DLGT$              										  !
        
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
ARCH1$="TF:NIVPRC"		                     	   																! Asignar nombre archivo 
Open ARCH1$ KEYED RECL 11 AS AREA1%                       										! Abre archivo de Entes de control
TERM$ = " "					           																								! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   																						!
CALL DM.ERR(RET.ERR%,INITDM$)				   																				!
RET.ERR% = OPNDIS("ADX_UPGM:GRNIVAUT.PNT")	  											          ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)																									!
FIN$="0"																																			!
While (FIN$ = "0") 					   																								! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   																						!
      CALL INICIO01					   																								!
      Call LISTA.NIVELES
      MSG$="Ingrese Nivel Precio a Modificar"																  !
      CALL MSG.ERR(34,MSG$)				         																		! Mensaje 
      RET.ERR% = NXTF(-20) 			           																		! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			     																		!
      ATTR$ = SETF(PRM.ON$)                                										!
      INP$ = UPDF                                          										! Captura dato en pantalla
      DM.OPCION$ = INP$                                    										! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEF. CONTROL DE PRECIOS  ","GRMNTNIV.TXT")                !
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
           CALL HELP("DEF. CONTROL DE PRECIOS  ","GRMNTNIV.TXT")              !
           CALL INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               										! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               										! Si presiona tecla ESC
      Wend
      If Val(DM.OPCION$) <= 0 Or Val(DM.OPCION$)> 10 Then Begin
      	Call Msg.Err(34,"OPCION NO VALIDA, REITENTE...")
      	WAIT ; 2000
      EndIf Else Begin
        CALL BUSQ.ENTE			                   																	! Busqueda convenio
        Call CAPTURA.DATOS	           																					! Captura datos 
      EndIf
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         IF ERRF% = AREA1% AND                              							   \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN            									! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 1,,,100 RECL 11                      \! si no existe lo crea.
                        AS AREA1% compound perupdate 		                      ! 
            Call Carga.Dato.Inicial                                           ! 
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
