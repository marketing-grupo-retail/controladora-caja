!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMNTUSR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Usuarios niveles   *
!                cambio de precio                 *
!**************************************************
! Version 1.0 - Agosto 12 2021
!--------------------------------------------------
! Mod 6/Dic/2021
! Se adiciona la funcionalidad de la tecla F4 para 
! borrado de registros.
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

!--- Definicion de Variables de trabajo

%ENVIRON C						   																										! Ambiente de controlador

String    Global ENTE.NAME$, Ente.Dlg1$, Ente.Dlg2$, Ente.Dlg3$, Ente.Dlg4$ 
String    Global Ente.DlgX$, Ente.DlgY$, Ente.DlgZ$, Ente.DlgT$, Ente.Monto$

%INCLUDE POSPVARI.BAS				  	   																					! Variables programa
%INCLUDE DMEXTR.J86          				   																			! Inclucion Libreria Display Manager
%INCLUDE POSPRUTI.BAS				  	   																					! Rutinas Comunes

FUNCTION QUIT2     					   																							! Funcion salida programa
    CLOSE AREA1%					   																								!
    CALL SETF("0000000")				   																					!
    CALL CLRSCR						   																								!
    RET.ERR%= CLSDIS					   																						!
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   																		!
    Stop																																		!
END FUNCTION
!--- Fin de la funcion de salida

Sub LISTA.NIVELES
Integer*1 XI%, Xpos%
String Xkey$, Xdat1$, Xdat2$, Xdat3$, Xdat4$
Xpos% = 5
For XI% = 1 To 20
  BAN.PRG$ = "0"					   																								!
  Xkey$ = Pack$(Right$("00"+Str$(XI%),2))
  READ FORM "C1 C6 C4 C1";#AREA1% KEY Xkey$;             							     \! Lee Reg Cabecera 
       Xdat1$, Xdat2$, Xdat3$, Xdat4$
  If BAN.PRG$ = "0" Then Begin 																							!
   Xdat1$ = unpack$(Xdat1$)
   Xdat2$ = Str$(Val(unpack$(Xdat2$)))
   Xdat3$ = Str$(Val(unpack$(Xdat3$)))
   Xdat4$ = unpack$(Xdat4$)
   Call MSG.ERR(Xpos%,(Xdat1$))
   Call MSG.ERR(Xpos%+1,(Xdat2$))
   Call MSG.ERR(Xpos%+2,(Xdat4$))

  EndIf Else Begin
  	Call MSG.ERR(Xpos%,UnPack$(Xkey$))
  	Call MSG.ERR(Xpos%+1,"   ")
  EndIf
  Xpos% = Xpos% + 3
Next XI%

End Sub 

Function BUSQ.ENTE					   																							! Busqueda de red prestadora
String Xkey$, Xdat1$, Xdat2$, Xdat3$, Xdat4$
  BAN.PRG$ = "0"					   																								!
  Xkey$ = PACK$(RIGHT$("00"+DM.OPCION$,2))            										  !
  LEC$="C1 C6 C4 C1"                										                    !
  READ FORM LEC$;#AREA1% KEY XKEY$;                  							         \! Lee Reg Cabecera 
       Xdat1$, Xdat2$, Xdat3$, Xdat4$
  IF BAN.PRG$ = "0" Then Begin 
   Xdat1$ = unpack$(Xdat1$)
   Xdat2$ = Str$(Val(unpack$(Xdat2$)))
   Xdat3$ = Str$(Val(unpack$(Xdat3$)))
   Xdat4$ = unpack$(Xdat4$)
   Call MSG.ERR(02,(Xdat2$))
   Call MSG.ERR(03,(Xdat3$))
   Call MSG.ERR(04,(Xdat4$))
  EndIf  																																			!
	
END FUNCTION						   																										!	
!--- Fin de la funcion busqueda de convenios

FUNCTION INICIO01
   CALL.ORDER% = 2                                            								! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              								! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
End FUNCTION
!--- Fin de la funcion de inicio

FUNCTION CAPTURA.DATOS					      																				! Funcion captura datos
STRING PLANES$, Xdat1$, Xdat2$, Xdat3$, Xdat4$
      
      CALL MSG.ERR(65,"Numero CC persona autorizada")
      Xdat2$      = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE Xdat2$ = " "  Or Val(Xdat2$) <= 0 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Xdat2$ = GET.DATOS                               								    ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend
      If (ENDF = F4.KEY) Then GoTo Elimina.Registro
      	
      CALL MSG.ERR(65,"Clave de autorización asignada")
      Xdat3$      = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE Xdat3$ = " "  Or Val(Xdat3$) <= 0 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Xdat3$ = GET.DATOS                               								    ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend

      CALL MSG.ERR(65,"Nivel de autorizacion asignada")
      Xdat4$      = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 								! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                 								! Si presiona tecla ESC
      WHILE Val(Xdat4$) <= 0 Or Val(Xdat4$) > 10 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Xdat4$ = GET.DATOS                               								    ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT FUNCTION              								! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION              								! Si presiona tecla ESC
      Wend
      Xdat1$ = Pack$(Right$("00"+DM.OPCION$,2))                               ! ID de autorizacion
      Xdat2$ = Pack$(Right$("000000000000"+Xdat2$,12))									      ! CC de autorizado
      Xdat3$ = Pack$(Right$("00000000"+Xdat3$,8))                             ! Clave autorizacion
      Xdat4$ = Pack$(Right$("00"+Xdat4$,2))																	  ! Nivel autorizacion
      
      Write FORM "C1 C6 C4 C1";#AREA1%;                        						   \! Grabacion registro 
        Xdat1$, Xdat2$, Xdat3$, Xdat4$
      Exit Function 
      Elimina.Registro:
        Xdat1$ = Pack$(Right$("00"+DM.OPCION$,2))                               ! ID de autorizacion
        Delrec Area1% ; Xdat1$
        CALL MSG.ERR(65,"Registro Eliminado")
        Wait ; 2000
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
ARCH1$="TF:AUTPRC"		                     	   																! Asignar nombre archivo 
Open ARCH1$ KEYED RECL 12 AS AREA1%                       										! Abre archivo de Entes de control
TERM$ = " "					           																								! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   																						!
CALL DM.ERR(RET.ERR%,INITDM$)				   																				!
RET.ERR% = OPNDIS("ADX_UPGM:GRNIVAUT.PNT")	     										          ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)																									!
FIN$="0"																																			!
While (FIN$ = "0") 					   																								! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   																						!
      CALL INICIO01					   																								!
      Call LISTA.NIVELES
      MSG$="Ingrese ID Precio a definir  "																    !
      CALL MSG.ERR(65,MSG$)				         																		! Mensaje 
      RET.ERR% = NXTF(-20) 			           																		! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			     																		!
      ATTR$ = SETF(PRM.ON$)                                										!
      INP$ = UPDF                                          										! Captura dato en pantalla
      DM.OPCION$ = INP$                                    										! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEF. CONTROL DE PRECIOS  ","GRMNTUSR.TXT")                !
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
           CALL HELP("DEF. CONTROL DE PRECIOS  ","GRMNTUSR.TXT")              !
           CALL INICIO01
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               										! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               										! Si presiona tecla ESC
      Wend
      If Val(DM.OPCION$) <= 0 Or Val(DM.OPCION$) > 20 Then Begin
      	Call Msg.Err(65,"OPCION NO VALIDA, REITENTE...")
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
            CREATE POSFILE ARCH1$ KEYED 1,,,100 RECL 12                      \! si no existe lo crea.
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


