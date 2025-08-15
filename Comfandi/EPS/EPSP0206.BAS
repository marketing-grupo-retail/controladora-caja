!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0206.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga Archivo de I.P.S           *
!**************************************************

!--- Definicion de Variables de trabajo
Integer*1  Global SO%
String     Global Input.File$

%INCLUDE EPSPVARI.BAS				  	   ! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86  	   ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	   ! Rutinas Comunes

FUNCTION INICIO01
   MSG$ = "CARGA ARCHIVO DE I.P.S    "
   CALL.ORDER% = 211                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION QUIT2     					   ! Funcion salida programa
    CLOSE AREA1%					   !
    CALL SETF("0000000")				   !
    CALL CLRSCR						   !
    RET.ERR%= CLSDIS					   !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			   !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Sub Mensaje
  
  If SO% = -1 Then Begin
    Call ADXSERVE(0,26,1,msg$)
  EndIf Else Call MSG.ERR(03,MSG$)				   ! Mensaje 
  
End Sub 

FUNCTION ACT.MAESTRO				           ! Actualizacion Maestro Convenios
  I% = 1
  WHILE EOF$ = "0" 					   ! Si no es EOF()
    READ #AREA2%;LINE REGISTRO$                            ! Lectura
    IF LEN(REGISTRO$) = 57 THEN BEGIN			  \!
      I% = I% + 1
      MSG$="Registros Procesados : "+STR$(I%)     	   !
      CALL Mensaje 
      DM.CONVEN$ = PACK$(MID$(REGISTRO$,1,4))		   ! Codigo ips
      DM.RAZON$  = MID$(REGISTRO$,5,40)			   ! Descripcion ips
      DM.CODALT$ = MID$(REGISTRO$,45,12)		   ! Accion a realizar
      DM.ACCION$ = MID$(REGISTRO$,57,1)			   ! Accion a realizar
      IF DM.ACCION$ = "3" THEN DELREC AREA1%;DM.CONVEN$   ! Borrado del registro
      LEC$="C2 C40 C12"
      WRITE FORM LEC$;#AREA1%;                             \! Graba registro IPS
       DM.CONVEN$, DM.RAZON$, DM.CODALT$
    ENDIF
  WEND							    ! Fin Ciclo
  WAIT;500						    ! 
  Delete AREA2%						    ! Cierra Archivo carga
  CALL QUIT2						    ! Fin Ejecucion Programa
END FUNCTION						    ! Fin de la funcion
!--- Fin de la actualizacion del maestro

Sub Carga.Interactiva

WHILE (FIN$ = "0") 					   ! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   !
      CALL INICIO01					   !
      MSG$="Digite Ruta del Archivo Plano de Carga"	   !
      CALL MSG.ERR(03,MSG$)				   ! Mensaje 
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("CARGA ARCHIVO DE I.P.S    ","EPSP0206.TXT") !
         CALL INICIO01
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT2                 ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT2                 ! Si presiona tecla ESC
      WHILE DM.OPCION$ = " "				   !
        MSG$="Digite Ruta del Archivo Plano de Carga"	   !
        CALL MSG.ERR(03,MSG$)				   ! Mensaje 
        RET.ERR% = NXTF(-20)				   !
        CALL DM.ERR(RET.ERR%,NXTF$)			   !
        ATTR$ = SETF(PRM.ON$)                              ! 
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.OPCION$ = INP$                                  ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN 		  \!
           CALL HELP("CARGA ARCHIVO DE I.P.S    ","EPSP0206.TXT") !
           CALL INICIO01				   !
        ENDIF						   !
        IF (ENDF = F3.SALIR) THEN CALL QUIT2               ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT2               ! Si presiona tecla ESC
      WEND
      ARCH2$ = ALLTRIM(DM.OPCION$)                         ! Busqueda de plano digitado
      OPEN ARCH2$ AS AREA2%
      CALL ACT.MAESTRO
WEND

End Sub 

Sub Carga.Background
      ARCH2$ = ALLTRIM(Input.File$)                         ! Busqueda de plano digitado
      OPEN ARCH2$ AS AREA2%
      Call ACT.MAESTRO
End Sub 

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0201
SO% = 0 
Input.File$ = COMMAND$                   ! Toma archivo de entrada como parametro
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11: AREA2% = 12: AREA3% = 13: AREA4% = 14         ! Definicion area de trabajo archivo
EOF$="0"
ARCH1$="IPS"			                       	   ! Asignar nombre archivo convenios
OPEN ARCH1$ KEYED RECL 54 AS AREA1%                        ! Abre archivo de Convenios
TERM$ = " "					           ! Inicializo Libreria
FIN$="0"


If Input.File$ <> "BACKGRND" Then Begin 
   RET.ERR%=INITDM(TERM$)					   !
   CALL DM.ERR(RET.ERR%,INITDM$)				   !
   RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
   CALL DM.ERR(RET.ERR%,OPNDIS$)

   Call Carga.Interactiva
EndIf Else Begin 
   SO% = -1 
   Input.File$ = "C:\DATOS\IPS.DAT"
   Call Carga.Background
EndIf 
Stop


!----
!---- Fin del bloque principal
!----

E0201:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 2,,,150 RECL 54   \! si no existe lo crea.
                        AS AREA1% compound perupdate 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa
         IF ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            MSG$="Error, Archivo "+ARCH2$+" No Existe ¢ No Se Puede Abrir"
            CALL Mensaje 
            WAIT;3000					   !
            CALL QUIT2
         ENDIF                                             ! del programa
         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! archivo             
            RESUME
         ENDIF
         IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN      \! Valida Lectura
            EOF$ = "1"                                     ! temporal de carga
            RESUME				           ! 
         ENDIF						   !
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         Call Mensaje 
         WAIT;3000
         STOP
!--- Fin despliegue de errores

!--- Fin del programa EPSP0206.BAS
