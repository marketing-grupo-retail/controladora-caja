!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPSP0109.bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Diagnosticos       *
!**************************************************


%INCLUDE EPSPVARI.BAS				  	     ! Definicion Variables
%INCLUDE DMEXTR.J86          				     ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	     ! Rutinas Comunes

!--- Fin  definicion de variables

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0106
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11    					           ! Definicion area de trabajo archivo
ARCH1$="DIAGNOST"		                       	   ! Asignar nombre archivo diagnosticos
OPEN ARCH1$ KEYED RECL 55 AS AREA1%                        ! Abre archivo de diagnosticos
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
WHILE (FIN$ = "0") 
 CICLO:
      DM.NIT$   = "  "
      MSG$ = "MANTENIMIENTO DE DIAGNOSTICOS  "
      CALL.ORDER% = 212                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)
      CALL MSG.ERR(1,MSG$)				   ! Mensaje cabecera pantalla
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.NIT$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION DIAGNOSTICOS  ","EPSP0109.TXT") !
         GOTO CICLO
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
      IF (ENDF = F7.KEY)   THEN BEGIN
         CLOSE AREA1%
         OPEN ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
         CALL VISUAL.REG6                                  ! Si presiona tecla F7
         CLOSE AREA1%
         OPEN ARCH1$ KEYED RECL 55 AS AREA1%               ! Abre archivo de diagnosticos
         GOTO CICLO
      ENDIF
      IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      WHILE DM.NIT$ = " " OR VAL(DM.NIT$) = 0
        CALL MSG.ERR(1,MSG$)				   ! Mensaje cabecera pantalla
        RET.ERR% = NXTF(-20) 			           ! Primer campo
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                          ! Captura dato en pantalla
        DM.NIT$ = INP$                                       ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("DEFINICION DIAGNOSTICOS  ","EPSP0109.TXT") !
           GOTO CICLO
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
        IF (ENDF = F7.KEY)   THEN BEGIN
           CLOSE AREA1%
           OPEN ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
           CALL VISUAL.REG6                                  ! Si presiona tecla F7
           CLOSE AREA1%
           OPEN ARCH1$ KEYED RECL 55 AS AREA1%               ! Abre archivo de control
           GOTO CICLO
        ENDIF
        IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      WEND
      MSG$="Digite la Descripci¢n            "
      CALL MNT.REG6			   		     ! Mantenimiento Registro
WEND

!----
!---- Fin del bloque principal
!----

E0106:
         IF ERRF% = AREA1% AND                              \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN            ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 3,,,30000 RECL 55   \! si no existe lo crea.
                        AS AREA1% compound perupdate 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIF                                             ! del programa

         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            RESUME
         ENDIF
         IF ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIF						       !
         CALL TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa EPSP0109.BAS
