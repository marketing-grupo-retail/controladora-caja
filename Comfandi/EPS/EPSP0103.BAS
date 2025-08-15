!**************************************************
!Empresa       : Casa Informatica Unidad  Retail  *
!Programa      : EPSP0103.bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenimiento Medicos            *
!**************************************************
! Modificaciones: 
! Cambia la longitud del registro de 46 a 58 caracteres colocando un 
! campo nuevo llamado DM.CODALT$ de longitud 12 sin empaquetar, conteniendo
! el codigo alterno del medico para interfaces. Desarrollado por OVS
! 14/nov/2001

%INCLUDE EPSPVARI.BAS				  	     ! Definicion Variables
%INCLUDE DMEXTR.J86          				     ! Inclucion Libreria Display Manager
%INCLUDE EPSPRUTI.BAS				  	     ! Rutinas Comunes

!--- Fin  definicion de variables

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0103
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 11    					           ! Definicion area de trabajo archivo
ARCH1$="MEDICO"			                       	   ! Asignar nombre archivo control
OPEN ARCH1$ KEYED RECL 58 AS AREA1%                        ! Abre archivo de control
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
WHILE (FIN$ = "0") 
 CICLO:
      DM.OPCION$   = "  "
      MSG$ = "MANTENIMIENTO DE MEDICOS    "
      CALL INICIO2
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.NIT$ = INP$                                       ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN \
         CALL HELP("DEFINICION DE MEDICOS    ","EPSP0103.TXT") !
         MSG$ = "MANTENIMIENTO DE MEDICOS      "
         CALL INICIO2
      ENDIF
      IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
      IF (ENDF = F7.KEY)   THEN BEGIN
         CLOSE AREA1%
         OPEN ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
         CALL VISUAL.REG7                                  ! Si presiona tecla F7
         CLOSE AREA1%
         OPEN ARCH1$ KEYED RECL 58 AS AREA1%               ! Abre archivo de control
         GOTO CICLO
      ENDIF
      IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      WHILE DM.NIT$ = " " OR VAL(DM.NIT$) = 0
        RET.ERR% = NXTF(-20)
        CALL DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        DM.NIT$ = INP$                                     ! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN \
           CALL HELP("DEFINICION DE MEDICOS    ","EPSP0103.TXT") !
           MSG$ = "MANTENIMIENTO DE MEDICOS      "
           CALL INICIO2
        ENDIF
        IF (ENDF = F3.SALIR) THEN CALL QUIT                ! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN CALL QUIT                ! Si presiona tecla ESC
        IF (ENDF = F7.KEY)   THEN BEGIN
           CLOSE AREA1%
           OPEN ARCH1$ DIRECT RECL 512 AS AREA1% NODEL       ! Open file
           CALL VISUAL.REG7                                  ! Si presiona tecla F7
           CLOSE AREA1%
           OPEN ARCH1$ KEYED RECL 58 AS AREA1%               ! Abre archivo de control
           GOTO CICLO
        ENDIF
      WEND
      MSG$="Digite Nombre del M‚dico         "
      CALL MNT.REG2  					   ! Mantenimiento Registro
WEND

!----
!---- Fin del bloque principal
!----

E0103:
         IF ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 6,,,5000 RECL 58  \! si no existe lo crea.
                        AS AREA1% compound perupdate 		   ! 
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

!--- Fin del programa EPSP0103.BAS
