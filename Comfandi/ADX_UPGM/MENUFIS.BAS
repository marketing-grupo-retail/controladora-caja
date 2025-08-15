!************************************************** 
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : GRFISCOL.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : MENU FISCAL                      *
!**************************************************
!

STRING   GLOBAL       \
         DM.OPCION$,  \         ! Variable Captura de opciones
         DM.OPCION2$, \         ! Variable Captura de opciones
         DM.NIT$,     \  	      ! Nit de la empresa
         DM.RAZON$,   \		      ! Razon social empresa
         DM.SIGLA$,   \ 	      ! Nombre Comercial
         DM.ALMACEN$, \	        ! Codigo del punto de venta
         DM.BASURA$,  \		      ! Resto del registro
         DM.LLAVE$,   \         ! Llave registro de control
         DM.NOMPRO$,  \         ! Nombre del proyecto
         DM.ESTPRO$,  \         ! Estado del proyecto
         DM.DATO$,    \         ! Captura de dato en pantalla
         TOT.CAM$,    \         ! Captura de dato en pantalla
         DM.PARAM$,   \         ! Captura datos de parametrizacion
         DM.NOFIELDS$, \	      ! Numero de campos
         DM.TMP.KEY$(1), \      ! Arreglo llaves del aplicativo
         TMP.CADENA$, \
         CADENA.FINAL$

INTEGER*2 GLOBAL        \
          DM.NOPROY%,   \	! Numero de proyectos definidos
          DM.NOCAMPOS%, \	! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          CALL.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          INI.CAMPO%    \       ! Control despliege de proyectos en pantalla

     
STRING GLOBAL ARCH1$,   \	! Variable nombre arch. control
              BAN.PRG$, \	! Bandera del programa
              LEC$,    	\	! Definicion formato registro
              FIN$,     \	! Variable de control programa
              FIN2$,    \	! Variable de control programa
              CICLO$,   \	! Variable de control programa
              TMP.MSG$, \       ! Variable temporal de trabajo
              TMP.KEY$, \	! Almacena temporalmente la llave del registro
              NOREG$,   \       ! Numero de registros
              NOCAMP$,  \       ! Numero de campos
              CADENA$   \       ! Cadena de caracteres parametrizacion

INTEGER   GLOBAL     \
          AREA1%,    \          ! Definicion area de trabajo
          AREA2%,    \          ! Definicion area de trabajo
          PNT.ANT%,  \          ! Captura de la pantalla anterior
          CURR.FLD%, \          ! Campo actual 
          ENTER.KEY, \         	! Definicion de la tecla Enter
          TAB.KEY,   \          ! Definicion de la tecla TAB
          ESC.KEY,   \        	! Definicion de la tecla ESCAPE
          F1.AYUDA,  \         	! Deficion de la tecla de funcion F1 
          F2.KEY  ,  \         	! Deficion de la tecla de funcion F2 
          F3.SALIR,  \      	! Deficion de la tecla de funcion F3
          F4.KEY  ,  \         	! Deficion de la tecla de funcion F4 
          F5.KEY  ,  \         	! Deficion de la tecla de funcion F5 
          F6.KEY  ,  \         	! Deficion de la tecla de funcion F6 
          F7.KEY  ,  \         	! Deficion de la tecla de funcion F7 
          F8.KEY  ,  \         	! Deficion de la tecla de funcion F8 
          F9.KEY  ,  \         	! Deficion de la tecla de funcion F9 
          FA.KEY  ,  \         	! Deficion de la tecla de funcion F10
          HELP%   ,  \        	! Llamado Segunda Pantalla Display Manager
          I%,        \		! Control de ciclos
          J%,        \		! Control de ciclos
          K%,        \		! Control de ciclos
          L%,        \		! Control de ciclos
          CAMPO%,    \          ! Captura posicion en pantalla del cursor
          F%,        \          ! Variable de control ciclico
          LONGITUD%, \          ! Logitud cadena de caracteres
          NOREG%, NOCAMP%

STRING  GLOBAL  INITDM$, OPNDIS$, DISPD$,  POSF$,   NXTF$,     \
                PUTF$,   CURS$,   CLSDIS$, PRM.ON$, PRM.OFF$,  \
                ON$,     OFF$,    TABS$,   TERM$,   INP$,      \
                ATTR$,   RET$,    KEY$,    KEY2$,   ERRFX$,    \
                Z$,      MSG$

INTEGER*4 GLOBAL HX%, S%, SX%, SUM%

STRING GLOBAL DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$, DM.OPER$, \!
              CLAVE.MEM$

Integer*1 Global TS.ER.RETURN

%INCLUDE ADX_UPGM:BASROUT.J86	      ! Rutinas generales
%INCLUDE ADX_UPGM:DMEXTR.J86          ! Inclucion Libreria Display Manager


!--- Funcion captura mensajes de error
FUNCTION DM.ERR(F.RET%, ERR.TYPE$)                         ! Captura error 
   INTEGER F.RET%					   ! Definicion de 
   STRING ERR.TYPE$					   ! Variables 
   IF F.RET% >= 0 THEN EXIT FUNCTION 			   ! No errores
   PRINT: PRINT						   ! Imprime Error
   PRINT ERR.TYPE$
   STOP
END FUNCTION
!--- fin de la funcion captura de mensajes error

!--- Funcion de Mensajes de error
FUNCTION MSG.ERR(POS%,MSG$)
    INTEGER*2 POS%, RET%
    STRING MSG$
    RET% = POSF(0)
    RET.ERR% = POSF(POS%)
    CALL DM.ERR(RET.ERR%,POSF$)
    RET.ERR% = PUTF(MSG$)
    CALL DM.ERR(RET.ERR%,PUTF$)
    RET.ERR% = POSF(RET%)
    CALL DM.ERR(RET.ERR%,POSF$)
END FUNCTION
!--- Fin del despliege de los mensajes de error

!--- Funcion de finalizacion de programa
FUNCTION QUIT
    CALL SETF("0000000")				     !
    CALL CLRSCR						     !
    RET.ERR%= CLSDIS					     !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			     !
    STOP						     !
END FUNCTION
!--- Fin de la funcion de finalizacion programa

!--- Funcion restauracion de pantalla despues de un help
FUNCTION REST.DISP
   RET.ERR% = DISPD(CALL.ORDER%)
   CALL DM.ERR(RET.ERR%,DISPD$)
   RET.ERR% = POSF(CURR.FLD%)
   CALL SETF(PRM.ON$)
   CALL CURS("0")
   CALL DM.ERR(RET.ERR%,POSF$)
END FUNCTION
!--- Funcion captura de datos de pantalla en Display Manager

!--- Funcion menu de ayuda del programa
FUNCTION HELP(HLP.PRG$,HLP.FILE$)                          ! Parametro Programa y archivo
  STRING HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  INTEGER*2 CNTI%, NRG%
      CALL.ORDER% = 100                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
      CALL MSG.ERR(1,HLP.PRG$)
      BAN.PRG$ = "0"
      HLP.FILE$="ADX_UPGM:"+HLP.FILE$			   ! Archivo de help
      OPEN HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      IF BAN.PRG$ = "1" THEN BEGIN                        \!
         MSG1$ = "Archivo de Help "+HLP.FILE$+" No Existe o Sin Informaci¢n"
         CALL MSG.ERR(17,MSG1$): WAIT;1800: EXIT FUNCTION
      ENDIF
    INP2$ = " ": NRG% = 1
    WHILE (INP2$ = " " )
      BAN.PRG$ = "0"
      FOR CNTI% = 1 TO 15                                  !
          READ #19; LINE REG.HLP$
          IF BAN.PRG$ = "0" THEN                          \!
             CALL MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          IF BAN.PRG$ = "1" THEN BEGIN                    \!
             CNTI% = 16					   !
             CLOSE 19					   !
             OPEN HLP.FILE$ AS 19 NODEL                    ! Apertura Archivo Help
          ENDIF
      NEXT CNTI%  
      RET.ERR% = NXTF(-20) 	                           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      IF (ENDF = F1.AYUDA) THEN INP2$="X"                  ! Si presiona tecla F1
      CALL.ORDER% = 100                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
      CALL MSG.ERR(1,HLP.PRG$)
    WEND
    CLOSE 19
    BAN.PRG$ = "0"
END FUNCTION
!--- Fin de la funcion de ayuda

FUNCTION GET.DATOS
STRING GET.DATOS
   RET.ERR% = NXTF(2)			!Proximo campo
   CALL DM.ERR(RET.ERR%,NXTF$)
   ATTR$ = SETF(PRM.ON$)
   INP$ = UPDF
   GET.DATOS = INP$
END FUNCTION

!--- Captura de datos en pantalla de proyectos
FUNCTION GET.DATOS2
STRING GET.DATOS2
   RET.ERR% = NXTF(2)			!Proximo campo
   CALL DM.ERR(RET.ERR%,NXTF$)
   ATTR$ = SETF(PRM.ON$)
   CAMPO%=POSF(0)			  	           ! Capturo posicion del campo
   IF (CAMPO%= 4 OR CAMPO%= 7 OR CAMPO%=10 OR             \! Captura unicamente
       CAMPO%=13 OR CAMPO%=16 OR CAMPO%=19 OR             \! de los campos seleccionados
       CAMPO%=22 OR CAMPO%=25 OR CAMPO%=28 OR             \!
       CAMPO%=31 OR CAMPO%=34 OR CAMPO%=37) THEN BEGIN    \
       INP$ = UPDF
       GET.DATOS2 = INP$
   ENDIF
   IF CAMPO%=38 THEN EXIT FUNCTION
END FUNCTION
!--- Fin de la funcion de captura de nombre de proyectos

!--- Funcion Inicializacion de Varibles del Display Manager
FUNCTION INICIADM
  INITDM$ = "Error:Inicializaci¢n de D.M. fallo"
  OPNDIS$ = "Error:Archivo de Formatos no se encontro"
  DISPD$  = "Error:Formato de Pantalla no se encontro"       !
  POSF$   = "Error:Campo no se encontro"	   	     ! Mensajes de ERROR
  NXTF$   = "Error:Siguiente Campo no se encontro"	     ! manejados en el 
  FIN$    = "Error:Siguiente Campo no se encontro"	     ! display manager 
  PUTF$   = "Error:No se puede desplegar el error"	     !                   
  CURS$   = "Error:Cambio de cursor (ON/OFF) fallo"
  CLSDIS$ = "Error:No se pudo cerrar el archivo de Formatos"
  ENTER.KEY   = 0      					     ! Definicion de la tecla Enter
  TAB.KEY     = 9      					     ! Definicion de la tecla TAB
  ESC.KEY     = 27     					     ! Definicion de la tecla ESCAPE
  F1.AYUDA    = -1     				             ! Definicion de la tecla de funcion F1
  F2.KEY      = -2					     ! Definicion de la tecla de funcion F2
  F3.SALIR    = -3     					     ! Definicion de la tecla de funcion F3
  F4.KEY      = -4					     ! Definicion de la tecla de funcion F4
  F5.KEY      = -5					     ! Definicion de la tecla de funcion F5
  F6.KEY      = -6					     ! Definicion de la tecla de funcion F6
  F7.KEY      = -7					     ! Definicion de la tecla de funcion F7
  F8.KEY      = -8					     ! Definicion de la tecla de funcion F8
  F9.KEY      = -9					     ! Definicion de la tecla de funcion F9
  FA.KEY      = -10					     ! Definicion de la tecla de funcion F10
  HELP%       = 2      					     ! Llamado Segunda Pantalla Display Manager
  PRM.ON$     = "031"  					     ! Parametros de pantalla
  PRM.OFF$    = "330"  					     ! Parametros de pantalla
  ON$         = "0"    					     ! Campo visible
  OFF$        = "1"    					     ! Campo invisible
  TABS$       = ""                                           ! Tabs para salida
END FUNCTION
!--- Fin de la funcion inicializacion display manager

!--- Despliegue de mensaje del aplicativo
FUNCTION MENSAJE
   CALL MSG.ERR(2,"Opci¢n En Desarrollo ...")
   WAIT;1800
   CALL MSG.ERR(2,"                        ")
END FUNCTION
!--- Fin de la funcion de mensajes

FUNCTION TRADUCE.ERROR                                       !
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END FUNCTION
!--- Fin procedimiento traduccion error 

Sub FISCAL.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$)
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%

			TS.ER.RETURN = -1
			X.FILE$ = "ADX_UDT1:NF" + Left$(DATE$,6) + ".TXT"
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            CALL MSG.ERR(05,"Error: Falla Creación Auditoria")
            WAIT;2000
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                                !
			X.Len% = Len(X.BUFF$)						  				  								            ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						            ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					          ! Graba registro      
			Close 56
End Sub 

FUNCTION CTRL.ACCESO
ACCESO:
      CALL.ORDER% = 250                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      CALL MSG.ERR(05,"Digite C¢digo de Usuario ....         ")
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.USER$ = INP$
      IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      IF (ENDF = F1.AYUDA) THEN BEGIN                     \! Si presiona tecla F3
        CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
        GOTO ACCESO 
      ENDIF
      WHILE DM.USER$ = ""
         CALL MSG.ERR(05,"Error: Entre C¢digo Usuario ...       ")
         RET.ERR% = NXTF(-20)
         CALL DM.ERR(RET.ERR%,NXTF$)
         INP$ = UPDF                                       ! Captura dato en pantalla
         DM.USER$ = INP$
         IF (ENDF = F3.SALIR) THEN CALL QUIT               ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN CALL QUIT               ! Si presiona tecla ESC
         IF (ENDF = F1.AYUDA) THEN BEGIN                  \! Si presiona tecla F3
            CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         ENDIF
      WEND
      BAN.PRG$ = "0"
      DM.USER$=PACK$(RIGHT$("0000000000"+DM.USER$,10))     ! Creacion llave usuario
      LEC$="C5 C4 C1 C22 C20 C20"
      READ FORM LEC$;#AREA2% KEY DM.USER$;                \! Lee Reg usuario
        DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$,          \!
        DM.OPER$,DM.RESTO$				   !
      IF BAN.PRG$ = "0" THEN BEGIN                        \!
         CALL MSG.ERR(03,DM.OPER$)
         IF VAL(UNPACK$(DM.MODELO$)) <> 9 AND             \!
            VAL(UNPACK$(DM.MODELO$)) <> 5 THEN BEGIN      \!
            CALL MSG.ERR(05,"Error: Nivel de Usuario No Autorizado ")
            WAIT;2000
            CALL QUIT
	 ENDIF  
         CALL MSG.ERR(05,"Digite Clave de Usuario  ....         ")
         CLAVE.MEM$ = GET.DATOS				   ! Captura la clave 
         IF (ENDF = F3.SALIR) THEN CALL QUIT               ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN CALL QUIT               ! Si presiona tecla ESC
         IF (ENDF = F1.AYUDA) THEN BEGIN                  \! Si presiona tecla F3
            CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         ENDIF
         WHILE CLAVE.MEM$ = ""
            CALL MSG.ERR(05,"Error: Entre Clave  Usuario ...       ")
            RET.ERR% = NXTF(-2)
            CALL DM.ERR(RET.ERR%,NXTF$)
            CLAVE.MEM$ = GET.DATOS
            IF (ENDF = F3.SALIR) THEN CALL QUIT            ! Si presiona tecla F3
            IF (ENDF = ESC.KEY)  THEN CALL QUIT            ! Si presiona tecla ESC
            IF (ENDF = F1.AYUDA) THEN BEGIN                  \! Si presiona tecla F3
               CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
               GOTO ACCESO 
            ENDIF
         WEND
         CLAVE.MEM$=PACK$(RIGHT$("00000000"+CLAVE.MEM$,8)) ! Arma clave capturada
         IF CLAVE.MEM$ <> DM.PASWD$ THEN  BEGIN           \!
            CALL MSG.ERR(05,"Error: Clave de Acceso Erronea ...    ") 
            WAIT;2000
            CALL QUIT
         ENDIF
      ENDIF
      IF BAN.PRG$ = "1" THEN BEGIN			  \!
         CALL MSG.ERR(05,"Error: Usuario No Autorizado o No Existe ")
         WAIT;2000
         CALL QUIT
      ENDIF
END FUNCTION
!--- Fin del control de acceso


!--------------------------!
!--------------------------!
!--------------------------!
!--- PROGRAMA PRINCIPAL ---!
!--------------------------!
!--------------------------!
!--------------------------!

ON ERROR GOTO EPSERROR
CALL INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 1: AREA2% = 2				           ! Definicion area de trabajo archivo
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              ! Abre archivo de operadores
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
CALL DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:FISCAL.PNT")	           ! Apertura de la forma de pantalla
CALL DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call CTRL.ACCESO					   ! Ctrl acceso aplicativo
Close AREA2%						   !
Call FISCAL.AUDITORIA("ACCESO APLICACION","USUARIO "+UNPACK$(DM.USER$),DATE$ + ":"+TIME$, DATE$ + ":"+TIME$)
WHILE (FIN$ = "0") 
      DM.OPCION$   = "  "
      CALL.ORDER% = 10                                      ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN \
         CALL HELP("MENU PRINCIPAL APLICATIVO","MENUFIS.TXT") !
      IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      IF VAL(DM.OPCION$) = 1 THEN CALL OSSHELL("ADX_UPGM:GRFISCOL.286 "+UNPACK$(DM.USER$))
      IF VAL(DM.OPCION$) = 2 THEN CALL OSSHELL("ADX_UPGM:GRMNTIVA.286")
Wend


!--- Definicion de las rutinas de error
EPSERROR:
        TS.ER.RETURN = 0
         CONSOLE                                           ! CLOSE PRINTER IF OPEN IR75403
         
         IF ERRF% = AREA2% AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
             MSG$ = "Error: EAMOPERA No disponible o no Existe"
             LOCATE 24,1: PRINT MSG$
             STOP
         ENDIF                                                 ! del programa

         IF ERRF% = 56 AND                                    \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN              ! el archivo de control
            Resume 
         ENDIF                                                 ! del programa

         IF ERRF% = AREA1% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            RESUME
         ENDIF
         IF ERRF% = AREA2% AND ERR = "EF" THEN BEGIN      \! Si encuentra fin de 
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

END

