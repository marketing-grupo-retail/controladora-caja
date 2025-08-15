!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MNTREMIS.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion Consecutivo de        *
!                Remisiones                       *
!**************************************************

String   GLOBAL       \
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
         CADENA.FINAL$

Integer*2 GLOBAL        \
          DM.NOPROY%,   \	      ! Numero de proyectos definidos
          DM.NOCAMPOS%, \	      ! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          Call.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          INI.CAMPO%    \       ! Control despliege de proyectos en pantalla

     
String GLOBAL ARCH1$,   \	! Variable nombre arch. control
              BAN.PRG$, \	! Bandera del programa
              LEC$,    	\	! Definicion Formato registro
              FIN$,     \	! Variable de control programa
              FIN2$,    \	! Variable de control programa
              CICLO$,   \	! Variable de control programa
              TMP.MSG$, \       ! Variable temporal de trabajo
              NOREG$,   \       ! Numero de registros
              NOCAMP$,  \       ! Numero de campos
              CADENA$   \       ! Cadena de caracteres parametrizacion

Integer   GLOBAL     \
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

String  GLOBAL  INITDM$, OPNDIS$, DISPD$,  POSF$,   NXTF$,     \
                PUTF$,   CURS$,   CLSDIS$, PRM.ON$, PRM.OFF$,  \
                ON$,     OFF$,    TABS$,   TERM$,   INP$,      \
                ATTR$,   RET$,    KEY$,    KEY2$,   ERRFX$,    \
                Z$,      MSG$

Integer*4 GLOBAL HX%, S%, SX%, SUM%
String Global Flg1$, Flg2$, Flg3$, Param$(1)
String GLOBAL DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$, DM.OPER$, \!
              CLAVE.MEM$

%Include ADX_UPGM:BASROUT.J86	      ! Rutinas generales
%Include ADX_UPGM:DMEXTR.J86          ! Inclucion Libreria Display Manager


!--- Funcion captura mensajes de error
Function DM.ERR(F.RET%, ERR.TYPE$)                         ! Captura error 
   Integer F.RET%					   ! Definicion de 
   String ERR.TYPE$					   ! Variables 
   If F.RET% >= 0 Then EXIT Function 			   ! No errores
   PRINT: PRINT						   ! Imprime Error
   PRINT ERR.TYPE$
   STOP
END Function
!--- fin de la funcion captura de mensajes error

!--- Funcion de Mensajes de error
Function MSG.ERR(POS%,MSG$)
    Integer*2 POS%, RET%
    String MSG$
    RET% = POSF(0)
    RET.ERR% = POSF(POS%)
    Call DM.ERR(RET.ERR%,POSF$)
    RET.ERR% = PUTF(MSG$)
    Call DM.ERR(RET.ERR%,PUTF$)
    RET.ERR% = POSF(RET%)
    Call DM.ERR(RET.ERR%,POSF$)
END Function
!--- Fin del despliege de los mensajes de error

!--- Funcion de finalizacion de programa
Function QUIT
    Call SETF("0000000")				     !
    Call CLRSCR						     !
    RET.ERR%= CLSDIS					     !
    Call DM.ERR(RET.ERR%,CLSDIS$)			     !
    Stop						     !
END Function
!--- Fin de la funcion de finalizacion programa

!--- Funcion restauracion de pantalla despues de un help
Function REST.DISP
   RET.ERR% = DISPD(Call.ORDER%)
   Call DM.ERR(RET.ERR%,DISPD$)
   RET.ERR% = POSF(CURR.FLD%)
   Call SETF(PRM.ON$)
   Call CURS("0")
   Call DM.ERR(RET.ERR%,POSF$)
END Function
!--- Funcion captura de datos de pantalla en Display Manager

!--- Funcion menu de ayuda del programa
Function HELP(HLP.PRG$,HLP.FILE$)                          ! Parametro Programa y archivo
  String HLP.PRG$, HLP.FILE$,MSG1$,REG.HLP$,INP2$
  Integer*2 CNTI%, NRG%
      Call.ORDER% = 100                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
      Call MSG.ERR(1,HLP.PRG$)
      BAN.PRG$ = "0"
      HLP.FILE$="ADX_UPGM:"+HLP.FILE$			   ! Archivo de help
      Open HLP.FILE$ AS 19 NODEL                           ! Apertura Archivo Help
      If BAN.PRG$ = "1" Then BEGIN                        \!
         MSG1$ = "Archivo de Help "+HLP.FILE$+" No Existe o Sin InFormaci¢n"
         Call MSG.ERR(17,MSG1$): WAIT;1800: EXIT Function
      EndIf
    INP2$ = " ": NRG% = 1
    While (INP2$ = " " )
      BAN.PRG$ = "0"
      For CNTI% = 1 TO 15                                  !
          Read #19; LINE REG.HLP$
          If BAN.PRG$ = "0" Then                          \!
             Call MSG.ERR(CNTI%+1,REG.HLP$): NRG%= NRG%+1
          If BAN.PRG$ = "1" Then BEGIN                    \!
             CNTI% = 16					   !
             CLOSE 19					   !
             Open HLP.FILE$ AS 19 NODEL                    ! Apertura Archivo Help
          EndIf
      NEXT CNTI%  
      RET.ERR% = NXTF(-20) 	                           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   !
      ATTR$ = SETF(PRM.ON$)                                !		
      INP2$ = UPDF                                         ! Captura dato en pantalla
      If (ENDF = F1.AYUDA) Then INP2$="X"                  ! Si presiona tecla F1
      Call.ORDER% = 100                                    ! Definicion de la Pantalla Help DM
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)			   ! Despliege de la pantalla
      Call MSG.ERR(1,HLP.PRG$)
    Wend
    CLOSE 19
    BAN.PRG$ = "0"
END Function
!--- Fin de la funcion de ayuda

Function GET.DATOS
String GET.DATOS
   RET.ERR% = NXTF(2)			!Proximo campo
   Call DM.ERR(RET.ERR%,NXTF$)
   ATTR$ = SETF(PRM.ON$)
   INP$ = UPDF
   GET.DATOS = INP$
END Function

!--- Funcion Inicializacion de Varibles del Display Manager
Function INICIADM
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
END Function
!--- Fin de la funcion inicializacion display manager

Function TRADUCE.ERROR                                       !
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    For S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END Function
!--- Fin procedimiento traduccion error 

Function CTRL.ACCESO
ACCESO:
      Call.ORDER% = 250                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      Call MSG.ERR(05,"Digite C¢digo de Usuario ....         ")
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.USER$ = INP$
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      If (ENDF = F1.AYUDA) Then BEGIN                     \! Si presiona tecla F3
        Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
        GOTO ACCESO 
      EndIf
      While DM.USER$ = ""
         Call MSG.ERR(05,"Error: Entre C¢digo Usuario ...       ")
         RET.ERR% = NXTF(-20)
         Call DM.ERR(RET.ERR%,NXTF$)
         INP$ = UPDF                                       ! Captura dato en pantalla
         DM.USER$ = INP$
         If (ENDF = F3.SALIR) Then Call QUIT               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT               ! Si presiona tecla ESC
         If (ENDF = F1.AYUDA) Then BEGIN                  \! Si presiona tecla F3
            Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         EndIf
      Wend
      BAN.PRG$ = "0"
      DM.USER$=PACK$(Right$("0000000000"+DM.USER$,10))     ! Creacion llave usuario
      LEC$="C5 C4 C1 C22 C20 C20"
      Read ForM LEC$;#AREA2% KEY DM.USER$;                \! Lee Reg usuario
        DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$,          \!
        DM.OPER$,DM.RESTO$				   !
      If BAN.PRG$ = "0" Then BEGIN                        \!
         Call MSG.ERR(03,DM.OPER$)
         If Val(UNPACK$(DM.MODELO$)) < 5 Then BEGIN      \!
            Call MSG.ERR(05,"Error: Nivel de Usuario No Autorizado ")
            WAIT;2000
            Call QUIT
	 EndIf  
         Call MSG.ERR(05,"Digite Clave de Usuario  ....         ")
         CLAVE.MEM$ = GET.DATOS				   ! Captura la clave 
         If (ENDF = F3.SALIR) Then Call QUIT               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT               ! Si presiona tecla ESC
         If (ENDF = F1.AYUDA) Then BEGIN                  \! Si presiona tecla F3
            Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         EndIf
         While CLAVE.MEM$ = ""
            Call MSG.ERR(05,"Error: Entre Clave  Usuario ...       ")
            RET.ERR% = NXTF(-2)
            Call DM.ERR(RET.ERR%,NXTF$)
            CLAVE.MEM$ = GET.DATOS
            If (ENDF = F3.SALIR) Then Call QUIT            ! Si presiona tecla F3
            If (ENDF = ESC.KEY)  Then Call QUIT            ! Si presiona tecla ESC
            If (ENDF = F1.AYUDA) Then BEGIN                  \! Si presiona tecla F3
               Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
               GOTO ACCESO 
            EndIf 
         Wend
         CLAVE.MEM$=PACK$(Right$("00000000"+CLAVE.MEM$,8)) ! Arma clave capturada
         If CLAVE.MEM$ <> DM.PASWD$ Then  BEGIN           \!
            Call MSG.ERR(05,"Error: Clave de Acceso Erronea ...    ") 
            WAIT;2000
            Call QUIT
         EndIf 
      EndIf
      If BAN.PRG$ = "1" Then BEGIN			  \!
         Call MSG.ERR(05,"Error: Usuario No Autorizado o No Existe ")
         WAIT;2000
         Call QUIT
      EndIf
END Function
!--- Fin del control de acceso

Sub INICIO01
      DM.OPCION$   = "  "
      Call.ORDER% = 3                                      ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      Call MSG.ERR(04,"Numero de Terminal")       	       ! Version de la aplicacion
End Sub 

Sub GRABA.ARCHIVO
String XKey$, Xnull$, XStat$
Open "TF:NUMREM" KEYED RECL 7 As Area1% 
Xkey$ = Pack$(Right$("0000"+Flg1$,4))
Flg2$ = Pack$(Right$("0000000000"+Flg2$,10))
Write Form "C2 C5";#Area1%; XKEY$, Flg2$
Close Area1%
End Sub 

Sub BUSCA.ARCHIVO
String XKey$, Xnull$, XStat$
FLG2$ = "0"
Open "TF:NUMREM" KEYED RECL 7 As Area1% 
Xkey$ = Pack$(Right$("0000"+Flg1$,4))
BAN.PRG$ = "0"
Read Form "C2 C5";#Area1% Key XKey$ ; Xnull$, XStat$
If BAN.PRG$ = "0" Then FLG2$ = Str$(Val(UnPack$(XStat$)))
Close Area1%
End Sub 

!
!----- Menu principal
!

On Error GoTo EPSERROR
Call INICIADM 				                                      ! Inicializacion Variables Display Manager
AREA1% = 1: AREA2% = 2				                              ! Definicion area de trabajo archivo
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL               ! Abre archivo de operadores
TERM$ = " "					                                        ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					                            !
Call DM.ERR(RET.ERR%,INITDM$)				                        !
RET.ERR% = OPNDIS("ADX_UPGM:REMPANTA.PNT")	                ! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
!Call CTRL.ACCESO					                                  ! Ctrl acceso aplicativo
Close AREA2%
While (FIN$ = "0") 
      Call INICIO01
      RET.ERR% = NXTF(-20) 			                           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      Flg1$ = INP$                                         ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("MANT CONSECUTIVO REMISION","MNTREMIS.TXT") !
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      While Val(Flg1$) < 0
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        Flg1$ = INP$                                  ! Asigna valor capturado
        If (ENDF = F1.AYUDA) Then Begin \
           Call HELP("MANT CONSECUTIVO REMISION","MNTREMIS.TXT") !
           Call INICIO01
        EndIf
        If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      Wend 
      Call BUSCA.ARCHIVO
      Call MSG.ERR(04,"Numero de Consecutivo de Remision")
      Call MSG.ERR(03,FLG2$)
      Flg2$ = GET.DATOS
      If (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC
      While Val(Flg2$) < 0 
         Call MSG.ERR(03,FlG2$)
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Flg2$ = GET.DATOS
         IF (ENDF = F3.SALIR) Then Call Quit                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Call Quit                 ! Si presiona tecla ESC
      Wend
      Call GRABA.ARCHIVO
Wend
Call Quit
Stop 

!--- Definicion de las rutinas de error
EPSERROR:
         If ERRF% = AREA1% AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de Parametros
             CREATE POSFILE "TF:NUMREM" KEYED 2,,,1000 RECL 7 \! si no existe lo crea.
                    AS Area1% Compound Perupdate               ! Apertura archivo 
            BAN.PRG$ = "1"				                             ! ejecucion normal del
            Resume                                             ! programa
         EndIf 
         If ERRF% = AREA1% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
            BAN.PRG$ = "1"				                             ! ejecucion normal del
            Resume 
         EndIf
         If ERRF% = AREA2% AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin              ! el archivo de control
             MSG$ = "Error: EAMOPERA No disponible o no Existe"
             LOCATE 24,1: PRINT MSG$
             STOP
         EndIf
         If ERRF% = AREA2% AND ERR = "EF" Then Begin          \! Si encuentra fin de 
            BAN.PRG$ = "1"				                             ! ejecucion normal del
            Resume
         EndIf
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then Begin  \! Valida la lectura
            BAN.PRG$ = "1"				                             ! del archivo de 
            RESUME					                                   ! help del aplicativo
         EndIf						                                     !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                                ! Presenta Error pantalla
         Stop 
