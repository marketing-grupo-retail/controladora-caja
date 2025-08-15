!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : SPVP0001.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Parametros Modulo Tirilla        *
!                Supervigilancia                  *
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
         If Val(UNPACK$(DM.MODELO$)) <> 9 AND             \!
            Val(UNPACK$(DM.MODELO$)) <> 8 Then BEGIN      \!
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

Sub CARGA.PARAMETROS
String TS.TEMP1$
   BAN.PRG$ = "0"
   Dim Param$(5)
   Open "TF:FACO05" AS AREA1%				  				        ! Apertura archivo parametrizacion
   If BAN.PRG$ <> "0" Then Begin								! Error en la apertura
      FLG1$ = "00" : FLG2$ = "00" : FLG3$ = "00"
      Param$(1)  = ""
      Param$(2)  = ""
      Param$(3)  = ""
      Param$(4)  = ""
      Exit Sub 											! Sale del procedimiento
   EndIf Else Begin 
      Read #AREA1%; TS.TEMP1$									! Proyecto Activo
      FLG1$ = Mid$(TS.TEMP1$,30,2)
      Read #AREA1%; TS.TEMP1$									! Numero de copias
      FLG2$ = Mid$(TS.TEMP1$,30,2)
      Read #AREA1%; TS.TEMP1$									! Tipo de validacion 
      FLG3$ = Mid$(TS.TEMP1$,30,2)
      Read #AREA1%; TS.TEMP1$									! Lista departamentos a controlar
      TS.TEMP1$  = Mid$(TS.TEMP1$,030,200)
      Param$(1)  = Mid$(TS.TEMP1$,001,050)
      Param$(2)  = Mid$(TS.TEMP1$,051,050)
      Param$(3)  = Mid$(TS.TEMP1$,101,050)
      Param$(4)  = Mid$(TS.TEMP1$,151,050)
   EndIf 
   Close AREA1%
End Sub 

Sub INICIO01
      DM.OPCION$   = "  "
      Call.ORDER% = 1                                      ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      Call MSG.ERR(09,"Activar / Desactivar Proyecto")	   ! Version de la aplicacion
      Call MSG.ERR(02,FLG1$)
      Call MSG.ERR(03,FLG2$)
      Call MSG.ERR(04,FLG3$)
      Call MSG.ERR(05,Param$(1))
      Call MSG.ERR(06,Param$(2))
      Call MSG.ERR(07,Param$(3))
      Call MSG.ERR(08,Param$(4))
End Sub 

Sub GRABA.ARCHIVO
String Ln1$, X.lec$, Finr$
Integer*4 X.len%
  CALL MSG.ERR(09,"Almacenando Informacion, Espere por favor...")
  FINR$ = CHR$(13)+CHR$(10)
  Create Posfile "TF:FACO05" AS Area1% compound perupdate  ! creacion archivo parametrizacion
  Ln1$ = "PROYECTO_ACTIVO              "+FLG1$
  X.Len% = Len(Ln1$)					! Toma longitud del registro
  X.Lec$ = "C"+Str$(X.len%)+" C2"		        ! Arma estructura de grabacion
  Write form X.Lec$; #Area1% ; Ln1$, Finr$           	! Graba registro
  Ln1$ = "NUMERO_COPIAS_VOUCHER        "+FLG2$
  X.Len% = Len(Ln1$)					! Toma longitud del registro
  X.Lec$ = "C"+Str$(X.len%)+" C2"		        ! Arma estructura de grabacion
  Write form X.Lec$; #Area1% ; Ln1$, Finr$           	! Graba registro
  Ln1$ = "TIPO_DATO_VALIDAR            "+FLG3$
  X.Len% = Len(Ln1$)					! Toma longitud del registro
  X.Lec$ = "C"+Str$(X.len%)+" C2"		        ! Arma estructura de grabacion
  Write form X.Lec$; #Area1% ; Ln1$, Finr$           	! Graba registro
  Ln1$ = Param$(1) + Param$(2) + Param$(3) + Param$(4)
  Ln1$ = "LISTA_A_VALIDAR              "+Ln1$
  X.Len% = Len(Ln1$)					! Toma longitud del registro
  X.Lec$ = "C"+Str$(X.len%)+" C2"		        ! Arma estructura de grabacion
  Write form X.Lec$; #Area1% ; Ln1$, Finr$           	! Graba registro
  Close Area1%
  CALL MSG.ERR(09,"Informacion Grabada con Exito!")
End Sub 
!
!----- Menu principal
!

ON ERROR GOTO EPSERROR
Call INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 1: AREA2% = 2				           ! Definicion area de trabajo archivo
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              ! Abre archivo de operadores
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
Call DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:SPVPANTA.PNT")	           ! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call CTRL.ACCESO					   ! Ctrl acceso aplicativo
Close AREA2%						   !
Call CARGA.PARAMETROS
While (FIN$ = "0") 
      Call INICIO01
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      Flg1$ = INP$                                         ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("PARAMETROS SUPER VIGILANC","SPVP0001.TXT") !
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      While Val(Flg1$) < -1 OR Val(Flg1$) > 0
        RET.ERR% = NXTF(-20)
        Call DM.ERR(RET.ERR%,NXTF$)
        ATTR$ = SETF(PRM.ON$)                                
        INP$ = UPDF                                        ! Captura dato en pantalla
        Flg1$ = INP$                                  ! Asigna valor capturado
        If (ENDF = F1.AYUDA) Then Begin \
           Call HELP("PARAMETROS SUPER VIGILANC","SPVP0001.TXT") !
           CALL INICIO01
        EndIf
        If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
        If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      Wend 
      If Val(Flg1$) = 0 Then Flg1$ = "00" Else Flg1$ = "-1"
      CALL MSG.ERR(09,"Numero de copias del voucher")
      Flg2$ = GET.DATOS
      If (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC
      While Val(Flg2$) <= 0 
         RET.ERR% = NXTF(-2)
         CALL DM.ERR(RET.ERR%,NXTF$)
         Flg2$ = GET.DATOS
         IF (ENDF = F3.SALIR) Then Call Quit                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Call Quit                 ! Si presiona tecla ESC
      Wend
      Flg2$ = Right$("00"+Flg2$,2)
      CALL MSG.ERR(09,"Tipo de validacion a realizar")
      Flg3$ = GET.DATOS
      IF (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC
      While Val(Flg3$) < 1 OR Val(Flg3$) > 10
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         Flg3$ = GET.DATOS
         IF (ENDF = F3.SALIR) THEN Call Quit                 ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN Call Quit                 ! Si presiona tecla ESC
      Wend
      Flg3$ = Right$("00"+Flg3$,2)
      CALL MSG.ERR(09,"Lista Parametros a validar separados por ; ")
      Param$(1) = GET.DATOS
      IF (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC

      Call MSG.ERR(09,"Lista Parametros a validar separados por ; ")
      Param$(2) = GET.DATOS
      IF (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC

      CALL MSG.ERR(09,"Lista Parametros a validar separados por ; ")
      Param$(3) = GET.DATOS
      IF (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC

      CALL MSG.ERR(09,"Lista Parametros a validar separados por ; ")
      Param$(4) = GET.DATOS
      IF (ENDF = F3.SALIR) Then Call Quit                    ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Call Quit                    ! Si presiona tecla ESC
      
      Call GRABA.ARCHIVO
      FIN$ = "1"
Wend
Call Quit
Stop 

!--- Definicion de las rutinas de error
EPSERROR:
         If ERRF% = AREA1% AND                             \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then BEGIN           ! el archivo de Parametros
            Create Posfile "TF:FACO05" AS Area1% compound perupdate ! Apertura archivo parametrizacion
            BAN.PRG$ = "1"				    !
            Resume 
         EndIf 
         If ERRF% = AREA1% AND ERR = "EF" Then BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            Resume 
         EndIf
         
         If ERRF% = AREA2% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then BEGIN          ! el archivo de control
             MSG$ = "Error: EAMOPERA No disponible o no Existe"
             LOCATE 24,1: PRINT MSG$
             STOP
         EndIf                                             ! del programa
         If ERRF% = AREA2% AND ERR = "EF" Then BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
