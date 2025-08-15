!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PARAMSEG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion Opciones por Modelo   *
!                de usuario                       *
!**************************************************

String   GLOBAL       \
         DM.OPCION$,  \         ! Variable Captura de opciones
         DM.OPCION2$, \         ! Variable Captura de opciones
         DM.NIT$,     \  	! Nit de la empresa
         DM.RAZON$,   \		! Razon social empresa
         DM.SIGLA$,   \ 	! Nombre Comercial
         DM.ALMACEN$, \	        ! Codigo del punto de venta
         DM.BASURA$,  \		! Resto del registro
         DM.LLAVE$,   \         ! Llave registro de control
         DM.NOMPRO$,  \         ! Nombre del proyecto
         DM.ESTPRO$,  \         ! Estado del proyecto
         DM.DATO$,    \         ! Captura de dato en pantalla
         TOT.CAM$,    \         ! Captura de dato en pantalla
         DM.PARAM$,   \         ! Captura datos de parametrizacion
         DM.NOFIELDS$, \	! Numero de campos
         DM.TMP.KEY$(1), \      ! Arreglo llaves del aplicativo
         Gr.Options(2),  \      ! Opciones parametrizadas
         TMP.CADENA$, \
         CADENA.FINAL$

Integer*2 GLOBAL        \
          DM.NOPROY%,   \	! Numero de proyectos definidos
          DM.NOCAMPOS%, \	! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          Call.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          GR.NROPT%,    \       ! Numero de opciones
          ENCRYPT.RC,   \
          INI.CAMPO%            ! Control despliege de proyectos en pantalla

     
String GLOBAL ARCH1$,   \	! Variable nombre arch. control
              BAN.PRG$, \	! Bandera del programa
              LEC$,    	\	! Definicion Formato registro
              FIN$,     \	! Variable de control programa
              FIN2$,    \	! Variable de control programa
              CICLO$,   \	! Variable de control programa
              TMP.MSG$, \       ! Variable temporal de trabajo
              TMP.KEY$, \	! Almacena temporalmente la llave del registro
              NOREG$,   \       ! Numero de registros
              NOCAMP$,  \       ! Numero de campos
              CADENA$           ! Cadena de caracteres parametrizacion

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
          TERROR%,   \          ! Ctrl de errores
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

String GLOBAL DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$, DM.OPER$, \!
              CLAVE.MEM$, NEW.PWD$

%Include ADX_UPGM:BASROUT.J86	      ! Rutinas generales
%Include ADX_UPGM:DMEXTR.J86          ! Inclucion Libreria Display Manager

SUB ADXCRYPT(RETCODE, PWIN$, PWOUT$) EXTERNAL
   STRING     PWIN$,PWOUT$
   INTEGER*2  RETCODE
END SUB


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
      ENDIf
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
          ENDIf
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

!--- Captura de datos en pantalla de proyectos
Function GET.DATOS2
String GET.DATOS2
   RET.ERR% = NXTF(2)			!Proximo campo
   Call DM.ERR(RET.ERR%,NXTF$)
   ATTR$ = SETF(PRM.ON$)
   CAMPO%=POSF(0)			  	           ! Capturo posicion del campo
   If (CAMPO%= 4 OR CAMPO%= 7 OR CAMPO%=10 OR             \! Captura unicamente
       CAMPO%=13 OR CAMPO%=16 OR CAMPO%=19 OR             \! de los campos seleccionados
       CAMPO%=22 OR CAMPO%=25 OR CAMPO%=28 OR             \!
       CAMPO%=31 OR CAMPO%=34 OR CAMPO%=37) Then BEGIN    \
       INP$ = UPDF
       GET.DATOS2 = INP$
   ENDIf
   If CAMPO%=38 Then EXIT Function
END Function
!--- Fin de la funcion de captura de nombre de proyectos

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
  F1.AYUDA    = -1     				       ! Definicion de la tecla de funcion F1
  F2.KEY      = -2					         ! Definicion de la tecla de funcion F2
  F3.SALIR    = -3     					     ! Definicion de la tecla de funcion F3
  F4.KEY      = -4					         ! Definicion de la tecla de funcion F4  
  F5.KEY      = -5					         ! Definicion de la tecla de funcion F5  
  F6.KEY      = -6					         ! Definicion de la tecla de funcion F6  
  F7.KEY      = -7					         ! Definicion de la tecla de funcion F7  
  F8.KEY      = -8					         ! Definicion de la tecla de funcion F8  
  F9.KEY      = -9					         ! Definicion de la tecla de funcion F9  
  FA.KEY      = -10					         ! Definicion de la tecla de funcion F10 
  HELP%       = 2      					     ! Llamado Segunda Pantalla Display Manager
  PRM.ON$     = "031"  					     ! Parametros de pantalla
  PRM.OFF$    = "330"  					     ! Parametros de pantalla
  ON$         = "0"    					     ! Campo visible
  OFF$        = "1"    					     ! Campo invisible
  TABS$       = ""                   ! Tabs para salida
End Function
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

Sub Carga.Menu
Integer*2 Xpos%, XI%
String A$, B$, C$, D$
Dim Gr.Options(25,3)
TERROR% = -1
GR.NROPT% = 0 
Open "TF:OPTSEG" AS 22
If TERROR% <> -1 Then Begin 
   Print "ERROR APERTURA OPCIONES DE SEGURIDAD"
   Stop
EndIf 
If End #22 THEN UE.SEG.FINAL		       																	  ! Si es EOF
While (1)
  Read #22; A$, B$, C$
  Gr.Options(Val(A$),0) = A$   ! Numero de Opcion 
  Gr.Options(Val(A$),1) = B$   ! Descriptor
  Gr.Options(Val(A$),2) = C$   ! Programa Ejecutar
Wend 	
UE.SEG.FINAL:
  Close 22
For XI% = 1 to 20
    Xpos% = 27
    Call MSG.ERR(XPOS%+XI%,Left$(Gr.Options(XI%,1),24))
 Next XI%

End Sub 

Function CTRL.ACCESO
ACCESO:
      CALL.ORDER% = 250                                    ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			                           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      CALL MSG.ERR(05,"Digite C¢digo de Usuario ....")
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.USER$ = INP$
      IF (ENDF = F3.SALIR) THEN CALL QUIT                  ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN CALL QUIT                  ! Si presiona tecla ESC
      IF (ENDF = F1.AYUDA) THEN BEGIN                     \! Si presiona tecla F3
        CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
        GOTO ACCESO 
      ENDIF
      WHILE DM.USER$ = ""
         CALL MSG.ERR(05,"Error: Entre C¢digo Usuario ...")
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
         CALL MSG.ERR(05,"Digite Clave de Usuario  ....         ")
         CLAVE.MEM$ = GET.DATOS				   ! Captura la clave 
         IF (ENDF = F3.SALIR) THEN CALL QUIT               ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN CALL QUIT               ! Si presiona tecla ESC
         IF (ENDF = F1.AYUDA) THEN BEGIN                  \! Si presiona tecla F3
            CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         ENDIF
         While CLAVE.MEM$ = ""
            CALL MSG.ERR(05,"Error: Entre Clave  Usuario ...       ")
            RET.ERR% = NXTF(-2)
            CALL DM.ERR(RET.ERR%,NXTF$)
            CLAVE.MEM$ = GET.DATOS
            If (ENDF = F3.SALIR) Then Call Quit            ! Si presiona tecla F3
            If (ENDF = ESC.KEY)  Then Call Quit            ! Si presiona tecla ESC
            If (ENDF = F1.AYUDA) Then Begin               \! Si presiona tecla F3
               CALL HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
               GOTO ACCESO 
            EndIf
         Wend
         CLAVE.MEM$=PACK$(RIGHT$("00000000"+CLAVE.MEM$,8)) ! Arma clave capturada

!         CALL ADXCRYPT(ENCRYPT.RC, STR$(VAL(UNPACK$(CLAVE.MEM$))), \
!                                 NEW.PWD$)
!         NEW.PWD$ = Pack$(NEW.PWD$)                                 

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
End Function
!--- Fin del control de acceso
  
Sub Busqueda.Modelo
String Xopt$(1), Xlista$, Xdummy$, Xdata$
Integer*1 XI%, Xpos%
Dim Xopt$(25)
  Terror% = -1
  Open "TF:SEGPAR" KEYED RECL 21 AS 44                   ! Parametro de opciones
  If Terror% <> -1 Then Begin 				 ! Error de apertura
     Exit Sub 
  EndIf
  Read Form "C1 C20";#44 KEY DM.OPCION$; Xdummy$, Xlista$
  If Terror% = -1 Then Begin 				 ! Existe parametros
   For XI% = 1 to 20 					 ! Recorre String parametros
     Xpos% = 3 + XI%
     Xopt$(XI%) = Mid$(Xlista$,XI%,1)
     Call MSG.ERR(Xpos%,Xopt$(XI%))
   Next XI%
  EndIf
      
  For XI% = 1 TO 20
    Xdata$ = Get.Datos
    If (ENDF = F3.SALIR) Then Call Quit                  ! Si presiona tecla F3
    If (ENDF = ESC.KEY)  Then Call Quit                  ! Si presiona tecla ESC
    While (MATCH(Xdata$,"10",1) = 0 )
      RET.ERR% = NXTF(-2)
      Call DM.ERR(RET.ERR%,NXTF$)
      Xdata$ = GET.DATOS
      If (ENDF = F3.SALIR) THEN Call Quit              ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  THEN Call Quit              ! Si presiona tecla ESC
    Wend
    Xopt$(XI%) = Xdata$
  Next XI%
  Xlista$ = ""
  For XI% = 1 TO 20
      Xlista$ = Xlista$ + Xopt$(XI%) 
  Next XI%
  Write Form "C1 C20";#44 ;DM.OPCION$, XLISTA$
  Close 44
  For XI% = 1 to 20 					 ! Recorre String parametros
     Xpos% = 3 + XI%
     Call MSG.ERR(Xpos%," ")
  Next XI%

End Sub 
!
!----- Menu principal
!

On Error GoTo SEGERROR
Call INICIADM 				                       ! Inicializacion Variables Display Manager 
AREA1% = 1: AREA2% = 2				               ! Definicion area de trabajo archivo       
TERM$ = " "					               ! Inicializo Libreria                      
RET.ERR%=INITDM(TERM$)					       !                                          
Call DM.ERR(RET.ERR%,INITDM$)				       !                                          
RET.ERR% = OPNDIS("ADX_UPGM:MENUSEG.PNT")	               ! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Terror% = -1
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              ! Abre archivo de operadores
If Terror% <> -1 Then Begin 

EndIf 
Call CTRL.ACCESO
DM.OPCION$   = "  "
Call.ORDER% = 20                                     ! Llamado Primera Pantalla D.M
RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
Call DM.ERR(RET.ERR%,DISPD$)
Call MSG.ERR(02,"1.00")	
Call Carga.Menu
While (FIN$ = "0") 
 ini.ciclo:
      DM.OPCION$   = "  "
      Call MSG.ERR(24,"Numero de Modelo a Parametizar")
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then Begin 
         Call HELP("MENU PRINCIPAL OPCIONES  ","PARAMSEG.TXT") !
         DM.OPCION$   = "  "
         Call.ORDER% = 20                                  ! Llamado Primera Pantalla D.M
         RET.ERR% = DISPD(Call.ORDER%)                     ! Llamado de la pantalla en DM
         Call DM.ERR(RET.ERR%,DISPD$)
         Call MSG.ERR(02,"1.00")	
         Call Carga.Menu
         GoTo ini.ciclo
      EndIf 
      If (ENDF = F3.SALIR) Then Call Quit                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call Quit                  ! Si presiona tecla ESC
      Call Busqueda.Modelo
Wend

!--- Definicion de las rutinas de error
SEGERROR:
         TERROR% = 0
         If ERR = "EF" Or ERR = "OE" Then BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         Stop 
