!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MENUSEG.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Menu principal Promociones       *
!**************************************************

String   GLOBAL       \
         DM.OPCION$,  \         ! Variable Captura de opciones
         DM.OPCION2$, \         ! Variable Captura de opciones
         DM.NIT$,     \  				! Nit de la empresa
         DM.RAZON$,   \					! Razon social empresa
         DM.SIGLA$,   \ 				! Nombre Comercial
         DM.ALMACEN$, \	        ! Codigo del punto de venta
         DM.BASURA$,  \					! Resto del registro
         DM.LLAVE$,   \         ! Llave registro de control
         DM.NOMPRO$,  \         ! Nombre del proyecto
         DM.ESTPRO$,  \         ! Estado del proyecto
         DM.DATO$,    \         ! Captura de dato en pantalla
         TOT.CAM$,    \         ! Captura de dato en pantalla
         DM.PARAM$,   \         ! Captura datos de parametrizacion
         DM.NOFIELDS$, \				! Numero de campos
         DM.TMP.KEY$(1), \      ! Arreglo llaves del aplicativo
         Gr.Options(2),  \      ! Opciones parametrizadas
         Gr.OptUser(1),  \      ! Opciones de reporte validos
         TMP.CADENA$, \
         CADENA.FINAL$

Integer*2 GLOBAL        \
          DM.NOPROY%,   \				! Numero de proyectos definidos
          DM.NOCAMPOS%, \				! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          Call.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          GR.NROPT%,    \       ! Numero de opciones
          INI.CAMPO%            ! Control despliege de proyectos en pantalla

     
String GLOBAL ARCH1$,   \				! Variable nombre arch. control
              BAN.PRG$, \				! Bandera del programa
              LEC$,    	\				! Definicion Formato registro
              FIN$,     \				! Variable de control programa
              FIN2$,    \				! Variable de control programa
              CICLO$,   \				! Variable de control programa
              TMP.MSG$, \       ! Variable temporal de trabajo
              TMP.KEY$, \				! Almacena temporalmente la llave del registro
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
          F3.SALIR,  \      		! Deficion de la tecla de funcion F3
          F4.KEY  ,  \         	! Deficion de la tecla de funcion F4 
          F5.KEY  ,  \         	! Deficion de la tecla de funcion F5 
          F6.KEY  ,  \         	! Deficion de la tecla de funcion F6 
          F7.KEY  ,  \         	! Deficion de la tecla de funcion F7 
          F8.KEY  ,  \         	! Deficion de la tecla de funcion F8 
          F9.KEY  ,  \         	! Deficion de la tecla de funcion F9 
          FA.KEY  ,  \         	! Deficion de la tecla de funcion F10
          HELP%   ,  \        	! Llamado Segunda Pantalla Display Manager
          I%,        \					! Control de ciclos
          J%,        \					! Control de ciclos
          K%,        \					! Control de ciclos
          L%,        \					! Control de ciclos
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

Integer*4 GLOBAL HX%, S%, SX%, SUM%, ENCRYPT.RC

String GLOBAL DM.USER$,DM.PASWD$,DM.MODELO$,DM.RESTO$, DM.OPER$, \!
              CLAVE.MEM$, NEW.PWD$

%Include ADX_UPGM:BASROUT.J86	      ! Rutinas generales
%Include ADX_UPGM:DMEXTR.J86          ! Inclucion Libreria Display Manager

Sub ADXCRYPT(RETCODE, PWIN$, PWOUT$) EXTERNAL
   STRING     PWIN$,PWOUT$
   INTEGER*2  RETCODE
End Sub

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
  ENTER.KEY   = 0      					     	! Definicion de la tecla Enter
  TAB.KEY     = 9      					     	! Definicion de la tecla TAB
  ESC.KEY     = 27     					     	! Definicion de la tecla ESCAPE
  F1.AYUDA    = -1     				       	! Definicion de la tecla de funcion F1
  F2.KEY      = -2					     			! Definicion de la tecla de funcion F2
  F3.SALIR    = -3     					     	! Definicion de la tecla de funcion F3
  F4.KEY      = -4					     			! Definicion de la tecla de funcion F4  
  F5.KEY      = -5					     			! Definicion de la tecla de funcion F5  
  F6.KEY      = -6					     			! Definicion de la tecla de funcion F6  
  F7.KEY      = -7					     			! Definicion de la tecla de funcion F7  
  F8.KEY      = -8					     			! Definicion de la tecla de funcion F8  
  F9.KEY      = -9					     			! Definicion de la tecla de funcion F9  
  FA.KEY      = -10					     			! Definicion de la tecla de funcion F10 
  HELP%       = 2      					     	! Llamado Segunda Pantalla Display Manager
  PRM.ON$     = "031"  					     	! Parametros de pantalla
  PRM.OFF$    = "330"  					     	! Parametros de pantalla
  ON$         = "0"    					     	! Campo visible
  OFF$        = "1"    					     	! Campo invisible
  TABS$       = ""                                           ! Tabs para salida
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
Open "TF:OPTPRM" AS 22
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
    Xpos% = 0
    If Val(Gr.Options(XI%,0)) = 1  Then XPOS% = 4
    If Val(Gr.Options(XI%,0)) = 2  Then XPOS% = 6
    If Val(Gr.Options(XI%,0)) = 3  Then XPOS% = 8
    If Val(Gr.Options(XI%,0)) = 4  Then XPOS% = 10
    If Val(Gr.Options(XI%,0)) = 5  Then XPOS% = 12
    If Val(Gr.Options(XI%,0)) = 6  Then XPOS% = 14
    If Val(Gr.Options(XI%,0)) = 7  Then XPOS% = 16
    If Val(Gr.Options(XI%,0)) = 8  Then XPOS% = 18
    If Val(Gr.Options(XI%,0)) = 9  Then XPOS% = 20
    If Val(Gr.Options(XI%,0)) = 10 Then XPOS% = 22

    If Val(Gr.Options(XI%,0)) = 11  Then XPOS% = 5
    If Val(Gr.Options(XI%,0)) = 12  Then XPOS% = 7
    If Val(Gr.Options(XI%,0)) = 13  Then XPOS% = 9
    If Val(Gr.Options(XI%,0)) = 14  Then XPOS% = 11
    If Val(Gr.Options(XI%,0)) = 15  Then XPOS% = 13
    If Val(Gr.Options(XI%,0)) = 16  Then XPOS% = 15
    If Val(Gr.Options(XI%,0)) = 17  Then XPOS% = 17
    If Val(Gr.Options(XI%,0)) = 18  Then XPOS% = 19
    If Val(Gr.Options(XI%,0)) = 19  Then XPOS% = 21
    If Val(Gr.Options(XI%,0)) = 20  Then XPOS% = 23
    If Xpos% <> 0 Then Begin 
       Call MSG.ERR(XPOS%,Left$(Gr.Options(XI%,1),20))
    EndIf 
Next XI%

End Sub 

Function CTRL.ACCESO
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

Sub Carga.Opciones
String Xkey$, Xlista$
  Terror% = -1

  CADENA.FINAL$ = String$(21,"1")
  Exit Sub 

  Open "TF:SEGPAR" KEYED RECL 21 AS 33 NODEL              ! Abre archivo de opciones por modelo de usuario
  If Terror% <> -1 Then Begin 
     Call MSG.ERR(25,"Error de Apertura TF:SEGPAR")
     CADENA.FINAL$ = String$(20,"0")
     wait ; 1200
     Exit Sub 
  EndIf 
  Terror% = -1
  DM.MODELO$ = Str$(Val(UNPACK$(DM.MODELO$)))
  Read Form "C1 C20";#33 KEY DM.MODELO$;                     \! Lee Reg usuario
       Xkey$, Xlista$ 
  If Terror% = -1 Then Begin 
     CADENA.FINAL$ = Xlista$
  EndIf Else Begin 
     Call MSG.ERR(25,"Modelo no Existe")
     CADENA.FINAL$ = String$(20,"0")
     wait ; 1200
  EndIf 
  Close 33
End Sub   

!
!----- Menu principal
!

On Error GoTo SEGERROR
Call INICIADM 				                                     ! Inicializacion Variables Display Manager 
AREA1% = 1: AREA2% = 2				                             ! Definicion area de trabajo archivo       
TERM$ = " "					                                       ! Inicializo Libreria                      
RET.ERR%=INITDM(TERM$)					                           !                                          
Call DM.ERR(RET.ERR%,INITDM$)				                       !                                          
RET.ERR% = OPNDIS("ADX_UPGM:MENUSEG.PNT")	               ! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Terror% = -1
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              ! Abre archivo de operadores
If Terror% <> -1 Then Begin 

EndIf 
!Call CTRL.ACCESO
DM.MODELO$ = Pack$("07")

DM.OPCION$   = "  "
Call.ORDER% = 10                                     ! Llamado Primera Pantalla D.M
RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
Call DM.ERR(RET.ERR%,DISPD$)
Call MSG.ERR(02,"1.00")	
Call carga.menu
Call Carga.Opciones
While (FIN$ = "0") 
ini.ciclo:
      Call MSG.ERR(01,"MENU OPCIONES PROMOCIONES")
      DM.OPCION$   = "  "
      RET.ERR% = NXTF(-20) 			                           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then Begin 
         Call HELP("MENU PRINCIPAL PROMOCION ","MENUPRM.TXT") !
         Call.ORDER% = 10                                     ! Llamado Primera Pantalla D.M
         RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
         Call DM.ERR(RET.ERR%,DISPD$)
         Call MSG.ERR(02,"1.00")		                   ! Version de la aplicacion
         Call carga.menu
         
         GoTo ini.ciclo
      EndIf 
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      
      If Val(DM.OPCION$) < 0 OR Val(DM.OPCION$) > 20 Then Begin 
         Call MSG.ERR(25,"Opcion No Valida")
         Wait ; 2500
      EndIf Else Begin
        Z$ = Mid$(CADENA.FINAL$,Val(DM.OPCION$),1)			! Valida si reporte permitido
        If Z$ = "1" Then Begin 						! Opcion Valida para el modelo
           I% = Val(Gr.Options(Val(DM.OPCION$),0))			! Toma el programa a ser ejecutado
           If I% > 0 Then Begin                                         ! Si opcion cargada
              DM.DATO$ = Gr.Options(Val(DM.OPCION$),2)                  !
              Call OSSHELL(DM.DATO$)     				! Ejecuta comando programado
           EndIf 							!
        EndIf Else Begin 						! Si opcion no valida para el modelo
         Call MSG.ERR(25,"Opcion No Valida para el modelo de usuario")
         Wait ; 1500
        EndIf 
      EndIf 
      DM.OPCION$   = "  "
      Call.ORDER% = 10                                     ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      Call MSG.ERR(02,"1.00")		                   ! Version de la aplicacion
      Call carga.menu
Wend

!--- Definicion de las rutinas de error
SEGERROR:
         TERROR% = 0
         If ERR = "EF" OR ERR="OE" Then BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         EndIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
         Stop 
         