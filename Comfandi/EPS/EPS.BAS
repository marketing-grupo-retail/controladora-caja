!************************************************** 
!Empresa       : Asic Consulting Group S.A Retail *
!Programa      : EPS.BAS                          *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Menu principal modulo convenios  *
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
         TMP.CADENA$, \
         CADENA.FINAL$

Integer*2 GLOBAL        \
          DM.NOPROY%,   \				! Numero de proyectos definidos
          DM.NOCAMPOS%, \				! Numero de campos en proyecto
          RET.ERR%,     \       ! Numero de rutina para Display Manager
          Call.ORDER%,  \       ! Numero de orden de llamado de pantalla
          DM.NOREG%,    \       ! Numero de registros
          DM.NOCAMP%,   \       ! Numero de campos
          INI.CAMPO%    \       ! Control despliege de proyectos en pantalla

     
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
          F3.SALIR,  \      	  ! Deficion de la tecla de funcion F3
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

Function LEER.CABECERA
   BAN.PRG$ = "0"
   KEY$=PACK$("000001")                                     ! Llave cabecera control
   LEC$="C3 C4 C30 C30 C13 I2 C22"
   Read ForM LEC$;#AREA1% KEY KEY$;                        \! Lee Reg Cabecera 
        DM.LLAVE$,DM.ALMACEN$,DM.RAZON$,DM.SIGLA$,DM.NIT$, \!
        DM.NOPROY%,DM.BASURA$				    ! Control
END Function
!--- Fin de la funcion de lectura

!--- Despliegue de mensaje del aplicativo
Function MENSAJE
   Call MSG.ERR(2,"Opci¢n En Desarrollo ...")
   WAIT;1800
   Call MSG.ERR(2,"                        ")
END Function
!--- Fin de la funcion de mensajes

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

Function MAESTROS                                            ! Menu mantenimiento
FIN2$="0"
While (FIN2$ = "0") 
      DM.OPCION2$ = "  "
      Call.ORDER% = 2                                        ! Llamado Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                          ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			             ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                            ! Captura dato en pantalla
      DM.OPCION2$ = INP$                                     ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("DEFINICION DE MAESTROS   ","EPSP0001.TXT") !
      If (ENDF = F3.SALIR) Then EXIT Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then EXIT Function                ! Si presiona tecla ESC
      If Val(DM.OPCION2$) = 1 Then Call OSSHELL("\EPS\EPSP0101.286")   ! Captura datos generales      
      If Val(DM.OPCION2$) = 2 Then Call OSSHELL("\EPS\EPSP0102.286")   ! Definicion IPS               
      If Val(DM.OPCION2$) = 3 Then Call OSSHELL("\EPS\EPSP0103.286")   ! Definicion Medicos
      If Val(DM.OPCION2$) = 4 Then Call OSSHELL("\EPS\EPSP0104.286")   ! Definicion Plu/Convenio
      If Val(DM.OPCION2$) = 5 Then Call OSSHELL("\EPS\EPSP0107.286")   ! Definicion Convenios   
      If Val(DM.OPCION2$) = 6 Then Call OSSHELL("\EPS\EPSP0108.286")   ! Definicion Red Prestadora
      If Val(DM.OPCION2$) = 7 Then Call OSSHELL("\EPS\EPSP0106.286")   ! Definicion Estado Afiliados
      If Val(DM.OPCION2$) = 8 Then Call OSSHELL("\EPS\EPSP0109.286")   ! Definicion Diagnosticos
      If Val(DM.OPCION2$) = 9 Then Call OSSHELL("\EPS\EPSP0110.286")   ! Definicion Liga Convenios
      If Val(DM.OPCION2$) =10 Then Call OSSHELL("\EPS\EPSP0111.286")   ! Definicion Entidades Pago Cuota
Wend
END Function
!--- Fin de la definicion de maestros

Function REPORTES                                            ! Menu mantenimiento
FIN2$="0"
While (FIN2$ = "0") 
      DM.OPCION2$ = "  "
      Call.ORDER% = 4                                        ! Llamado Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                          ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			             ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                            ! Captura dato en pantalla
      DM.OPCION2$ = INP$                                     ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("GENERACION DE REPORTES   ","EPSP0004.TXT") !
      If (ENDF = F3.SALIR) Then EXIT Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then EXIT Function                ! Si presiona tecla ESC
      If Val(DM.OPCION2$) = 1 Then Call OSSHELL("\EPS\EPSP0401.286")  ! Reporte Beneficiarios
      If Val(DM.OPCION2$) = 2 Then BEGIN 
         Call MSG.ERR(03,"Generando Reporte, Espere Por Favor ...")
         Call OSSHELL("\EPS\EPSP0402.286")    ! Reporte Convenios
      ENDIf
      If Val(DM.OPCION2$) = 4 Then Call OSSHELL("\EPS\EPSP0404.286")   ! Reporte Detalle Formulas
      If Val(DM.OPCION2$) = 5 Then Call OSSHELL("\EPS\EPSP0405.286")   ! Reporte Plu/Convenios
      If Val(DM.OPCION2$) = 6 Then Call OSSHELL("\EPS\EPSP0406.286")   ! Reporte Consolidado Convenios
Wend
END Function
!--- Fin de la definicion de reportes

Function CARGAS                                              ! Menu Cargas de maestros
FIN2$="0"
While (FIN2$ = "0") 
      DM.OPCION2$ = "  "
      Call.ORDER% = 3                                        ! Llamado Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                          ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			             ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                            ! Captura dato en pantalla
      DM.OPCION2$ = INP$                                     ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("CARGA ARCHIVOS MAESTROS  ","EPSP0002.TXT") !
      If (ENDF = F3.SALIR) Then EXIT Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then EXIT Function                ! Si presiona tecla ESC
      If Val(DM.OPCION2$) = 1 Then Call OSSHELL("\EPS\EPSP0201.286")     ! Carga maestro de Planes   
      If Val(DM.OPCION2$) = 2 Then Call OSSHELL("\EPS\EPSP0202.286")     ! Carga maestro de afiliados
      If Val(DM.OPCION2$) = 3 Then Call OSSHELL("\EPS\EPSP0203.286")     ! Carga maestro de Plu/Planes  
      If Val(DM.OPCION2$) = 4 Then Call OSSHELL("\EPS\EPSP0204.286")     ! Carga maestro de Medicos
      If Val(DM.OPCION2$) = 5 Then Call OSSHELL("\EPS\EPSP0205.286")     ! Carga maestro de Red Prestadora
      If Val(DM.OPCION2$) = 6 Then Call OSSHELL("\EPS\EPSP0206.286")     ! Carga maestro de IPS
      If Val(DM.OPCION2$) = 7 Then Call OSSHELL("\EPS\EPSP0207.286")     ! Carga maestro de Estado Afiliados
      If Val(DM.OPCION2$) = 8 Then Call OSSHELL("\EPS\EPSP0208.286")     ! Carga maestro de Convenios
      If Val(DM.OPCION2$) = 9 Then Call OSSHELL("\EPS\EPSP0209.286")     ! Carga maestro de Diagnosticos
      If Val(DM.OPCION2$) =10 Then Call OSSHELL("\EPS\EPSP0210.286")     ! Carga maestro de Entidades Pago Cuota
 Wend
END Function
!--- Fin de la funcion de carga de maestros

Function SALIDAS                                             ! Menu Salida de planos
FIN2$="0"
While (FIN2$ = "0") 
      DM.OPCION2$ = "  "
      Call.ORDER% = 5                                        ! Llamado Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                          ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      RET.ERR% = NXTF(-20) 			             ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                            ! Captura dato en pantalla
      DM.OPCION2$ = INP$                                     ! Asigna Valor capturado
      If (ENDF = F3.SALIR) Then EXIT Function                ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then EXIT Function                ! Si presiona tecla ESC
      If (ENDF = F1.AYUDA) Then \
         Call HELP("GENERACION DE INTERFACES ","EPSP0003.TXT") !
      If Val(DM.OPCION2$) = 1 Then Call OSSHELL("\EPS\EPSP0301.286")    ! Generacion plano de movimiento
      If Val(DM.OPCION2$) = 2 Then Call OSSHELL("\EPS\EPSP0302.286")    ! Generacion plano de movimiento
 Wend
END Function
!--- Fin de la funcion de carga de maestros

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
      ENDIf
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
         ENDIf
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
            Val(UNPACK$(DM.MODELO$)) <> 5 Then BEGIN      \!
            Call MSG.ERR(05,"Error: Nivel de Usuario No Autorizado ")
            WAIT;2000
            Call QUIT
	 ENDIf  
         Call MSG.ERR(05,"Digite Clave de Usuario  ....         ")
         CLAVE.MEM$ = GET.DATOS				   ! Captura la clave 
         If (ENDF = F3.SALIR) Then Call QUIT               ! Si presiona tecla F3
         If (ENDF = ESC.KEY)  Then Call QUIT               ! Si presiona tecla ESC
         If (ENDF = F1.AYUDA) Then BEGIN                  \! Si presiona tecla F3
            Call HELP("ACCESO APLICATIVO CONTROL","HLPP999.TXT")
            GOTO ACCESO 
         ENDIf
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
            ENDIf
         Wend
         CLAVE.MEM$=PACK$(Right$("00000000"+CLAVE.MEM$,8)) ! Arma clave capturada
         If CLAVE.MEM$ <> DM.PASWD$ Then  BEGIN           \!
            Call MSG.ERR(05,"Error: Clave de Acceso Erronea ...    ") 
            WAIT;2000
            Call QUIT
         ENDIf
      ENDIf
      If BAN.PRG$ = "1" Then BEGIN			  \!
         Call MSG.ERR(05,"Error: Usuario No Autorizado o No Existe ")
         WAIT;2000
         Call QUIT
      ENDIf
END Function
!--- Fin del control de acceso

!
!----- Menu principal
!

ON ERROR GOTO EPSERROR
Call INICIADM 				                   ! Inicializacion Variables Display Manager
AREA1% = 1: AREA2% = 2				           ! Definicion area de trabajo archivo
ARCH1$="CONTROL"					   ! Asignar nombre archivo control
Open ARCH1$ KEYED  RECL 104 AS AREA1%                      ! Abre archivo de control
Open "EAMOPERA" KEYED RECL 72 AS AREA2% NODEL              ! Abre archivo de operadores
TERM$ = " "					           ! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   !
Call DM.ERR(RET.ERR%,INITDM$)				   !
RET.ERR% = OPNDIS("ADX_UPGM:EPSPANTA.PNT")	           ! Apertura de la Forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)
FIN$="0"
Call CTRL.ACCESO					   ! Ctrl acceso aplicativo
CLOSE AREA1%                                               !
CLOSE AREA2%						   !
While (FIN$ = "0") 
      DM.OPCION$   = "  "
      Call.ORDER% = 1                                      ! Llamado Primera Pantalla D.M
      RET.ERR% = DISPD(Call.ORDER%)                        ! Llamado de la pantalla en DM
      Call DM.ERR(RET.ERR%,DISPD$)
      Call MSG.ERR(06,"2.01")				   ! Version de la aplicacion
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      INP$ = UPDF                                          ! Captura dato en pantalla
      DM.OPCION$ = INP$                                    ! Asigna Valor capturado
      If (ENDF = F1.AYUDA) Then \
         Call HELP("MENU PRINCIPAL APLICATIVO","EPSP0000.TXT") !
      If (ENDF = F3.SALIR) Then Call QUIT                  ! Si presiona tecla F3
      If (ENDF = ESC.KEY)  Then Call QUIT                  ! Si presiona tecla ESC
      If Val(DM.OPCION$) = 1 Then Call MAESTROS            ! Captura datos generales      
      If Val(DM.OPCION$) = 2 Then Call CARGAS              ! Carga maestros 
      If Val(DM.OPCION$) = 3 Then Call SALIDAS             ! Definicion de proyectos
      If Val(DM.OPCION$) = 4 Then Call REPORTES            ! Definicion de reportes
Wend

!--- Definicion de las rutinas de error
EPSERROR:
         CONSOLE                                           ! CLOSE PRINTER If Open IR75403
         If ERRF% = AREA1% AND                            \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then BEGIN          ! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 3,,,1000 RECL 104 \! si no existe lo crea.
                        AS AREA1% 			   ! 
            RESUME                                         ! retorna ejecucion
         ENDIf                                             ! del programa
         If ERRF% = AREA2% AND                                \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then BEGIN              ! el archivo de control
             MSG$ = "Error: EAMOPERA No disponible o no Existe"
             LOCATE 24,1: PRINT MSG$
             STOP
         ENDIf                                                 ! del programa
         If ERRF% = AREA1% AND ERR = "EF" Then BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            RESUME
         ENDIf
         If ERRF% = AREA2% AND ERR = "EF" Then BEGIN      \! Si encuentra fin de 
            BAN.PRG$ = "1"				   ! ejecucion normal del
            RESUME
         ENDIf
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" Then BEGIN  \! Valida la lectura
            BAN.PRG$ = "1"				       ! del archivo de 
            RESUME					       ! help del aplicativo
         ENDIf						       !
         Call TRADUCE.ERROR
         MSG$ = "Error: "+ERR+" Sesi¢n: "+STR$(ERRF%)+"-"+ERRFX$
         LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla

END
