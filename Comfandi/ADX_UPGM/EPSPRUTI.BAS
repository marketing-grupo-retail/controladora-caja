!************************************************** 
!Empresa       : Global Datatel - Unidad  Retail  *
!Programa      : EPSRUTI.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Diciembre 9 de 1.999             *
!Observaciones : Definici¢n de rutinas comunes    *
!                del aplicativo.                  *
!                desarrollados por Global Datatel *
!**************************************************
!
! INICIADM     : Inicializacion Variables del Display
!                Manager
! DM.ERR       : Funcion del D.M
! MSG.ERR      : Funcion del D.M
! REST.DISP    : Funcion del D.M
! GET.DATOS    : Captura datos Por D.M
! MNT.REG      : Mantenimiento de archivo
! MNT.REG2     : Mantenimiento archivo de convenios
! MNT.REG3     : Mantenimiento archivo de convenios
! VISUAL.REG   : Visualizacion de archivo
! TRADUCE.ERROR: Traducci¢n error de aplicativo
! QUIT         : Salida de Programa
! ALLTRIM      : Ajustar espacios en blanco
!
!-----------------------------------------------------

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
FUNCTION GET.DATOS
STRING GET.DATOS
   RET.ERR% = NXTF(2)			!Proximo campo
   CALL DM.ERR(RET.ERR%,NXTF$)
   ATTR$ = SETF(PRM.ON$)
   INP$ = UPDF
   GET.DATOS = INP$
END FUNCTION

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

FUNCTION VISUAL.REG
INTEGER*4 X, REC.NO, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC,R.S
STRING    H$, PZERO$, C01$, C02$
CALL.ORDER% = 201   				             ! Definicion Pantalla DM
RET.ERR% = DISPD(CALL.ORDER%)                                ! Llamado de la pantalla en DM
CALL DM.ERR(RET.ERR%,DISPD$)		                     !
REC.NO = 1					             ! Initialize Counter
READ FORM "T43 I4 I2 T55 I2 C456"; #AREA1%,REC.NO;          \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! EAMFBACT
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
BAN.PRG$ = "0"					             ! Program Control
I%=4: J%=5
FOR REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  READ FORM "T5 C508"; #AREA1%, REC.NO;  H$                  ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
  WHILE KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    C01$=UNPACK$(MID$(H$,X+0,1))                             ! Unpack data record 
    C02$=(MID$(H$,X+1,40))                                   ! Next sentence data record 
    CALL MSG.ERR(I%,C01$)
    CALL MSG.ERR(J%,C02$)
    I% = I% + 2: J% = J% + 2
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
   IF (J% > 32) THEN BEGIN
      I% = 4: J% = 5
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      DM.DATO$ = UPDF                                      ! Captura dato en pantalla
      CALL.ORDER% = 201   				   ! Definicion Pantalla DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)		           !
      IF (ENDF = ESC.KEY)  THEN K% = DM.NOREG% + 1
      IF (ENDF = F3.SALIR) THEN K% = DM.NOREG% + 1
   ENDIF
  WEND
NEXT REC.NO
RET.ERR% = NXTF(-20) 			                     ! Primer campo
CALL DM.ERR(RET.ERR%,NXTF$)
ATTR$ = SETF(PRM.ON$)                                
DM.DATO$ = UPDF                                              ! Captura dato en pantalla
END FUNCTION
!--- Fin de la rutina de visualizacion de registros

FUNCTION VISUAL.REG2
INTEGER*4 X, REC.NO, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC,R.S
STRING    H$, PZERO$, C01$, C02$
CALL.ORDER% = 208   				             ! Definicion Pantalla DM
RET.ERR% = DISPD(CALL.ORDER%)                                ! Llamado de la pantalla en DM
CALL DM.ERR(RET.ERR%,DISPD$)		                     !
REC.NO = 1					             ! Initialize Counter
READ FORM "T43 I4 I2 T55 I2 C456"; #AREA1%,REC.NO;          \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! EAMFBACT
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
BAN.PRG$ = "0"					             ! Program Control
I%=4: J%=5
FOR REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  READ FORM "T5 C508"; #AREA1%, REC.NO;  H$                  ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
  WHILE KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    C01$=UNPACK$(MID$(H$,X+0,6))                             ! Unpack data record 
    C02$=(MID$(H$,X+6,40))                                   ! Next sentence data record 
    CALL MSG.ERR(I%,C01$)
    CALL MSG.ERR(J%,C02$)
    I% = I% + 2: J% = J% + 2
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
   IF (J% > 32) THEN BEGIN
      I% = 4: J% = 5
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      DM.DATO$ = UPDF                                      ! Captura dato en pantalla
      CALL.ORDER% = 208   				   ! Definicion Pantalla DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)		           !
      IF (ENDF = ESC.KEY)  THEN K% = DM.NOREG% + 1
      IF (ENDF = F3.SALIR) THEN K% = DM.NOREG% + 1
   ENDIF
  WEND
NEXT REC.NO
RET.ERR% = NXTF(-20) 			                     ! Primer campo
CALL DM.ERR(RET.ERR%,NXTF$)
ATTR$ = SETF(PRM.ON$)                                
DM.DATO$ = UPDF                                              ! Captura dato en pantalla
END FUNCTION
!--- Fin de la rutina de visualizacion de registros

FUNCTION VISUAL.REG3
INTEGER*4 X, REC.NO, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC,R.S
STRING    H$, PZERO$, C01$, C02$
CALL.ORDER% = 201   				             ! Definicion Pantalla DM
RET.ERR% = DISPD(CALL.ORDER%)                                ! Llamado de la pantalla en DM
CALL DM.ERR(RET.ERR%,DISPD$)		                     !
REC.NO = 1					             ! Initialize Counter
READ FORM "T43 I4 I2 T55 I2 C456"; #AREA1%,REC.NO;          \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! EAMFBACT
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
BAN.PRG$ = "0"					             ! Program Control
I%=4: J%=5
FOR REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  READ FORM "T5 C508"; #AREA1%, REC.NO;  H$                  ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
  WHILE KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    C01$=UNPACK$(MID$(H$,X+0,2))                             ! Unpack data record 
    C02$=(MID$(H$,X+2,40))                                   ! Next sentence data record 
    CALL MSG.ERR(I%,C01$)
    CALL MSG.ERR(J%,C02$)
    I% = I% + 2: J% = J% + 2
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
   IF (J% > 32) THEN BEGIN
      I% = 4: J% = 5
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      DM.DATO$ = UPDF                                      ! Captura dato en pantalla
      CALL.ORDER% = 208   				   ! Definicion Pantalla DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)		           !
      IF (ENDF = ESC.KEY)  THEN K% = DM.NOREG% + 1
      IF (ENDF = F3.SALIR) THEN K% = DM.NOREG% + 1
   ENDIF
  WEND
NEXT REC.NO
RET.ERR% = NXTF(-20) 			                     ! Primer campo
CALL DM.ERR(RET.ERR%,NXTF$)
ATTR$ = SETF(PRM.ON$)                                
DM.DATO$ = UPDF                                              ! Captura dato en pantalla
END FUNCTION
!--- Fin de la rutina de visualizacion de registros

FUNCTION VISUAL.REG4
INTEGER*4 X, REC.NO, BLK.NUM, REC.LEN, KEY.LEN, MAX.R.SEC,R.S
STRING    H$, PZERO$, C01$, C02$
CALL.ORDER% = 208   				             ! Definicion Pantalla DM
RET.ERR% = DISPD(CALL.ORDER%)                                ! Llamado de la pantalla en DM
CALL DM.ERR(RET.ERR%,DISPD$)		                     !
REC.NO = 1					             ! Initialize Counter
READ FORM "T43 I4 I2 T55 I2 C456"; #AREA1%,REC.NO;          \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! EAMFBACT
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
BAN.PRG$ = "0"					             ! Program Control
I%=4: J%=5
FOR REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  READ FORM "T5 C508"; #AREA1%, REC.NO;  H$                  ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
  WHILE KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    C01$=UNPACK$(MID$(H$,X+0,5))                             ! Unpack data record 
    C02$=(MID$(H$,X+5,40))                                   ! Next sentence data record 
    CALL MSG.ERR(I%,C01$)
    CALL MSG.ERR(J%,C02$)
    I% = I% + 2: J% = J% + 2
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
   IF (J% > 32) THEN BEGIN
      I% = 4: J% = 5
      RET.ERR% = NXTF(-20) 			           ! Primer campo
      CALL DM.ERR(RET.ERR%,NXTF$)
      ATTR$ = SETF(PRM.ON$)                                
      DM.DATO$ = UPDF                                      ! Captura dato en pantalla
      CALL.ORDER% = 208   				   ! Definicion Pantalla DM
      RET.ERR% = DISPD(CALL.ORDER%)                        ! Llamado de la pantalla en DM
      CALL DM.ERR(RET.ERR%,DISPD$)		           !
      IF (ENDF = ESC.KEY)  THEN K% = DM.NOREG% + 1
      IF (ENDF = F3.SALIR) THEN K% = DM.NOREG% + 1
   ENDIF
  WEND
NEXT REC.NO
RET.ERR% = NXTF(-20) 			                     ! Primer campo
CALL DM.ERR(RET.ERR%,NXTF$)
ATTR$ = SETF(PRM.ON$)                                
DM.DATO$ = UPDF                                              ! Captura dato en pantalla
END FUNCTION
!--- Fin de la rutina de visualizacion de registros

FUNCTION QUIT						     ! Funcion salida programa
    CLOSE AREA1%					     !
    CALL SETF("0000000")				     !
    CALL CLRSCR						     !
    RET.ERR%= CLSDIS					     !
    CALL DM.ERR(RET.ERR%,CLSDIS$)			     !
    STOP
END FUNCTION
!--- Fin de la funcion de salida

FUNCTION MNT.REG
  BAN.PRG$   = "0"
  DM.NOMPRO$ = ""
  DM.CODALT$ = ""
  LEC$="C1 C40 C12"
  DM.NIT$=PACK$(RIGHT$("00"+DM.NIT$,2))                      ! Creacion llave proyecto
  READ FORM LEC$;#AREA1% KEY DM.NIT$;                       \! Lee Reg Cabecera 
       DM.LLAVE$,DM.RAZON$, DM.CODALT$				     ! 	
  IF BAN.PRG$ = "0" THEN DM.NOMPRO$ = DM.RAZON$		     !
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(15,DM.CODALT$)                                 ! Presenta descripcion
  CALL MSG.ERR(3,DM.NOMPRO$)                                 ! Presenta descripcion
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.NOMPRO$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.NOMPRO$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.NOMPRO$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  MSG$ = "Digite c¢digo alterno"  
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.CODALT$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.CODALT$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.CODALT$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  WRITE FORM LEC$;#AREA1%;DM.NIT$,DM.NOMPRO$,DM.CODALT$      ! Graba registro IPS
END FUNCTION
!--- Fin del mantenimiento del registro

FUNCTION MNT.REG2
  BAN.PRG$  = "0"
  DM.NOMPRO$ = "   "
  DM.CODALT$ = "   "
  LEC$="C6 C40 C12"
  DM.NIT$=PACK$(RIGHT$("000000000000"+DM.NIT$,12))           ! Creacion llave proyecto
  READ FORM LEC$;#AREA1% KEY DM.NIT$;                       \! Lee Reg Cabecera 
       DM.LLAVE$,DM.RAZON$, DM.CODALT$			     ! 	
  IF BAN.PRG$ = "0" THEN DM.NOMPRO$ = DM.RAZON$		     !
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(16,DM.CODALT$)                                 ! Presenta descripcion
  CALL MSG.ERR(3,DM.NOMPRO$)                                 ! Presenta descripcion
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.NOMPRO$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.NOMPRO$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.NOMPRO$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  MSG$ = "Digite c¢digo alterno"  
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.CODALT$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.CODALT$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.CODALT$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  WRITE FORM LEC$;#AREA1%;DM.NIT$,DM.NOMPRO$,DM.CODALT$      ! Graba registro IPS
END FUNCTION
!--- Fin del mantenimiento del registro

FUNCTION MNT.REG3
  BAN.PRG$  = "0"
  DM.NOMPRO$ = ""
  DM.CODALT$ = ""
  LEC$="C2 C40 C12"
  DM.NIT$=PACK$(RIGHT$("0000"+DM.NIT$,4))                    ! Creacion llave proyecto
  READ FORM LEC$;#AREA1% KEY DM.NIT$;                       \! Lee Reg Cabecera 
       DM.LLAVE$,DM.RAZON$,DM.CODALT$			     ! 	
  IF BAN.PRG$ = "0" THEN DM.NOMPRO$ = DM.RAZON$		     !
  IF BAN.PRG$ = "0" THEN CALL MSG.ERR(15,DM.CODALT$)                                 ! Presenta descripcion
  CALL MSG.ERR(3,DM.NOMPRO$)                                 ! Presenta descripcion
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.NOMPRO$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.NOMPRO$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.NOMPRO$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  MSG$ = "Digite c¢digo alterno"  
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.CODALT$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.CODALT$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.CODALT$ = GET.DATOS				     !
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  WRITE FORM LEC$;#AREA1%;DM.NIT$,DM.NOMPRO$,DM.CODALT$      ! Graba registro IPS
END FUNCTION
!--- Fin del mantenimiento del registro

FUNCTION MNT.REG5
  BAN.PRG$  = "0"
  DM.NOMPRO$ = "   "
  DM.CODALT$ = " "
  LEC$="C5 C40 C12"
  DM.NIT$=PACK$(RIGHT$("0000000000"+DM.NIT$,10))           ! Creacion llave proyecto
  READ FORM LEC$;#AREA1% KEY DM.NIT$;                       \! Lee Reg Cabecera 
       DM.LLAVE$,DM.RAZON$,DM.CODALT$			     ! 	
  IF BAN.PRG$ = "0" THEN DM.NOMPRO$ = DM.RAZON$		     !
  CALL MSG.ERR(3,DM.NOMPRO$)                                 ! Presenta descripcion
  CALL MSG.ERR(15,DM.CODALT$)                                ! Presenta descripcion
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.NOMPRO$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.NOMPRO$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.NOMPRO$ = GET.DATOS
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  MSG$ = "Digite c¢digo alterno"  
  CALL MSG.ERR(4,MSG$)                                       ! Mensaje a pantalla
  DM.CODALT$ = GET.DATOS				     ! Captura dato en pantalla
  IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                    ! Si presiona tecla F3
  IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION                    ! Si presiona tecla ESC
  WHILE DM.CODALT$ = " "				     ! Valida captura de dato
     RET.ERR% = NXTF(-2)
     CALL DM.ERR(RET.ERR%,NXTF$)
     DM.CODALT$ = GET.DATOS				     !
     IF (ENDF = F3.SALIR) THEN EXIT FUNCTION                 ! Si presiona tecla F3
     IF (ENDF = ESC.KEY)  THEN EXIT FUNCTION		     ! Si presiona tecla ESC
  WEND
  WRITE FORM LEC$;#AREA1%;DM.NIT$,DM.NOMPRO$,DM.CODALT$      ! Graba registro IPS
END FUNCTION
!--- Fin del mantenimiento del registro

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

FUNCTION INICIO
   CALL.ORDER% = 200                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
   CALL MSG.ERR(4,"C¢digo a Definir        ")                 ! 
END FUNCTION
!--- Fin de la funcion de inicio

FUNCTION INICIO2
   CALL.ORDER% = 209                                          ! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(CALL.ORDER%)                              ! Llamado de la pantalla en DM
   CALL DM.ERR(RET.ERR%,DISPD$)
   CALL MSG.ERR(1,MSG$)				              ! Mensaje cabecera pantalla
   CALL MSG.ERR(4,"C¢digo a Definir        ")                 ! 
END FUNCTION
!--- Fin de la funcion de inicio

!--- Funcion ALLTRIM
FUNCTION ALLTRIM(TMP.CADENA$)
 STRING ALLTRIM, TMP.CADENA$, CADENA.FINAL$
 INTEGER TMP.LON%, LONGITUD%, F%
    LONGITUD% = LEN(TMP.CADENA$) + 1
    TMP.LON% = 0
    CADENA.FINAL$ = ""
    FOR F% = 1 TO LONGITUD%
        IF MID$(TMP.CADENA$,LONGITUD% - F%,1) = " " THEN \
           TMP.LON% = TMP.LON% + 1
        IF MID$(TMP.CADENA$,LONGITUD% - F%,1) <> " " THEN \
           F% = LONGITUD% + 1 
    NEXT F%   
    TMP.LON% = TMP.LON% + 1
    CADENA.FINAL$=LEFT$(TMP.CADENA$,(LONGITUD%-TMP.LON%))
    ALLTRIM = CADENA.FINAL$
END FUNCTION
!--- Fin de la funcion ALLTRIM

!--- Funcion VAL.FECHA
FUNCTION VAL.FECHA(DATO$)
STRING DATO$
BAN.PRG$ = "0"
   IF (VAL(MID$(DATO$,3,2)) > 12 OR VAL(MID$(DATO$,3,2)) < 1) THEN \
      BAN.PRG$ = "1"
   IF (VAL(MID$(DATO$,5,2)) > 31 OR VAL(MID$(DATO$,5,2)) < 1) THEN \
      BAN.PRG$ = "1"
END FUNCTION
!--- Fin de la funcion VAL.FECHA

!************* 	FIN DEL PROGRAMA EPSPRUTI.BAS **************!
