!************************************************** 
!Empresa       : CellStar de Colombia             *
!Programa      : PINCOMUN.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Julio de 2006                    *
!Observaciones : Rutinas Genericas Aplicaciones   *
!**************************************************

%Environ T

Integer   Global Sl.End
Integer*1 Global PIN.LLAVE%, Pin.Pay.Impr%
Integer*2 Global TS11.OVRFLAG, PIN.USER.SAVE(1) 				! Save area for Subroutine calls
String    Global PIN.USER.SAVE$(1)			                ! Save area for Subroutine calls
Integer*4 Global Pin.Save.Data(1), Ue.Pin.Apun%

%INCLUDE PINITEMR.011          				! Variables Eamitemr
%INCLUDE ADX_UPGM:EAMTSWKG.J86				! Definicion de variables
%INCLUDE ADX_UPGM:EAMTRANS.J86				! Definicion de variables
%INCLUDE ADX_UPGM:EAMP4VAJ.J86 			        ! 4610 Printer
%INCLUDE ADX_UPGM:EAMTOPTS.J86				! Definicion de Terminal Options

Sub TSDSEC01 External					        ! Rutina display
End Sub

Sub TSPREC01 External					        ! Rutina de Impresion
End Sub

Sub TSTPEC01 External                 ! Grabacion del String 
End Sub 

Sub TSCSECRK External       					! READ KEYBOARD
End Sub

Sub TSCSEC08 External
End Sub 

Sub TSHIECET External 
End Sub 

! write entry to summary log array
Function Write.SL.STR(INDEX, SOURCE$) EXTERNAL
Integer*2 WRITE.SL.STR
Integer*2 INDEX
String SOURCE$
End Function


Function GETN1(P1$,P2) EXTERNAL                       
     Integer*1 GETN1
     String P1$
     Integer*2 P2
End Function

Function GETN2(P1$,P2) External 
     Integer*2 GETN2
     String P1$
     Integer*2 P2
End Function 

Sub PIN.TRADUCE.ERROR(ERRVALUE%,ERRVALUE$) Public
  INTEGER*4 ERRVALUE%
  STRING ERRVALUE$
  INTEGER*4 HX%,SX%,THE.SUM%,S%
  STRING ERRFX$,Z$
  ERRVALUE$ = ""
  HX%=ERRVALUE%
  ERRFX$=""
  FOR S%=28 TO 0 STEP -4
  SX%=SHIFT(HX%,S%)
  THE.SUM%=SX%AND 000FH
  IF THE.SUM%>9 THEN THE.SUM%=THE.SUM%+55 \
  ELSE THE.SUM%=THE.SUM%+48
  Z$=CHR$(THE.SUM%)
  ERRFX$=ERRFX$+Z$
  NEXT S%
  ERRVALUE$ =ERRFX$
End Sub


Sub PIN.SALVAR.DISPLAY
  Dim Pin.Save.Data(7)
  Pin.Save.Data(1) = TS.LINETYPE
  Pin.Save.Data(2) = TS.LINEDATA
  Pin.Save.Data(3) = TS.LINEDATA2
  Pin.Save.Data(4) = TS.LINEDATA3
  Pin.Save.Data(5) = TS.XXMOD
  Pin.Save.Data(6) = TS.YYMOD
  Pin.Save.Data(7) = TS.DS.NOSAVE
End Sub
   
! restore display routine

Sub PIN.RESTAURA.DISPLAY 
  TS.LINETYPE  = Pin.Save.Data(1)
  TS.LINEDATA  = Pin.Save.Data(2)
  TS.LINEDATA2 = Pin.Save.Data(3)
  TS.LINEDATA3 = Pin.Save.Data(4) 
  TS.XXMOD = Pin.Save.Data(5)
  TS.YYMOD = Pin.Save.Data(6)
  TS.DS.NOSAVE = Pin.Save.Data(7)
End Sub

Sub PIN.SALVAR.VARIABLES Public
      Dim PIN.USER.SAVE(12)
      PIN.USER.SAVE(1) = TS.LINETYPE
      PIN.USER.SAVE(2) = TS.LINEDATA
      PIN.USER.SAVE(3) = TS.LINEDATA2
      PIN.USER.SAVE(4) = TS.LINEDATA3
      PIN.USER.SAVE(5) = TS.PRT.PARM
      PIN.USER.SAVE(6) = TS.PRT.OPT
      PIN.USER.SAVE(7) = TS.PRT.SJDI
      PIN.USER.SAVE(8) = TS.SAVPRT.OPT
      PIN.USER.SAVE(9) = TS.XXMOD
      PIN.USER.SAVE(10) = TS.YYMOD
      PIN.USER.SAVE(11) = TS.ZMOD
      PIN.USER.SAVE(12) = TS.PRINTPRM
      DIM PIN.USER.SAVE$(5)
      PIN.USER.SAVE$(1) = TS.PRTBUF$
      PIN.USER.SAVE$(2) = TS.PRDATA$
      PIN.USER.SAVE$(3) = TS.FORMCR$
      PIN.USER.SAVE$(4) = TS.SJDATA$
      PIN.USER.SAVE$(5) = TS.SAVPRT$
End Sub
 
Sub PIN.RESTAURA.VARIABLES Public 
      TS.LINETYPE   = PIN.USER.SAVE(1)
      TS.LINEDATA   = PIN.USER.SAVE(2)
      TS.LINEDATA2  = PIN.USER.SAVE(3)
      TS.LINEDATA3  = PIN.USER.SAVE(4)
      TS.PRT.PARM   = PIN.USER.SAVE(5)
      TS.PRT.OPT    = PIN.USER.SAVE(6)
      TS.PRT.SJDI   = PIN.USER.SAVE(7)
      TS.SAVPRT.OPT = PIN.USER.SAVE(8)
      TS.XXMOD      = PIN.USER.SAVE(9)
      TS.YYMOD      = PIN.USER.SAVE(10)
      TS.ZMOD       = PIN.USER.SAVE(11)
      TS.PRINTPRM   = PIN.USER.SAVE(12)
      TS.PRTBUF$    = PIN.USER.SAVE$(1)
      TS.PRDATA$    = PIN.USER.SAVE$(2)
      TS.FORMCR$    = PIN.USER.SAVE$(3)
      TS.SJDATA$    = PIN.USER.SAVE$(4)
      TS.SAVPRT$    = PIN.USER.SAVE$(5)
End Sub

Sub MSG.VISOR(X.MSG$) Public 
  String X.MSG$
  Call PIN.SALVAR.DISPLAY
  TS.TEMP1$ = LEFT$(X.MSG$,20)
  TS.TEMP2$ = MID$(X.MSG$,21,20) 
  TS.LINETYPE = 12
  Call TSCSEC08
  Call PIN.RESTAURA.DISPLAY
End Sub

Sub MSG.DISPLAY(X.MSG$) Public 
  String X.MSG$
  Call PIN.SALVAR.DISPLAY
  TS.TEMP1$ = LEFT$(X.MSG$,20)
  TS.TEMP2$ = MID$(X.MSG$,21,20) 
  TS.LINETYPE = 12
  Call TSDSEC01
  Call PIN.RESTAURA.DISPLAY
End Sub

Function Pin.Datos$(Umsg1$,Umsg2$) Public     									          ! Captura De Datos
String UMSG1$, UMSG2$, A.DAT$, AB$, AQ$								        						! Definicion de variables
String PIN.DATOS$,  ADATA$																								!
  Locate #30;1,1,OFF										                                  ! Posiciona cursor
  Write Form "2C20";#30;Umsg1$,Umsg2$	                             				! Mensajes
   UNLOCKDEV 32,7                                													! Desbl Teclado y pasa
   Wait 32;100                                   													! al Estado ACCT NO
   Read #32;ADATA$,AB$,AB$,AB$,               						               \! Lectura Teclado y Status
            AB$,AB$,AB$,AB$,AB$,AB$,AQ$              											! teclado con 10 Datos
   If Mid$(ADATA$,11,1)  = "1" Then PIN.LLAVE% = -1				  							! Llave Puesta
   If Mid$(ADATA$,13,1)  = "I" Then PIN.DATOS$ = "E"											! Presiono Borrar
   If Mid$(ADATA$,14,1) <> " " Then               						       		 \! Si hay Errror
      Call Msg.Visor("Dato No Valido")	                  								! Msg al operador
   If Mid$(ADATA$,13,1) = "P" Then               						       			 \! si presiono ENTER
      PIN.DATOS$ = Right$(AQ$,LEN(AQ$)-1)           											! Asigna dato capturado
End Function 
!--- Fin Entrada De Datos


!Parametros a pasar UE.STRING$ = String a Imprimir
!  	            UE.STATION = Destino de la impresion
!                		4000H  - print on CR
!                		2000H  - print on SJ
!                		1000H  - print on DI
!                		0100H  - number of linefeeds (eg here = 1)
!		                0010H  - Double High (Only 4610)
!		                0020H  - Double Wide 
!		                0040H  - Double Wide Double High 
!		                0080H  - c (Only 4610)
!		                0001H  - Bold
Sub PIN.IMPRIME(UE.STRING$,UE.STATION) Public		      				! Rutina de Impresion
String UE.STRING$, LF$ 											                  ! Variables temporales
Integer*2 UE.STATION											                    !
    LF$ = CHR$(0DH)                                           !
    If PRT4610.ENABLE NE 0 Then Begin									        ! Impresoras 4610
     If UE.STRING$ = "" Then UE.STRING$ = LF$								  !
    EndIf												                              !
    Call PIN.SALVAR.VARIABLES								                  ! Salva variables impresion
    TS.LINETYPE   = 29											                  ! Genera impresion al
    TS.SAVPRT$    = UE.STRING$										            ! dispositivo requerido
    TS.SAVPRT.OPT = UE.STATION										            !
    Call TSPREC01											                        !
    Call PIN.RESTAURA.VARIABLES								                ! Restaura control aplicacion
End Sub 
!--- Fin rutina de impresion 

Sub PIN.EXPLUSADI Public									                ! Expulsa papel en el 
    If PRT4610.ENABLE NE 0 Then \ 									          ! Document Insert
       Write #35;CHR$(1BH)+CHR$(6DH)\									        ! Si impresora 4610
    ELSE\												                              !
       Write Form "2C1 A0"; #35; CHR$(27), CHR$(12)           ! Si impresora Mod. 4
End Sub
!--- Fin Rutina Expulsion Papel DI				

Sub PIN.CORTACR Public										                    ! Corta Papel en 
 String LF$                                                   ! CR
    LF$ = CHR$(0DH)                                           !
    If PRT4610.ENABLE NE 0 Then Begin									        ! Impresoras 4610
        WRITE #34; CHR$(1BH)+CHR$(6DH)+LF$+LF$               	!
    EndIf Else \											                        ! Impresoras Matriz
       Write Form "2C1 A2"; #34; CHR$(27), CHR$(80)	        	!
End Sub
!--- Fin Rutina corte de papel

Function PIN.Detalle Public                                   ! Imprime Detalle De La Operacion
String PIN.Detalle, X.Oper$, X.A$
Integer*4 X.Trx%
X.Trx% = Sl.Hd.Transnum 
X.Oper$ = Unpack$(Ts.Oper$)
X.Oper$ = Str$(Val(X.Oper$))

Pin.Detalle =   Left$(Date$,2)+"/"+Mid$(Date$,3,2)+"/"+Right$(Date$,2)+ " "+\
                Left$(Time$,2)+":"+Mid$(Time$,3,2)+" "+\
                Right$("   "+Ts.Store$,3)+" "+\
                Right$("   "+Ts.Terminal$,3)+" "+\
	              Right$("0000" +Str$(X.Trx%),4)+" "+\
                X.Oper$
End  Function

Sub Pin.Cadena.Usuario(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     TS.TEMP1I1   = 99																					! Indica String de usuario
     TS.USERDATA$ = Pack$(X.Clave$) + ":"       +	       \! Numero Del Proyecto
		          X.Datos$  + ":"  			! Datos Almacenados
     Call TSTPEC01                                              ! Grabacion del String 
End Sub

!Sub Pin.Cadena.Usuario2(X.Clave$,X.Datos$) Public
!String X.Clave$, X.Datos$
!     Sl.End = Sl.End + 1 
!     Sl.Str$(Sl.End) = Pack$("99")              +				       \! En String Reservado
!		":"+Pack$(X.Clave$)             +		                       \! Numero Del Proyecto
!		":"+X.Datos$ + ":"                                          ! Datos Para Almacenar
!
!End Sub 
!--- Fin Grabacion De Datos

Sub Pin.Cadena.Usuario2(X.Clave$,X.Datos$) Public
  String X.Clave$, X.Datos$											  ! Variables
  SL.END = SL.END + 1                             ! BUMP ARRAY POINTER
  X.Datos$ = Pack$("99")            +			       \! En String Reservado
		         ":"+Pack$(X.Clave$)    +		         \! Numero Del Proyecto
		         ":"+X.Datos$ + ":"                   !
  Call Write.SL.STR(SL.END, X.Datos$)             ! PUT DATA IN ARRAY
End Sub                                           !

Function Pin.Getunpk(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Pin.Getunpk
  Ue.Pin.Apun% = Match(":",X.B$,X.J%) 			                                     ! Busca Separador De Campo
  Pin.Getunpk  = Unpack$(Mid$(X.B$,X.J%,Ue.Pin.Apun% - X.J%))                   ! Desempaqueta Campo
  Ue.Pin.Apun% = Ue.Pin.Apun% + 1
End Function

Function Pin.Getunpk2(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Pin.Getunpk2
  Ue.Pin.Apun% = Match("|",X.B$,X.J%) 			                             ! Busca Separador De Campo
  Pin.Getunpk2 = (Mid$(X.B$,X.J%,Ue.Pin.Apun% - X.J%))                   ! Desempaqueta Campo
  Ue.Pin.Apun% = Ue.Pin.Apun% + 1
End Function

!--- Funciones para manejo de Pipes de memoria
Function CREA.PIPES(UE.PIPE.NAME$, UE.PIPE.SESION%) Public
   STRING  UE.PIPE.NAME$
   INTEGER UE.PIPE.SESION%
   UE.PIPE.NAME$ = "PI:"+UE.PIPE.NAME$
   CREATE UE.PIPE.NAME$ AS UE.PIPE.SESION%
End Function
!--- Fin funcion de creacion de pipes

Function ABRE.PIPES(UE.PIPE.NAME$, UE.PIPE.SESION%) Public
   STRING  UE.PIPE.NAME$
   INTEGER UE.PIPE.SESION%
   UE.PIPE.NAME$ = "PI:"+UE.PIPE.NAME$
   OPEN UE.PIPE.NAME$ AS UE.PIPE.SESION%
End Function
!--- Fin funcion de apertura de pipes

Function GRABA.PIPES(UE.PIPE.SESION%,UE.PIPE.FORM$,UE.PIPE.DATA$) Public
   STRING  UE.PIPE.FORM$, UE.PIPE.DATA$
   INTEGER UE.PIPE.SESION%
   WRITE FORM UE.PIPE.FORM$;#UE.PIPE.SESION% AUTOUNLOCK;UE.PIPE.DATA$
   EXIT FUNCTION
End Function
!--- Fin de la funcion de grabacion de pipes

Function LEER.PIPES(UE.PIPE.SESION%,UE.PIPE.FORM$) Public
   STRING  UE.PIPE.FORM$, LEER.PIPES,UE.PIPE.DATA$
   INTEGER UE.PIPE.SESION%
   READ FORM UE.PIPE.FORM$;#UE.PIPE.SESION%;UE.PIPE.DATA$
   LEER.PIPES = UE.PIPE.DATA$
End Function
!--- Fin de la funcion de lectura de pipes

Sub SPLIT.ITEMRECORD (RECORD$) Public
  STRING    RECORD$    !* The record (including the key) read from file      *!

  !***************************************************************************!
  !* Split the record into the various item fields                           *!
  !***************************************************************************!
  PIN.ITEMCODE$ = MID$(RECORD$,1,6)
  PIN.INDICAT0  = GETN1(RECORD$,6)
  PIN.INDICAT1  = GETN1(RECORD$,7)
  PIN.INDICAT1A = GETN1(RECORD$,8)
  PIN.INDICAT2$ = MID$(RECORD$,10,1)
  PIN.DEPARTME$ = MID$(RECORD$,11,2)
  PIN.FAMILYNU$ = MID$(RECORD$,13,3)
  PIN.MPGROUP$  = MID$(RECORD$,16,1)
  PIN.SALEQUAN$ = MID$(RECORD$,17,1)
  PIN.SALEPRIC$ = MID$(RECORD$,18,5)
  PIN.LINKEDTO$ = MID$(RECORD$,23,2)
  PIN.ITEMNAME$ = MID$(RECORD$,25,18)
  PIN.USEREXIT1 = GETN2(RECORD$,42)
  PIN.USEREXIT2 = GETN2(RECORD$,44)

END SUB

Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) External
  STRING ClassName
  STRING MethodName
  STRING TheRequest
End Sub

Sub JavaCall.AddParameter.String(TheRequest,TheParameter) External
  STRING TheRequest
  STRING TheParameter
End Sub

Sub JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) External
  STRING TheRequest
  STRING ReturnValue
  STRING Exception
End Sub

Function Pin.Rutina.Java(Java.CLASS$, Java.METHOD$, Java.MESSAGE$) Public 					! Inicializacion Clases de Java
String Java.METHOD$, Java.REQUEST$, Java.EXCEPTION$, Java.CLASS$,             			       \! Def. Variables Tmp
       Java.RETURNVALUE$, Java.MESSAGE$, Pin.Rutina.Java            					!

Java.REQUEST$     = "C$"										! Init de variables
Java.EXCEPTION$   = ""											! para la ejecucion
Java.RETURNVALUE$ = ""											! del modulo
Java.EXCEPTION$   = "" 											! Init variable
Call Javacall.Initialize.Request(Java.CLASS$,Java.METHOD$,Java.REQUEST$)  				! Inicializacion de clases
Call Javacall.AddParameter.String(Java.REQUEST$,Java.MESSAGE$)						! Adicion de parametros
Call Javacall.InvokeMethod.ReturnString(Java.REQUEST$,Java.RETURNVALUE$,                               \! Ejecucion de la clase
        				Java.EXCEPTION$)						!

If Len(Java.EXCEPTION$) > 0 Then Begin									! Si hay Exception
   Call Pin.Imprime("Error en el envio",4100H)								! Imprime Error
   Call Pin.Imprime(LEFT$(Java.EXCEPTION$,38),4100H)							! en SJ
   Java.RETURNVALUE$ = ""										! Returna Nulo
EndIf
Pin.Rutina.Java = Java.RETURNVALUE$										! Retorna Respuesta
   
End Function
!--- Fin invocacion rutinas de Java

Function Pin.Trama.Msg(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$) Public			! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Pin.Trama.Msg, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$
Integer*4 XL.TRX%

If Pin.Pay.Impr% = 2 Then Begin 
   XL.TRX% = SL.HD.TRANSNUM
EndIf Else Begin 
   XL.TRX% = SL.HD.TRANSNUM + 1
EndIf 

U.X1$ = Right$("00"+U.X1$,2)        							! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							! Factura de venta
!HORA.MUNDIAL$ = DATE$ + TIME$

U.Tmp1$ = U.X5$                                       +     \! Codigo de la cadena
          Right$("0000"+TS.STORE$,4)                  +     \! Numero del almacen
          Right$("000000"+Str$(SL.HD.TERMINAL),6)     +     \! Numero de terminal
          Unpack$(TS.OPER$)                           +     \! Numero de operador
          U.X6$                                       +     \! Factura fiscal                     
          Right$("000000"+Str$(XL.TRX%),6)            +     \! Numero de transaccion
          DATE$ + TIME$				      +     \! Fecha y Hora de la operacion
          U.X4$ + 					    \! Estado del requerimiento
          U.X3$                                              ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Pin.Trama.Msg = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					          ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Function Pin.Valida.Rta(Asc.Lcl.Tmp2$) Public			  		        ! Validacion De La Respuesta
String  X.Rta$(1), X.Err$, Errfx$, Asc.Lcl.Tmp2$, Pin.Valida.Rta
Dim X.Rta$(20)										! Vector De Respuesta 

X.Rta$(01) = Mid$(Asc.Lcl.Tmp2$,001,02)							! Codigo De Aplicacion
X.Rta$(02) = Mid$(Asc.Lcl.Tmp2$,003,02)							! Codigo De Funcion
X.Rta$(03) = Mid$(Asc.Lcl.Tmp2$,005,03)							! Longitud Respuesta  
X.Rta$(04) = Mid$(Asc.Lcl.Tmp2$,008,01)							! Rta Appl Mngr  
X.Rta$(05) = Mid$(Asc.Lcl.Tmp2$,009,01)							! Rta De La Trx       
X.Rta$(06) = Mid$(Asc.Lcl.Tmp2$,010,02)							! Estado Del Appl Mngr 

If (X.Rta$(05) = "0")  Then Begin						        ! Si Operacion Exitosa
   Pin.Valida.Rta = "00"
Endif Else Begin						                        ! Si Se Presenta Error 
   Call Pin.Imprime("----  Error de Operacion  ----",2100H)			        ! Msg Error En Sj
   X.Err$ = "ERROR DESCONOCIDO "+X.Rta$(05)						! Init Variable
   Call Pin.Imprime("Error "+X.Rta$(05),2100H)				                ! Msg Error En Sj
   If X.Rta$(05) = "1" Then X.Err$ = "ERROR DESPUES DE CONEXION"			! Reporta Msg De Error Al 
   If X.Rta$(05) = "2" Then X.Err$ = "CONEXION NO ESTABLECIDA  "			! Rollo De Auditoría
   If X.Rta$(05) = "3" Then X.Err$ = "SIN RESPUESTA EN SERVIDOR"			!
   If X.Rta$(05) = "K" Then X.Err$ = "ABANDONO DE OPERADOR     "			!
   If X.Rta$(05) = "T" Then X.Err$ = "TERMINAL NO INICIALIZADA "			!
   If X.Rta$(05) = "G" Then X.Err$ = "FUNCION NO DEFINIDA APPL "			!
   If X.Rta$(05) = "O" Then X.Err$ = "MOVIMIENTO NO ENCONTRADO "
   If MATCH(X.Rta$(05),"123KTGO",1) <= 0 Then Begin					! Error Desconocido
      Call PIN.Traduce.Error(Errn,Errfx$)				                        !
      X.Err$ = "CODIGO DE ERROR     NO DEFINIDO... "                  			!
      Call Pin.Imprime(X.Err$,2100H)							!
   Endif
   Pin.Valida.Rta = X.Err$
   Call TSHIECET
   Call MSG.DISPLAY(X.ERR$)
   Wait ; 600   
   Call Pin.Imprime(X.Err$,2100H)					                        !
   Call Pin.Imprime("--------------------------------",2100H)		    		!
Endif
End Function 