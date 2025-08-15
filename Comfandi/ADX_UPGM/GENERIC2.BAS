!************************************************** 
!Empresa       : Asic S.A.  Unidad de Retail      *
!Programa      : GENERICA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Marzo 15 de 2.004                *
!Observaciones : Rutinas Genericas Aplicaciones   *
!**************************************************
! Revision rutina captura de dato EAN-128 bloqueo
! de la terminal OVS
!--------------------------------------------------
! Se adiciona rutina para el manejo de pips
! OVS Mzo 3 2006


%ENVIRON T

String    Global ASC.USER.SAVE$(1), HORA.MUNDIAL$
Integer   Global Sl.End
Integer*2 Global ASC.USER.SAVE(1), TS11.OVRFLAG
Integer*1 Global ASIC.LLAVE% , Asc.Pay.Impr%, Asic.Device%, Asic.Detalle%, PRT4610.REPRINT
Integer*2 Global USER.SAVE(1) 				! Save area for Subroutine calls
String    Global USER.SAVE$(1)			  ! Save area for Subroutine calls
Integer*4 Global Asc.Tmp.Apun%,      \! Apuntador Lectura Strings
                 Asc.Tmp.Apun2%       !
Integer*4 Global Asc.Save.Data(1)
Integer*2 GLOBAL IRRF.RECL            ! Current record length of itemr
String    GLOBAL TS.WEIGHTSEP$        !   

String    Global GR.IO.DATA$(1)
Integer*2 Global GR.IO.MOTORKEY
Integer*4 Global GR.IO.KEYS(1)

%Include EAMTOPTS.J86	                ! Definicion Terminal Options
%INCLUDE EAMTSWKG.J86									! Definicion de variables
%INCLUDE EAMTRANS.J86									! Definicion de variables
%INCLUDE EAMP4VAJ.J86        				  ! 4610 Printer
%INCLUDE EAMP4VGJ.J86                 ! 4610 Variable definitions
%INCLUDE EAMSOPTS.J86				          ! Definicion de Store Options    

%INCLUDE GRITEMR.J86        				  ! Variables Itemr

%Include EAMTSXHC.J86                 !

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

Sub TSCSEC10 External                 ! Termination procedure
End Sub

Sub INIT.TRX External                 ! Inicializacion de una transaccion
End Sub                        

Sub TSBDEC01 EXTERNAL          ! balance due (normal/foodstamp)
End Sub                        !

Sub TSETEC02 EXTERNAL            ! VOID TRANSACTION
End Sub                          !


SUB ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL
	INTEGER*4 RET
	INTEGER*2 FUNC, PARM1
	STRING PARM2
END SUB


SUB REPRINT.RECEIPT EXTERNAL           ! 4610 routine to reprint the receipt
END SUB

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


Sub TRADUCE.ERROR(ERRVALUE%,ERRVALUE$) Public
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

Sub SALVAR.DISPLAY Public
  Dim Asc.Save.Data(7)
  Asc.Save.Data(1) = TS.LINETYPE
  Asc.Save.Data(2) = TS.LINEDATA
  Asc.Save.Data(3) = TS.LINEDATA2
  Asc.Save.Data(4) = TS.LINEDATA3
  Asc.Save.Data(5) = TS.XXMOD
  Asc.Save.Data(6) = TS.YYMOD
  Asc.Save.Data(7) = TS.DS.NOSAVE
End Sub
   
! restore display routine

Sub RESTAURA.DISPLAY Public
  TS.LINETYPE  = Asc.Save.Data(1)
  TS.LINEDATA  = Asc.Save.Data(2)
  TS.LINEDATA2 = Asc.Save.Data(3)
  TS.LINEDATA3 = Asc.Save.Data(4) 
  TS.XXMOD = Asc.Save.Data(5)
  TS.YYMOD = Asc.Save.Data(6)
  TS.DS.NOSAVE = Asc.Save.Data(7)
End Sub

Sub SALVAR.VARIABLES Public
      Dim USER.SAVE(13)
      USER.SAVE(1) = TS.LINETYPE
      USER.SAVE(2) = TS.LINEDATA
      USER.SAVE(3) = TS.LINEDATA2
      USER.SAVE(4) = TS.LINEDATA3
      USER.SAVE(5) = TS.PRT.PARM
      USER.SAVE(6) = TS.PRT.OPT
      USER.SAVE(7) = TS.PRT.SJDI
      USER.SAVE(8) = TS.SAVPRT.OPT
      USER.SAVE(9) = TS.XXMOD
      USER.SAVE(10) = TS.YYMOD
      USER.SAVE(11) = TS.ZMOD
      USER.SAVE(12) = TS.PRINTPRM
      USER.SAVE(13) = TS.PROCEDURE
      DIM USER.SAVE$(5)
      USER.SAVE$(1) = TS.PRTBUF$
      USER.SAVE$(2) = TS.PRDATA$
      USER.SAVE$(3) = TS.FORMCR$
      USER.SAVE$(4) = TS.SJDATA$
      USER.SAVE$(5) = TS.SAVPRT$
End Sub
 
Sub RESTAURA.VARIABLES Public
      TS.LINETYPE   = USER.SAVE(1)
      TS.LINEDATA   = USER.SAVE(2)
      TS.LINEDATA2  = USER.SAVE(3)
      TS.LINEDATA3  = USER.SAVE(4)
      TS.PRT.PARM   = USER.SAVE(5)
      TS.PRT.OPT    = USER.SAVE(6)
      TS.PRT.SJDI   = USER.SAVE(7)
      TS.SAVPRT.OPT = USER.SAVE(8)
      TS.XXMOD      = USER.SAVE(9)
      TS.YYMOD      = USER.SAVE(10)
      TS.ZMOD       = USER.SAVE(11)
      TS.PRINTPRM   = USER.SAVE(12)
      TS.PROCEDURE  = USER.SAVE(13)
      TS.PRTBUF$    = USER.SAVE$(1)
      TS.PRDATA$    = USER.SAVE$(2)
      TS.FORMCR$    = USER.SAVE$(3)
      TS.SJDATA$    = USER.SAVE$(4)
      TS.SAVPRT$    = USER.SAVE$(5)
End Sub

Sub VISOR.AND.BORRAR(X.MSG$) Public 
  STRING X.MSG$
  CALL SALVAR.DISPLAY
  TS.TEMP1$ = LEFT$(X.MSG$,20)
  TS.TEMP2$ = MID$(X.MSG$,21,20) 
  TS.LINETYPE = 12
  CALL TSCSEC08
  CALL RESTAURA.DISPLAY
End Sub

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) Public		! Msg Display
! U.VISOR%    : Visor al que se desea desplegar el mensaje 
!               1. Visor del Operador 
!               2. Visor del Cliente  
! U.LINEA1$   : Primera linea del mensaje 20 caracteres
! U.LINEA2$   : Segunda linea del mensaje 20 caracteres
! U.TIEMPO%   : Tiempo de espera del Msg en milisegundos
! U.POSICION% : Fomato de presentacion L Izquierda, R Derecha C centrado

String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$								      ! Variables de
Integer*2 U.d.visor%, U.d.tiempo%, u.t.visor%, ud.pos%							! trabajo
String U.D.Linea1$, U.D.Linea2$										                  !
  
  If UCASE$(U.D.POSICION$) = "L" Then Begin 								        ! Justificacion a la izquierda
     U.D.Linea1$ = Left$(U.D.Linea1$ + String$(20," "),20)
     U.D.Linea2$ = Left$(U.D.Linea2$ + String$(20," "),20)
  EndIf  
  IF UCASE$(U.D.POSICION$) = "R" Then Begin 								        ! Justificacion a la derecha
     U.D.Linea1$ = Right$(STRING$(20," ") + U.D.Linea1$,20)
     U.D.Linea2$ = Right$(STRING$(20," ") + U.D.Linea2$,20)
  EndIf  
  If UCASE$(U.D.POSICION$) = "C" Then Begin 								        ! Justificacion centrado
     ud.pos% = 20 - LEN(U.D.Linea1$)
     ud.pos% = ud.pos% / 2
     U.D.Linea1$ = STRING$(ud.pos%," ")+U.D.Linea1$
     ud.pos% = 20 - LEN(U.D.Linea2$)
     ud.pos% = ud.pos% / 2
     U.D.Linea2$ = STRING$(ud.pos%," ")+U.D.Linea2$
  EndIf  
  Call SALVAR.DISPLAY

!  If U.D.VISOR% = 1 Then Begin
!    TS.TEMP1$ = U.D.Linea1$
!	  TS.TEMP2$ = U.D.Linea2$
!	EndIf Else Begin 
!    TS2.DISP1$ = U.D.Linea1$
!    TS2.DISP2$ = U.D.Linea2$
!	EndIf   
!	TS.LINETYPE = 12
!	Call TSDSEC01
!  Wait ; U.d.Tiempo%											                          ! 

  If U.d.visor% = 1 Then U.d.Visor% = 30 Else U.d.Visor% = 65
  Locate #U.D.VISOR%;1,1,OFF										! Posiciona cursor
  Write Form "2C20";#U.D.VISOR%;U.D.Linea1$, U.D.Linea2$						! Mensajes
  Wait ; U.d.Tiempo%											                          ! 
  Call RESTAURA.DISPLAY
  
End Function												                                !
!--- Fin despliege de mensajes a los visores 


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

Function U.IMPRIME(UE.STRING$,UE.STATION) Public							! Rutina de Impresion
String UE.STRING$, LF$ 											                  ! Variables temporales
INTEGER*2 UE.STATION											                    !
    LF$ = CHR$(0DH)                                           !
    If PRT4610.ENABLE NE 0 Then Begin									        ! Impresoras 4610
     If UE.STRING$ = "" Then UE.STRING$ = LF$								  !
     If Ue.Station And 10H Then                              \! Double High
        Ue.String$ = Chr$(1Bh)+Chr$(68H)+Chr$(1) + Ue.String$	! 
     If Ue.Station And 20H Then                              \! Double Wide 4610
        Ue.String$ = Chr$(1Bh)+Chr$(57H)+Chr$(1) + Ue.String$	!
     If Ue.Station And 40H Then Begin                         ! Double High Double Wide 4610
     	  Ue.String$ = Chr$(1Bh)+Chr$(68H)+Chr$(1) + Ue.String$	! 
        Ue.String$ = Chr$(1Bh)+Chr$(57H)+Chr$(1) + Ue.String$	!
     EndIf																										!
     If Ue.Station And 80H Then        				               \! Invert
       Ue.String$ = Chr$(1Bh)+Chr$(48H)+Chr$(1) + Ue.String$  !
     If Ue.Station And 1H Then                               \! Bold
        Ue.String$ = Chr$(1Bh)+Chr$(47H)+Chr$(1) + Ue.String$ !
    EndIf												                              !
    Call SALVAR.VARIABLES										                  ! Salva variables impresion
    TS.LINETYPE   = 29											                  ! Genera impresion al
    TS.SAVPRT$    = UE.STRING$										            ! dispositivo requerido
    TS.SAVPRT.OPT = UE.STATION										            !
    Call TSPREC01											                        !
    Call RESTAURA.VARIABLES										                ! Restaura control aplicacion
    
End Function
!--- Fin rutina de impresion 

Function U.EXPLUSADI Public										                ! Expulsa papel en el 
    If PRT4610.ENABLE NE 0 THEN \ 									          ! Document Insert
       WRITE #35;CHR$(1BH)+CHR$(6DH)\									        ! Si impresora 4610
    ELSE\												                              !
       WRITE FORM "2C1 A0"; #35; CHR$(27), CHR$(12)           ! Si impresora Mod. 4
End FUNCTION												!
!--- Fin Rutina Expulsion Papel DI				

Function U.CORTACR PUBLIC										                  ! Corta Papel en 
 STRING LF$                                                   ! CR
    LF$ = CHR$(0DH)                                           !
    IF PRT4610.ENABLE NE 0 THEN BEGIN									        ! Impresoras 4610
        WRITE #34; CHR$(1BH)+CHR$(6DH)+LF$+LF$               	!
    EndIF ELSE \											                        ! Impresoras Matriz
       WRITE FORM "2C1 A2"; #34; CHR$(27), CHR$(80)	        	!
End FUNCTION
!--- Fin Rutina corte de papel

FUNCTION ASIC.DATOS$(UMSG1$,UMSG2$) PUBLIC     								! Captura de datos
STRING UMSG1$, UMSG2$, A.DAT$, AB$, AQ$								        ! Definicion de variables
STRING ASIC.DATOS$, ADATA$			!
   TS.IO.STATE = 10                    																			! Enter/Data state
   ASIC.DEVICE% = 1 
   ASIC.LLAVE% = 0 
   Call VISORES4690(1,"MUESTRE TECLA NUM","",100,"L")											  ! Msg al operador   
   Call VISORES4690(1,UMSG1$,UMSG2$,0,"L")																	! Msg al operador
   UNLOCKDEV 32,7                                														! Desbl Teclado y pasa
   WAIT 32;100                                   														! al Estado ACCT NO
   
   READ #32;ADATA$,AB$,AB$,AB$,               						                 \! Lectura Teclado y Status
            AB$,AB$,AB$,AB$,AB$,AB$,AQ$              												! teclado con 10 Datos
               
   IF MID$(ADATA$,11,1)  = "1" THEN ASIC.LLAVE% = -1												! Llave Puesta
   IF MID$(ADATA$,13,1)  = "I" THEN ASIC.DATOS$ = "E"												! Presiono Borrar
   IF MID$(ADATA$,14,1) <> " " THEN ASIC.DATOS$ = "E"           						! Si hay Errror
   
     !Call VISORES4690(1,"Dato No Valido","...",500,"C")				      ! Msg al operador
      
!   If MID$(ADATA$,13,1) = "P" THEN Begin               					     \! si presiono ENTER
!      ASIC.DATOS$ = RIGHT$(AQ$,LEN(AQ$)-1)           						! Asigna dato capturado
!      ASIC.DEVICE% = Val(MID$(ADATA$,10,1))                                ! Forma capturada
!   EndIf 
   
   If Mid$(Adata$,13,1) = "P" Then Begin         											      ! Si Presiono Enter
   	 If (Len(Aq$)-1) > 0 Then Begin
        Asic.Datos$ = Right$(Aq$,Len(Aq$)-1)         				                ! Asigna Dato Capturado
        Asic.Device% = Val(Mid$(ADATA$,10,1))                  				      ! Tipo de dato ingresado
     EndIf Else Asic.Datos$ = ""																						! Error retorna borrar
   EndIf 
   
END FUNCTION
!--- Fin entrada de datos


Function Valida.Rta(Asc.Lcl.Tmp2$) Public			  														! Validacion De La Respuesta
String  X.Rta$(1), X.Err$, Errfx$, Asc.Lcl.Tmp2$, Valida.Rta								!
Dim X.Rta$(20)																															! Vector De Respuesta 
X.Rta$(01) = Mid$(Asc.Lcl.Tmp2$,001,02)																			! Codigo De Aplicacion
X.Rta$(02) = Mid$(Asc.Lcl.Tmp2$,003,02)																			! Codigo De Funcion
X.Rta$(03) = Mid$(Asc.Lcl.Tmp2$,005,03)																			! Longitud Respuesta  
X.Rta$(04) = Mid$(Asc.Lcl.Tmp2$,008,01)																			! Rta Appl Mngr  
X.Rta$(05) = Mid$(Asc.Lcl.Tmp2$,008,02)																			! Rta De La Trx       
X.Rta$(06) = Mid$(Asc.Lcl.Tmp2$,012,02)																			! Estado Del Appl Mngr 
!1043058100099Subsidio no disponible 
If (X.Rta$(05) = "20") Or (Left$(X.Rta$(05),1) = "1")  Then Begin     			! Si Operacion Exitosa
   Valida.Rta = "00"
Endif Else Begin						                                          			! Si Se Presenta Error 
   Call U.Imprime("----  Error de Operacion  ----",2100H)			        			! Msg Error En Sj
   X.Err$ = "ERROR DESCONOCIDO "+X.Rta$(05)						                			! Init Variable
   Call U.Imprime("Error "+X.Rta$(05),2100H)				                  			! Msg Error En Sj
   If Left$(X.Rta$(05),1) = "1" Then X.Err$ = "ERROR DESPUES DE CONEXION"		! Reporta Msg De Error Al 
   If Left$(X.Rta$(05),1) = "2" Then X.Err$ = "CONEXION NO ESTABLECIDA  "		! Rollo De Auditoría
   If Left$(X.Rta$(05),1) = "3" Then X.Err$ = "SIN RESPUESTA EN SERVIDOR"		!
   If Left$(X.Rta$(05),1) = "K" Then X.Err$ = "ABANDONO DE OPERADOR     "		!
   If Left$(X.Rta$(05),1) = "T" Then X.Err$ = "TERMINAL NO INICIALIZADA "		!
   If Left$(X.Rta$(05),1) = "G" Then X.Err$ = "FUNCION NO DEFINIDA APPL "		!
   If Left$(X.Rta$(05),1) = "O" Then X.Err$ = "MOVIMIENTO NO ENCONTRADO "		!
   If Left$(X.Rta$(05),1) = "C" Then X.Err$ = "TIME OUT OPERACION       "		!
   Valida.Rta = "99"																												!
   Call TSHIECET																														!
   !Call VISOR.AND.BORRAR(Left$(X.ERR$,20)+Mid$(X.ERR$,21,20))   						!
   Call VISORES4690(1,Left$(X.ERR$,20),Mid$(X.ERR$,21,20),1500,"L")         !
   
   Call U.Imprime(X.Err$,2100H)					                        						!
   Call U.Imprime("--------------------------------",2100H)		    					!
Endif
End Function 

Function Rutina.Java(Java.CLASS$, Java.METHOD$, Java.MESSAGE$) Public 			! Inicializacion Clases de Java
String Java.METHOD$, Java.REQUEST$, Java.EXCEPTION$, Java.CLASS$,          \! Def. Variables Tmp
       Java.RETURNVALUE$, Java.MESSAGE$, Rutina.Java            						!

Java.REQUEST$     = "C$"																										! Init de variables
Java.EXCEPTION$   = ""																											! para la ejecucion
Java.RETURNVALUE$ = ""																											! del modulo
Java.EXCEPTION$   = "" 																											! Init variable
Call Javacall.Initialize.Request(Java.CLASS$,Java.METHOD$,Java.REQUEST$)  	! Inicializacion de clases
Call Javacall.AddParameter.String(Java.REQUEST$,Java.MESSAGE$)							! Adicion de parametros
Call Javacall.InvokeMethod.ReturnString(Java.REQUEST$,Java.RETURNVALUE$,   \! Ejecucion de la clase
        				Java.EXCEPTION$)																						!

If LEN(Java.EXCEPTION$) > 0 THEN BEGIN																			! Si hay Exception
   CALL U.Imprime("Error en el envio",4100H)																! Imprime Error
   CALL U.Imprime(LEFT$(Java.EXCEPTION$,38),4100H)													! en SJ
   Java.RETURNVALUE$ = ""																										! Returna Nulo
EndIF																																				!
Rutina.Java = Java.RETURNVALUE$																							! Retorna Respuesta
End Function
!--- Fin invocacion rutinas de Java

Function Armar.Trama.Msg(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$) Public			! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$, Xseg$
Integer*4 XL.TRX%

If Asc.Pay.Impr% = 2 Then Begin 									! Si proceso al final de trx UE 02
   XL.TRX% = SL.HD.TRANSNUM
EndIf Else Begin 																	! Si proceso antes de cerrar la trx 
   XL.TRX% = SL.HD.TRANSNUM + 1
EndIf 

U.X1$ = Right$("00"+U.X1$,2)        							! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							! Factura de venta

If Not(TS.INTRX) Then Begin 
   HORA.MUNDIAL$ = DATE$ + TIME$
Endif Else Begin
   Xseg$ = Right$(Time$,2)																						  ! Segundos de la trx 
   HORA.MUNDIAL$ = UnPack$(SL.HD.DATETIME$) + Xseg$                     ! AAMMDDHHMMSS
EndIf

U.Tmp1$ = U.X5$ +                                      				       \! Codigo de la cadena
          Right$("0000"+TS.STORE$,4)                  +  	           \! Numero del almacen
          Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 			       \! Numero de terminal
          Unpack$(TS.OPER$)                           +              \! Numero de operador
          U.X6$                                       +              \! Factura fiscal                     
          Right$("000000"+Str$(XL.TRX%),6)            + 			       \! Numero de transaccion
          Hora.Mundial$                               +              \! Fecha y Hora de la operacion AAMMDDHHMMSS
          U.X4$ + 								                                   \! Estado del requerimiento
          U.X3$                                                       ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Armar.Trama.Msg = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					          ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Function Armar.Trama.Fonede(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$) Public			! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Fonede, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$, Xseg$
Integer*4 XL.TRX%

If Asc.Pay.Impr% = 2 Then Begin 
   XL.TRX% = SL.HD.TRANSNUM
EndIf Else Begin 
   XL.TRX% = SL.HD.TRANSNUM + 1
EndIf 

U.X1$ = Right$("00"+U.X1$,2)        							! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							! Factura de venta
HORA.MUNDIAL$ = DATE$ + TIME$

!Xseg$ = Right$(Time$,2)
!HORA.MUNDIAL$ = UnPack$(SL.HD.DATETIME$) + Xseg$                      ! AAMMDDHHMMSS - Se adicionan los segundos de la trx 

U.Tmp1$ = U.X5$ +                                      				       \! Codigo de la cadena
          Right$("0000"+TS.STORE$,4)                  +  	           \! Numero del almacen
          Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 			       \! Numero de terminal
          Unpack$(TS.OPER$)                           +              \! Numero de operador
          U.X6$                                       +              \! Factura fiscal                     
          Right$("000000"+Str$(XL.TRX%),6)            + 			       \! Numero de transaccion
          HORA.MUNDIAL$                               +              \! Fecha y Hora de la operacion AAMMDDHHMMSS
          U.X4$ + 								                                   \! Estado del requerimiento
          U.X3$                                                       ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Armar.Trama.Fonede = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					      ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Function Grabacion.String.Usuario(X.clave$,X.datos$) Public
String X.clave$, X.datos$
     SL.End = SL.End + 1 
     SL.STR$(SL.End) = PACK$("11")              +				       \! En String reservado
		":"+PACK$(X.clave$)             +		                       \! Numero del Proyecto
		":"+";"+X.datos$                +	      	                       \! Datos para almacenar
		";"+PACK$("000")+";"            +				       \! Cierre de String
		":"+":"+":"+":"
End Function
!--- Fin grabacion de datos

!Function Asic.Getunpk(X.B$,X.J%) Public
!Integer*2 X.J%, X.K%
!String    X.B$, Asic.Getunpk
!  Asc.Tmp.Apun% = Match(":",X.B$,X.J%) 				                                    ! Busca Separador De Campo
!  Asic.Getunpk  = Unpack$(Mid$(X.B$,X.J%,Asc.Tmp.Apun% - X.J%))                   ! Desempaqueta Campo
!  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
!End Function

Function Asic.Getunpk(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk
  Asc.Tmp.Apun% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk  = Unpack$(Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk  = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk2(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk2
  Asc.Tmp.Apun% = Match(";",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk2 = Unpack$(Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk2 = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk3(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk3
  Asc.Tmp.Apun% = Match(";",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk3 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk3 = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk4(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk4
  Asc.Tmp.Apun% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk4 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk4 = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk5(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk5
  Asc.Tmp.Apun% = Match(",",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk5 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk5 = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk6(ST.B$,ST.J%) Public
Integer*4 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk6
  Asc.Tmp.Apun2% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun2% > 0 Then \
     Asic.Getunpk6 = (Mid$(ST.B$,ST.J%,Asc.Tmp.Apun2% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk6 = ""
  Asc.Tmp.Apun2% = Asc.Tmp.Apun2% + 1
End Function

Function Linea.Detalle(X.Ses%)   Public                  ! Imprime Detalle De La Operacion
String Linea.Detalle, X.Oper$, X.A$
Integer*1 X.Ses%
Integer*4 X.Trx%
X.Trx% = Sl.Hd.Transnum
X.Oper$ = Unpack$(Ts.Oper$)
X.Oper$ = Str$(Val(X.Oper$))
If Asic.Detalle% = 0 Then X.Trx% = Sl.Hd.Transnum + 1 
Linea.Detalle = Left$(Date$,2)+"/"+Mid$(Date$,3,2)+"/"+Right$(Date$,2)+ " "+\
                Left$(Time$,2)+":"+Mid$(Time$,3,2)+" "+\
                Right$("   "+Ts.Store$,3)+" "+\
                Right$("   "+Ts.Terminal$,3)+" "+\
	              Right$("0000" +Str$(X.Trx%),4)+" "+\
                X.Oper$
End  Function

!--- Se Modifica La Rutina Para Que Grabe El String Como Un 99 Y No Como 11
!--- Cambio Realizado El 29 De Oct. 2004 Por Oscar Valencia

Function Grabacion.Cadena.Usuario(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     TS.TEMP1I1   = 99																					! Indica String de usuario
     TS.USERDATA$ = Pack$(X.Clave$) + ":" +	\! Numero Del Proyecto
		    X.Datos$+":"                 ! Datos Almacenados
     Call TSTPEC01                               ! Grabacion del String 
End Function
!--- Fin Grabacion De Datos

Function Grabacion.Cadena.Usuario2(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     
     Sl.End = Sl.End + 1 
     Sl.Str$(Sl.End) = Pack$("99")              +				       \! En String Reservado
		":"+Pack$(X.Clave$)             +		                       \! Numero Del Proyecto
		":"+X.Datos$ + ":"                                          ! Datos Para Almacenar
     
End Function
!--- Fin Grabacion De Datos


Sub Grabacion.Data.Entry(DATO1,DATO2,DATO3,DATO4,DATO5,DATO6) Public
String DATO1,DATO2,DATO3,DATO4,DATO5,DATO6
   For I% = 0 TO 10
     TS.IO.PREV.KEYS(I%) = TS.IO.KEYS(I%)   ! SAVE KEYED DATA
     TS.IO.PREV.DATA$(I%) = TS.IO.DATA$(I%) ! SAVE KEYED DATA
   Next I%
   Dim TS.IO.KEYS(10)                       ! CLEAR KEYED DATA
   Dim TS.IO.DATA$(10)                      ! CLEAR KEYED DATA
   TS.IO.KEYS(10) = 63                      ! Data entry key
   TS.IO.KEYS(4)  = 100                     ! No-sale key
   TS.IO.DATA$(2) = DATO1 				          ! Campo 1
   TS.IO.DATA$(3) = DATO2				            ! Campo 2
   TS.IO.DATA$(4) = DATO3				            ! Campo 3
   TS.IO.DATA$(5) = DATO4				            ! Campo 4
   TS.IO.DATA$(6) = DATO5				            ! Campo 5
   TS.IO.DATA$(7) = DATO6				            ! Campo 6
   TS.TEMP1I1 = 11                          ! Data entry
   Call TSTPEC01                            ! Process data entry
   For I% = 0 TO 10
     TS.IO.KEYS(I%) = TS.IO.PREV.KEYS(I%)   ! RESTORE KEYED DATA
     TS.IO.DATA$(I%) = TS.IO.PREV.DATA$(I%) ! RESTORE KEYED DATA
   Next I%
End Sub


Sub Msg.Girar  Public 
INTEGER*4 XI%, XJ%
STRING X1$, X2$, X3$

If NOT(TS.INTRX) Then Begin
   X1$ = Right$(TS.SDESC$(44),20)
   X3$ = Left$(TS.SDESC$(44),20)
   X2$ = String$(20," ") 

While (1)
  For XI% = 1 TO LEN(X1$)
    X2$ = Right$(X2$+MID$(X1$,XI%,1),20)
    CALL VISORES4690(2,X3$,X2$,0,"L")
    Wait 32 ; 50
    If EVENT% = 32 Then Exit Sub
  Next XI%
  
  For XJ% = 1 TO 20
    X2$ = RIGHT$(X2$+" ",20)
    CALL VISORES4690(2,X3$,X2$,0,"L")
    Wait 32 ; 50
    If EVENT% = 32 Then Exit Sub

  Next XJ%
Wend
EndIf 
End Sub 

Function Mirar.Datos$(Umsg1$,Umsg2$) Public    														! Captura De Datos
String Umsg1$, Umsg2$, A.Dat$, Ab$, Aq$								        						! Definicion De Variables
String Mirar.Datos$, Adata$

   Call Visores4690(1,Umsg1$,Umsg2$,0,"L")								    						! Msg Al Operador
   Unlockdev 32,7                                													! Desbl Teclado Y Pasa
   Wait 32;100                                   													! Al Estado Acct No
   Read #32;Adata$,Ab$,Ab$,Ab$,               						   						 \! Lectura Teclado Y Status
            Ab$,Ab$,Ab$,Ab$,Ab$,Ab$,Aq$              			  							! Teclado Con 10 Datos
   MIRAR.DATOS$ = Mid$(Adata$,13,1)
End Function

!--- Fin Entrada De Datos

Function ENTRADA.CLIENTE.DEMO Public
INTEGER*4 XI%, XJ%
STRING EQUIVALE(1), NOMBRE$, ENTRADA.CLIENTE.DEMO
  
IF TS.IO.MOTORKEY = 255 THEN BEGIN  ! Secuencias de tecleo letras
  DIM EQUIVALE(300)
  EQUIVALE( 61) = "A"
  EQUIVALE(100) = "B" 
  EQUIVALE(253) = "C" 
  EQUIVALE( 90) = "D" 
  EQUIVALE( 63) = "E" 
  EQUIVALE( 79) = "F" 
  EQUIVALE( 70) = "G" 
  EQUIVALE( 77) = "H" 
  EQUIVALE( 71) = "I" 
  EQUIVALE( 82) = "J" 
  EQUIVALE( 72) = "K" 
  EQUIVALE( 74) = "L" 
  EQUIVALE( 75) = "M" 
  EQUIVALE( 80) = "N" 
  EQUIVALE( 73) = "O" 
  EQUIVALE( 89) = "P" 
  EQUIVALE(252) = "Q" 
  EQUIVALE(204) = "R" 
  EQUIVALE(203) = "S" 
  EQUIVALE(238) = "T" 
  EQUIVALE(201) = "U" 
  EQUIVALE( 62) = "V" 
  EQUIVALE(254) = "W" 
  EQUIVALE(251) = "X" 
  EQUIVALE(250) = "Y" 
  EQUIVALE(217) = "Z" 
  EQUIVALE(220) = CHR$(32)
  
  NOMBRE$ = ""
  FOR XI% = 1 TO 30
     TS.TEMP1$ = MIRAR.DATOS$("INGRESE NOMBRE",NOMBRE$)
     XJ% = ASC(TS.TEMP1$)
     IF XJ% <> 91 THEN BEGIN  ! Presiono una letra
       IF XJ% = 81 THEN BEGIN ! Presiono BackSpace
         XI% = XI% - 1
         IF XI% < 1 THEN BEGIN  ! Cancelo todo
            NOMBRE$ = ""
            CALL VISOR.AND.BORRAR("PROCESO CANCELADO")
            DIM TS.IO.DATA$(10): DIM TS.IO.KEYS(10)
            TS.IO.MOTORKEY = 73
            EXIT FUNCTION 
         ENDIF 
         NOMBRE$ = LEFT$(NOMBRE$,LEN(NOMBRE$)-1)
       ENDIF ELSE BEGIN
          NOMBRE$ = NOMBRE$ + EQUIVALE(XJ%)
       ENDIF 
     ENDIF ELSE XI% = 100
     
  NEXT XI%
  ENTRADA.CLIENTE.DEMO = NOMBRE$ 
  DIM TS.IO.DATA$(10): DIM TS.IO.KEYS(10)
  TS.IO.MOTORKEY = 73
  EXIT FUNCTION 
ENDIF 

End Function 

Function Grabacion.String.Usuario2(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     Sl.End = Sl.End + 1 
     Sl.Str$(Sl.End) = Pack$("99")              +				       \! En String Reservado
		":"+Pack$(X.Clave$)             +		                       \! Numero Del Proyecto
		":"+X.Datos$ + ":"                                          ! Datos Para Almacenar

End Function
!--- Fin Grabacion De Datos

Function Entrada.Datos.Varios$(Umsg1$,Umsg2$) Public
  STRING REQ.MESSAGE$, DATA.ENTERED$, UE.WORK$, UE.SAVDISP$, Umsg1$,Umsg2$, Entrada.Datos.Varios$
  Call VISORES4690(1,"MUESTRE TECLA NUM","",100,"L")											  ! Msg al operador   
  UE.SAVDISP$ = TS.SAVDISP1$ + TS.SAVDISP2$
  UE.WORK$ = REQ.MESSAGE$ + STRING$(40," ")
  TS.SAVDISP1$ = Umsg1$               ! Split msg for display
  TS.SAVDISP2$ = Umsg2$
  TS.IO.MOTORKEY = 0                  ! Set for no input yet
  Entrada.Datos.Varios$ = ""
  TS.IO.STATE = 10                    ! Enter/Data state
  WHILE TS.IO.MOTORKEY = 0            ! While still no input
    TS.LINETYPE = 9                   ! Redisplay prompt
    CALL TSDSEC01                     ! Prompt for Date
    UNLOCKDEV 32, TS.IO.STATE, PRIORITY ! Keyed expiration date
    WAIT  32; 100                      ! Wait for input
    TS11.OVRFLAG = 33                  ! Mark for exit 14
    Call TSCSECRK                      ! Parse input / exit 14
    IF TS.IO.MOTORKEY = 73 THEN \      ! Clear key
    Begin 
        Entrada.Datos.Varios$ = "E"
    EndIf Else BEGIN                               ! Not clear key
        Entrada.Datos.Varios$ = TS.IO.DATA$(2) ! Assure 4 digits
        DIM TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 										    ! Init vectores de carga
        TS.IO.MOTORKEY = 73	
    EndIf                               ! Not clear key
  Wend                                  ! While still no input
  UE.WORK$ = ""
  TS11.OVRFLAG = 0                         ! Clear exit marker
  TS.SAVDISP1$ = LEFT$(UE.SAVDISP$,20)     ! Restore prior display
  TS.SAVDISP2$ = MID$(UE.SAVDISP$,21,20)
  UE.SAVDISP$ = ""
  TS.IO.NEXTSTATE = 10                     ! Restore state = MAIN
  TS.IO.STATE = 10                         ! Restore state = MAIN
  UNLOCKDEV 32, TS.IO.STATE                ! Restore state = MAIN
End Function 

Sub SPLIT.ITEMFILE (RECORD$) Public
  STRING    RECORD$    !* The record (including the key) read from file      *!

  !***************************************************************************!
  !* Split the record into the various item fields                           *!
  !***************************************************************************!
  GR.ITEMCODE$ = MID$(RECORD$,1,6)
  GR.INDICAT0  = GETN1(RECORD$,6)
  GR.INDICAT1  = GETN1(RECORD$,7)
  GR.INDICAT1A = GETN1(RECORD$,8)
  GR.INDICAT2$ = MID$(RECORD$,10,1)
  GR.DEPARTME$ = MID$(RECORD$,11,2)
  GR.FAMILYNU$ = MID$(RECORD$,13,3)
  GR.MPGROUP$  = MID$(RECORD$,16,1)
  GR.SALEQUAN$ = MID$(RECORD$,17,1)
  GR.SALEPRIC$ = MID$(RECORD$,18,5)
  GR.LINKEDTO$ = MID$(RECORD$,23,2)
  GR.ITEMNAME$ = MID$(RECORD$,25,18)
  GR.USEREXIT1 = GETN2(RECORD$,42)
  GR.USEREXIT2 = GETN2(RECORD$,44)
  !***************************************************************************!
  !* If the record length is longer than the default put the extra user data *!
  !* in another variable                                                     *!
  !***************************************************************************!
  If (IRRF.RECL > 46) Then Begin
    GR.USERDATA$ = RIGHT$(RECORD$, IRRF.RECL - 46)
  EndIf Else GR.USERDATA$ = ""
  	
End Sub

Sub DUPLICADO.TIQUETE.VENTA Public 
Integer*2 GR.SAVE.DUP.NUMBER            	       		! save 4610 var EP.ETO.DUPLICATE.NUMBER
    GR.SAVE.DUP.NUMBER = EP.ETO.DUPLICATE.NUMBER    ! save the 4610 variable
    EP.ETO.DUPLICATE.NUMBER = 1                  		! only print one receipt
    PRT4610.REPRINT = -1                         		! flag re-printing the receipt 
    CALL REPRINT.RECEIPT                         		! reprint the receipt
    PRT4610.REPRINT = 0                          		! reset flag 
    EP.ETO.DUPLICATE.NUMBER = GR.SAVE.DUP.NUMBER    ! restore 4610 variable
End Sub

Sub FORMAT.PESO ( WGT ) Public 
  Integer*4 WGT

  TS.TEMP1$ = STR$ ( WGT )
  IF (LEN(TS.TEMP1$) < TO.WGT.DECPOS + 1) THEN          \ more 0s reqd
      TS.TEMP1$ = RIGHT$( "0000" + TS.TEMP1$, TO.WGT.DECPOS+1)
  TS.TEMP1$ = LEFT$(TS.TEMP1$, LEN(TS.TEMP1$)-TO.WGT.DECPOS ) + \
        TS.WEIGHTSEP$ + RIGHT$(TS.TEMP1$, TO.WGT.DECPOS)        !IR87202

End Sub 

SUB SAVE.PRINT PUBLIC
 Call SALVAR.VARIABLES
END SUB


SUB RESTORE.PRINT PUBLIC
  Call RESTAURA.VARIABLES
End SUB

SUB ESTADO.TERMINAL PUBLIC
String    GParm2$, GStore$, Gterm$, Gprg$, Gctrl$, Gtipo$, Gmodel$
Integer*2 GParm1%, GRET
Call ADXSERVE(GRET,4,GPARM1%,GPARM2$)                												! Revisa estado de la Terminal
GSTORE$ = Mid$(GPARM2$,1,4)                       													! Numero de la tienda
Gterm$  = Mid$(GPARM2$,8,3)                       													! Numero de la terminal
Gprg$   = Mid$(GPARM2$,13,24)                     													! Programa de Carga Terminal
Gtipo$  = Mid$(GPARM2$,49,1)                       													! Tipo terminal (terminal, Terminal/Controller)
Gmodel$ = Mid$(GPARM2$,52,1)                      													! Modelo equipo
Gctrl$  = Mid$(GPARM2$,46,2)                      													! Controlador de carga

TS.TEMP1$ = "*** ESTADO TERMINAL ***"
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

TS.TEMP1$ = "TIENDA   :"+GSTORE$
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

TS.TEMP1$ = "TERMINAL :"+GTERM$+" "+Gctrl$
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

TS.TEMP1$ = "CARGA    :"+GPRG$  
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

IF GTIPO$ = "0" THEN TS.TEMP1$ = "TERMINAL" ELSE TS.TEMP1$ = "CONTROLLER/TERMINAL"
TS.TEMP1$ = "TIPO     :"+TS.TEMP1$
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

IF GMODEL$ = "1" THEN TS.TEMP1$ = "IBM 4693 "
IF GMODEL$ = "2" THEN TS.TEMP1$ = "IBM 4694 "
IF GMODEL$ = "3" THEN TS.TEMP1$ = "IBM 4683 "
IF GMODEL$ = "4" THEN TS.TEMP1$ = "IBM 4684 "
IF GMODEL$ = "5" THEN TS.TEMP1$ = "IBM SP700"
TS.TEMP1$ = "MODELO   :"+TS.TEMP1$
TS.TEMP2$ = LEFT$(TS.TEMP1$+STRING$(37," "),37)
CALL U.IMPRIME(TS.TEMP2$,2100H)

TS.TEMP1$ = STRING$(37,"-")
CALL U.IMPRIME(TS.TEMP1$,2100H)

END SUB 

Sub Gr.Init.Trx Public
      Dim  TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
      TS.IO.MOTORKEY = 73   
      TS.IO.COUNT = 10                                                      ! FORCE VOID TRANSACTION
      CALL TSETEC02                                                         ! CALL VOID TRANSACTION RTINE
      TS.NS.VOID = -1                                                            ! 
      Call TSCSEC10                                                              ! Genera Anulacion Total
      Call Dim.SL.STR(GC.SL.SIZE + 10)																					 ! 
End Sub

Function  Captura.Autorizacion Public
Integer*1 XI%, X.pasa%, Ui%
String    Xclave$, Umsg1$, Umsg2$, Udata$, Captura.Autorizacion
Integer*4 X.GETLONG

TS.ER.RETURN = -1
!Call VISOR.AND.BORRAR(TS.SDESC$(32))
Call TSHIECET
Call visores4690(1, "INGRESE SECUENCIA", "AUTORIZ. SUPERVISOR", 0, "L")
Captura.Autorizacion = "E"

UNLOCKDEV 32, 6, PRIORITY            ! set input state to enter/clear
Call TSCSECRK                        ! read the keyboard
LOCKDEV 32                           ! lock it
If ((TS.IO.KEYS(10) = 79) AND (TS.IO.MOTORKEY = 80)) THEN BEGIN      ! ALLOW FOR OVERRIDE HERE
   If TS.IO.DATA$(10) <> "" Then Begin
      Captura.Autorizacion = TS.IO.DATA$(10)
   EndIf
EndIf
Asic.Llave% = Val(MID$(TS.IO.HDR$, 11, 1))
End Function

Sub Entrada.Autorizacion Public
Integer*1 XI%, X.pasa%
String    Xclave$

Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
Asic.Llave% = 0
TS.TEMP2I4  = 0

TS.TEMP2I4  = -1
Exit Sub 

INGRESO.CLAVE.AUTOR:
XClave$ = Captura.Autorizacion
If (XClave$ = " " OR XClave$ = "E") Then Begin    ! Proceso cancelado
   X.pasa% = X.pasa% + 1                          ! Intentos fallidos
   If X.pasa% > 1 Then Begin                      ! Si mas de una vez
      TS.TEMP2I4 = 0                              ! Falla del proceso
      Exit Sub                                    ! Sale de la rutina
   EndIf
   GoTo INGRESO.CLAVE.AUTOR                       ! Vuelve a capturar el peso
EndIf

If Asic.Llave% <> 1 Then Begin                              ! No hay llave puesta
   Call Visor.And.Borrar("NECESITA LLAVE DEL  SUPERVISOR")
   TS.TEMP2I4 = 0                                           ! Falla del proceso
   Exit Sub
EndIf

For XI% = 1 TO 20                                           ! Valida si Aut. OK
   If TO.OVRIDNUM(XI%) = Val(XCLAVE$) Then Begin            ! Si autorizacion correcta
      TS.TEMP2I4 = -1                                       ! Proceso exitoso
      TS.TEMP6$  = XCLAVE$                                  ! Retorna clave utilizada
   EndIf
Next XI%

If TS.TEMP2I4 <> -1 Then Begin
   Call VISOR.AND.BORRAR("REVISE NUMERO DE    AUTORIZACION")
   TS.TEMP2I4 = 0                                           ! Falla del proceso
EndIf
End Sub

Sub SALVAR.SECUENCIA Public                    ! Salvar secuencias de tecleo
Integer*2 XI%

Dim GR.IO.DATA$(10)
Dim GR.IO.KEYS(10)

For XI% = 1 TO 10
   GR.IO.DATA$(XI%) = TS.IO.DATA$(XI%)
   GR.IO.KEYS(XI%)  = TS.IO.KEYS(XI%)
Next XI%
GR.IO.MOTORKEY = TS.IO.MOTORKEY
End Sub

Sub RESTAURAR.SECUENCIA Public
Integer*2 XI%

For XI% = 1 TO 10
   TS.IO.DATA$(XI%) = GR.IO.DATA$(XI%)
   TS.IO.KEYS(XI%)  = GR.IO.KEYS(XI%)
Next XI%
TS.IO.MOTORKEY = GR.IO.MOTORKEY
End Sub

Sub GrAnulacion.Total Public
    Exit Sub 
    If TS.IO.STATE <> 10 Then Exit Sub 																			! Enter/Data state
    Call Visor.And.Borrar("ANUL TOTAL TRX      " +                         \!
                           Str$(TS.TOTALS(0,0,0))+" /Borrar")               !
    Call Entrada.Autorizacion																								! Control autorizacion / llave
    If TS.TEMP2I4 = -1 Then Begin																						! Proceso OK
      TS.BD.DSPPARM = 0                                                     ! GET BAL DUE FOR VOID TRX
      CALL TSBDEC01                                                         ! GET THE CURRENT BALANCE DUE
      TS.IO.COUNT = 10                                                      ! FORCE VOID TRANSACTION
      CALL TSETEC02                                                         ! CALL VOID TRANSACTION RTINE
      TS.IO.MOTORKEY = 73																										!
    EndIf Else Begin																												!
   	  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)															! Init secuencias
   	  TS.IO.MOTORKEY = 73 																									! Tecla borrar
    EndIf
End Sub 

!------------------------------------------------------------------------------
! Imprime un buffer binario mayor de 240 bytes en el CR
! se debe usar cuando el buffer tiene un comando de mas de 240 bytes
! como codigos PDF417, graficos, etc.
!------------------------------------------------------------------------------
Sub imprimeData( s$ ) Public
   String    s$, p$, c$
   Integer*4 Xn%, Xb%, XI%, Xj%

   !comando continuacion
   c$ = Chr$(1BH) + Chr$(2EH)
   Xb% = 238
   Xn% = len(s$)
   Xi% = 1
   p$ = ""
   While Xi% <= Xn%
      If Mid$(s$, Xi%, Xb%) <> "" Then \
         p$ = p$ + Mid$(s$, Xi%, Xb%)
      Wait; 200
      Write #34; p$
      p$ = c$
      Xi% = Xi% + Xb%
   Wend

End Sub 

!------------------------------------------------------------------------------
! Imprime un codigo QR conteniendo la cadena s$
!    #select page mode \x1b\x4c
!    #select Scale of QR code.
!    Note 0 or 1 will work \x1d\x5f\x00
!    #left margin = 0 \x1b\x24\x00\x00
!    #Set Vertical position - down 180 = x00b4 \x1d\x24\x00\xb4
!    #setup size of page \x1b\x58\x00\x00\x00\x3c\x01\x90\x00\xcc
!    #QRCODE Left side \x1d\x4f\x04\x00\x1a data /x00
!    #set up position for QRcode Right side - Move horizontal position over 200 dots
!    \x1b\x24\x00\xc8
!    #QRCODE Right side \x1d\x4f\x04\x00\x1a data /x00
!    #Print Page \x1b\x0c
!    #select Standard Mode \x1b\x4f
!------------------------------------------------------------------------------
Sub imprimeQR( Xdata$ ) Public
   String Xdata$, xq$

   !escala 0 -> 3 puntos por pixel, 5 es el valor por defecto
   !write #34; chr$(1DH) + chr$(5FH) + chr$(5)

   xq$ = Chr$(1DH) + Chr$(4FH) + Chr$(0) + Chr$(1) + Chr$(0) + \
         Xdata$ + Chr$(0)

   Call imprimeData(xq$)
   
End Sub

Function TrimRight$( xv$ ) Public
 String TrimRight$, xv$
 Integer*2 xk%
 TrimRight$ = ""
 xk% = Len(xv$)
 While xk% > 0
  If mid$(xv$, xk%, 1) = " " Then Begin
     xk% = xk% - 1
  EndIf Else Begin
     TrimRight$ = Left$( xv$, xk% )
     xk% = 0
  EndIf 
 Wend
End Function

Function AllTrim$( xv$ ) Public
 String AllTrim$, xv$
 Integer*4 xk%
 AllTrim$ = ""
 xk% = Len(xv$)
 While xk% > 0
  If mid$(xv$, xk%, 1) = " " Then Begin
     xk% = xk% - 1
  EndIf Else Begin
     AllTrim$ = Left$( xv$, xk% )
     xk% = 0
  EndIf 
 Wend
End Function

