!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GENERICA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Marzo 15 de 2.004                *
!Observaciones : Rutinas Genericas Aplicaciones   *
!**************************************************
! 

%ENVIRON T

Integer*4 Global Asc.Pay.Impr%
String    Global HORA.MUNDIAL$
Integer*4 Global Asc.Save.Data(1)
Integer*2 Global USER.SAVE(1) 				! Save area for Subroutine calls
String    Global USER.SAVE$(1)			  ! Save area for Subroutine calls

%Include EAMTOPTS.J86	                ! Definicion Terminal Options
%INCLUDE EAMTSWKG.J86									! Definicion de variables
%INCLUDE EAMTRANS.J86									! Definicion de variables
%INCLUDE EAMP4VAJ.J86        				  ! 4610 Printer
%INCLUDE EAMP4VGJ.J86                 ! 4610 Variable definitions
%INCLUDE EAMSOPTS.J86				          ! Definicion de Store Options    

%Include EAMTSXHC.J86                 !

Sub TSDSEC01 External					        ! Rutina display
End Sub

Sub TSPREC01 External					        ! Rutina de Impresion
End Sub

Sub TSTPEC01 External                 ! Grabacion del String 
End Sub 

Sub TSCSEC08 External
End Sub 

Sub TSHIECET External 
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

Sub JAVA.AUDITORIA(X.ENVIA$, X.LLEGA$,X.SALE$,X.RTA$) Public
String X.ENVIA$, X.LLEGA$, X.FILE$, X.LEC$, X.FINR$, X.REG$, X.SALE$, X.RTA$, X.BUFF$
Integer*4 X.Len%
			TS.ER.RETURN = -1
			X.FILE$ = "R::ADX_UDT1:TM" + Left$(DATE$,6) + "." + Right$("000"+Str$(SL.HD.TERMINAL),3)
			Open X.FILE$ AS 56 Append
			If TS.ER.RETURN <> -1 Then Begin    ! Si no existe
			   TS.ER.RETURN = -1
				 CREATE X.FILE$ AS 56
         If TS.ER.RETURN <> -1 Then Begin 
            Call VISOR.AND.BORRAR("ERROR EN CREACION DE AUDITORIA ")
            Exit Sub 
         EndIf 
			EndIf 
			X.Finr$ = Chr$(13) + Chr$(10)
			X.BUFF$ = "["+X.SALE$+"]"+"MSG:"+X.ENVIA$
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro
			X.BUFF$ = "["+X.RTA$+"]"+"RTA:"+X.LLEGA$                              !
			X.Len% = Len(X.BUFF$)						  				  								          ! Toma longitud del registro
			X.Lec$ = "C"+Str$(X.len%)+" C2"								  						          ! Arma estructura de grabacion
			Write form X.Lec$; #56 ; x.buff$, X.Finr$            					        ! Graba registro      
			Close 56
End Sub 

Function Armar.Trama.Msg(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$) Public			! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
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

!Xseg$ = Right$(Time$,2)																							! Segundos de la trx 
!HORA.MUNDIAL$ = UnPack$(SL.HD.DATETIME$) + Xseg$                     ! AAMMDDHHMMSS

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


Function Rutina.Java(Java.CLASS$, Java.METHOD$, Java.MESSAGE$) Public 			! Inicializacion Clases de Java
String Java.METHOD$, Java.REQUEST$, Java.EXCEPTION$, Java.CLASS$,          \! Def. Variables Tmp
       Java.RETURNVALUE$, Java.MESSAGE$, Rutina.Java            					  !

Java.REQUEST$     = "C$"										! Init de variables
Java.EXCEPTION$   = ""											! para la ejecucion
Java.RETURNVALUE$ = ""											! del modulo
Java.EXCEPTION$   = "" 											! Init variable
Call Javacall.Initialize.Request(Java.CLASS$,Java.METHOD$,Java.REQUEST$)  				! Inicializacion de clases
Call Javacall.AddParameter.String(Java.REQUEST$,Java.MESSAGE$)						! Adicion de parametros
Call Javacall.InvokeMethod.ReturnString(Java.REQUEST$,Java.RETURNVALUE$,                               \! Ejecucion de la clase
        				Java.EXCEPTION$)						!

If LEN(Java.EXCEPTION$) > 0 THEN BEGIN									! Si hay Exception
   Write #34 ;"Error en el envio" + Chr$(10)
   Write #34 ;Left$(Java.EXCEPTION$,38) + Chr$(10)
   Java.RETURNVALUE$ = ""										! Returna Nulo
EndIF													!

Rutina.Java = Java.RETURNVALUE$										! Retorna Respuesta
   
End Function
!--- Fin invocacion rutinas de Java

Function Valida.Rta(Asc.Lcl.Tmp2$) Public			  		! Validacion De La Respuesta
String  X.Rta$(1), X.Err$, Errfx$, Asc.Lcl.Tmp2$, Valida.Rta
Dim X.Rta$(20)																			! Vector De Respuesta 

X.Rta$(01) = Mid$(Asc.Lcl.Tmp2$,001,02)							! Codigo De Aplicacion
X.Rta$(02) = Mid$(Asc.Lcl.Tmp2$,003,02)							! Codigo De Funcion
X.Rta$(03) = Mid$(Asc.Lcl.Tmp2$,005,03)							! Longitud Respuesta  
X.Rta$(04) = Mid$(Asc.Lcl.Tmp2$,008,01)							! Rta Appl Mngr  
X.Rta$(05) = Mid$(Asc.Lcl.Tmp2$,008,02)							! Rta De La Trx       
X.Rta$(06) = Mid$(Asc.Lcl.Tmp2$,012,02)							! Estado Del Appl Mngr 

If (X.Rta$(05) = "20") Or (Left$(X.Rta$(05),1) = "1")  Then Begin     ! Si Operacion Exitosa
   Valida.Rta = "00"
Endif Else Begin						                                          ! Si Se Presenta Error 
   Call U.Imprime("----  Error de Operacion  ----",2100H)			        ! Msg Error En Sj
   X.Err$ = "ERROR DESCONOCIDO "+X.Rta$(05)						                ! Init Variable
   Call U.Imprime("Error "+X.Rta$(05),2100H)				                  ! Msg Error En Sj
   If Left$(X.Rta$(05),1) = "1" Then X.Err$ = "ERROR DESPUES DE CONEXION"			! Reporta Msg De Error Al 
   If Left$(X.Rta$(05),1) = "2" Then X.Err$ = "CONEXION NO ESTABLECIDA  "			! Rollo De Auditoría
   If Left$(X.Rta$(05),1) = "3" Then X.Err$ = "SIN RESPUESTA EN SERVIDOR"			!
   If Left$(X.Rta$(05),1) = "K" Then X.Err$ = "ABANDONO DE OPERADOR     "			!
   If Left$(X.Rta$(05),1) = "T" Then X.Err$ = "TERMINAL NO INICIALIZADA "			!
   If Left$(X.Rta$(05),1) = "G" Then X.Err$ = "FUNCION NO DEFINIDA APPL "			!
   If Left$(X.Rta$(05),1) = "O" Then X.Err$ = "MOVIMIENTO NO ENCONTRADO "
   If Left$(X.Rta$(05),1) = "C" Then X.Err$ = "TIME OUT OPERACION       "
   Valida.Rta = "99"
   Call TSHIECET
   Call VISOR.AND.BORRAR(Left$(X.ERR$,20)+Mid$(X.ERR$,21,20))   
   Call U.Imprime(X.Err$,2100H)					                        !
   Call U.Imprime("--------------------------------",2100H)		    		!
Endif
End Function 


!-- Libreria para invocacion servicio Java
!-- Recibe como parametros
!-- Xbuffer$ el dato que se va a validar
!-- Xapp$ numero de aplicacion
!-- Xfunc$ numero de funcion 

Function VALIDA.DATO(Xbuffer$,Xapp$, Xfunc$) Public
String  xbuffer$, xtemp4$, xtrama$, xapp$, Xfunc$, valida.dato
String  Xsnd$, Xfin$
 Asc.Pay.Impr% = 1                                                          ! para tomar numero actual de trx (definir tipo integer*4  global)
 XTemp4$ = Armar.Trama.Msg(Xapp$,Xfunc$,Xbuffer$,"00","0001","123456")      ! Armar trama MSG
 Xsnd$ = DATE$ +":"+ Time$                                                  ! Fecha y hora del requerimiento
 XTrama$ = Rutina.Java("com.appl.ApplKernel","threader", XTemp4$)           ! Ejecuta Requerimiento
 Xfin$ = DATE$ +":"+ Time$                                                  ! Fecha y hora fin del requerimiento
 Call JAVA.AUDITORIA(Xtemp4$,Xtrama$, Xsnd$, Xfin$)                         ! Rastreo movimiento
 XTEMP4$ = Valida.Rta(XTrama$)                                              ! Valida rta entregada
 
 If Xtemp4$ = "00" Then Begin                                               ! Proceso satisfactorio
    If Mid$(XTrama$,12,2) <> "00" Then Begin                                ! Con Error de ejecucion
       Call VISOR.AND.BORRAR(Mid$(XTrama$,14,40))                           ! Presenta mensaje de Error
       TS.TEMP1I1 = 0                                                       ! Control proceso
       Exit Function                                                        ! Sale del proceso
    EndIf 
    valida.dato = Xtrama$                                                   ! trama de respuesta 
    TS.TEMP1I1 = -1                                                         ! proceso exitoso
    Exit Function 
 EndIf Else Begin
    TS.TEMP1I1 = 0
    Exit Function  
 EndIf
 
End Function  

