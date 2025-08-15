!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : ASCLBCSU.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Libreria para CheckOut Support   *
!**************************************************

%INCLUDE EPAYCSVA.011 

Sub JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) External
  String ClassName
  String MethodName
  String TheRequest
End Sub

Sub JavaCall.AddParameter.String(TheRequest,TheParameter) External
  String TheRequest
  String TheParameter
End Sub

Sub JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) External
  String TheRequest
  String ReturnValue
  String Exception
End Sub

Function Rutina.Java(Java.CLASS$, Java.METHOD$, Java.MESSAGE$) Public 					! Inicializacion Clases de Java
String Java.METHOD$, Java.REQUEST$, Java.EXCEPTION$, Java.CLASS$,             			       \! Def. Variables Tmp
       Java.RETURNVALUE$, Java.MESSAGE$, Rutina.Java            					!

Java.REQUEST$     = "C$"										! Init de variables
Java.EXCEPTION$   = ""											! para la ejecucion
Java.RETURNVALUE$ = ""											! del modulo
Java.EXCEPTION$   = "" 											! Init variable
Call Javacall.Initialize.Request(Java.CLASS$,Java.METHOD$,Java.REQUEST$)  				! Inicializacion de clases
Call Javacall.AddParameter.String(Java.REQUEST$,Java.MESSAGE$)						! Adicion de parametros
Call Javacall.InvokeMethod.ReturnString(Java.REQUEST$,Java.RETURNVALUE$,                               \! Ejecucion de la clase
        				Java.EXCEPTION$)						!

If LEN(Java.EXCEPTION$) > 0 Then Begin									! Si hay Exception
   Java.RETURNVALUE$ = ""										
EndIf

Rutina.Java = Java.RETURNVALUE$										! Retorna Respuesta
   
End Function
!--- Fin invocacion rutinas de Java

! Parametros de la funcion
!U.X1$ --> codigo de la aplicacion
!U.X2$ --> codigo de la funcion
!U.X3$ --> trama del mensaje
!U.X4$ --> Estado del requerimiento
!U.X5$ --> Codigo de la cadena
!U.X6$ --> Factura de venta
!U.X7$ --> numero almacen
!U.X8$ --> numero de la terminal
!U.X9$ --> cajero
!U.XA$ --> numero de la trx
!U.XB$ --> Fecha y hora de la trx
         
Function Armar.Trama.Msg(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$,U.X7$,U.X8$,U.X9$, U.XA$, U.XB$)             ! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$, HORA.MUNDIAL$, U.X7$,U.X8$,U.X9$, U.XA$, U.XB$

U.X1$ = Right$("00"+U.X1$,2)        							                    ! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							                    ! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							                    ! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							                    ! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							                    ! Factura de venta
HORA.MUNDIAL$ = Right$("000000000000"+U.XB$,12)                       ! Ajuste fecha y hora
U.Tmp1$ = U.X5$ +                                      				       \! Codigo de la cadena
          Right$("0000"+U.X7$,4)                      +  	           \! Numero del almacen
          Right$("000000"+Str$(Val(U.X8$)),6)         + 			       \! Numero de terminal
          Right$("0000000000"+Str$(Val(U.X9$)),10)    +              \! Numero de operador
          U.X6$                                       +              \! Factura fiscal                     
          Right$("000000"+Str$(Val(U.XA$)),6)         + 			       \! Numero de transaccion
          HORA.MUNDIAL$				                        +              \! Fecha y Hora de la operacion
          U.X4$ + 								                                   \! Estado del requerimiento
          U.X3$                                                       ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Armar.Trama.Msg = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					          ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Function Asic.Getunpk(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk
  Asc.Tmp.Apun% = Match(":",X.B$,X.J%) 				                                    ! Busca Separador De Campo
  Asic.Getunpk  = Unpack$(Mid$(X.B$,X.J%,Asc.Tmp.Apun% - X.J%))                   ! Desempaqueta Campo
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function
