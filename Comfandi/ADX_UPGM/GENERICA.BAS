!************************************************** 
!Empresa       : Asic S.A.  Unidad De Retail      *
!Programa      : Generica.Bas                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 Ibm                   * 
!Fecha         : Marzo 15 De 2.004                *
!Observaciones : Rutinas Genericas Aplicaciones   *
!**************************************************
! Visores4690  : Esta Rutina Permite Administrar 
!                Los Visores De Cliente Y Operador 
!                De Una Forma Mas Eficiente

%Environ T


String    Global Asc.User.Save$(1)
String    Global Asc.Mod.Datetime$, HORA.MUNDIAL$
Integer   Global Sl.End
Integer*2 Global Asc.User.Save(1), TS11.OVRFLAG
Integer*1 Global Asic.Llave%, Asic.Device%
Integer*2 Global User.Save(1) 				! Save Area For Subroutine Calls
String    Global User.Save$(1)				! Save Area For Subroutine Calls
Integer*4 Global Asc.Tmp.Apun%        ! Apuntador Lectura Strings
Integer*4 Global Asc.Save.Data(1)
Integer*4 Global GR.IO.KEYS(1)
String    Global GR.IO.DATA$(1)
Integer*2 Global GR.IO.MOTORKEY
String    Global Gr.CTLCSC.Superv$(2)
Integer*1 GLOBAL UE.CTLCSC.ACTIVO%	! SI PROYECTO ACTIVO

%Include Adx_upgm:EAMTOPTS.J86	                        ! Terminal Options
%Include Adx_Upgm:Eamtswkg.J86				! Definicion De Variables
%Include Adx_Upgm:Eamtrans.J86				! Definicion De Variables
%Include Eamp4Vaj.J86        			        !4610 Printer Status
%Include Eamp4VGJ.J86        			        !4610 Printer Variables

%Include Eamp4exj.j86                                   ! Rutinas Impresora 4610

Sub Tsdsec01 External					        ! Rutina Display
End Sub

Sub Tsprec01 External					        ! Rutina De Impresion
End Sub

Sub Tshiecet External
End Sub

Sub TSTPEC01 EXTERNAL       ! add data entry string to summary log
End Sub

Sub TSCSEC08 External
End Sub 

Sub TSCSECRK EXTERNAL       				! READ KEYBOARD
End Sub

Sub Javacall.Initialize.Request(Classname,Methodname,Therequest) External
  String Classname
  String Methodname
  String Therequest
End Sub

Sub Javacall.Addparameter.String(Therequest,Theparameter) External
  String Therequest
  String Theparameter
End Sub

Sub Javacall.Invokemethod.Returnstring(Therequest,Returnvalue, Exception) External
  String Therequest
  String Returnvalue
  String Exception
End Sub


Sub Salvar.Variables Public
      Dim User.Save(12)
      User.Save(1) = Ts.Linetype
      User.Save(2) = Ts.Linedata
      User.Save(3) = Ts.Linedata2
      User.Save(4) = Ts.Linedata3
      User.Save(5) = Ts.Prt.Parm
      User.Save(6) = Ts.Prt.Opt
      User.Save(7) = Ts.Prt.Sjdi
      User.Save(8) = Ts.Savprt.Opt
      User.Save(9) = Ts.Xxmod
      User.Save(10) = Ts.Yymod
      User.Save(11) = Ts.Zmod
      User.Save(12) = Ts.Printprm
      Dim User.Save$(5)
      User.Save$(1) = Ts.Prtbuf$
      User.Save$(2) = Ts.Prdata$
      User.Save$(3) = Ts.Formcr$
      User.Save$(4) = Ts.Sjdata$
      User.Save$(5) = Ts.Savprt$
End Sub
 
Sub Restaura.Variables Public
      Ts.Linetype   = User.Save(1)
      Ts.Linedata   = User.Save(2)
      Ts.Linedata2  = User.Save(3)
      Ts.Linedata3  = User.Save(4)
      Ts.Prt.Parm   = User.Save(5)
      Ts.Prt.Opt    = User.Save(6)
      Ts.Prt.Sjdi   = User.Save(7)
      Ts.Savprt.Opt = User.Save(8)
      Ts.Xxmod      = User.Save(9)
      Ts.Yymod      = User.Save(10)
      Ts.Zmod       = User.Save(11)
      Ts.Printprm   = User.Save(12)
      Ts.Prtbuf$    = User.Save$(1)
      Ts.Prdata$    = User.Save$(2)
      Ts.Formcr$    = User.Save$(3)
      Ts.Sjdata$    = User.Save$(4)
      Ts.Savprt$    = User.Save$(5)
End Sub

Sub SALVAR.SECUENCIA Public                    ! Salvar secuencias de tecleo
Integer*2 XI%
  Dim GR.IO.DATA$(10)
  Dim GR.IO.KEYS(10)
  For XI% = 1 TO 10
      GR.IO.DATA$(XI%) = TS.IO.DATA$(XI%)
      GR.IO.KEYS(XI%) = TS.IO.KEYS(XI%)
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

Function Java.Msg.Encryption(Java.Class$, Java.Method$, Java.Message1$, Java.Message2$) External 	! Inicializacion Clases De Java
String Java.Method$, Java.Request$, Java.Exception$, Java.Class$,             			       \! Def. Variables Tmp
       Java.Returnvalue$, Java.Message1$, Java.Message2$,Java.Msg.Encryption           			!
End Function 

Function  Visores4690(U.D.Visor%, U.D.Linea1$, U.D.Linea2$,U.D.Tiempo%,U.D.Posicion$) Public		! Msg Display
! U.Visor%    : Visor Al Que Se Desea Desplegar El Mensaje 
!               1. Visor Del Operador 
!               2. Visor Del Cliente  
! U.Linea1$   : Primera Linea Del Mensaje 20 Caracteres
! U.Linea2$   : Segunda Linea Del Mensaje 20 Caracteres
! U.Tiempo%   : Tiempo De Espera Del Msg En Milisegundos
! U.Posicion% : Fomato De Presentacion L Izquierda, R Derecha C Centrado

String U.D.Linea1$, U.D.Linea2$, U.D.Posicion$								! Variables De
Integer*2 U.D.Visor%, U.D.Tiempo%, U.T.Visor%, Ud.Pos%							! Trabajo
String U.D.Linea1$, U.D.Linea2$										!
  
  If Ucase$(U.D.Posicion$) = "L" Then Begin 								! Justificacion A La Izquierda
     U.D.Linea1$ = Left$(U.D.Linea1$ + String$(20," "),20)
     U.D.Linea2$ = Left$(U.D.Linea2$ + String$(20," "),20)
  Endif  
  If Ucase$(U.D.Posicion$) = "R" Then Begin 								! Justificacion A La Derecha
     U.D.Linea1$ = Right$(String$(20," ") + U.D.Linea1$,20)
     U.D.Linea2$ = Right$(String$(20," ") + U.D.Linea2$,20)
  Endif  
  If Ucase$(U.D.Posicion$) = "C" Then Begin 								! Justificacion Centrado
     Ud.Pos% = 20 - Len(U.D.Linea1$)
     Ud.Pos% = Ud.Pos% / 2
     U.D.Linea1$ = String$(Ud.Pos%," ")+U.D.Linea1$
     Ud.Pos% = 20 - Len(U.D.Linea2$)
     Ud.Pos% = Ud.Pos% / 2
     U.D.Linea2$ = String$(Ud.Pos%," ")+U.D.Linea2$
  Endif  
  If U.D.Visor% = 1 Then U.D.Visor% = 30 Else U.D.Visor% = 65
  Locate #U.D.Visor%;1,1,Off										! Posiciona Cursor
  Write Form "2C20";#U.D.Visor%;U.D.Linea1$, U.D.Linea2$						! Mensajes
  Wait ; U.D.Tiempo%											! 
End Function												!
!--- Fin Despliege De Mensajes A Los Visores 

Function Asic.Datos$(Umsg1$,Umsg2$) Public     								! Captura De Datos
String Umsg1$, Umsg2$, A.Dat$, Ab$, Aq$								        ! Definicion De Variables
String Asic.Datos$, Adata$			!
   Asic.Llave% = 0
   Call Visores4690(1,Umsg1$,Umsg2$,0,"L")						! Msg Al Operador
   Unlockdev 32,7                                					! Desbl Teclado Y Pasa
   Wait 32;100                                   					! Al Estado Acct No
   Read #32;Adata$,Ab$,Ab$,Ab$,               					       \! Lectura Teclado Y Status
            Ab$,Ab$,Ab$,Ab$,Ab$,Ab$,Aq$              			  		! Teclado Con 10 Datos
   If Mid$(Adata$,11,1)  = "1" Then Asic.Llave% = -1					! Llave Puesta
   If Mid$(Adata$,13,1)  = "I" Then Asic.Datos$ = "E"					! Presiono Borrar
   If Mid$(Adata$,14,1) <> " " Then               				       \! Si Hay Errror
      Call Visores4690(1,"Dato No Valido","...",500,"C")			        ! Msg Al Operador
      Asic.Datos$ = ""
   If Mid$(Adata$,13,1) = "P" Then Begin         					! Si Presiono Enter
      Asic.Datos$ = Right$(Aq$,Len(Aq$)-1)           				        ! Asigna Dato Capturado
      Asic.Device% = Val(Mid$(ADATA$,10,1))                   				! Tipo de dato ingresado
   EndIf 
End Function
!--- Fin Entrada De Datos

Sub Agrega.Boleta.Pangui(Linea$) Public						! Rutina Pangui Pantalla Tactil
End Sub 

!Parametros A Pasar Ue.String$ = String A Imprimir
!  	            Ue.Station = Destino De La Impresion
!                		4000H  - Print On Cr
!                		2000H  - Print On Sj
!                		1000H  - Print On Di
!                		0100H  - Number Of Linefeeds (Eg Here = 1)
Function U.Imprime(Ue.String$,Ue.Station) Public			! Rutina De Impresion
String Ue.String$, Lf$ 							! Variables Temporales
Integer*2 Ue.Station							!
    Lf$ = Chr$(0Dh)                                   			!
    If Prt4610.Enable Ne 0 Then Begin					! Impresoras 4610
       If Ue.String$ = "" Then Ue.String$ = Lf$				!
    Endif								!
    Call Salvar.Variables						! Salva Variables Impresion
    Ts.Linetype   = 29							! Genera Impresion Al
    Ts.Savprt$    = Ue.String$						! Dispositivo Requerido
    Ts.Savprt.Opt = Ue.Station						!
    Call Tsprec01							!
    Call Restaura.Variables						! Restaura Control Aplicacion
    If EP.ETO.PRINT.POST.TRANS NE 0 Then Begin 				! Generacion Tirilla Limpia
       Call Agrega.Boleta.Pangui(Ue.String$)                            ! Envia dato pantalla 
    EndIf 
End Function
!--- Fin Rutina De Impresion 

Function U.Explusadi Public										! Expulsa Papel En El 
    If Prt4610.Enable Ne 0 Then \ 									! Document Insert
       Write #35;Chr$(1Bh)+Chr$(6Dh)\									! Si Impresora 4610
    Else\												!
       Write Form "2C1 A0"; #35; Chr$(27), Chr$(12)             					! Si Impresora Mod. 4
End Function												!
!--- Fin Rutina Expulsion Papel Di				

Function U.Cortacr Public										! Corta Papel En 
 String Lf$                                                     					! Cr
    Lf$ = Chr$(0Dh)                                             					!
    If Prt4610.Enable Ne 0 Then Begin									! Impresoras 4610
     If EP.ETO.PRINT.POST.TRANS NE 0 Then  				                               \! Generacion Tirilla Limpia
        Call CR.BUFFER.GUILLOTINE Else                                                                 \!
        Write #34; Chr$(1Bh)+Chr$(6Dh)+Lf$+Lf$                  					!
    Endif Else \											! Impresoras Matriz
       Write Form "2C1 A2"; #34; Chr$(27), Chr$(80)	        					!
End Function
!--- Fin Rutina Corte De Papel

Function Rutina.Java(Java.Class$, Java.Method$, Java.Message$) Public 					! Inicializacion Clases De Java
String Java.Method$, Java.Request$, Java.Exception$, Java.Class$,             			       \! Def. Variables Tmp
       Java.Returnvalue$, Java.Message$, Rutina.Java            					!

Java.Request$     = "C$"										! Init De Variables
Java.Exception$   = ""											! Para La Ejecucion
Java.Returnvalue$ = ""											! Del Modulo
Java.Exception$   = "" 											! Init Variable
Call Javacall.Initialize.Request(Java.Class$,Java.Method$,Java.Request$)  				! Inicializacion De Clases
Call Javacall.Addparameter.String(Java.Request$,Java.Message$)						! Adicion De Parametros
Call Javacall.Invokemethod.Returnstring(Java.Request$,Java.Returnvalue$,                               \! Ejecucion De La Clase
        				Java.Exception$)						!

If Len(Java.Exception$) > 0 Then Begin									! Si Hay Exception
   Call U.Imprime("Error En El Envio",4100H)								! Imprime Error
   Call U.Imprime(Left$(Java.Exception$,38),4100H)							! En Sj
   Java.Returnvalue$ = ""										! Returna Nulo
Endif Else Begin											! Ejecucion Ok

!   Call U.Imprime("Respuesta Entregada",4100H)								! Imprime Error
!   Call U.Imprime(Left$(Java.Returnvalue$,38),4100H)							! En Sj

Endif													!

Rutina.Java = Java.Returnvalue$										! Retorna Respuesta
   
End Function
!--- Fin Invocacion Rutinas De Java

Function Armar.Trama.Msg(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$) Public			! Armar Trama Mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg, U.Tmp1$, U.Tmp2$			! Definicion Variables
String U.Tmp3$, U.X5$

U.X1$ = Right$("00"+U.X1$,2)        							! Codigo De Aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							! Codigo De Funcion
U.X4$ = Right$("00"+U.X4$,2)        							! Estado Del Requerimiento
U.X5$ = Right$("0000"+U.X5$,4)        							! Codigo De La Cadena

Asc.Mod.Datetime$ = Date$ + Time$

U.Tmp1$ = Right$("000000"+Str$(Sl.Hd.Terminal),6)     + 			       \! Numero De Terminal
          Right$("000000"+Str$(Sl.Hd.Transnum+1),6)   + 			       \! Numero De Transaccion
          Date$ + Time$				      +				       \! Fecha Y Hora De La Operacion
          U.X4$ + 								       \! Estado Del Requerimiento
          U.X3$ + 								       \! Trama Del Mensaje
          U.X5$ +                                      				       \! Codigo De La Cadena
          Right$("0000"+Ts.Store$,4)                    	                        ! Numero Del Almacen
          
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						! Longitud De La Trama

Armar.Trama.Msg = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					! Arma Trama Del Mensaje

End Function
!--- Fin De La Rutina Armada De Trama De Mensajes

Sub Traduce.Error(Errvalue%,Errvalue$) Public
  Integer*4 Errvalue%
  String Errvalue$
  Integer*4 Hx%,Sx%,The.Sum%,S%
  String Errfx$,Z$, traduce.error
  Errvalue$ = ""
  Hx%=Errvalue%
  Errfx$=""
  For S%=28 To 0 Step -4
  Sx%=Shift(Hx%,S%)
  The.Sum%=Sx%And 000Fh
  If The.Sum%>9 Then The.Sum%=The.Sum%+55 \
  Else The.Sum%=The.Sum%+48
  Z$=Chr$(The.Sum%)
  Errfx$=Errfx$+Z$
  Next S%
  Errvalue$ =Errfx$
  TS.TEMP1$ = ERRVALUE$
End Sub

Function Grabacion.String.Usuario2(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$
     Sl.End = Sl.End + 1 
     Sl.Str$(Sl.End) = Pack$("99")              +				       \! En String Reservado
		":"+Pack$(X.Clave$)             +		                       \! Numero Del Proyecto
		":"+X.Datos$ + ":"                                          ! Datos Para Almacenar

End Function
!--- Fin Grabacion De Datos


!--- Se Modifica La Rutina Para Que Grabe El String Como Un 99 Y No Como 11
!--- Cambio Realizado El 29 De Oct. 2004 Por Oscar Valencia

Function Grabacion.String.Usuario(X.Clave$,X.Datos$) Public
String X.Clave$, X.Datos$, XOldData$
     XOldData$ = TS.USERDATA$
     TS.TEMP1I1   = 99																					! Indica String de usuario
     TS.USERDATA$ = Pack$(X.Clave$) + ":"       +	       \! Numero Del Proyecto
		    X.Datos$+":" 			        ! Datos Almacenados
     Call TSTPEC01                                              ! Grabacion del String 
     TS.USERDATA$ = XoldData$
     
End Function
!--- Fin Grabacion De Datos

Function Valida.Rta(Asc.Lcl.Tmp2$) Public			  		! Validacion De La Respuesta
String  X.Rta$(1), X.Err$, Errfx$, Asc.Lcl.Tmp2$, Valida.Rta
Dim X.Rta$(20)										! Vector De Respuesta 

X.Rta$(01) = Mid$(Asc.Lcl.Tmp2$,001,02)							! Codigo De Aplicacion
X.Rta$(02) = Mid$(Asc.Lcl.Tmp2$,003,02)							! Codigo De Funcion
X.Rta$(03) = Mid$(Asc.Lcl.Tmp2$,005,03)							! Longitud Respuesta  
X.Rta$(04) = Mid$(Asc.Lcl.Tmp2$,008,01)							! Rta Appl Mngr  
X.Rta$(05) = Mid$(Asc.Lcl.Tmp2$,009,01)							! Rta De La Trx       
X.Rta$(06) = Mid$(Asc.Lcl.Tmp2$,010,02)							! Estado Del Appl Mngr 
X.Rta$(07) = Mid$(Asc.Lcl.Tmp2$,012,06)							! Numero Autorizacion 
X.Rta$(08) = Mid$(Asc.Lcl.Tmp2$,018,02)							! Rta Del Hosts

!Call U.Imprime("Appl  "+X.Rta$(01),4100H)
!Call U.Imprime("Func  "+X.Rta$(02),4100H)
!Call U.Imprime("Long  "+X.Rta$(03),4100H)
!Call U.Imprime("Rtap  "+X.Rta$(04),4100H)
!Call U.Imprime("Rttr* "+X.Rta$(05),4100H)
!Call U.Imprime("Estap "+X.Rta$(06),4100H)
!Call U.Imprime("Autor "+X.Rta$(07),4100H)
!Call U.Imprime("Rthos "+X.Rta$(08),4100H)
    

If X.Rta$(05) = "0" Then Begin								      ! Si Operacion Exitosa
   Valida.Rta = X.Rta$(08)
Endif Else Begin						                        ! Si Se Presenta Error 
   Call U.Imprime("---- Error Comunicaciones ----",2100H)				! Msg Error En Sj
   X.Err$ = "Error Desconocido "+X.Rta$(05)							! Init Variable
   If X.Rta$(05) = "1" Then X.Err$ = "ERROR DESPUES DE CONEXION"			! Reporta Msg De Error Al 
   If X.Rta$(05) = "2" Then X.Err$ = "CONEXION NO ESTABLECIDA  "			! Rollo De Auditoría
   If X.Rta$(05) = "3" Then X.Err$ = "SIN RESPUESTA EN SERVIDOR"			!
   If X.Rta$(05) = "K" Then X.Err$ = "ABANDONO DE OPERADOR     "			!
   If X.Rta$(05) = "T" Then X.Err$ = "TERMINAL NO INICIALIZADA "			!
   If X.Rta$(05) = "G" Then X.Err$ = "FUNCION NO DEFINIDA APPL "			!
   If X.Rta$(05) = "O" Then X.Err$ = "MOVIMIENTO NO ENCONTRADO "
   If MATCH(X.Rta$(05),"123KTGO",1) <= 0 Then Begin							      ! Error Desconocido
      Call Traduce.Error(Errn,Errfx$)							!
      X.Err$ = "Error Comm No Conocido   "				!
      Call U.Imprime(X.Err$,2100H)							  !
      X.Err$ = ("ERROR="+TS.TS11WERR$+" COD="+TS.DISPERR$)	!
   Endif
   Valida.Rta = X.Rta$(05)
   Call U.Imprime(X.Err$,2100H)					!
   Call U.Imprime("--------------------------------",2100H)				!
Endif
End Function 

Function Status.Rta(X.Rta$) Public                                        ! Valida Rta Entregada Por Rutina Java
String X.Rta$ 
Integer*2 Status.Rta
   Status.Rta = -1																												! Supone Que Ok
   If X.Rta$ = "1" Then Status.Rta = 0
   If X.Rta$ = "2" Then Status.Rta = 0
   If X.Rta$ = "3" Then Status.Rta = 0
   If X.Rta$ = "K" Then Status.Rta = 0
   If X.Rta$ = "T" Then Status.Rta = 0
   If X.Rta$ = "Z" Then Status.Rta = 0
   
End Function 

Function Asic.Getunpk(ST.B$,ST.J%) Public
Integer*2 ST.J%, ST.K%
String    ST.B$, Asic.Getunpk
  Asc.Tmp.Apun% = Match(":",ST.B$,ST.J%) 			                     ! Busca Separador De Campo
  If Asc.Tmp.Apun% > 0 Then \
     Asic.Getunpk  = Unpack$(Mid$(ST.B$,ST.J%,Asc.Tmp.Apun% - ST.J%)) Else \         ! Desempaqueta Campo
     Asic.Getunpk  = ""
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk2(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk2
  Asc.Tmp.Apun% = Match(";",X.B$,X.J%) 				                                    ! Busca Separador De Campo
  Asic.Getunpk2 = Unpack$(Mid$(X.B$,X.J%,Asc.Tmp.Apun%-X.J%)) 	                  ! Desempaqueta Campo
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk3(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk3
  Asc.Tmp.Apun% = Match(";",X.B$,X.J%) 				                                    ! Busca Separador De Campo
  Asic.Getunpk3 =  Mid$(X.B$,X.J%,Asc.Tmp.Apun%-X.J%)       	                    ! Desempaqueta Campo
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Asic.Getunpk4(X.B$,X.J%) Public
Integer*2 X.J%, X.K%
String    X.B$, Asic.Getunpk4
  Asc.Tmp.Apun% = Match(":",X.B$,X.J%) 				                                    ! Busca Separador De Campo
  Asic.Getunpk4 = (Mid$(X.B$,X.J%,Asc.Tmp.Apun%-X.J%)) 	                          ! Desempaqueta Campo
  Asc.Tmp.Apun% = Asc.Tmp.Apun% + 1
End Function

Function Linea.Detalle(X.Ses%)   Public                  ! Imprime Detalle De La Operacion
String Linea.Detalle, X.Oper$, X.A$
Integer*1 X.Ses%

Ts.Er.Return = -1 

!Open "R::Eamopera" Keyed Recl 72 As X.Ses% Nowrite Nodel   !Read Operator File
!If Ts.Er.Return = -1 Then Begin 
!  Read Form "C5 C67";#X.Ses% Key Ts.Oper$; \
!	       X.A$, X.Oper$
!  If Ts.Er.Return = -1 Then \	     
!     X.Oper$ = Mid$(X.Oper$,28,10) Else \
!     X.Oper$ = "Cajero(A) "
!  Close X.Ses%   
!EndIf Else X.Oper$ = "Cajero(A)*"

If TS.INTRX Then \
   X.A$ = Right$("0000" +Str$(Sl.Hd.Transnum + 1),4) Else \
   X.A$ = Right$("0000" +Str$(Sl.Hd.Transnum),4)
   
X.Oper$ = Str$(Val(Unpack$(TS.OPER$)))   ! Codigo del operador
   
Linea.Detalle = Mid$(Date$,3,2)+"/"+Right$(Date$,2)+"/"+Left$(Date$,2)+ " "+\
                Left$(Time$,2)+":"+Mid$(Time$,3,2)+" "+\
                Right$("0000"+Ts.Store$,4)+" "+\
                Right$("   "+Ts.Terminal$,3)+" "+\
                X.A$ + " " + X.Oper$

End  Function

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


Sub VISOR.AND.BORRAR(X.MSG$) Public 
  STRING X.MSG$
  CALL SALVAR.DISPLAY
  TS.TEMP1$ = LEFT$(X.MSG$,20)
  TS.TEMP2$ = MID$(X.MSG$,21,20) 
  TS.LINETYPE = 12
  CALL TSCSEC08
  CALL RESTAURA.DISPLAY
End Sub

Function Armar.Trama.Msg2(U.X1$, U.X2$, U.X3$,U.X4$,U.X5$,U.X6$) Public			! Armar trama mensajeria
String U.X1$, U.X2$, U.X3$, U.X4$, Armar.Trama.Msg2, U.Tmp1$, U.Tmp2$			  ! Definicion Variables
String U.Tmp3$, U.X5$, U.X6$
Integer*4 XL.TRX%


XL.TRX% = SL.HD.TRANSNUM + 1

U.X1$ = Right$("00"+U.X1$,2)        							! Codigo de aplicacion
U.X2$ = Right$("00"+U.X2$,2)        							! Codigo de funcion
U.X4$ = Right$("00"+U.X4$,2)        							! Estado del requerimiento
U.X5$ = Right$("0000"+U.X5$,4)      							! Codigo de la cadena
U.X6$ = Right$("000000"+U.X6$,6)     							! Factura de venta
HORA.MUNDIAL$ = DATE$ + TIME$
U.Tmp1$ = U.X5$ +                                      				       \! Codigo de la cadena
          Right$("0000"+TS.STORE$,4)                  +  	           \! Numero del almacen
          Right$("000000"+Str$(SL.HD.TERMINAL),6)     + 			       \! Numero de terminal
          Unpack$(TS.OPER$)                           +              \! Numero de operador
          U.X6$                                       +              \! Factura fiscal                     
          Right$("000000"+Str$(XL.TRX%),6)            + 			       \! Numero de transaccion
          DATE$ + TIME$				                        +              \! Fecha y Hora de la operacion
          U.X4$ + 								                                   \! Estado del requerimiento
          U.X3$                                                       ! Trama del mensaje
U.Tmp2$ = Right$("000"+Str$(Len(U.Tmp1$)),3)						              ! Longitud de la trama
Armar.Trama.Msg2 = U.X1$ + U.X2$ + U.Tmp2$ + U.Tmp1$					          ! Arma trama del mensaje

End Function
!--- Fin de la rutina armada de trama de mensajes

Function Valida.Rta2(Asc.Lcl.Tmp2$) Public			  		! Validacion De La Respuesta
String  X.Rta$(1), X.Err$, Errfx$, Asc.Lcl.Tmp2$, Valida.Rta2
Dim X.Rta$(20)										! Vector De Respuesta 

X.Rta$(01) = Mid$(Asc.Lcl.Tmp2$,001,02)							! Codigo De Aplicacion
X.Rta$(02) = Mid$(Asc.Lcl.Tmp2$,003,02)							! Codigo De Funcion
X.Rta$(03) = Mid$(Asc.Lcl.Tmp2$,005,03)							! Longitud Respuesta  
X.Rta$(04) = Mid$(Asc.Lcl.Tmp2$,008,01)							! Rta Appl Mngr  
X.Rta$(05) = Mid$(Asc.Lcl.Tmp2$,009,01)							! Rta De La Trx       
X.Rta$(06) = Mid$(Asc.Lcl.Tmp2$,010,02)							! Estado Del Appl Mngr 
X.Rta$(07) = Mid$(Asc.Lcl.Tmp2$,012,02)							! Respuesta de la operacion 


If (X.Rta$(05) = "0")  Then Begin						        ! Si Operacion Exitosa
   Valida.Rta2 = X.Rta$(07)
Endif Else Begin						                                          ! Si Se Presenta Error 
   Call U.Imprime("---- Error Comunicaciones ----",2100H)				      ! Msg Error En Sj
   X.Err$ = "Error Desconocido "+X.Rta$(05)							              ! Init Variable
   Call U.Imprime("Error "+X.Rta$(05),2100H)				                  ! Msg Error En Sj
   If X.Rta$(05) = "1" Then X.Err$ = "ERROR DESPUES DE CONEXION"			! Reporta Msg De Error Al 
   If X.Rta$(05) = "2" Then X.Err$ = "CONEXION NO ESTABLECIDA  "			! Rollo De Auditoría
   If X.Rta$(05) = "3" Then X.Err$ = "SIN RESPUESTA EN SERVIDOR"			!
   If X.Rta$(05) = "K" Then X.Err$ = "ABANDONO DE OPERADOR     "			!
   If X.Rta$(05) = "T" Then X.Err$ = "TERMINAL NO INICIALIZADA "			!
   If X.Rta$(05) = "G" Then X.Err$ = "FUNCION NO DEFINIDA APPL "			!
   If X.Rta$(05) = "O" Then X.Err$ = "MOVIMIENTO NO ENCONTRADO "
   If MATCH(X.Rta$(05),"123KTGO",1) <= 0 Then Begin							        ! Error Desconocido
      Call Traduce.Error(Errn,Errfx$)							                    !
      X.Err$ = "Codigo de Error     No Definido... "                  !
      Call U.Imprime(X.Err$,2100H)							                      !
   Endif
   Valida.Rta2 = X.Rta$(04) + X.Rta$(05)
   Call TSHIECET
   Call Visores4690(1,Left$(X.ERR$,20),Mid$(X.ERR$,21,20),1500,"L")
   Call U.Imprime(X.Err$,2100H)					                              !
   Call U.Imprime("--------------------------------",2100H)				    !
Endif
End Function 

 Function NULLBLANK(TMP.CADENA$) Public
 String NULLBLANK, TMP.CADENA$, CADENA.FINAL$
 Integer*4 TMP.LON%, LONGITUD%, F%
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
    NULLBLANK = CADENA.FINAL$
 End Function 

Function Digito.Ean13(XDATO$) Public 
String XDATO$, Digito.Ean13
Integer*4 XDATO(2), XTOTAL, XH%, XT%, XI%, XJ%

DIM XDATO(12,1)
XDATO(01,0)=1:XDATO(02,0)=3:XDATO(03,0)=1:XDATO(04,0)=3
XDATO(05,0)=1:XDATO(06,0)=3:XDATO(07,0)=1:XDATO(08,0)=3
XDATO(09,0)=1:XDATO(10,0)=3:XDATO(11,0)=1:XDATO(12,0)=3
XTOTAL = 0
If Len(XDATO$) = 12 Then Begin 
  For XI% = 1 to 12
      XDATO(XI%,1) = Val(Mid$(XDATO$,XI%,1))
  Next XI%
  For XJ% = 1 to 12
      XTOTAL = XTOTAL + (XDATO(XJ%,1)* XDATO(XJ%,0))
  Next XJ%
  XH% = (10*INT(XTOTAL/10))+10
  XT% = XH% - XTOTAL
  IF XT% <= 9 THEN \ 
     Xdato$ = Xdato$ + RIGHT$("0"+STR$(XT%),1) \
  Else Xdato$ = Xdato$ + "0"
EndIf
Digito.Ean13 = Xdato$   
End Function 

Sub ADICIONAR.DATAENTRY(DATO1,DATO2,DATO3,DATO4,DATO5,DATO6) Public 
String DATO1,DATO2,DATO3,DATO4,DATO5,DATO6
Integer*2 XI%
   For XI% = 0 TO 10
     TS.IO.PREV.KEYS(XI%)  = TS.IO.KEYS(XI%)   ! SAVE KEYED KEYS
     TS.IO.PREV.DATA$(XI%) = TS.IO.DATA$(XI%)  ! SAVE KEYED DATA
   Next XI%
   Dim TS.IO.KEYS(10)                          ! CLEAR KEYED DATA
   Dim TS.IO.DATA$(10)                         ! CLEAR KEYED DATA
   TS.IO.KEYS(10) = 63                         ! Data entry key
   TS.IO.KEYS(4)  = 100                        ! No-sale key
   TS.IO.DATA$(2) = DATO1 		       ! Campo 1
   TS.IO.DATA$(3) = DATO2		       ! Campo 2
   TS.IO.DATA$(4) = DATO3		       ! Campo 3
   TS.IO.DATA$(5) = DATO4		       ! Campo 4
   TS.IO.DATA$(6) = DATO5		       ! Campo 5
   TS.IO.DATA$(7) = DATO6		       ! Campo 6
   TS.TEMP1I1 = 11                             ! Data entry
   Call TSTPEC01                               ! Process data entry
   For XI% = 0 TO 10
     TS.IO.KEYS(XI%)  = TS.IO.PREV.KEYS(XI%)   ! RESTORE KEYED DATA
     TS.IO.DATA$(XI%) = TS.IO.PREV.DATA$(XI%)  ! RESTORE KEYED DATA
   Next XI%
End Sub

Function  Captura.Autorizacion Public 
Integer*1 XI%, X.pasa%,  Ui%
String    Xclave$, Umsg1$, Umsg2$, Udata$, Captura.Autorizacion
Integer*4 X.GETLONG

 TS.ER.RETURN = -1 
 
 !Umsg1$ = Mid$(TS.SDESC$(32), 1,20)
 !Umsg2$ = Mid$(TS.SDESC$(32),21,20)
 
 Call VISOR.AND.BORRAR(TS.SDESC$(32))
 Call visores4690(1,"INGRESE SECUENCIA","AUTORIZ. SUPERVISOR",0,"L")
 Captura.Autorizacion = "E"
 
 UNLOCKDEV 32, 6, PRIORITY            ! set input state to enter/clear
 Call TSCSECRK                        ! read the keyboard
 LOCKDEV 32                           ! lock it
 If ((TS.IO.KEYS(10) = 79) AND (TS.IO.MOTORKEY = 80)) THEN BEGIN      ! ALLOW FOR OVERRIDE HERE
  If TS.IO.DATA$(10) <> "" Then Begin 
     Captura.Autorizacion = TS.IO.DATA$(10)
  EndIf 
 EndIf 
 Asic.Llave% = Val(MID$(TS.IO.HDR$,11,1))
End Function 

Sub Entrada.Autorizacion Public 
Integer*1 XI%, X.pasa%
String    Xclave$, Xclv$, Xusr$
       Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
       Asic.Llave% = 0 
       TS.TEMP2I4 =  0 
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
       If Asic.Llave% <> 1 Then Begin                    ! No hay llave puesta
          Call Visor.And.Borrar("NECESITA LLAVE DEL  SUPERVISOR")
          TS.TEMP2I4 = 0                                 ! Falla del proceso 
          Exit Sub 
       EndIf 
       If UE.CTLCSC.ACTIVO% = -1 Then Begin                 ! Modulo Claves supervisor activo
        For XI% = 1 TO 20                                   ! Valida si Aut. OK
           XCLV$ = XClave$                                  ! Clave capturada
           XUSR$ = Gr.CTLCSC.Superv$(XI%,0)                 ! Usuario
           XCLV$ = Java.Msg.Encryption("com.grpretail.utils.Encryption","encryption",XCLV$,XUSR$) ! Ejecuta encryption
           If Val(XCLV$) = Val(Gr.CTLCSC.Superv$(XI%,2)) Then Begin  ! Encontrado
              TS.TEMP2I4 = -1                               ! Falla del proceso 
              TS.TEMP6$ = XCLAVE$                           ! Retorna clave utilizada
           EndIf 
        Next XI%
       EndIf Else Begin 				   ! Modulo Claves Supervisor apagado
         For XI% = 1 TO 20                                 ! Valida si Aut. OK
             If TO.OVRIDNUM(XI%) = Val(XCLAVE$) Then Begin ! Si autorizacion correcta
              TS.TEMP2I4 = -1                              ! Falla del proceso 
              TS.TEMP6$ = XCLAVE$                          ! Retorna clave utilizada
           EndIf 
         Next XI%
       EndIf 
       If TS.TEMP2I4 <> -1 Then Begin  
          Call VISOR.AND.BORRAR("REVISE NUMERO DE    AUTORIZACION")
       EndIf 
End Sub 
