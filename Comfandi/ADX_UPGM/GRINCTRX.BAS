!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRINCTRX.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Carga de transacciones Tlog      *
!                Pago Movil                       *
!**************************************************
! Version 1.0 - 17/Mzo/2020
!-------------------------------------------------

%ENVIRON C						   																										! Ambiente de controlador


Integer*1 Global TS.ERROR, Nstr%																						! 
String    Global Ifile$, ERRFX$, Tmp$, Msg$,Ustr$(2), Registro$  						!
String    Global Sl.Str$(1)
Integer*4 Global NroStr%, NroTrx%, Nveces%

%Include EAMITEMR.J86			            																			! Variable EAMITEMR
%Include BASROUT.J86																												!

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  								! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Function TRADUCE.ERROR                                       								!
Integer*4 HX%, S%, SX%, SUM%																								!
String    Z$																																!
    HX% = ERRN                                                              ! Traduccion de los     
    ERRFX$=""                                                               ! codigos de error      
    FOR S% = 28 TO 0 STEP -4                                                ! del sistema operativo 
        SX% = SHIfT(HX%,S%)                                                 !                       
        SUM% = SX% AND 000FH                                                !                       
        If SUM% > 9 Then SUM% = SUM% + 55                                  \!                       
        Else SUM% = SUM% + 48                                               !                       
        Z$ = CHR$(SUM%)                                                     !                       
        ERRFX$ = ERRFX$ + Z$                                                !                       
    NEXT S%                                                                 !                       
End Function																																!

Sub Mensaje(Xmsg$)																													!
String XMSG$																																!
 If IFILE$ = "BACKGRND" Then Call ADXSERVE(0,26,1,XMSG$)                   \!
  Else Print XMSG$																													!
End Sub 																																		!

Sub CALCULO.HORA																														!
String Xh$, Xa$, X.Men$																											!
Integer*4 Xa%, Xb%																													!
Xh$ = Left$(TIME$,2)                                                        ! Toma hora y minutos               
Xa% = Val(Xh$)                                                              ! Toma Valor Numerico               
Xa% = Xa% + 1                                                               ! Calcula Proxima Hora de ejecucion 
If Xa% = 24 Then Xa% = 0                                                    !
Xa$ = (Right$("00"+Str$(Xa%),2)+"00")                                       ! Ajusta Formato
Xa$ = Left$(Xa$,2) + ":" + Right$(Xa$,2)                                    !
Xh$ = Mid$(TIME$,3,2)                                                       ! Toma Minutos                              
Xa% = val(Xh$)                                                              ! Convierte valor numerico                  
Xa% = 60 - Xa%                                                              ! Calcula resto de tiempo para proxima hora 
Xb% = (Xa% * 60) * 1000                                                     ! Calcula tiempo en tiempos de Relog        
X.Men$ = "Carga Trx PosMovil V1.0 "+Xa$									                    ! Valida cada hora
Call Mensaje(X.Men$)																											  !
If IFILE$ = "BACKGRND" Then Begin                                          \! En Background
   !Wait ; Xb%																																! Calculo proxima hora
EndIf Else Begin																														! Interactivo
   !wait ; 1500																															! cada 30 seg
EndIf																																				!
End Sub																																			!
!--- Fin calculo hora

Sub Carga.Parametros																												! Carga parametros 
String D1$,D2$,D3$
Dim Ustr$(300,2)
Nstr% = 0
TS.ERROR = -1																																!
Open "TF:TRXPAR" AS 44 																											! Archivo parametros
If TS.ERROR = 0 Then Begin																									! Si falla apertura
   MSG$ = "Error OE Parametros TF:TRXPAR"                                   !
   Call Mensaje(MSG$)																											  !
	 Stop 
EndIf
If End # 44 Then Fin.Parametros
While (1)																																		! Recorre archivo
 Read #44 ; D1$,D2$,D3$
 Nstr% = Nstr% + 1
 Ustr$(Nstr%,0) = D2$																												! Codigo proyecto
 Ustr$(Nstr%,1) = D3$																												! Parametros del proyecto
Wend 
Fin.Parametros:
 Close 44
End Sub 																																		! Fin Carga parametros

Sub Graba.String(Data$,Nro%)
String Data$
Integer*2 Nro%
NroStr% = NroStr% + 1 
Sl.Str$(NroStr%) = Data$
 
End Sub 

Sub S00																																			! Carga cabecera trx
String Buffer$, Ntx$
Ntx$ = Right$("0000"+str$(NroTrx%),4)
Buffer$ = "" + \
          Pack$("00")   + ":" +      \! Tipo de String 
          Pack$("0015") + ":" +      \! Numero de terminal
          Pack$(Ntx$) + ":" +      \! Numero de trx 
          Pack$("2004241031") + ":" +\! Fecha y hora de la trx
          Pack$("00")  + ":" +       \! tipo de trx
          Pack$("05")  +   ":" +     \! Numero de strings
          Pack$("4690") + ":" + \! Operador
          Pack$("1") + ":" + \! Password
          Pack$("4000")       + ":" + \! Gross +
          Pack$("00")         + ":" + \! Gross -
          Pack$("01")         + ":" + \! Transaction ring time
          Pack$("10")         + ":" + \! Tender time in seconds
          Pack$("00")         + ":" + \! Special sign-off time
          Pack$("15")         + ":" + \! Inactive time in seconds
          Pack$("0")  + ":"           ! Indicat 1
Call Graba.String(Buffer$,1)
Registro$ = Buffer$
End Sub 																																		!

Sub S01
String Buffer$
Buffer$ = Pack$("01") + ":" + \
          Pack$("73263") + ":" +\
          Pack$("4000")  + ":" +\
          Pack$("53")    + ":" +\
          Pack$("0") + ":" +\
          Pack$("52")   + ":" +\
          Pack$("0") + ":" +\
          Pack$("01") + ":" 
Call Graba.String(Buffer$,1)
Registro$ = Registro$ + Buffer$
End Sub 

Sub S05
String Buffer$
Buffer$ = Pack$("05") +   ":" +\ 
          Pack$("11") +   ":" +\
          Pack$("5000") + ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") +    ":" +\
          Pack$("00") + ":"
Call Graba.String(Buffer$,1)
Registro$ = Registro$ + Buffer$
End Sub 

Sub S07
String Buffer$
Buffer$ = Pack$("07") + ":" +  \ 
          Pack$("0") +   ":" +\
          Pack$("0") + ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") +    ":" +\
          Pack$("0") + ":" 
Call Graba.String(Buffer$,1)
Registro$ = Registro$ + Buffer$
End Sub 


Sub S09
String Buffer$
Buffer$ = Pack$("09") +   ":" +\ 
          Pack$("11") +   ":" +\
          Pack$("1000") + ":" 
Call Graba.String(Buffer$,1)
Registro$ = Registro$ + Buffer$
End Sub 

Sub Graba.Compra
String Reg$, LF$
  Print "Total String "+Str$(NroStr%)
  Write Matrix #41 ; Sl.Str$(1), NroStr%  
  Dim Sl.Str$(1000)
  NroStr% = 0

End Sub 

Sub Carga.Trx
String X.Men$
X.Men$ = "Buscando Movimientos, Espere... "							                    ! Valida cada hora
Call Mensaje(X.Men$)																											  !
!Wait ; 2000
Dim Sl.Str$(1000)
NroStr% = 0
Call S00																																		! Carga cabecera trx
Call S01
Call S05
Call S09
Call S07
Call Graba.Compra
End Sub 																																		!

Sub Init.Vta.Terminal
!"20:0005":"0001":"2003020710":"17":"":"32":""0D0A
String Buffer$, Ntx$

!Dim Sl.Str$(1000)
!NroStr% = 0
!Ntx$ = Right$("0000"+str$(NroTrx%),4)
!Buffer$ = "" + \
!          Pack$("20")   + ":" +      \! Tipo de String 
!          "0015:0001:2004241031:17::32::"
!Call Graba.String(Buffer$,1)
!Call Graba.Compra

Dim Sl.Str$(1000)
NroStr% = 0

!"20:0005":"0001":"2003020715":"11":"F42478":"31":"31393933":"":"":"":""0D0A

Buffer$ = "" + \
          Pack$("20")   + ":" +      \! Tipo de String 
          Pack$("0015") + ":"         ! Numero de terminal
          Call Graba.String(Buffer$,1)
          
Buffer$ = Pack$("0001") + ":"  ! Numero de trx 
Call Graba.String(Buffer$,1)

          
Buffer$ = Pack$("2004241031") + ":" ! Fecha y hora de la trx
Call Graba.String(Buffer$,1)


Buffer$ = Pack$("11") + ":" ! tipo de trx
Call Graba.String(Buffer$,1)
Buffer$ = ("4690")  + ":" ! Numero de strings
Call Graba.String(Buffer$,1)
Buffer$ = ("31") + ":" ! Operador
Call Graba.String(Buffer$,1)          
Buffer$ = Pack$("1234") + ":"  ! Clave Operador
Call Graba.String(Buffer$,1)

Buffer$ = Pack$("")
Call Graba.String(Buffer$,1)
Buffer$ = Pack$("")
Call Graba.String(Buffer$,1)
Buffer$ = Pack$("")
Call Graba.String(Buffer$,1)
Buffer$ = Pack$("")
Call Graba.String(Buffer$,1)



Call Graba.Compra


End Sub 

!---
!--- Bloque Principal
!---

On error GoTo Fail.Program																									! Control rutina de errores
IFILE$ = Command$                						 																! Como esta ejecutando
If Ucase$(IFILE$) = "VERSION" Then Begin 																		!
	 Print "Carga Transacciones PosMovil Ver 1.0 17/Mzo/2020 02:49 pm"	      !
	 Stop																																			!
EndIf 																																			!
Call Carga.Parametros
Nveces% = 0
NroTrx% = 1
TS.ERROR = -1
Create "ADX_UDT1:LOG.DAT" As 41
If TS.ERROR = 0 Then Begin
	 Print "Error Acceso LOG"
	 Stop 
EndIf
Call Calculo.Hora               						 																! Cada Hora Verifica si encuentra archivos para cargar
Call Init.Vta.Terminal
While Nveces% < 10                           																! Ciclo infinito
 Call Carga.Trx                 				     																! Carga transacciones
 NroTrx% = NroTrx% + 1
 Nveces% = Nveces% + 1
Wend																																				!
Close 41
Print "fin programa"
Stop 

!---
!--- Rutina control errores Aplicacion
!---
Fail.Program:
		 TS.ERROR = 0																														!
     Call TRADUCE.ERROR																											!
     If ERR = "IH" Then Resume 																							!
     If ERR = "CU" Then Resume 																							!
     If ERR = "*I" Then Resume 																							!
     If (ERR = "OE" OR ERR = "FU") Then Begin																! ERROR DE APERTURA
			  Resume																															!
		 EndIf																																	!
     If (ERR = "EF") Then Begin																							! Si eof
			  Resume																															!
		 EndIf																																	!
     MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$								!
     Call Mensaje(MSG$)																											!
		 Stop																																		!
