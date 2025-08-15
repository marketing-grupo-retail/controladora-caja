!************************************************** 
!Empresa       : ASIC S.A. UNIDAD DE RETAIL       *
!Programa      : BKINTPAY.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Interface PAGO ELECTRONICO       *
!**************************************************
! Se modifica para que el modulo solo envie las 
! trx efectuadas como cobradas.
! OVS Sept 13 2.005
!--------------------------------------------------
! Se ajusta modulo para que solamente ejecute la 
! interface y no envie el archivo via FTP y dispare
! el mensaje al WAS. Desarrollado por Oscar Valencia
! Sept 28 de 2.005 segun requerimiento de Freddy
! Guerrero y Jaime Rodriguez.
!--------------------------------------------------

%ENVIRON C						   												! Ambiente de controlador

String    Global ovs$, OLD.TRX$, OLD.CAJA$, Ue.Convenio$, Ue.Plan$, Ctrl.Trx$
String    Global Ue.Recibo$, Recibos$(2), Ue.Salida$, Ue.Data1$, Ue.Data2$, DATO.SO$, Ue.Data3$
Integer*4 Global NRO.PAGOS%, Ind.Colegio%, X.Cpag%, PP, VTAS.TOTALES%, Cnt.Reg% , CITM%
String    Global FECMOV$, HORA.FINAL$, UE.FECMOV$
INTEGER*1 Global ARC%
Integer*4 Global CPAG%, CDSC%, X.PARA%, X.CAMBIO%, X.Datos%
String    Global Buffer$(2)

!%Include BKCNVVAR.011					                  ! Variables del programa

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86

!--- Definicion de rutinas de la aplicacion

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) EXTERNAL                  ! Msg background
   INTEGER*4 RET
   INTEGER*2 FUNC,PARM1
   String PARM2
End Sub

Sub ADXCOPYF(RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
   INTEGER*4 RETC
   String INFILE, OUTFILE
   INTEGER*2 OPT0, OPT1
End Sub

Function INICIO1
  Call.ORDER% = 11                                ! Llamado Primera Pantalla D.M
  RET.ERR% = DISPD(Call.ORDER%)                   ! Llamado de la pantalla en DM
  Call DM.ERR(RET.ERR%,DISPD$)
End Function
!--- Fin de la funcion de inicio


!---- Rutinas de Java


SUB JavaCall.Initialize.Request(ClassName,MethodName,TheRequest) EXTERNAL
  STRING ClassName
  STRING MethodName
  STRING TheRequest
END SUB

SUB JavaCall.AddParameter.String(TheRequest,TheParameter) EXTERNAL
  STRING TheRequest
  STRING TheParameter
END SUB

SUB JavaCall.InvokeMethod.ReturnString(TheRequest,ReturnValue, Exception) EXTERNAL
  STRING TheRequest
  STRING ReturnValue
  STRING Exception
END SUB

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

If LEN(Java.EXCEPTION$) > 0 THEN BEGIN									! Si hay Exception
   Locate 23,1 : Print string$(78," ")
   Locate 23,1 : Print "Error Java "+Java.EXCEPTION$
   Java.RETURNVALUE$ = ""										
ENDIF Else Begin											! Ejecucion OK

!   CALL U.Imprime("Respuesta Entregada",4100H)								! Imprime Error
!   CALL U.Imprime(LEFT$(Java.RETURNVALUE$,38),4100H)							! en SJ

Endif													!

Rutina.Java = Java.RETURNVALUE$										! Retorna Respuesta
   
End Function
!--- Fin invocacion rutinas de Java


!--- Fin rutinas de java


Function ENTRADA.LOG
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
   Else Locate 15,1: Print MEN$
  Wait;1500
End Function 
Function BARRA.ESTADO
  INTEGER*1 N, TOT.PORC.LEIDO
  TOT.PORC.LEIDO = TOT.LEIDO * 100 / TOT.TAMANO
  N = TOT.PORC.LEIDO / 10
  BARRA$ = " 0% ¯"+String$(N,CHR$(219))+String$(10-N,CHR$(177))+"® "+STR$(TOT.PORC.LEIDO)+"%"
End Function 

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

Sub Grabacion.Interface.String(X.Cadena$, X.stat%, X.Valor$, X.Crd$)
Integer*1 X.Stat%
Integer*4 X.I%
String    X.Cadena$, X.Valor$, X.Crd$

If X.Stat% = 1 Then Begin    ! Pago realizado
   X.Datos% = X.Datos% + 1
   Buffer$(X.Datos%,0) = X.Crd$
   Buffer$(X.Datos%,1) = X.Cadena$
   Buffer$(X.Datos%,2) = X.VALOR$
EndIf Else Begin 
   For X.I% = 1 to X.Datos%              ! Recorre vector
    If Buffer$(X.I%,0) = X.Crd$ Then Begin 
       Buffer$(X.I%,2) = "0"
    EndIf 
   Next X.I%
EndIf 

End Sub 


Sub Grabacion.Interface
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%, X.I%
  For X.I% = 1 TO X.DATOS%
      If Val(Buffer$(X.I%,2)) > 0 Then Begin                              ! Si pago efectuado
         Rbuffer$ = Buffer$(X.I%,1)                                       ! Asigna String
         X.Len% = Len(Rbuffer$)						  					  								  ! Toma longitud del registro
         X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
         Write form X.Lec$; #51 ; Rbuffer$, Finr$           							! Graba registro
      EndIf     
  Next X.I%
  
End Sub
!--- Fin grabacion de interface

Sub Armar.File.Trx
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$, xx$
Integer*4 X.Len%, X.J%, X.Comp%

 Rbuffer$ = "machine titan login zdtpos password zdtpos macdef init"
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro

 Rbuffer$ = "bin"
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro

 Rbuffer$ = "prompt"
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro

 !Rbuffer$ = "lcd /adx_udt1"
 !X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 !X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 !Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro
   
 Rbuffer$ = "cd /home/pos"
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro
 
 Rbuffer$ = "put " + Right$(Ue.Salida$,12)
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro
 
 Rbuffer$ = "by"
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro 
 
 Rbuffer$ = ""
 X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
 X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
 Write form X.Lec$; #57 ; Rbuffer$, Finr$           							! Graba registro 
 
 Close 57

! Call OSSHELL("COPY adx_sdt1:adxhsigf.dat adx_sdt1:adxhsigf.tmp")
! Call OSSHELL("COPY adx_udt1:adxhsigf.txt adx_sdt1:adxhsigf.dat")
! wait ; 1000

End Sub 
!--- 

Function TERMINE.PROG
String X.TMP$
  Call BARRA.ESTADO
  MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
    Else Begin
     Locate 12,15: Print MEN$
  EndIf
  
  Call Grabacion.Interface
  
  MEN$ = "Interface Generada "+Ue.Salida$
  If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
    Else Begin
     Locate 22,1: Print MEN$
  EndIf
  Close 51
  Call Armar.File.Trx         ! Arma msg para el FTP 
!  Call OSSHELL("transmit.bat")   ! Ejecuta el FTP 
!  Call OSSHELL("COPY adx_sdt1:adxhsigf.tmp adx_sdt1:adxhsigf.dat") ! Restaura FTP
!  Call OSSHELL("msg.bat")     ! Mensaje al WAS 

!  UE.SALIDA$ = Right$(UE.SALIDA$,12)
!  DM.ALMACEN$ = Right$("0000"+DM.ALMACEN$,4)
!  X.TMP$ = DM.ALMACEN$                                + \ NUMERO ALMACEN
!           UE.FECMOV$                                 + \ FECHA MOVIMIENTO AAMMDD
!           Left$(Ue.Salida$+STRING$(20," "),20)       + \ NOMBRE ARCHIVO
!           Left$("ADX_UDT1"+STRING$(40," "),40)       + \ RUTA DE RETORNO
!           Left$("RETORNA.TXT"+STRING$(20," "),20)    + \ NOMBRE ARCHIVO RETORNO
!           Left$("10.5.100.10"+STRING$(15," "),15)   + \ IP CONTROLADOR
!           Left$("3916"+STRING$(15," "),15)           + \ USUARIO FTP
!           Left$("7005"+STRING$(15," "),15)             ! PASWORD FTP
           
!  X.TMP$ = armar.trama.msg("10","10",X.TMP$,"00","0001","123456","9999","9999","9999999999","9999",DATE$+TIME$)
!  X.TMP$ = Rutina.Java("com.appl.ApplKernel","threader", X.TMP$)              ! Ejecuta Requerimiento  
!  Print "RTA "+X.TMP$

  Call SETF("0000000")				   								 !
  Call CLRSCR					   												 !
  RET.ERR%= CLSDIS				   										 !
  Call DM.ERR(RET.ERR%,CLSDIS$)			   					 !
  Stop
  
End Function
!--- Fin de la ejecucion del programa

Function LEER.CABECERA
String X.Tmp$
     Open "c:\adxalma.dat" as 91
     Read # 91 ; X.Tmp$
     DM.ALMACEN$ = Left$(X.Tmp$,4)						   ! Numero del almacen
     FECMOV$     = MID$(X.TMP$,5,8)              ! Fecha de movimiento
     Close 91
End Function
!--- Fin de la funcion de lectura

Function INI.VAR.PROG		! Inicializa Variables del Programa en GRAL
  OLD.TRX$       = "XXX"
  NRO.ERROR      = 0
  BARRA%         = 1
  TOTAL.REG      = 0
  CLIENTE.FREC   = 0
  ACUMULADOR%    = 0
  TRANS.AUTONOMO = 0
  TRANS.NORMAL   = 0
  TOT.DEPTOS     = 0
  NETMSC         = 0
  TOT.TERM       = 0
  TERM.INI       = 0
  NUM.SEP$       = ","
  PRIMERA.VEZ    = 1
  Ctrl.Trx$      = ""
  VTAS.TOTALES%  = 0 
  Cnt.Reg%       = 0   ! Contador de registros
  Dim Buffer$(5000,2)
  X.Datos% = 0 
  FINR$=CHR$(13)+CHR$(10)
  TLOG = 25		    :CONTROLFILE = 29	:INTERFAZ = 28
  LISTRAN  = 34		:OUTFIL = 36		  :LOGINTERFAZ = 20
  SERDIAN = 27		:NUM.ALMACEN = 38
End Function 

Function INICIALICE.VAR.TRANSACCION		! Para cada Transacci¢n Nueva
Q                 = 1
NRO.StringS.FOUND = 0
NRO.ARTICULOS     = 0
NO.VENTA          = 0
SW.MISC           = 0
DEPTO%            = 0
T%                = 0
DEPTO$            = ""
NRO.REG           = 0      ! Control del header a cero 
CITM%             = 0
CPAG%             = 0
CDSC%             = 0 
X.PARA%           = 0
X.CAMBIO%         = 0
Ind.Colegio%      = 0 
Cnt.Reg%       = 0   ! Contador de registros
End Function 

Function APERTURA.ARCHIVOS.PRINC
INTEGER*4 X%
  X% = 0
  If X% = 0 Then Begin                                          ! Si copia OK
     OPEN B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL
     ARCHIVO.PROC$ = B$
     TOT.TAMANO = SIZE(B$)
     B$ = ""
  EndIf Else Begin                                              ! ERROR APERTURA
     MEN$="Error proceso log de transacciones, Finalizado ... "
     Call ADXSERVE(PP,26,1,MEN$)
     Wait;1800
     STOP
  EndIf
End Function 

Function LEA.REG.TRANS.SUM.LOG	    ! DEVUELVE EL ARCHIVO PREVIO  
String LLAVE$
 Open "EAMCSCF1" KEYED RECL 36 AS 37 NOWRITE NODEL	! Busca Eamtran Activo 
 LLAVE$ = PACK$("9998")
 Read Form "C2 C8 C8 I4 C5 C9";#37  KEY LLAVE$; TERM$, \
       SLOGNAME$,OSLOGNAME$,CLOSEPNT,FECHA.CIER$,RESERVED$  
 Close 37
 If DM.CODIGO$ = "2" Then Begin 
   If OSLOGNAME$="        " Then Begin 
  	    MEN$="No Existe EAMTRAN? Previo. Procesando el Actual ... "
        Call ADXSERVE(PP,26,1,MEN$)
        Wait;1800
	      B$ = SLOGNAME$ 
        FECHA.CIER$ = DATE$
   EndIf Else Begin 
         B$ = OSLOGNAME$ 			! CARGA EAMTRAN PREVIO
         FECHA.CIER$ = UNPACK$(FECHA.CIER$)
         FECHA.CIER$ = LEFT$(FECHA.CIER$,6)	! Tomo primeros 6 caracteres aammdd
   EndIf
EndIf
TOT.TAMANO = SIZE(B$)
End Function 

Function PANTALLA.PRINCIPAL
String X.INPUT$
Call INICIADM 				                    ! Inicializacion Variables Display Manager
CAMPO% = 0
AREA1% = 11: AREA2% = 4				            ! Definicion area de trabajo archivo
Call LEER.CABECERA
DM.CODIGO$ = "2" 
DATO.SO$ = COMMAND$						! Dato S.O

If DATO.SO$ = "BACKGRND" Then B$ = "" Else B$ = DATO.SO$

If DATO.SO$ <> "BACKGRND" Then Begin 				! Si entro dato
   DATO.SO$ = "0"
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****       INTERFACE    PAGO ELECTRONICO SUBSIDIO    ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Septiembre 28 2005 ASIC ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO   
   
EndIf Else DATO.SO$ = "1"
If LEN(B$) = 0 Then Begin 					! Dia previo
   Call LEA.REG.TRANS.SUM.LOG    ! DEVUELVE ARCHIVO PREVIO  
EndIf
If LEN(B$) = 1 Then Begin 
   B$ = "C:\ADX_IDT4\EAMTRAN"+B$+".DAT" 			! Tomo log capturado
   FECHA.CIER$ = DATE$						! Asigno fecha dia
EndIf
End Function 

Function CUENTE.StringS	       	! Esta funcion cuenta primero el #String y lo
INTEGER*2 POS1%,CUENTE.StringS	! compara con el #reportado por SMA
String CARACTER
 POS1% = 1
 NRO.StringS.FOUND = 0
 CARACTER = CHR$(34)+CHR$(44)+CHR$(34)
 WHILE (POS1% <> 0)
	POS1% = MATCH(CARACTER,INAREA$,POS1%+1)
	NRO.StringS.FOUND = NRO.StringS.FOUND + 1
 WEnd
 NRO.StringS.FOUND = NRO.StringS.FOUND - 1
 If (NRO.StringS.FOUND < NRO.REG) Then Begin
	MEN$ = "FALTA String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND > NRO.REG) Then Begin
	MEN$ = "SOBRAN String'S TERM : "+TERMINAL$+" TRX: "+NRO.TRANS$
	Call ENTRADA.LOG
 EndIf
 If (NRO.StringS.FOUND = NRO.REG) Then CUENTE.StringS = 1	\ Retorna 1 si es
 Else CUENTE.StringS = 0				! satisfactorio el resultado y 0
End Function						! si el resultado es fallido.

Sub GRABE.IDENTIfICADOR
 String C$, DINNER.POS$, DINNER.NEG$
 PROCESO$ = " INSERTANDO IDENT "
 DUPLICADA = 0
 TOTAL.REG = TOTAL.REG + 1
 Locate 20,25
 Call BARRA.ESTADO
 TOTAL.REG = TOTAL.REG + 1
 MEN$ = BARRA$+" Reg.Procesados => "+STR$(TOTAL.REG)
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Begin
   Locate 12,15: Print MEN$
 EndIf
End Sub

Function ERRNSTR$(ERRNUM)
Integer*1 I
Integer*4 ERRNUM,WORK
String HEX$,ERRNSTR$,WORK$
    HEX$="0123456789ABCDEF"
    ERRNSTR$="":WORK$=""
    For I = 1 TO 8
      WORK   = ERRNUM AND 0000000FH         ! AND OFF ALL BUT LOW NYBBLE
      WORK$  = MID$(HEX$,WORK+1,1)+WORK$    ! ADD HEX VALUE TO OUTPUT String
      ERRNUM = SHIfT(ERRNUM,4)              ! SET UP NEXT NYBBLE
    NEXT I
    ERRNSTR$=WORK$                          ! Return Error Code
End Function 

Function CALCULO.HORA
String A$, H$, M$, S$
A$ = TIME$							    !
H$ = LEFT$(A$,2)						! Tomo hora
M$ = MID$(A$,3,2)						! Tomo minutos
S$ = RIGHT$(A$,2)						! Tomo segundos
M$ = STR$(VAL(M$) + 10)			! Toma 10 Minutos
M$ = RIGHT$("00"+M$,2)			!
If VAL(M$) > 59 Then Begin 	!
   M$ = STR$(VAL(M$) - 60)	!
   M$ = RIGHT$("00"+M$,2)		!
   H$ = STR$(VAL(H$) + 1)		!
   If VAL(H$) > 23 Then H$ = "00"	 !
EndIf								               !
HORA.FINAL$ =  H$+M$+"00"					 ! Hora de Inicio 
End Function

!Sub Cabecera.Interface
!String Rbuffer$, X.Lec$
!Integer*4 X.Len%
!  Rbuffer$ = "1"+dm.almacen$+fecmov$
!  X.Len% = Len(Rbuffer$)														  								! Toma longitud del registro
!  X.Lec$ = "C"+Str$(X.len%)+" C2"										    							! Arma estructura de grabacion
!  Write form X.Lec$; #51 ; Rbuffer$, Finr$              							! Graba registro
!End Sub
!--- Fin grabacion cabecera


!
!-------------------------------------------
!----- Bloque Principal --------------------
!-------------------------------------------
!

  ON ERROR GOTO IO.ARCHIVOS
  Call INI.VAR.PROG	
  Call CALCULO.HORA
  ARC% = 0
  CALL INICIALICE.VAR.TRANSACCION		! Inicializamos las Var de Trans.
  Call PANTALLA.PRINCIPAL
  Call APERTURA.ARCHIVOS.PRINC
  Locate 8,12: Print "Procesando .... "+ARCHIVO.PROC$

!********************** LECTURA SUCESIVA DE NUEVA TRANSACCION *******************

  NXTRCD:
  Q                 = 1
  NRO.STRINGS.FOUND = 0
  NRO.ARTICULOS     = 0
  NO.VENTA          = 0
  SW.MISC           = 0
  DEPTO%            = 0
  T%                = 0
  PROCESO$ = " PROCESO PRINCIPAL "
  READ #TLOG; LINE INAREA$
  IF END #TLOG THEN REPORTE.FINAL
  TOT.LEIDO = TOT.LEIDO+LEN(INAREA$)+2		! PARA CALCULAR BYTES LEIDOS DEL TLOG
  IF LEN(INAREA$) < 12 THEN BEGIN		! SI HAY ALGO EN REGISTRO
	  GOTO NXTRCD
  ENDIF
  INAREA$ = INAREA$ + ","
  While (Q < LEN(INAREA$))	! SI HAY ALGO EN REGISTRO
    P = MATCH (",",INAREA$,Q) 	! ENCONTRAR DELIMITADOR, COMA
    IF (P-Q) < 3 THEN \ 	! REVISAR FALTA DE STRING
    	BEGIN
	  P=0
	  MEN$ ="FALTA STRING ID TRANSACCION"
	  CALL ENTRADA.LOG
	  Q=P+1 			      ! PREPARANDO POSICION PARA PROX. STRING
	  GoTo AGAIN		   	! VUELVA A LEER OTRO REGISTRO
	ENDIF
    B$ = MID$(INAREA$,Q+1,(P-Q)-2) 	! CAPTURA CADENA SIN COMILLAS.	MENOS 13 Y 
    B$ = B$+":" 			! SUMA UN SEMI-COLON
    Q = P + 1 				! PREPARA POSC. PARA NUEVO DATO
    A = Val(Unpack$(Left$(B$,1))) 	! DETERMINA TIPO DE STRING
    If A = 0 THEN GoSub S0:GoTo AGAIN	! SI ES CHECKOUT TRANSACCION
    If A = 99 THEN GoSub S99:GoTo AGAIN
    If (A < 0) OR (A > 21) THEN GOTO AGAIN
    If (SW.ITEM = 1) AND (A <> 2) AND (A <> 3) THEN \	! SI YA HUBO ITEM Y NO EXTENSION NI DESCUENTO
    If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
		   CANT.PESO$ = "000000000" 
       SW.ITEM = 0
    EndIf
    If (A > 3) AND (SW.ITEM = 1) THEN \	!SI PASO CANTIDAD Y DESCUENTO Y NO GRABO
	   If (TIPO.TRANS = 0) OR (TIPO.TRANS = 18) THEN BEGIN
          	SW.ITEM = 0
     EndIf
     
    On A GOSUB S1,S2,S3,S3,S5,S5,S7,S7,S9,S10, \
               S11,S12,S13,S14,S15,S16,S16,S16,S16,S20,S21
               
    AGAIN:
     If (A=0) AND (STRINGS.COMPLETOS=0) THEN NRO.ERROR = NRO.ERROR + 1:GOTO NXTRCD
     If NO.VENTA THEN GOTO NXTRCD		! Si no es una venta leemos otro registro
  Wend
GOTO NXTRCD			! VUELVA A LEER NUEVO REGISTRO 


S0:
  ! ****************** UNA NUEVA TRANSACCION SE EJECUTA ************************
  J = 3
  GOSub GETUNPK																			! 
  TERMINAL$=RIGHT$(A$,4)	        									! CAPTURA NUMERO DE TERMINAL
  GOSub GETUNPK																			! 
  NRO.TRANS$=A$			        												! CAPTURA NUMERO DE TRANSACCION
  If OLD.TRX$ = "XXX" Then BEGIN 
     OLD.TRX$  = NRO.TRANS$
     OLD.CAJA$ = TERMINAL$ 
  EndIf   
  GOSub GETUNPK																			!
!  FECMOV$ = A$
  FECHA$=LEFT$(A$,6)																! CAPTURA FECHA DE TRANSACCION
  FECHA.ARCH$=FECHA$
  If ARC% = 0 Then Begin                						! Creacion archivos de salida
     UE.FECMOV$ = FECMOV$
     FECHA.CIER$ = Right$(FECMOV$,4)     						! Toma fecha de movimiento del
     ARC% = 1
     Ue.Salida$ = "ADX_UDT1:X"+Right$(DM.ALMACEN$,3)+FECHA.CIER$+".DAT"
     create Ue.Salida$ AS 51 
     create "adx_udt1:adxhsigf.txt" As 57
!     Call Cabecera.Interface
  EndIf
  HORA$ = RIGHT$(A$,4)+"00"	        								! CAPTURA HORA DE LA TRANSACCION
  GOSub GETUNPK
  TIPO.TRANS=VAL(A$)		        										! CAPTURA EL TIPO DE LA TRANSACCION
  If tipo.trans = 17 Then GOTO NXTRCD
  If TIPO.TRANS = 2 Then X.CAMBIO% = -1 
  GOSub GETUNPK
  NRO.REG=VAL(A$)  		        											! NUMERO DE StringS EN TODA LA TRANSACCION
  StringS.COMPLETOS = 0
  If (TIPO.TRANS <> 0) AND (TIPO.TRANS <> 18) AND (TIPO.TRANS <> 2) AND 	\
	(TIPO.TRANS <> 1) Then Begin 											! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
		NO.VENTA = 1
  EndIf Else	\
	StringS.COMPLETOS = CUENTE.StringS								! FUNCION PARA CORROBORAR INTEGRIDAD DE StringS
  GOSub GETUNPK
  COD.OPERA$=A$  																		! NUMERO DEL OPERADOR
  GoSub GETUNPK 													  			  ! PASSWORD DEL OPERADOR
  GOSub GETUNPK																			! Venta positiva
  GROSS.POSITIVO$=A$
  GOSub GETUNPK																			! Venta negativa
  GROSS.NEGATIVO$=A$
  If (OLD.CAJA$ <> TERMINAL$) OR (OLD.TRX$ <> NRO.TRANS$) Then BEGIN 
   X.Cpag% = Cpag%
   Cpag% = 0
   If Ind.Colegio% = -1 Then begin 
     Call INICIALICE.VAR.TRANSACCION		            ! Inicializamos las Var de Trans.
     OLD.TRX$  = NRO.TRANS$
     OLD.CAJA$ = TERMINAL$      
     Ind.Colegio% = 0
   EndIf 
  EndIf   
  If StringS.COMPLETOS AND (TIPO.TRANS = 0 OR TIPO.TRANS = 18 OR \
	TIPO.TRANS = 2 OR TIPO.TRANS = 1) Then  Begin			! TIPO 2 PARA TEndER EXCHANGE Y TIPO 1 PARA TEndER CASHING ...
     Call GRABE.IDENTIfICADOR
     If DUPLICADA Then GOTO NXTRCD									! 
  EndIf
Return

S1:
  J = 3
Return

S2:
  J = 3 		        ! EXTENSION DE ENTRADA
Return

S3:
  J = 3
Return

S5:
  J = 3
Return

S7:
  J = 3!TAX
RETURN

S9:
  J = 3
Return

S10:
  J = 3
RETURN

S11:
  J = 3!DTENTRY
Return

S12:
  J = 3!CHANGE PRICE
RETURN

S13:
  J = 3!LOAN-PICKUP
RETURN

S14:
  J = 3
RETURN

S15:
  J = 3
RETURN

S16:
RETURN

S20:
  J = 3
RETURN

S21:
  J = 3
Return

S99:
  J = 3
  GOSub GETUNPK																   	! LLAVE DEL ARCHIVO
    If A$ = "01082005" Then Begin 								! Recaudo Colegio
        GoSub GETUNPK4												  	! EXTRAIGO DATOS DE LA VENTA
        Ue.Data1$ = A$
        GoSub GETUNPK 													  ! Numero del recaudo
        Ue.Data3$ = A$                            ! Valor trx
        
        Ue.Data2$ = Armar.Trama.Msg("10", "01", Right$(Ue.Data1$,18),"00","0001","123456",mid$(Ue.Data1$,1,4), \
                    mid$(Ue.Data1$,5,6),mid$(Ue.Data1$,11,10), mid$(Ue.Data1$,21,6), mid$(Ue.Data1$,27,12))

        Call Grabacion.Interface.String(UE.DATA2$,1,Ue.Data3$, Right$(Ue.Data1$,18))
        
    EndIf 
    If A$ = "02082005" Then BEGIN                 !
        GoSub GETUNPK4												  	! EXTRAIGO DATOS DE LA VENTA
        Ue.Data1$ = A$
        GoSub GETUNPK 													  ! Numero del recaudo
        Ue.Data3$ = A$                            ! Valor trx
            
        Ue.Data2$ = Armar.Trama.Msg("10", "02", Right$(Ue.Data1$,18),"00","0001","123456",mid$(Ue.Data1$,1,4), \
                    mid$(Ue.Data1$,5,6),mid$(Ue.Data1$,11,10), mid$(Ue.Data1$,21,6), mid$(Ue.Data1$,27,12))
                    
        Call Grabacion.Interface.String(UE.DATA2$,2,Ue.Data3$, Right$(Ue.Data1$,18))
        
    EndIf 

Return

GETEXCPT:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  If ((K-J)-1) > 0 Then \ ! If DATA IN FIELD
    A$ = UNPACK$(MID$(B$,J,(K-J)-1)) \ ! UNPACK FIELD WITHOUT QUOTES
  Else \
    A$ = "" ! SET A$ = NULLS
  J=K+2 ! POINT TO BeginNING OF NEXT FIELD
RETURN

!*********************************************************************
GETUNPK:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************

!*********************************************************************
GETUNPK3:
  K = MATCH(";",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = UNPACK$(MID$(B$,J,K-J)) ! UNPACK FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN
!*********************************************************************
GETUNPK4:
  K = MATCH(":",B$,J) ! SEARCH FOR FIELD SEPERATOR
  A$ = (MID$(B$,J,K-J)) ! FIELD
  J=K+1 ! POINT TO BeginNING OF NEXT FIELD
RETURN

!*********************************************************************
GETFLAG:
  FLAG = VAL(A$) ! CONVERT FLAG TO INTEGER
                 ! START BUILDING String TO CONTAIN INDIVIDUAL FLAGS
  If (FLAG AND 00000001H) Then A$ = "1" Else A$ = "0"
  FOR I = 1 TO 15
    FLAG = SHIfT(FLAG,1) ! SET UP NEXT BIT
    If (FLAG AND 00000001H) Then A$ = "1" + A$ Else A$ = "0" + A$
  NEXT I
RETURN

!*********************************************************************
!
REPORTE.FINAL:
        Call TERMINE.PROG
RETURN
!*********************************************************************
!

IO.ARCHIVOS:
  ERRORCOD$ = ERR
  P=0
If ERR = "SS" Then RESUME NXTRCD:

If ERRF% = OUTFIL and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 

If ERRF% = 4 and ERR = "EF" Then Begin 
   BAN.PRG$ = "1"
   RESUME           
EndIf 
If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Maestro de Articulos"
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
EndIf 

If ERRF% = 50 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Numeracion Fiscal "
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
EndIf 

If ERRF% = 50 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf

If ERRF% = 55 AND (ERR = "OE" OR ERR = "FU") Then Begin
    MEN$="Error: No Se Logro Abrir Convenios"
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
EndIf 

If ERRF% = 55 AND ERR = "EF" Then Begin          ! Si encuentra EOF
   BAN.PRG$ = "1"
   RESUME        
EndIf


If ERRF% = TLOG AND (ERR = "OE" OR ERR = "FU") Then \
 Begin
    MEN$="Error: No Se Logro Abrir Log de Transacciones "
    Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
    Stop 
 EndIf
If ERRF% = TLOG   AND ERR = "EF" Then Begin          \! Si encuentra fin de 
     MEN$ = "No se han registrado ventas para este periodo ..."
     Call ADXSERVE(PP,26,1,MEN$) : Print MEN$
     Call TERMINE.PROG
EndIf				                      !
Call TRADUCE.ERROR
MEN$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Call ENTRADA.LOG
MEN$ = "TRX :"+NRO.TRANS$+" TERM: "+TERMINAL$
Locate 22,1: Print String$(78," ")
Locate 22,1: Print MEN$
STOP
!*********************************************************************
