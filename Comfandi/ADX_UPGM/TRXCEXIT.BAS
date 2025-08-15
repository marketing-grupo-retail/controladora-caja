!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : TRXCEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo transacciones en linea    *
!**************************************************
!*
!Modificaciones:

%Environ C

Integer*1 Global Gr.TrxOnl.Error%, Gr.TrxOnl.Session%, Gr.TrxOnl.Session2%, Gr.TrxOnl.Act%,         \!
                 Gr.TrxOnl.Close%																								!
Integer*4 Global Gr.Consec.Np%, GR.Tmp.Apun%																!
String    Global Gr.File.Ruta$, Gr.TrxOnl.Appl$, Gr.TrxOnl.Func$													!

%INCLUDE EAMCSGLO.J86          																							! working storage
%INCLUDE EAMCSEVA.J86          																							! working storage
%INCLUDE EAMADXRT.J86	         																							! rutinas de la aplicacion
%INCLUDE EAMASMCT.J86          																							! Rutinas asembler
%INCLUDE BASROUT.J86           																							! Rutinas del basrout

Sub CSMLEW01 External          																							! Common error routine
End Sub

Function CSVFEC02( P1 ) EXTERNAL   																					! Lee la informacion del string 
Integer*2 P1                                                   							!
End Function																																!

Function BIGLEN(A$) EXTERNAL
  Integer*4 BIGLEN
  String A$
End Function


Function Gr.Getunpk(X.B$,X.J%) 																							! Desempaqueta datos tlog
Integer*2 X.J%, X.K%																												! 
String    X.B$, GR.Getunpk																									! 
  GR.Tmp.Apun% = Match(":",X.B$,X.J%) 				                              ! Busca Separador De Campo
  GR.Getunpk  = Unpack$(Mid$(X.B$,X.J%,GR.Tmp.Apun% - X.J%))                ! Desempaqueta Campo
  GR.Tmp.Apun% = GR.Tmp.Apun% + 1
End Function

Sub Val.Dir.Trx(Yfec$)																						          ! Creacion directorio 
String Yfec$, Yruta$																												! para transferencia de archivos
Yruta$ = ""																																	!
Yruta$ = "GRQTX:20" + Left$(Yfec$,6)                                   			! Arma fecha 
MkDir Yruta$																																!
Gr.File.Ruta$ = ""																													!
Gr.File.Ruta$ = Yruta$ + "\"																								!
End Sub 																																		!

Sub Almacena.TrxVta(Y.Caja$,Y.Trx$)																					! Grabacion de trx 
String Y.Caja$, Y.Trx$, X.Line$, X.Nota$, X.Lec$														! Definicion de variables
String Yfile$, YFinr$, Xcab$
Integer*4 X.Len%, PP, G.Err%

On Error GoTo GRTX.ERR1																											! Ctrl de errores
     YFinr$ = CHR$(13)+CHR$(10)																							!
     Y.Caja$ = Str$(Val(Y.caja$))																						!
     Y.Trx$  = Str$(Val(Y.trx$))																						!
     If Gr.TrxOnl.Close% = 0 Then 																					   \!
      YFile$ = Gr.File.Ruta$ + Right$("0000"+Y.Caja$,4) +                  \!
               Right$("0000"+Y.Trx$,4) + ".00Q" Else                       \!
      YFile$ = Gr.File.Ruta$ + Right$("0000"+Y.Caja$,4) +                  \!
               Right$("0000"+Y.Trx$,4) + ".01Q" 														!
     G.Err% = -1																														!
     CREATE POSFILE Yfile$ AS Gr.TrxOnl.Session2%																	! Creacion archivo de salida
     If G.Err% = -1 Then Begin 																							!
        X.Len% = BigLen(CS.INPUT$)																					!
        Xcab$  = Gr.TrxOnl.Appl$ + Gr.TrxOnl.Func$ + Right$("000000"+Str$(X.Len%),6)			!
        X.Len% = BigLen(Xcab$)		            															! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"										  								! Arma estructura de grabacion
        Write form X.Lec$;#Gr.TrxOnl.Session2%;Xcab$, YFINR$                      	! Grabacion del registro
        Write #Gr.TrxOnl.Session2%;CS.INPUT$ 																			!
        WAIT ; 150																													!
        Close Gr.TrxOnl.Session2%																									!
        WAIT ; 150																													!
     EndIf																																	!
     Exit Sub 																															!
GRTX.ERR1:																																	!
    G.Err% = 0 																															!
    Resume 																																	!
End Sub 																																		! Fin rutina

Sub Val.Tip.Trx(Xdata$)																									! 
String    Xdata$, Xa$																												!
Integer*1 Gr.TrxOnl.Error%																													!
Integer*4 pp																																!
On Error GoTo TX2.ERR1																											! Control de errores
    Gr.TrxOnl.Act% = 0 																													!
    Xa$ = Pack$(Xdata$)																											!
    Gr.TrxOnl.Error% = -1																													!
    Open "TF:TRXONL" KEYED RECL 11 AS Gr.TrxOnl.Session%														!
    If Gr.TrxOnl.Error% <> -1 Then Begin																						!
    	 Gr.TrxOnl.Act% = -1																											!
    	 Close Gr.TrxOnl.Session%																										!
    	 Exit Sub																															!
    EndIf  																																	!
    Gr.TrxOnl.Error% = -1																													!
    Read Form "C11" ;#Gr.TrxOnl.Session% KEY Xa$; Xdata$														!
    If Gr.TrxOnl.Error% <> -1 Then Begin																						!
    	 Gr.TrxOnl.Act% = -1																											!
    EndIf  																																	!
 	  Close Gr.TrxOnl.Session%																												!
    Exit Sub																																!
TX2.ERR1:  																																	!
    Gr.TrxOnl.Error% = 0 																													!
    Resume 																																	!
End Sub 																																		!

Sub TRXCSU01.011 Public									 																		! Analisis de trx
Integer*4 Gr.TrxOnl.Error%, X.L%								 																		! Definicion de 
String Ue.Proyecto$, Ue.Nota$, X.Lec$, X.KEY$, X.POS$, X.DATA$				 			! variables
String X.caja$, X.trx$, X.Fec$, Xfec$, Xmov$, Xtipo$, xstr$, XMSG$					!
Integer*1 X.IND%									 																					!
Integer*4 PP										 																						!
X.IND% = 0 										 																							!

For X.L% = 1 TO CS.SIZE									         														! Hasta el tamaño del string
  Call CSVFEC02(X.L%)								             														! Tomo tipo de String
  If CS.TYPE(X.L%) = 00 Then Begin 				 			                   					! Si cabecera de la trx
     X.Caja$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))       ! Terminal
     X.Trx$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))       ! Transaccion
     X.Fec$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))       ! Fecha y Hora Trx AAMMDDHHmm
     Xtipo$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))       ! Tipo de trx
     Xstr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))       ! Tipo de trx

!     Xmsg$ = "VT:"+Right$("0000"+X.caja$,4)+"-"+Right$("0000"+X.trx$,4) +   \!
!             " "+Xtipo$+" "+xstr$  
!     Call ADXSERVE(PP,26,1,xmsg$)
!     Wait ; 2500     

     If Xtipo$ <> "20" Then Begin																						!
        Xmov$   = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4)+   \!
                  Right$("0000"+X.Trx$,4) +                                \!
                  Right$("0000000000"+X.Fec$,10)														!
        Call Val.Dir.Trx(X.Fec$)					                            			! Directorio de movimientos
        Call Val.Tip.Trx(Xmov$)																							!
     EndIf																																	!  
  EndIf   																																	!
If CS.TYPE(X.L%) = 21 Then Begin                         										! String Cierre de tienda
	 GR.Tmp.Apun% = 3																													!
	 X.FEC$ = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)															!
	 X.CAJA$ = MID$(X.FEC$,3,2)   																						! Simula numero de caja
	 X.TRX$  = MID$(X.FEC$,5,2)   																						! Simula numero de trx
	 X.FEC$  = MID$(X.FEC$,1,6)   																						! Fecha del cierre
   Xmov$ = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4) +         \!
           Right$("0000"+X.Trx$,4) + Right$("0000000000"+X.Fec$,10)         !
   Call Val.Dir.Trx(X.Fec$)					                                  			! Directorio de movimientos
   Call Val.Tip.Trx(Xmov$)																									!
   If Gr.TrxOnl.Act% = -1 Then Begin 								 					 									! Trx con Nota Pedido
   	  Gr.TrxOnl.Close% = -1                                    									! Cierre de dia
      Call Almacena.TrxVta(X.Caja$, X.Trx$)						         							! Almacena trx
      Gr.TrxOnl.Act% = 0 									                     									! Init variable
      Gr.TrxOnl.Close% = 0 																											!
   EndIf 											                             									! 
EndIf																																				!
Next X.L%																																		!

If Gr.TrxOnl.Act% = -1 Then Begin 								 															! Trx con Nota Pedido
   Call Almacena.TrxVta(X.Caja$, X.Trx$)						         								! Almacena trx
   Gr.TrxOnl.Act% = 0 									 																				! Init variable 
EndIf 											 																								! 
End Sub

Sub TRXCSU02.011 Public									 																		! Analisis de trx
Integer*4 Gr.TrxOnl.Error%, X.L%								 ! Definicion de 
String Ue.Nota$, X.caja$, X.trx$, X.Fec$, Xfec$, Xmov$, Xtipo$, Xmsg$								 !
Integer*4 PP

GR.Tmp.Apun% = 1
Ue.Nota$ = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)

If Val(UE.NOTA$) = 0 Then BEGIN
 X.CAJA$ = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)
 X.TRX$  = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)
 X.FEC$  = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)

! Xmsg$ = "NVT:"+Right$("0000"+X.caja$,4)+"-"+Right$("0000"+X.trx$,4) 
! Call ADXSERVE(PP,26,1,xmsg$)
! Wait ; 2500     

 Xmov$ = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4)+Right$("0000"+X.Trx$,4) + \
         Right$("0000000000"+X.Fec$,10)
 Call Val.Dir.Trx(X.Fec$)					                                             ! Directorio de movimientos
 Call Val.Tip.Trx(Xmov$)
 
 If Gr.TrxOnl.Act% = -1 Then Begin 								 ! Trx con Nota Pedido
   Call Almacena.TrxVta(X.Caja$, X.Trx$)						         ! Almacena trx
   Gr.TrxOnl.Act% = 0 									 ! Init variable 
 EndIf 											 ! 

EndIf

If Val(UE.NOTA$) = 21 Then Begin                         ! String Cierre de tienda
	 GR.Tmp.Apun% = 3
	 X.FEC$ = Gr.Getunpk(CS.INPUT$,GR.Tmp.Apun%)
	 X.CAJA$ = MID$(X.FEC$,3,2)   ! Simula numero de caja
	 X.TRX$  = MID$(X.FEC$,5,2)   ! Simula numero de trx
	 X.FEC$  = MID$(X.FEC$,1,6)   ! Fecha del cierre
   Xmov$ = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4)+Right$("0000"+X.Trx$,4) + \
           Right$("0000000000"+X.Fec$,10)
 Call Val.Dir.Trx(X.Fec$)					                                             ! Directorio de movimientos
 Call Val.Tip.Trx(Xmov$)
 If Gr.TrxOnl.Act% = -1 Then Begin 								 					 ! Trx con Nota Pedido
   	  Gr.TrxOnl.Close% = -1                                    ! Cierre de dia
      Call Almacena.TrxVta(X.Caja$, X.Trx$)						         ! Almacena trx
      Gr.TrxOnl.Act% = 0 									                     ! Init variable
      Gr.TrxOnl.Close% = 0 
   EndIf 											                             ! 
	 
EndIf

End Sub

Sub TRXCSU12.011 Public
Integer*1 Gr.TrxOnl.Error%
Integer*4 PP
On Error GoTo TXX.ERR
    Gr.TrxOnl.Session%  = 95
    Gr.TrxOnl.Session2% = 96
    Gr.TrxOnl.Close% = 0
    Gr.TrxOnl.Appl$ = "99"
    Gr.TrxOnl.Func$ = "95"
    Gr.TrxOnl.Error% = -1
    Exit Sub 
TXX.ERR:
    Gr.TrxOnl.Error% = 0
    If (ERR = "OE") or (ERR = "FU")  Then Begin         ! Error de apertura
         Resume 
    EndIf 
    Call CSMLEW01             ! Process other errors
    If CS.RESUME Then Begin
      Resume                  ! resume after error
    EndIf Else Begin
      Resume RETRY            ! retry failing instn.
    EndIf
    
End Sub 


Sub TRXCSU16.011 Public
Integer*4 PP, Gr.TrxOnl.Error%
On Error GoTo NPY.ERR
    Gr.TrxOnl.Error% = -1
    CREATE POSFILE "TF:TRXONL" KEYED 11,,,200000 RECL 11 AS Gr.TrxOnl.Session% compound perupdate
    If Gr.TrxOnl.Error% = -1 Then Begin
    	 Wait ; 1000
    	 Close Gr.TrxOnl.Session%
    EndIf
    Exit Sub 

NPY.ERR:
      Gr.TrxOnl.Error% = 0 
      Resume                  ! resume after error
End Sub 
