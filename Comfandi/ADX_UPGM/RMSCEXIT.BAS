!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RMSCEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo transacciones en linea    *
!                Cuenta transitoria               *
!**************************************************
!*
!Modificaciones:

%Environ C

Integer*1 Global Gr.Error%, NP.SESSION%, NP.SESSION2%, Gr.Rms.Act%,         \!
                 Gr.Np.Close%
Integer*4 Global Gr.Consec.Np%, GR.Tmp.Apun%																!
String    Global Gr.File.Ruta$, Gr.Appl$, Gr.Func$													!

%INCLUDE EAMCSGLO.J86                                                       ! working storage           
%INCLUDE EAMCSEVA.J86                                                       ! working storage           
%INCLUDE EAMTRANS.J86                                                       ! working storage           
%INCLUDE EAMTLIST.J86                                                       ! working storage           
%INCLUDE EAMCUSTA.J86                                                       ! working storage           
%INCLUDE EAMADXRT.J86	                                                      ! rutinas de la aplicacion  
%INCLUDE EAMASMCT.J86                                                       ! Rutinas asembler          
%INCLUDE BASROUT.J86                                                        ! Rutinas del basrout       

Sub CSMLEW01 External          																							! Common error routine
End Sub

Function CSVFEC02( P1 ) EXTERNAL   																					! Lee la informacion del string 
Integer*2 P1                                                   
End Function

Function BIGLEN(A$) EXTERNAL																								! Longitud de dato
  Integer*4 BIGLEN																													!
  String A$																																	!
End Function																																!

Function Gr.Getunpk(X.B$,X.J%) 																							! Rutina empaquetamiento datos
Integer*2 X.J%, X.K%																												!
String    X.B$, GR.Getunpk																									!
  GR.Tmp.Apun% = Match(":",X.B$,X.J%) 				                              ! Busca Separador De Campo
  GR.Getunpk  = Unpack$(Mid$(X.B$,X.J%,GR.Tmp.Apun% - X.J%))                ! Desempaqueta Campo
  GR.Tmp.Apun% = GR.Tmp.Apun% + 1																						!
End Function																																!

Sub Valide.Directorio(Yfec$)																								! Creacion directorio 
String Yfec$																																! para transferencia de archivos
Yfec$ = "GRQRM:20" + Left$(Yfec$,6)                                   			! Arma fecha 
MkDir Yfec$																																	!
Gr.File.Ruta$ = Yfec$ + "\"																									!
End Sub 																																		!

Sub Almacena.Np(Y.Caja$,Y.Trx$)																							! Grabacion de trx 
String Y.Caja$, Y.Trx$, X.Line$, X.Nota$, X.Lec$														! Definicion de variables
String Yfile$, YFinr$, Xcab$																								!
Integer*4 X.Len%, PP, G.Err%																								!
On Error GoTo GRNP.ERR1																											! Ctrl de errores
     YFinr$ = CHR$(13)+CHR$(10)																							!
     Y.Caja$ = Str$(Val(Y.caja$))																						!
     Y.Trx$  = Str$(Val(Y.trx$))																						!
     
     !Call ADXSERVE(PP,26,1,"GRB:"+Y.Caja$+"-"+Y.Trx$)     
     !WAIT ; 2500
     If Gr.Np.Close% = 0 Then                                              \!
      YFile$ = Gr.File.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".00Q" Else \
      YFile$ = Gr.File.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".01Q" 
     G.Err% = -1																														!
     CREATE POSFILE Yfile$ AS NP.SESSION2%																	!
     If G.Err% = -1 Then Begin 																							!
        X.Len% = BigLen(CS.INPUT$)																					!
        Xcab$  = Gr.Appl$ + Gr.Func$ + Right$("000000"+Str$(X.Len%),6)			!
        X.Len% = BigLen(Xcab$)		            															! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"										  							  ! Arma estructura de grabacion
        Write form X.Lec$;#NP.SESSION2%;Xcab$, YFINR$                       ! Grabacion del registro
        Write #NP.SESSION2%;CS.INPUT$ 																			!
        WAIT ; 150																													!
        Close NP.SESSION2%																									!
        WAIT ; 150																													!
     EndIf																																	!
     Exit Sub 																															!
GRNP.ERR1:																																	!
    G.Err% = 0 																															!
    Resume 																																	!
End Sub 																																		!

Sub Valida.Tipo.Trx(Xdata$)																									! Valida si trx
String    Xdata$, Xa$																												! fue ya procesada 
Integer*1 Gr.Error%																													!
Integer*4 pp																																!
On Error GoTo NP2.ERR1																											! Control de errores
    Gr.Rms.Act% = 0 																													!
    Xa$ = Pack$(Xdata$)																											!
    Gr.Error% = -1																													!
    Open "TF:RMSONL" KEYED RECL 11 AS NP.SESSION%														!
    If Gr.Error% <> -1 Then Begin																						!
    	 Call ADXSERVE(PP,26,1,"error abriendo rmsonl")
    	 wait ; 1200
    	 Gr.Rms.Act% = 0 																											!
    	 Close NP.SESSION%																										!
    	 Exit Sub																															!
    EndIf  																																	!
    Gr.Error% = -1																													!
    Read Form "C11" ;#NP.SESSION% KEY Xa$; Xdata$														!
    If Gr.Error% <> -1 Then Begin																						!
    	 Gr.Rms.Act% = -1																											!
    EndIf  																																	!
 	  Close NP.SESSION%																												!
    Exit Sub																																!
NP2.ERR1:  																																	!
    Gr.Error% = 0 																													!
    Resume 																																	!
End Sub 																																		!

Sub RMSCSU01.011 Public									 																		! Analisis de trx
Integer*4 Gr.Error%, X.L%								 																		! Definicion de 
String Ue.Proyecto$, Ue.Nota$, X.Lec$, X.KEY$, X.POS$, X.DATA$				 			! variables
String X.caja$, X.trx$, X.Fec$, Xfec$, Xmov$, Xtipo$, xstr$ 								!
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
     If Xtipo$ <> "20" Then Begin																						!
        Xmov$   = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4)+   \!
                  Right$("0000"+X.Trx$,4) + Right$("0000000000"+X.Fec$,10)  !
     EndIf  																																!
  EndIf   																																	!
  If CS.TYPE(X.L%) = 99 Then Begin 				 			 														! Si User Data
     Ue.Proyecto$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))  ! Codigo de proyecto
     If Val(Ue.Proyecto$) = 20110114 Then Begin 					 									! Cuenta transitoria
    	 Call Valide.Directorio(X.Fec$)					                              ! Directorio de movimientos
    	 
!       Call ADXSERVE(PP,26,1,"PRC:"+X.Caja$+"-"+X.Trx$)
!       WAIT ; 2500

       !Call Valida.Tipo.Trx(Xmov$)																					!
       
       Gr.Rms.Act% = -1
     EndIf 										 																							!
  EndIf										         																					!

Next X.L%

If Gr.Rms.Act% = -1 Then Begin 								 															! Trx con remisiones
   Gr.Rms.Act% = 0 									 																				! Init variable 
   Call Almacena.NP(X.Caja$, X.Trx$)						         										! Almacena trx
EndIf 											 																								! 
End Sub																																			!

Sub RMSCSU12.011 Public																											! 
Integer*1 Gr.Error%																													!
Integer*4 PP																																!
On Error GoTo NPX.ERR																												!
    NP.SESSION%  = 95																												!
    NP.SESSION2% = 96																												!
    Gr.Np.Close% = 0																												!
    Gr.Appl$ = "99"																													!
    Gr.Func$ = "94"																													!
    Gr.Error% = -1																													!
    Exit Sub 																																!
NPX.ERR:																																		!
    Gr.Error% = 0																														!
    If (ERR = "OE") or (ERR = "FU")  Then Begin         										! Error de apertura
         Resume 																														!
    EndIf 																																	!
    Call CSMLEW01             																							! Process other errors
    If CS.RESUME Then Begin																									!
      Resume                  																							! resume after error
    EndIf Else Begin																												!
      Resume RETRY            																							! retry failing instn.
    EndIf																																		!
End Sub 																																		!

Sub RMSCSU16.011 Public																											! En proceso de cierre
Integer*4 PP, Gr.Error%																											!
On Error GoTo NPY.ERR																												!
    Gr.Error% = -1																													!
!    CREATE POSFILE "TF:RMSONL" KEYED 11,,,200000 RECL 11 AS NP.SESSION%    \!
!    compound perupdate																											!
    If Gr.Error% = -1 Then Begin																						!
    	 Wait ; 1000																													!
!    	 Close NP.SESSION%																										!
    EndIf																																		!
    Exit Sub 																																!

NPY.ERR:																																		!
      Gr.Error% = 0 																												!
      Resume                  																							! resume after error
End Sub 																																		!
