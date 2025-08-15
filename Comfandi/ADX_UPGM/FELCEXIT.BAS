!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : FELCEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo transacciones en linea    *
!                facturacion electronica          *
!                Minifacturas.
!**************************************************
!*
!Modificaciones:
! Mod 17Oct2024
! Se controla el no envio de reintegros fuera de 
! tiquete a expressmed.
! Desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------

%Environ C

Integer*1 Global Gr.Fel.Error%, NP.SESSION%, NP.SESSION2%, Gr.Fel.Act%,    \!
                 Gr.Np.Close%, Gr.Fel.NoTax%, NF.SESION1%
Integer*4 Global Gr.Consec.Np%, GR.Tmp.Apun%																!
Integer*4 Global Gr.MinFac.Vlr%
String    Global Gr.Fel.Ruta$, Gr.Fel.Appl$, Gr.Fel.Func$										!
String    Global Gr.Fel.Appl2$, Gr.Fel.Func2$																! Para dia Sin iva

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

Sub Valide.Ruta.Fe(Yfec$)																								    ! Creacion directorio 
String Yfec$, Yruta$																												! para transferencia de archivos
Integer*4 xx
Yruta$ = ""
Yruta$ = "GRQFE:20" + Left$(Yfec$,6)                                   			! Arma fecha 
MkDir Yruta$
Gr.Fel.Ruta$ = ""																																	!
Gr.Fel.Ruta$ = Yruta$ + "\"																									!

End Sub 																																		!

Sub Almacena.NoTax(Y.Caja$,Y.Trx$)																					! Grabacion de trx 
String Y.Caja$, Y.Trx$, X.Line$, X.Nota$, X.Lec$														! Definicion de variables
String Yfile$, YFinr$, Xcab$																								!
Integer*4 X.Len%, PP, G.Err%																								!
On Error GoTo GRNP.ERR1																											! Ctrl de errores
     YFinr$ = CHR$(13)+CHR$(10)																							!
     Y.Caja$ = Str$(Val(Y.caja$))																						!
     Y.Trx$  = Str$(Val(Y.trx$))																						!
     If Gr.Np.Close% = 0 Then                                              \!
      YFile$ = Gr.Fel.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".00Q" Else \
      YFile$ = Gr.Fel.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".01Q" 
     G.Err% = -1																														!
     CREATE Yfile$ AS NP.SESSION2%																	!
     If G.Err% = -1 Then Begin 																							!
        X.Len% = BigLen(CS.INPUT$)																					!
        Xcab$  = Gr.Fel.Appl2$ + Gr.Fel.Func2$ + Right$("000000"+Str$(X.Len%),6)			!
        X.Len% = BigLen(Xcab$)		            															! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"										  							  ! Arma estructura de grabacion
        Write form X.Lec$;#NP.SESSION2%;Xcab$, YFINR$                       ! Grabacion del registro
        Write #NP.SESSION2%;CS.INPUT$ 																			!
        WAIT ; 150																													!
        Close NP.SESSION2%																									!
        WAIT ; 150																													!
     EndIf																									!
     Exit Sub 																															!
GRNP.ERR1:																																	!
    G.Err% = 0 																															!
    Resume 																																	!
End Sub 																																		!

Sub Almacena.Fe(Y.Caja$,Y.Trx$)																							! Grabacion de trx 
String Y.Caja$, Y.Trx$, X.Line$, X.Nota$, X.Lec$														! Definicion de variables
String Yfile$, YFinr$, Xcab$																								!
Integer*4 X.Len%, PP, G.Err%																								!
On Error GoTo GRNP.ERR1																											! Ctrl de errores
     YFinr$ = CHR$(13)+CHR$(10)																							!
     Y.Caja$ = Str$(Val(Y.caja$))																						!
     Y.Trx$  = Str$(Val(Y.trx$))																						!
     If Gr.Np.Close% = 0 Then                                              \!
      YFile$ = Gr.Fel.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".00Q" Else \
      YFile$ = Gr.Fel.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".01Q" 
     G.Err% = -1																														!
     CREATE Yfile$ AS NP.SESSION2%																	!
     If G.Err% = -1 Then Begin 																							!
        X.Len% = BigLen(CS.INPUT$)																					!
        Xcab$  = Gr.Fel.Appl$ + Gr.Fel.Func$ + Right$("000000"+Str$(X.Len%),6)			!
        X.Len% = BigLen(Xcab$)		            															! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"										  							  ! Arma estructura de grabacion
        Write form X.Lec$;#NP.SESSION2%;Xcab$, YFINR$                       ! Grabacion del registro
        Write #NP.SESSION2%;CS.INPUT$ 																			!
        WAIT ; 150																													!
        Close NP.SESSION2%																									!
        WAIT ; 150																													!
     EndIf
     Exit Sub 																															!
GRNP.ERR1:																																	!
    G.Err% = 0 																															!
    Resume 																																	!
End Sub 																																		!

Sub Graba.Consec.Fiscal(ytpf$, ynpr$, ycfc$, yCaja$)
String X.Key$, ytpf$, ynpr$, ycfc$, yCaja$, X.Lec$
String Gr.Fis.Numer$(2)
Integer*4 PP
Integer*1 Gr.Fel.Error%

On Error GoTo FEL.ERR1

!Call ADXSERVE(PP,26,1,"FIS:"+yCaja$+" "+ytpf$+" "+ycfc$)
!WAIT ; 1500

 Dim Gr.Fis.Numer$(3,8)
 Gr.Fel.Error% = -1
 X.KEY$ = RIGHT$("0000"+yCaja$,4)															!
 X.KEY$ = PACK$(X.KEY$)																							!
 X.Lec$ = "C2 C4 3C5 C7 2C3 C4 3C5 C7 2C3 C4 3C5 C7 2C3"            ! Nuevo Formato 05Abr2024 FECO
 Read FORM X.LEC$;#NF.SESION1% KEY X.KEY$;            \! Busca Terminal
            X.KEY$, 							  \! Numero de la terminal
            Gr.Fis.Numer$(1,1),     \! Prefijo factura fiscal          
            Gr.Fis.Numer$(1,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(1,3),     \! Rango factura final             
            Gr.Fis.Numer$(1,4),     \! Numero actual factura           
            Gr.Fis.Numer$(1,5),     \! Numero resolucion               
            Gr.Fis.Numer$(1,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(1,7),     \! Fecha final resolucion          
            Gr.Fis.Numer$(2,1),     \! Prefijo factura contingencia    
            Gr.Fis.Numer$(2,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(2,3),     \! Rango factura final             
            Gr.Fis.Numer$(2,4),     \! Numero actual factura           
            Gr.Fis.Numer$(2,5),     \! Numero resolucion               
            Gr.Fis.Numer$(2,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(2,7),     \! Fecha final resolucion          
            Gr.Fis.Numer$(3,1),     \! Prefijo notas credito           
            Gr.Fis.Numer$(3,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(3,3),     \! Rango factura final             
            Gr.Fis.Numer$(3,4),     \! Numero actual factura           
            Gr.Fis.Numer$(3,5),     \! Numero resolucion
            Gr.Fis.Numer$(3,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(3,7)       ! Fecha final resolucion          
 If Gr.Fel.Error% = -1 Then Begin
 	  If ytpf$ = "01" Then \
 	   Gr.Fis.Numer$(1,4) = Pack$(Right$("0000000000"+ycfc$,10))	 						! Actualiza consecutivo Electronico
 	  If ytpf$ = "02" Then \
 	   Gr.Fis.Numer$(2,4) = Pack$(Right$("0000000000"+ycfc$,10))	 						! Actualiza consecutivo Electronico contingencia
 	  If ytpf$ = "03" Then \
 	   Gr.Fis.Numer$(3,4) = Pack$(Right$("0000000000"+ycfc$,10))	 						! Actualiza consecutivo notas credito
 	  Gr.Fel.Error% = -1
    Write Form X.LEC$;#NF.SESION1%;                								   \! Graba numero de terminal
            X.KEY$,                 \! Numero de terminal
            Gr.Fis.Numer$(1,1),     \! Prefijo factura fiscal          
            Gr.Fis.Numer$(1,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(1,3),     \! Rango factura final             
            Gr.Fis.Numer$(1,4),     \! Numero actual factura           
            Gr.Fis.Numer$(1,5),     \! Numero resolucion               
            Gr.Fis.Numer$(1,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(1,7),     \! Fecha final resolucion          
            Gr.Fis.Numer$(2,1),     \! Prefijo factura contingencia    
            Gr.Fis.Numer$(2,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(2,3),     \! Rango factura final             
            Gr.Fis.Numer$(2,4),     \! Numero actual factura           
            Gr.Fis.Numer$(2,5),     \! Numero resolucion               
            Gr.Fis.Numer$(2,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(2,7),     \! Fecha final resolucion          
            Gr.Fis.Numer$(3,1),     \! Prefijo notas credito           
            Gr.Fis.Numer$(3,2),     \! Rango factura inicial           
            Gr.Fis.Numer$(3,3),     \! Rango factura final             
            Gr.Fis.Numer$(3,4),     \! Numero actual factura           
            Gr.Fis.Numer$(3,5),     \! Numero resolucion                           
            Gr.Fis.Numer$(3,6),     \! Fecha inicial resolucion        
            Gr.Fis.Numer$(3,7)       ! Fecha final resolucion      
 	  
 EndIf
Exit Sub 
FEL.ERR1:
    Gr.Fel.Error% = 0																												!
    If (ERR = "OE") or (ERR = "FU")  Then Begin         										! Error de apertura
         Resume 																														!
    EndIf 																																	!
    If (ERR = "KF") or (ERR = "EF")  Then Begin         										! Error de keyed / lectura
    	  Resume 																														  !
    EndIf																																		!
    Call CSMLEW01             																							! Process other errors
    If CS.RESUME Then Begin																									!
      Resume                  																							! resume after error
    EndIf Else Begin																												!
      Resume RETRY            																							! retry failing instn.
    EndIf																																		!

End Sub 

Sub FELCSU01.011 Public									 																		! Analisis de trx
Integer*4 Gr.Fel.Error%, X.L%								 																! Definicion de 
String Ue.Proyecto$, Ue.Nota$, X.Lec$, X.KEY$, X.POS$, X.DATA$				 			! variables
String X.caja$, X.trx$, X.Fec$, Xfec$, Xmov$, Xtipo$, xstr$ 								!
String Xtpf$, Xnpr$, Xcfc$, Gpos$, Gneg$
Integer*1 X.IND%									 																					!
Integer*4 PP										 																						!
X.IND% = 0 										 																							!

For X.L% = 1 TO CS.SIZE									         														! Hasta el tamaño del string
  Call CSVFEC02(X.L%)								             														! Tomo tipo de String
  If CS.TYPE(X.L%) = 00 Then Begin 				 			                   					! Si cabecera de la trx
  	 Gr.Fel.Act% = 0
  	 Gr.Fel.NoTax% = 0
  	 Gr.MinFac.Vlr% = 0
     X.Caja$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))       ! Terminal
     X.Trx$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))       ! Transaccion
     X.Fec$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))       ! Fecha y Hora Trx AAMMDDHHmm
     Xtipo$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))       ! Tipo de trx
     Xstr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))       ! numero de strings
     Xstr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(6)+1,CS.FLD.LEN(6)))       ! Operador
     Xstr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(7)+1,CS.FLD.LEN(7)))       ! Password
     Gpos$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(8)+1,CS.FLD.LEN(8)))       ! Gross Pos
     Gneg$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(9)+1,CS.FLD.LEN(9)))       ! Gross Neg
     Gr.MinFac.Vlr% = Val(Gpos$) - Val(Gneg$)
     If Xtipo$ <> "20" Then Begin																						!
        Xmov$   = Right$("0000"+EMSS.STORE$,4)+Right$("0000"+X.Caja$,4)+   \!
                  Right$("0000"+X.Trx$,4) + Right$("0000000000"+X.Fec$,10)  !
     EndIf  																																!
  EndIf   																																	!
  If CS.TYPE(X.L%) = 99 Then Begin 				 			 														! Si User Data
     Ue.Proyecto$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))  ! Codigo de proyecto
     If Val(Ue.Proyecto$) = 31 And Gr.MinFac.Vlr% > 0 Then Begin			      ! Facturacion electronica
    	 Call Valide.Ruta.FE(X.Fec$)					                                ! Directorio de movimientos
       Gr.Fel.Act% = -1
       Gr.Fel.NoTax% = 0
     EndIf 										 																							!
     If Val(Ue.Proyecto$) = 20200617 Then Begin			 									      ! Dia Sin IVA
    	 Call Valide.Ruta.FE(X.Fec$)					                                ! Directorio de movimientos
       Gr.Fel.Act% = 0
       Gr.Fel.NoTax% = -1
     EndIf 										 																							!

!     If Val(Ue.Proyecto$) = 65 Then Begin			 									      			! Facturas Electronicas
!        Xtpf$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2))) 			! Tipo de Factura
!        Xnpr$ = (Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(2))) 						! Prefijo
!        Xcfc$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4))) 			! Numero de factura
!        !Call Graba.Consec.Fiscal(Xtpf$, Xnpr$, Xcfc$,X.Caja$)               ! Actualiza numero fiscal
!     EndIf 										 																							!

  EndIf										         																					!

Next X.L%

If Gr.Fel.Act% = -1 Then Begin 								 															! Trx con minifactura FE
   Gr.Fel.Act% = 0 									 																				! Init variable 
   Call Almacena.FE(X.Caja$, X.Trx$)						         										! Almacena trx
EndIf 											 																								! 

If Gr.Fel.NoTax% = -1 Then Begin 								 														! Trx con dia Sin iva
   Gr.Fel.NoTax% = 0 								 																				! Init variable 
   Call Almacena.NoTax(X.Caja$, X.Trx$)						         										! Almacena trx
EndIf 											 																								! 

End Sub																																			!

Sub FELCSU12.011 Public																											! 
Integer*1 Gr.Fel.Error%																											!
Integer*4 PP																																!
On Error GoTo FEX.ERR																												!
    NP.SESSION%  = 95																												!
    NP.SESSION2% = 96																												!
    NF.SESION1%  = 85																												! Sesion numeracion fiscal
    
    Gr.Np.Close% = 0																												!
    Gr.Fel.Appl$ = "20"																											! Trx en linea
    Gr.Fel.Func$ = "70"																											! Factura electronica
    Gr.Fel.Appl2$ = "20"																										! Trx en linea
    Gr.Fel.Func2$ = "90"																										! Factura electronica dia Sin iva
    Gr.Fel.Error% = -1																											!

!    Open "TF:NMFECO" KEYED RECL 98 As NF.SESION1%    								    		! Abre control numeacion fiscal
!    If Gr.Fel.Error% <> -1 Then Begin																				!
!       Call ADXSERVE(PP,26,1,"ERR OPEN TF:NMFECO")
!       WAIT ; 1500
!    EndIf																																		!

    Exit Sub 																																!
FEX.ERR:																																		!
    Gr.Fel.Error% = 0																												!
    If (ERR = "OE") or (ERR = "FU")  Then Begin         										! Error de apertura
         Resume 																														!
    EndIf 																																	!
    If (ERR = "KF") or (ERR = "EF")  Then Begin         										! Error de keyed / lectura
    	  Resume 																														  !
    EndIf																																		!
    Call CSMLEW01             																							! Process other errors
    If CS.RESUME Then Begin																									!
      Resume                  																							! resume after error
    EndIf Else Begin																												!
      Resume RETRY            																							! retry failing instn.
    EndIf																																		!
End Sub 																																		!

Sub FELCSU16.011 Public																											! En proceso de cierre
Integer*4 PP, Gr.Fel.Error%																									!

    Gr.Fel.Error% = -1																											!
    Exit Sub 																																!

End Sub 																																		!
