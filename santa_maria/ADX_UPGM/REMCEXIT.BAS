!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : REMCEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Remates                   *
!**************************************************
!*
!Modificaciones:

%Environ C

Integer*1 Global Gr.Remate.Error%, NP.SESSION%, NP.SESSION2%,              \!
                 Gr.Np.Close%, Gr.Remate.InTrx%															!
Integer*4 Global Gr.Consec.Np%, GR.Tmp.Apun%, PP														!
String    Global Gr.Remate.Ruta$, Gr.Remate.Appl$, Gr.Remate.Func$					!
String    Global Gr.Rmt(1), Gr.Store$, Gr.Fiscal$

%INCLUDE EAMCSGLO.J86                                                       ! working storage           
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

Sub Valide.Ruta.Remate(Yfec$)																						    ! Creacion directorio 
String Yfec$, Yruta$																												! para transferencia de archivos
Integer*4 xx
Yruta$ = ""
Yruta$ = "GRQRM:20" + Left$(Yfec$,6)                                   			! Arma fecha 
MkDir Yruta$
Gr.Remate.Ruta$ = ""																												!
Gr.Remate.Ruta$ = Yruta$ + "\"																							!

End Sub 																																		!

Sub Almacena.Remates(Y.Caja$,Y.Trx$,Y.data%)																! Grabacion de trx 
String Y.Caja$, Y.Trx$, X.Line$, X.Nota$, X.Lec$														! Definicion de variables
String Yfile$, YFinr$, Xcab$, X$																						!
Integer*4 X.Len%, G.Err%, Gi%, Y.data%																			!
On Error GoTo GRNP.ERR1																											! Ctrl de errores
     YFinr$ = CHR$(13)+CHR$(10)																							!
     Y.Caja$ = Str$(Val(Y.caja$))																						!
     Y.Trx$  = Str$(Val(Y.trx$))																						!
     If Gr.Np.Close% = 0 Then                                              \!
      YFile$ = Gr.Remate.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".00Q" Else \
      YFile$ = Gr.Remate.Ruta$ + Right$("0000"+Y.Caja$,4) + Right$("0000"+Y.Trx$,4) + ".01Q" 
     X$ = "Grabando:"+Yfile$
     Call ADXSERVE(PP,26,1,X$)
     Wait ; 1200
     G.Err% = -1																														!
     CREATE Yfile$ AS NP.SESSION2%																	        !
     If G.Err% = -1 Then Begin 																							!
     	Gi% = 0
      X.Len% = BigLen(CS.INPUT$)																					  !
      Xcab$  = Gr.Remate.Appl$ + Gr.Remate.Func$                            !
      X.Len% = BigLen(Xcab$)		            															  ! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"										  							    ! Arma estructura de grabacion
      Write form X.Lec$;#NP.SESSION2%;Xcab$, YFINR$                         ! Grabacion del registro

     	For GI% = 1 To Y.data%
        X.Len% = BigLen( Gr.Rmt(Gi%) )																		  !
        Xcab$  = Gr.Rmt(Gi%)                          									  	!
        X.Len% = BigLen(Xcab$)		            														  ! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"										  							  ! Arma estructura de grabacion
        Write form X.Lec$;#NP.SESSION2%;Xcab$, YFINR$                       ! Grabacion del registro
      Next GI%
      Close NP.SESSION2%																									  !
     EndIf																									                !
     Exit Sub 																														  !
GRNP.ERR1:																																  !
    G.Err% = 0 																														  !
    Resume 																																  !
End Sub 																																	  !


Sub REMCSU01.011 Public									 																	  ! Analisis de trx
Integer*4 Gr.Remate.Error%, X.L%								 													  ! Definicion de 
String Ue.Proyecto$, Ue.Nota$, X.Lec$, X.KEY$, X.POS$, X.DATA$				 		  ! variables
String X.caja$, X.trx$, X.Fec$, Xfec$, Xmov$, Xtipo$, xstr$ 						 	  !
String Xdat1$, Xdat2$,Xdat3$,Xdat4$,Xdat5$,Xdat6$,Xdat7$, Xdat8$, X$
String XResumen(2)
Integer*1 X.IND%, Xfnd%						 																				  !
Integer*4 Xpos%, Xj%, Xi%					 																				  !
X.IND% = 0 										 																						  !
Xpos% = 0 : Xfnd% = 0
Dim Gr.Rmt(1000)
Dim Xresumen(700,7)
For X.L% = 1 TO CS.SIZE									         													  ! Hasta el tamaño del string
  Call CSVFEC02(X.L%)								             													  ! Tomo tipo de String
  If CS.TYPE(X.L%) = 00 Then Begin 				 			                   				  ! Si cabecera de la trx
  	 Gr.Remate.InTrx% = 0
     X.Caja$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))       ! Terminal
     X.Trx$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))       ! Transaccion
     X.Fec$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))       ! Fecha y Hora Trx AAMMDDHHmm
     Xtipo$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))       ! Tipo de trx
     Xstr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))       ! Tipo de trx
     If Xtipo$ <> "20" Then Begin																					  !
        Xmov$   = Right$("0000"+GR.STORE$,4)+Right$("0000"+X.Caja$,4)+     \!
                  Right$("0000"+X.Trx$,4) + Right$("0000000000"+X.Fec$,10)  !
    	 Call Valide.Ruta.Remate(X.Fec$)			                                ! Directorio de movimientos
    	 Gr.Fiscal$ = String$(23,"0")																					! Dato fiscal
     EndIf  																																!
  EndIf   																																	!
  If CS.TYPE(X.L%) = 99 Then Begin 				 			 														! Si User Data
     Ue.Proyecto$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))  ! Codigo de proyecto
     If Val(Ue.Proyecto$) = 11 Then Begin																	  ! Dato fiscal
     	  Xdat1$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))     ! Factura Fiscal
     	  Gr.Fiscal$ = Right$("000"+GR.STORE$,3)  +                          \! Nro Tienda
     	               Right$("000"+X.Caja$,3)    +                          \! Nro caja
     	               Right$("0000"+X.Trx$,4)    +                          \! Nro trx
     	               Right$("000000000"+Xdat1$,9)                           ! Nro factura
     EndIf																																	! Fin Dato Fiscal
     If Val(Ue.Proyecto$) = 53    Then Begin    														! Descuentos
       Xdat1$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))      ! Promocion
       Xdat2$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))      ! Dscto Aplicado
       Xdat3$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))      ! Producto
       Xj% = 0
       For XJ% = 1 To Xpos%
         If Val(Xresumen(Xj%,0)) = Val(Xdat3$) Then Begin                   ! Encuentra plu 
         	  Xresumen(Xj%,5) = Str$( Val(Xresumen(Xj%,5)) + Val(Xdat2$) )    ! Actualiza desctos
         EndIf
       Next XJ%
     EndIf
     If Val(Ue.Proyecto$) = 1001  Then Begin    														! Remate productos
     	  Xdat1$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))     ! Item Vendido
        Xdat2$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))     ! Precio unitario
        Xdat3$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))     ! Venta total
        Xdat4$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))     ! Cantidad
        Xdat5$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(6)+1,CS.FLD.LEN(6)))     ! Codigo interno
        Xdat8$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(7)+1,CS.FLD.LEN(7)))     ! Pesable 00.No 01.Si
        Xdat6$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(8)+1,CS.FLD.LEN(8)))     ! Signo operacion 
        Xfnd% = 0 : Xi% = 0
        For XI% = 1 To XPOS%
         If Val(Xresumen(Xi%,0)) = Val(Xdat1$) Then Begin                   ! Si ya almacenado
         	  Xfnd% = -1
         	  If Xdat6$ = "01" Then Begin 
         	  	 Xdat4$ = Str$(Val(Xdat4$) * -1)          										! Anulacion Item
         	  	 Xdat3$ = Str$(Val(Xdat3$) * -1)          										! Anulacion Venta
         	  EndIf
         	  If Xdat8$ = "00" Then                                          \! Venta unitaria
         	     Xresumen(Xi%,1) = Str$( Val(Xresumen(Xi%,1)) + (Val(Xdat4$)*1000) ) Else    ! Actualiza cantidad
         	     Xresumen(Xi%,1) = Str$( Val(Xresumen(Xi%,1)) + Val(Xdat4$) ) 
         	  Xresumen(Xi%,2) = Str$( Val(Xresumen(Xi%,2)) + Val(Xdat3$) )    ! Actualiza vta total
         EndIf
        Next Xi%
        
        If Xfnd% = 0 Then Begin																							! Producto nuevo
     	     Xpos% = Xpos% + 1 
     	     Xresumen(Xpos%,0) = Xdat1$  ! PLU
     	     Xresumen(Xpos%,1) = Xdat4$  ! QTY
     	     Xresumen(Xpos%,2) = Xdat3$  ! VTA TOTAL
     	     Xresumen(Xpos%,3) = Xdat2$  ! PVU
     	     Xresumen(Xpos%,4) = Xdat5$  ! COD INTERNO
     	     Xresumen(Xpos%,5) = "0"     ! DESCTO
     	     Xresumen(Xpos%,6) = "0"     ! BANDEJAS
     	     Xresumen(Xpos%,7) = Xdat8$  ! PESABLE
        	 If Xdat8$ = "00" Then                                          \! Venta unitaria
         	     Xresumen(Xpos%,1) = Str$( (Val(Xdat4$)*1000) ) Else        \! Actualiza cantidad
         	     Xresumen(Xpos%,1) = Str$( Val(Xdat4$) ) 
     	     
     	  EndIf
        Gr.Remate.InTrx% = -1																								! Activa grabacion
     EndIf 										 																							!

     If Val(Ue.Proyecto$) = 1002  Then Begin    														! Venta bandejas
     	  Xdat1$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))     ! Item Vendido
        Xdat2$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))     ! Precio unitario
        Xdat3$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))     ! Venta total
        Xdat4$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))     ! Cantidad
        Xdat5$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(6)+1,CS.FLD.LEN(6)))     ! Codigo interno
        Xdat8$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(7)+1,CS.FLD.LEN(7)))     ! Pesable 00.No 01.Si
        Xdat6$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(8)+1,CS.FLD.LEN(8)))     ! Signo operacion 
        Xfnd% = 0 : Xi% = 0
        For XI% = 1 To XPOS%
         If Val(Xresumen(Xi%,0)) = Val(Xdat1$) Then Begin                   ! Si ya almacenado
         	  Xfnd% = -1
         	  Xdat7$ = "1"
         	  If Xdat6$ = "01" Then Begin 
         	  	 Xdat4$ = Str$(Val(Xdat4$) * -1)          										! Anulacion Item
         	  	 Xdat3$ = Str$(Val(Xdat3$) * -1)          										! Anulacion Venta
         	  	 Xdat7$ = "-1"
         	  EndIf
         	  If Xdat8$ = "00" Then                                          \! Venta unitaria
         	     Xresumen(Xi%,1) = Str$( Val(Xresumen(Xi%,1)) + (Val(Xdat4$)*1000) ) Else    ! Actualiza cantidad
         	     Xresumen(Xi%,1) = Str$( Val(Xresumen(Xi%,1)) + Val(Xdat4$) ) 

         	  Xresumen(Xi%,2) = Str$( Val(Xresumen(Xi%,2)) + Val(Xdat3$) )    ! Actualiza vta total
         	  Xresumen(Xi%,6) = Str$( Val(Xresumen(Xi%,6)) + Val(Xdat7$) )    ! Actualiza total bandejas
         EndIf
        Next Xi%
        
        If Xfnd% = 0 Then Begin																							! Producto nuevo
     	     Xpos% = Xpos% + 1 
     	     Xresumen(Xpos%,0) = Xdat1$  ! PLU
     	     Xresumen(Xpos%,1) = Xdat4$  ! QTY
     	     Xresumen(Xpos%,2) = Xdat3$  ! VTA TOTAL
     	     Xresumen(Xpos%,3) = Xdat2$  ! PVU
     	     Xresumen(Xpos%,4) = Xdat5$  ! COD INTERNO
     	     Xresumen(Xpos%,5) = "0"     ! DESCTO
     	     Xresumen(Xpos%,6) = "1"     ! BANDEJAS
     	     Xresumen(Xpos%,7) = Xdat8$  ! PESABLE     	     
        	 If Xdat8$ = "00" Then                                          \! Venta unitaria
         	     Xresumen(Xpos%,1) = Str$( (Val(Xdat4$)*1000) ) Else        \! Actualiza cantidad
         	     Xresumen(Xpos%,1) = Str$( Val(Xdat4$) ) 
     	     
     	  EndIf
        Gr.Remate.InTrx% = -1																								! Activa grabacion
     EndIf 										 																							!

     
  EndIf										         																					!
Next X.L%

If Gr.Remate.InTrx% = -1 Then Begin 								 												! Trx con remates
   Gr.Remate.InTrx% = 0 								 																		! Init variable 
   XI% = 0 : XJ% = 0 
   XJ% = XPOS% : XPOS% = 0
   For XI% = 1 To XJ%
       Xdat6$ = "00"
       If Val(Xresumen(Xi%,1)) < 0 Then Begin
       	  Xresumen(Xi%,1) = Str$( Val(Xresumen(Xi%,1)) * -1)
       	  Xresumen(Xi%,2) = Str$( Val(Xresumen(Xi%,2)) * -1)
       	  Xdat6$ = "01"
       EndIf
       If Val(Xresumen(Xi%,1)) <> 0 Then Begin															! Hay movimiento
       	 Xpos% = Xpos% + 1
   	     Xdat7$ = Right$("000"+Str$(Xi%),3)       +                        \! Numero registro      Asc 3
    	           Gr.Fiscal$                       +                        \! Factura Fiscal       Asc 15
    	           Right$("0000000000"+X.Fec$,10)   +                        \! Fecha y hora compra  Asc 10
    	           Right$("000000000000"+Xresumen(Xi%,0),12) +               \! Item                 Asc 12
    	           Right$("000000000000"+Xresumen(Xi%,4),12) +               \! Codigo interno       Asc 12
    	           Right$("0000000000"+Xresumen(Xi%,1),10)   +               \! Cantidad vendida     Asc 10
                 Right$("0000000000"+Xresumen(Xi%,3),10)   +               \! Precio unitario      Asc 10
                 Right$("0000000000"+Xresumen(Xi%,2),10)   +               \! Venta total          Asc 10 
                 Right$("00000"+Xresumen(Xi%,6),5)         +               \! Cantidad bandeja     Asc 5
                 Right$("0000000000"+Xresumen(Xi%,5),10)   +               \! Total descuento      Asc 10
                 Xdat6$                                                     ! Signo operacion      Asc 2 00.Venta 01.Anul
     	  Gr.Rmt(Xpos%) = Xdat7$																						  ! almacena para grabar
     	 EndIf																																! fin hay movimiento
   Next Xi%
   Call Almacena.Remates(X.Caja$,X.Trx$, Xpos%)                             ! Almacena registro
EndIf 											 																								! 


End Sub																																			!

Sub REMCSU12.011 Public																											! 
Integer*1 Gr.Remate.Error%																									!
String X$
On Error GoTo FEX.ERR																												!
    NP.SESSION%  = 95																												!
    NP.SESSION2% = 96																												!
    Gr.Np.Close% = 0																												!
    Gr.Remate.Appl$ = "30"																									! 
    Gr.Remate.Func$ = "10"																									! Remates productos
    Gr.Remate.Error% = -1																										!
    X$ = STRING$(80,CHR$(0))                                                ! SET UP STRING FOR ADXSERVE
    CALL ADXSERVE(PP,4,0,X$)                                                ! GET STATUS DATA
    GR.STORE$ = LEFT$(X$,4)                                                 ! ISOLATE STORE NUMBER
    X$ = "Modulo Remates Ver. 1.0 30/Mzo/2023"
    CALL ADXSERVE(PP, 26, 0, X$)
    Wait ; 1200
    Exit Sub 																																!
FEX.ERR:																																		!
    Gr.Remate.Error% = 0																										!
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

Sub REMCSU16.011 Public																											! En proceso de cierre
Integer*4 Gr.Remate.Error%																							    !

    Gr.Remate.Error% = -1																										!
    Exit Sub 																																!

End Sub 																																		!
