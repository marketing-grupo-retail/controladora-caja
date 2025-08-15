!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MRVCEXIT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generacion Movimientos Reservas  *
!**************************************************
!*
!Modificaciones:

%Environ C

Integer*1 Global Gr.Error%, NP.SESSION%, NP.SESSION2%, Gr.Np.Act%
Integer*4 Global Gr.Consec.Np%
String    Global Gr.File.Ruta$, Gr.Appl$, Gr.Func$, Ue.Data$, Pagos$(1), Ue.Almacen$, CA.ST.DATETIME$

%INCLUDE EAMCSGLO.J86          ! working storage
%INCLUDE EAMADXRT.J86	       ! rutinas de la aplicacion
%INCLUDE EAMASMCT.J86          ! Rutinas asembler
%INCLUDE BASROUT.J86           ! Rutinas del basrout

Sub CSMLEW01 External          ! Common error routine
End Sub

Function CSVFEC02( P1 ) EXTERNAL   ! Lee la informacion del string 
Integer*2 P1                                                   
End Function

Sub CSRDEC07  EXTERNAL
End Sub

 SUB CSWREC07  EXTERNAL
 END SUB


Sub Graba.Registro
String Rbuffer$, X.Lec$, Finr$, Ue.Salida$, X.Tmp$
Integer*4 X.Len%, Gr.Error%
 On Error GoTo RSV.ERR							! Control de errores
      Finr$ = CHR$(13)+CHR$(10)
      Gr.Error% = -1
      Ue.Salida$ = "ADX_UDT1:IRSH"+RIGHT$(DATE$,4)+"."+Ue.Almacen$
      Open Ue.Salida$ AS NP.SESSION% Append 
      If GR.ERROR% <> -1 Then Begin 
         Create Ue.Salida$ AS NP.SESSION%
         X.Tmp$ = "01"           + \ Caracter Fijo
                  "8903032085"   + \ Nit recaudadora
                  "20"+date$     + \ Fecha generacion archivo
                  Left$(Time$,6) + \ Hora de Generacion del reporte
                  String$(92," ")  ! Filler
         Rbuffer$ = X.Tmp$
         X.Len% = Len(Rbuffer$)						 ! Toma longitud del registro
         X.Lec$ = "C"+Str$(X.len%)+" C2"				 ! Arma estructura de grabacion
         Write form X.Lec$; #NP.SESSION% ; Rbuffer$, Finr$       	 ! Graba registro
      EndIf 
      Rbuffer$ = Ue.Data$
      X.Len% = Len(Rbuffer$)						 ! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)+" C2"					 ! Arma estructura de grabacion
      Write form X.Lec$; #NP.SESSION% ; Rbuffer$, Finr$       		 ! Graba registro
      Close NP.SESSION%
      Exit Sub 
RSV.ERR:
    Gr.Error% = 0
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

Sub Almacena.Pago(Ytipo$, YVlr$)
Integer*2 Xpos%
String Ytipo$, Yvlr$
Xpos% = Val(Ytipo$)
Pagos$(Xpos%) = Str$( Val(Pagos$(Xpos%)) + Val(Yvlr$) )
End Sub 

Sub Graba.Mrv(Y.caja$,Y.trx$,Y.Fec$,Y.Hor$,Y.Docu$,Y.Vlr$)                             !
String Y.caja$,Y.trx$,Y.Fec$,Y.Hor$,Y.Docu$,Y.Vlr$, Y.Alm$
String PARM2$, Ydata$
Integer*2 PARM1,RET
       Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
       Y.Alm$ = Right$("000"+STR$(Val(Mid$(PARM2$,1,4))),3)
       Ue.Almacen$ = Right$("000"+Y.Alm$,3)
       Y.Docu$ = Right$("0000000000"+Y.Docu$,10)        ! Numero de documento
       Y.Vlr$  = Right$("0000000000"+Y.Vlr$,10)         ! Valor de la reserva
       YDATA$  = "02"                                 + \ Tipo de registro
                 Y.Docu$                              + \ Numero de referencia
                 Y.Fec$                               + \ Fecha de registro
                 Y.Hor$+"00"                          + \ Hora del movimiento
                 Y.Vlr$                               + \ Valor del recaudo
                 Right$("000000"+Y.Alm$,6)            + \ Codigo del almacen
                 Right$("000000"+Y.Caja$,6)           + \ Codigo de la terminal
                 Right$("0000000000"+Y.Trx$,10)         ! Numero de transaccion
       Ue.Data$ = Ydata$																! Entrega String armado                 
End Sub 										 														!

Sub MRVCSU01.011 Public									 ! Analisis de trx
Integer*4 Gr.Error%, X.L%								 ! Definicion de 
String Ue.Proyecto$, Ue.Nota$, X.Lec$, X.KEY$, X.POS$, X.DATA$,X.Tipo$, X.Vlr$           ! Variables
String X.caja$, X.trx$, X.Fec$, Ue.Ean$, Ue.Tipo$,Ue.Docu$,Ue.Vlr$, X.Hor$, Xlista$      !
Integer*1 X.IND%									 !
Integer*4 PP, XI%										 !
For X.L% = 1 TO CS.SIZE									 ! Hasta el tamaño del string
 Call CSVFEC02(X.L%)								         ! Tomo tipo de String
 If CS.TYPE(X.L%) = 00 Then Begin 				 			 ! Si cabecera de la trx
     XLISTA$= ""
     If Gr.Np.Act% = -1 Then Begin 
        XI% = 0
        For XI% = 1 TO 100
          If Val(Pagos$(XI%)) > 0 Then Begin 			! Si pago registrado
             XLISTA$ = XLISTA$ + RIGHT$("00"+STR$(XI%),2) + \
                       RIGHT$("0000000000"+PAGOS$(XI%),10)
          EndIf 
        Next XI%
        XLISTA$ = LEFT$(XLISTA$+STRING$(60,"0"),60)
        Ue.Data$ = Ue.Data$ + Xlista$
        Call Graba.Registro
     EndIf 
  Dim Pagos$(100)
  For XI% = 1 to 100
      Pagos$(XI%) = "0"
  Next XI%
  Gr.Np.Act% = 0 
  Ue.Data$ = ""
  X.Caja$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))                    ! Terminal
  X.Trx$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))                    ! Transaccion
  X.Fec$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))                    ! Fecha y Hora Trx AAMMDDHHmm
  X.Hor$  = Right$(X.fec$,4)
  X.Fec$  = "20"+Left$(X.Fec$,6)
 EndIf   

  If CS.TYPE(X.L%) = 05 Then Begin 				 			 ! Formas de Pago
     X.Tipo$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))                    ! Terminal
     X.Vlr$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))                    ! Transaccion
     Call Almacena.Pago(X.tipo$,X.vlr$)
  EndIf   

  If CS.TYPE(X.L%) = 06 Then Begin 				 			 ! Anulacion de pagos 
     X.Tipo$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))                    ! Terminal
     X.Vlr$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))                    ! Transaccion
     X.Vlr$  = Str$( Val(X.Vlr$) * -1)
     Call Almacena.Pago(X.tipo$,X.vlr$)
  EndIf   

  If CS.TYPE(X.L%) = 09 Then Begin 				 			 ! Registro de vueltas
     X.Tipo$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))                    ! Terminal
     X.Vlr$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))                    ! Transaccion
     X.Vlr$  = Str$( Val(X.Vlr$) * -1)
     Call Almacena.Pago(X.tipo$,X.vlr$)
  EndIf   
  
  If CS.TYPE(X.L%) = 99 Then Begin 				 			 ! Si User Data
     Ue.Proyecto$ = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(1)+1,CS.FLD.LEN(1)))               ! Codigo de proyecto
     If Val(Ue.Proyecto$) = 20090105 Then Begin 					 ! Recaudo Reserva hotelera
        Ue.EAN$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(2)+1,CS.FLD.LEN(2)))               ! EAN
        Ue.Tipo$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(3)+1,CS.FLD.LEN(3)))               ! TIPO PROCESO
        Ue.Docu$  = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(4)+1,CS.FLD.LEN(4)))               ! NUMERO DE DOCUMENTO
        Ue.Vlr$   = UnPack$(Mid$(CS.INPUT$,CS.FLD.POS(5)+1,CS.FLD.LEN(5)))               ! VALOR DEL RECIBO
        Gr.Np.Act% = -1									 !
        Call Graba.Mrv(X.caja$,x.trx$,X.Fec$,X.Hor$,Ue.Docu$,Ue.Vlr$)                    !
     EndIf 										 !
  EndIf										         !
Next X.L%
								 ! 
End Sub

Sub MRVCSU12.011 Public
     Dim Pagos$(100)
     NP.SESSION%  = 95
End Sub 

