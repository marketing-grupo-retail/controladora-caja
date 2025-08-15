
%ENVIRON T
               
%INCLUDE ADX_UPGM:EAMTSCVA.J86
%INCLUDE ADX_UPGM:EAMTSWKG.J86          ! working storage
%INCLUDE ADX_UPGM:EAMTRANS.j86          ! 
%INCLUDE ADX_UPGM:EAMTERMS.J86          ! 
%INCLUDE ADX_UPGM:EAMITEMR.J86          ! Variables Eamitemr

%INCLUDE ADX_UPGM:EAMP4VAJ.J86 			  ! 4610 Printer
%INCLUDE ADX_UPGM:EAMSOPTS.J86				! Definicion de Store Options    

%INCLUDE ADX_UPGM:EAMTOPTS.J86	         ! Variables de la terminal options
%INCLUDE ADX_UPGM:EAMTSXHC.J86          ! Rutinas de Assembler
%INCLUDE ADX_UPGM:EAMASMRT.J86          ! Rutinas de Assembler


Sub TSDSEC01 EXTERNAL          ! display routine
End Sub

Sub TSPREC01 EXTERNAL          ! print routine
End Sub

SUB TSTCEC01 EXTERNAL          ! tender cashing
END SUB                        !

SUB TSCSEC10 EXTERNAL          ! non sales initialization
END SUB                        !

Sub INIT.TRX External                 ! Inicializacion de una transaccion
End Sub                        

Sub Pin.Init.Secuencia.Entrada Public 
   Dim  TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
   TS.IO.MOTORKEY = 73   
End Sub 

Sub Pin.Init.Trx Public
      CALL Pin.Init.Secuencia.Entrada
      TS.NS.VOID = -1                                                            ! 
      Call TSCSEC10                                                              ! Genera Anulacion Total
      Call INIT.TRX																														   ! Inicializa transaccion
      Call Dim.SL.STR(GC.SL.SIZE + 10)																					 ! 
End Sub

Sub pruebapos(Xi%) Public
Integer*4 Xi%

If xi% = 2 Then begin
   TS.LINETYPE = 18																												  ! Genera cabecera 
   TS.LINEDATA = 1																												  ! en proximo tiquete

Endif

If Xi% = 23 Then begin
	 If MATCH("COMFANDI",TS.DISP1$,1) > 0 Then BEGIN
	 	  !Write #34 ; "LINETYPE  "+STR$(TS.LINETYPE) + CHR$(10)
	 	  !Write #34 ; "LINEDATA  "+STR$(TS.LINEDATA) + CHR$(10)
	 	  !Write #34 ; "LINEDATA2 "+STR$(TS.LINEDATA2) + CHR$(10)
	 ENDIF
endif 

If Xi% = 14 Then begin 
	 If TS.IO.MOTORKEY = 255 Then BEGIN 
               Dim TS.IO.DATA$(10) : DIM TS.IO.KEYS(10) 									  ! Init vectores de carga
               TS.IO.DATA$(5) = "1"																					! Secuencia cambio a efectivo
               TS.IO.KEYS(5)  = 61																					!
               TS.IO.MOTORKEY = 61																					!
         	     CALL TSTCEC01
               WAIT ; 2000
               Call Pin.Init.Trx																						! Init Trx

               To.USEREXIT(20) = -1																					!
               To.USEREXIT(60) = -1																					!
               TS.IO.STATE    = 10            															! PREPARE FOR NORMAL SALES STATE
         	     TS.LINETYPE = 6 																						  ! Genera cabecera 
               TS.LINEDATA = 13																				      ! en proximo tiquete
               TS.LINEDATA2 = 3
               Call TSDSEC01
               
         	     Exit Sub
    ENDIF 
EndIf

End Sub 