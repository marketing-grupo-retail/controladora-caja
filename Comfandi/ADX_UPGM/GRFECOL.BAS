!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : FECOCOL.BAS                      *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Definicion de Numeracion Fiscal  *
!                FECO - COLOMBIA                  *
!**************************************************
! Modificaciones
! Ver 1.0 - 2 Abr 2024
!
! Mod 28May2024
! Se cambia la tecla de borrado de registros pasando
! de F4 a F8
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%ENVIRON C		                 																							! Ambiente de controlador

!--- Definicion de Variables de trabajo

String    Global U.SERIAL$, U.PREF$, U.AUT$, U.FEC$, U.TMP$, U.Aut2$,U.FECV$ !
String    Global Gr.Fiscal(2)
Integer*4 Global U.INI%, U.FIN%, U.ACT%																			 !
Integer*2 Global GI%
String    Global U.Opt$, U.Numera$

%INCLUDE FISPVARI.BAS				  	   																					! Variables programa
%INCLUDE ADX_UPGM:DMEXTR.J86          				   														! Inclucion Libreria Display Manager
%INCLUDE FISPRUTI.BAS				  	   																					! Rutinas Comunes

Function QUIT2     					   																							! Funcion salida programa
    Close AREA1%																														!
    Call SETF("0000000")				   																					!
    Call CLRSCR						   																								!
    RET.ERR%= CLSDIS					   																						!
    Call DM.ERR(RET.ERR%,CLSDIS$)			   																		!
    Stop																																		!
End Function																																!
!--- Fin de la funcion de salida

FUNCTION INICIO01
   Call.ORDER% = 60                                           							! Llamado Primera Pantalla D.M
   RET.ERR% = DISPD(Call.ORDER%)                              							! Llamado de la pantalla en DM
   Call DM.ERR(RET.ERR%,DISPD$)																							!
END Function																																!
!--- Fin de la funcion de inicio

Sub CAPTURA.DATOS
String U.Key$
Integer*1 XI%
      U.KEY$ = Pack$(RIGHT$("0000"+DM.OPCION$,4))
      Gr.Fiscal(1,4) = UnPack$(Gr.Fiscal(1,4))
      Gr.Fiscal(2,4) = UnPack$(Gr.Fiscal(2,4))
      Gr.Fiscal(3,4) = UnPack$(Gr.Fiscal(3,4))
!--- DATOS FACTURA ELECTRONICA
      Call MSG.ERR(27,"Prefijo de Facturacion Factura Electronica")					!
      U.PREF$ = GET.DATOS
      IF (ENDF = F3.SALIR) Then Exit SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Exit SUB                      							! Si presiona tecla ESC
      IF (ENDF = F8.KEY) Then Begin
      	  Call MSG.ERR(27,"Eliminando Terminal "+DM.OPCION$)
      	  WAIT ; 2500
      	  Delrec AREA1% ; U.KEY$
      	  Exit Sub 
      EndIf
      While U.PREF$ = " " 
         Call MSG.ERR(27,"Prefijo de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.PREF$ = GET.DATOS                               									! Asigna valor capturado
         IF (ENDF = F3.SALIR) Then Exit SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Exit SUB                   							! Si presiona tecla ESC
      Wend
      Gr.Fiscal(1,1) = U.Pref$

      NUM.INICIAL.ELEC:
      Call MSG.ERR(27,"Rango Inicial de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Inicial de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      Gr.Fiscal(1,2) = U.Tmp$
      U.INI% = Val(U.Tmp$)

      Call MSG.ERR(27,"Rango Final de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Final de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.FIN% = VAL(U.TMP$)
      IF U.FIN% < U.INI% THEN BEGIN 
         Call POSF(4)
         GoTo NUM.INICIAL.ELEC
      ENDIF
      Gr.Fiscal(1,3) = U.Tmp$
      
      Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
      U.AUT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.AUT$ = " " 
         Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.AUT$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      WEND
      Gr.Fiscal(1,5) = U.Aut$

      Call MSG.ERR(27,"Rango Actual Factura Electrónica")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Actual Factura Electrónica")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.Act% = VAL(U.TMP$)
      IF (U.Act% < U.INI%) Or (U.Act% > U.FIN%) THEN BEGIN 
         Call POSF(4)
         GoTo NUM.INICIAL.ELEC
      ENDIF
      Gr.Fiscal(1,4) = U.Tmp$

      Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
      U.FEC$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FEC$ = " " 
         Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FEC$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(1,6) = U.fec$

      Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
      U.FECV$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FECV$ = " " 
         Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FECV$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(1,7) = U.fecv$
      
      If Val(U.FECV$) < Val(U.FEC$) Then Begin
      	 	  Call MSG.ERR(27,"Error definicion fechas autorizacion fiscal")
            wait ; 3000
            Call POSF(4)
         GoTo NUM.INICIAL.ELEC

      EndIf

!--- NUMERACION CONTINGENCIA
      Call MSG.ERR(27,"Prefijo de Facturacion Factura Contingencia")				!
      U.PREF$ = GET.DATOS
      IF (ENDF = F3.SALIR) Then Exit SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Exit SUB                      							! Si presiona tecla ESC
      While U.PREF$ = " " 
         Call MSG.ERR(27,"Prefijo de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.PREF$ = GET.DATOS                               									! Asigna valor capturado
         IF (ENDF = F3.SALIR) Then Exit SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Exit SUB                   							! Si presiona tecla ESC
      Wend
      Gr.Fiscal(2,1) = U.Pref$

      NUM.INICIAL.CONTIN:
      Call MSG.ERR(27,"Rango Inicial de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Inicial de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      Gr.Fiscal(2,2) = U.Tmp$
      U.INI% = Val(U.Tmp$)

      Call MSG.ERR(27,"Rango Final de Facturacion")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Final de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.FIN% = VAL(U.TMP$)
      IF U.FIN% < U.INI% THEN BEGIN 
         Call POSF(12)
         GoTo NUM.INICIAL.CONTIN
      ENDIF
      Gr.Fiscal(2,3) = U.Tmp$
      
      Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
      U.AUT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.AUT$ = " " 
         Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.AUT$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      WEND
      Gr.Fiscal(2,5) = U.Aut$

      Call MSG.ERR(27,"Rango Actual Factura Contingencia")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Actual Factura Contingencia")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.Act% = VAL(U.TMP$)
      IF (U.Act% < U.INI%) Or (U.Act% > U.FIN%) THEN BEGIN 
         Call POSF(12)
         GoTo NUM.INICIAL.ELEC
      ENDIF
      Gr.Fiscal(2,4) = U.Tmp$


      Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
      U.FEC$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FEC$ = " " 
         Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FEC$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(2,6) = U.fec$

      Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
      U.FECV$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FECV$ = " " 
         Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FECV$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(2,7) = U.fecv$
      
      If Val(U.FECV$) < Val(U.FEC$) Then Begin
      	 	  Call MSG.ERR(27,"Error definicion fechas autorizacion fiscal")
            wait ; 3000
            Call POSF(12)
         GoTo NUM.INICIAL.CONTIN

      EndIf


!--- NUMERACION NOTAS CREDITO

      Call MSG.ERR(27,"Prefijo de Facturacion Notas Credito")				!
      U.PREF$ = GET.DATOS
      IF (ENDF = F3.SALIR) Then Exit SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  Then Exit SUB                      							! Si presiona tecla ESC
      While U.PREF$ = " " 
         Call MSG.ERR(27,"Prefijo de Facturacion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.PREF$ = GET.DATOS                               									! Asigna valor capturado
         IF (ENDF = F3.SALIR) Then Exit SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  Then Exit SUB                   							! Si presiona tecla ESC
      Wend
      Gr.Fiscal(3,1) = U.Pref$

      NUM.INICIAL.NTACRD:
      Call MSG.ERR(27,"Rango Inicial de Nota Credito")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      WHILE U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Inicial de Nota Credito")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      WEND
      Gr.Fiscal(3,2) = U.Tmp$
      U.INI% = Val(U.Tmp$)

      Call MSG.ERR(27,"Rango Final de Nota Credito")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Final de Nota Credito")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.FIN% = VAL(U.TMP$)
      IF U.FIN% < U.INI% THEN BEGIN 
         Call POSF(12)
         GoTo NUM.INICIAL.NTACRD
      ENDIF
      Gr.Fiscal(3,3) = U.Tmp$
      
      Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
      U.AUT$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.AUT$ = " " 
         Call MSG.ERR(27,"Numero Autorizacion Asignado por la DIAN")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.AUT$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      WEND
      Gr.Fiscal(3,5) = U.Aut$

      Call MSG.ERR(27,"Rango Actual Nota Credito")
      U.TMP$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      							! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      							! Si presiona tecla ESC
      While U.TMP$ = " " OR VAL(U.TMP$) <= 0
         Call MSG.ERR(27,"Rango Actual Nota Credito")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.TMP$ = GET.DATOS                                   							! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   							! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   							! Si presiona tecla ESC
      Wend
      U.Act% = VAL(U.TMP$)
      IF (U.Act% < U.INI%) Or (U.Act% > U.FIN%) THEN BEGIN 
         Call POSF(16)
         GoTo NUM.INICIAL.ELEC
      ENDIF
      Gr.Fiscal(3,4) = U.Tmp$

      Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
      U.FEC$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FEC$ = " " 
         Call MSG.ERR(27,"Fecha de Autorizacion de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FEC$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(3,6) = U.fec$

      Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
      U.FECV$ = GET.DATOS
      IF (ENDF = F3.SALIR) THEN EXIT SUB                      ! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN EXIT SUB                      ! Si presiona tecla ESC
      WHILE U.FECV$ = " " 
         Call MSG.ERR(27,"Fecha de Vencimiento  de la Resolucion")
         RET.ERR% = NXTF(-2)
         Call DM.ERR(RET.ERR%,NXTF$)
         U.FECV$ = GET.DATOS                               ! Asigna valor capturado
         IF (ENDF = F3.SALIR) THEN EXIT SUB                   ! Si presiona tecla F3
         IF (ENDF = ESC.KEY)  THEN EXIT SUB                   ! Si presiona tecla ESC
      Wend
      Gr.Fiscal(3,7) = U.fecv$
      
      If Val(U.FECV$) < Val(U.FEC$) Then Begin
      	 	  Call MSG.ERR(27,"Error definicion fechas autorizacion fiscal")
            wait ; 3000
            Call POSF(20)
         GoTo NUM.INICIAL.NTACRD

      EndIf

!--- Grabacion registro control numeracion fiscal 

      DM.OPCION$ = Pack$(RIGHT$("0000"+DM.OPCION$,4))												! Numero de terminal
      !-- Actualizacion FECO Electronica
      If Val(Gr.Fiscal(1,4)) = 0 Then Gr.Fiscal(1,4) = Gr.Fiscal(1,2)
      If Val(Gr.Fiscal(2,4)) = 0 Then Gr.Fiscal(2,4) = Gr.Fiscal(2,2)
      If Val(Gr.Fiscal(3,4)) = 0 Then Gr.Fiscal(3,4) = Gr.Fiscal(3,2)
      Gr.Fiscal(1,1) = Left$(Gr.Fiscal(1,1)+"    ",4)
      Gr.Fiscal(1,2) = Pack$(Right$("0000000000"+Gr.Fiscal(1,2),10))
      Gr.Fiscal(1,3) = Pack$(Right$("0000000000"+Gr.Fiscal(1,3),10))
      Gr.Fiscal(1,4) = Pack$(Right$("0000000000"+Gr.Fiscal(1,4),10))
      Gr.Fiscal(1,5) = Pack$(Right$("00000000000000"+Gr.Fiscal(1,5),14))
      Gr.Fiscal(1,6) = Pack$(Right$("000000"+Gr.Fiscal(1,6),6))
      Gr.Fiscal(1,7) = Pack$(Right$("000000"+Gr.Fiscal(1,7),6))
      !-- Actualizacion FECO Contingencia
      Gr.Fiscal(2,1) = Left$(Gr.Fiscal(2,1)+"    ",4)
      Gr.Fiscal(2,2) = Pack$(Right$("0000000000"+Gr.Fiscal(2,2),10))
      Gr.Fiscal(2,3) = Pack$(Right$("0000000000"+Gr.Fiscal(2,3),10))
      Gr.Fiscal(2,4) = Pack$(Right$("0000000000"+Gr.Fiscal(2,4),10))
      Gr.Fiscal(2,5) = Pack$(Right$("00000000000000"+Gr.Fiscal(2,5),14))
      Gr.Fiscal(2,6) = Pack$(Right$("000000"+Gr.Fiscal(2,6),6))
      Gr.Fiscal(2,7) = Pack$(Right$("000000"+Gr.Fiscal(2,7),6))
      !-- Actualizacion FECO Notas Credito
      Gr.Fiscal(3,1) = Left$(Gr.Fiscal(3,1)+"    ",4)
      Gr.Fiscal(3,2) = Pack$(Right$("0000000000"+Gr.Fiscal(3,2),10))
      Gr.Fiscal(3,3) = Pack$(Right$("0000000000"+Gr.Fiscal(3,3),10))
      Gr.Fiscal(3,4) = Pack$(Right$("0000000000"+Gr.Fiscal(3,4),10))
      Gr.Fiscal(3,5) = Pack$(Right$("00000000000000"+Gr.Fiscal(3,5),14))
      Gr.Fiscal(3,6) = Pack$(Right$("000000"+Gr.Fiscal(3,6),6))
      Gr.Fiscal(3,7) = Pack$(Right$("000000"+Gr.Fiscal(3,7),6))
      
      Write Form "C2 C4 3C5 C7 2C3 C4 3C5 C7 2C3 C4 3C5 C7 2C3"	;#AREA1% ;         \! Grabacion de registros
        DM.OPCION$,         \! Numero de terminal             
        Gr.Fiscal(1,1),     \! Prefijo factura fiscal         
        Gr.Fiscal(1,2),     \! Rango factura inicial          
        Gr.Fiscal(1,3),     \! Rango factura final            
        Gr.Fiscal(1,4),     \! Numero actual factura          
        Gr.Fiscal(1,5),     \! Numero resolucion              
        Gr.Fiscal(1,6),     \! Fecha inicial resolucion       
        Gr.Fiscal(1,7),     \! Fecha final resolucion         
        Gr.Fiscal(2,1),     \! Prefijo factura contingencia   
        Gr.Fiscal(2,2),     \! Rango factura inicial          
        Gr.Fiscal(2,3),     \! Rango factura final            
        Gr.Fiscal(2,4),     \! Numero actual factura          
        Gr.Fiscal(2,5),     \! Numero resolucion              
        Gr.Fiscal(2,6),     \! Fecha inicial resolucion       
        Gr.Fiscal(2,7),     \! Fecha final resolucion         
        Gr.Fiscal(3,1),     \! Prefijo notas credito          
        Gr.Fiscal(3,2),     \! Rango factura inicial          
        Gr.Fiscal(3,3),     \! Rango factura final            
        Gr.Fiscal(3,4),     \! Numero actual factura          
        Gr.Fiscal(3,5),     \! Numero resolucion              
        Gr.Fiscal(3,6),     \! Fecha inicial resolucion       
        Gr.Fiscal(3,7)       ! Fecha final resolucion         

END SUB

Sub VALIDA.DATOS
String U.KEY$, LEC$, XX$
Integer*1 XI%, Xpos%
  U.KEY$ = RIGHT$("0000"+DM.OPCION$,4)
  U.KEY$ = PACK$(U.KEY$)
  BAN.PRG$ = "0"
  Lec$ = "C2 C4 3C5 C7 2C3 C4 3C5 C7 2C3 C4 3C5 C7 2C3"
  READ FORM LEC$;#AREA1% KEY U.KEY$;                        \! Lee Reg Cabecera 
    U.KEY$,             \! Numero de terminal
    Gr.Fiscal(1,1),     \! Prefijo factura fiscal
    Gr.Fiscal(1,2),     \! Rango factura inicial
    Gr.Fiscal(1,3),     \! Rango factura final
    Gr.Fiscal(1,4),     \! Numero actual factura
    Gr.Fiscal(1,5),     \! Numero resolucion
    Gr.Fiscal(1,6),     \! Fecha inicial resolucion
    Gr.Fiscal(1,7),     \! Fecha final resolucion 
    Gr.Fiscal(2,1),     \! Prefijo factura contingencia
    Gr.Fiscal(2,2),     \! Rango factura inicial
    Gr.Fiscal(2,3),     \! Rango factura final
    Gr.Fiscal(2,4),     \! Numero actual factura
    Gr.Fiscal(2,5),     \! Numero resolucion
    Gr.Fiscal(2,6),     \! Fecha inicial resolucion
    Gr.Fiscal(2,7),     \! Fecha final resolucion 
    Gr.Fiscal(3,1),     \! Prefijo notas credito
    Gr.Fiscal(3,2),     \! Rango factura inicial
    Gr.Fiscal(3,3),     \! Rango factura final
    Gr.Fiscal(3,4),     \! Numero actual factura
    Gr.Fiscal(3,5),     \! Numero resolucion
    Gr.Fiscal(3,6),     \! Fecha inicial resolucion
    Gr.Fiscal(3,7)       ! Fecha final resolucion 
  
  IF BAN.PRG$ = "0" THEN BEGIN
   For XI% = 1 To 3
     If XI% = 1 Then Xpos% = 4
     If XI% = 2 Then Xpos% = 12
     If XI% = 3 Then Xpos% = 20
     Call Msg.Err(Xpos%  ,(Gr.Fiscal(XI%,1)))
     Call Msg.Err(Xpos%+1,UnPack$(Gr.Fiscal(XI%,2)))
     Call Msg.Err(Xpos%+2,UnPack$(Gr.Fiscal(XI%,3)))
     Call Msg.Err(Xpos%+3,UnPack$(Gr.Fiscal(XI%,5)))
     Call Msg.Err(Xpos%+4,UnPack$(Gr.Fiscal(XI%,4)))
     Call Msg.Err(Xpos%+5,UnPack$(Gr.Fiscal(XI%,6)))
     Call Msg.Err(Xpos%+6,UnPack$(Gr.Fiscal(XI%,7)))
   Next XI%  
  EndIf Else Begin 
  	U.ACT% = 0
  	Gr.Fiscal(1,4) = Pack$("00")
  	Gr.Fiscal(2,4) = Pack$("00")
  	Gr.Fiscal(3,4) = Pack$("00")
  EndIf
End Sub

!----
!----
!---- Bloque Principal 
!----
!----

ON ERROR GOTO E0101
U.Opt$ = Command$
If Ucase$(U.Opt$) = "VERSION" Then BEGIN
	 Print "Grupo Retail Ltda"
	 !Print "Definicion Numeracion Factura Electronica - Colombia  Ver. 02/Abr/2024"
	 Print "Definicion Numeracion Factura Electronica - Colombia  Ver. 28/May/2024"
	 Stop 
EndIf
Call INICIADM 				                   																		! Inicializacion Variables Display Manager
AREA1% = 1																																	!
ARCH1$ = "TF:NMFECO"																												! Archivo para Comfandi
OPEN ARCH1$ KEYED RECL 98 AS AREA1%                        									! Abre archivo de control
TERM$ = " "					           																							! Inicializo Libreria
RET.ERR%=INITDM(TERM$)					   																					!
Call DM.ERR(RET.ERR%,INITDM$)				   																			!
RET.ERR% = OPNDIS("ADX_UPGM:FISCAL.PNT")	           												! Apertura de la forma de pantalla
Call DM.ERR(RET.ERR%,OPNDIS$)																								!
FIN$="0"																																		!
While (FIN$ = "0") 					   																							! Mientras que no F3/Esc
      DM.OPCION$   = "  "				   																					!
      Call INICIO01					   																							!
      Dim Gr.Fiscal(3,8)																										! Datos numeracion
      MSG$="Digite numero de la terminal"		   															!
      Call MSG.ERR(27,MSG$)				   																				! Mensaje 
      RET.ERR% = NXTF(-20) 			           																	! Primer campo
      Call DM.ERR(RET.ERR%,NXTF$)			   																		!
      ATTR$ = SETF(PRM.ON$)                                									!
      INP$ = UPDF                                          									! Captura dato en pantalla
      DM.OPCION$ = INP$                                    									! Asigna valor capturado
      IF (ENDF = F1.AYUDA) THEN BEGIN                   										!
         Call HELP("Numeracion Fiscal","NUMFIS.TXT") 												!
         Call INICIO01																											!
      ENDIF																																	!
      IF (ENDF = F3.SALIR) THEN Call QUIT2                 									! Si presiona tecla F3
      IF (ENDF = ESC.KEY)  THEN Call QUIT2                 									! Si presiona tecla ESC
      WHILE DM.OPCION$ = " " OR VAL(DM.OPCION$) = 0													!
        RET.ERR% = NXTF(-20)																								!
        Call DM.ERR(RET.ERR%,NXTF$)																					!
        ATTR$ = SETF(PRM.ON$)                                								!
        INP$ = UPDF                                        									! Captura dato en pantalla
        DM.OPCION$ = INP$                                  									! Asigna valor capturado
        IF (ENDF = F1.AYUDA) THEN BEGIN 																		!
           Call HELP("Numeracion Fiscal","NUMFIS.TXT") 											!
           Call INICIO01																										!
        ENDIF																																!
        IF (ENDF = F3.SALIR) THEN Call QUIT2               									! Si presiona tecla F3
        IF (ENDF = ESC.KEY)  THEN Call QUIT2               									! Si presiona tecla ESC
      Wend																																	!
      Call VALIDA.DATOS				           																		! 
      Call CAPTURA.DATOS                                   									! CAPTURA DATOS NUMFISCAL
WEND

!----
!---- Fin del bloque principal
!----

E0101:
         If ERRF% = AREA1% And                                             \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") THEN BEGIN          									! el archivo de control
            CREATE POSFILE ARCH1$ KEYED 2,,,500 RECL 98   						     \! si no existe lo crea.
                        As AREA1% compound perupdate 		   									! 
            Resume                                         									! retorna ejecucion
         ENDIF                                             									! del programa
         If ERRF% = AREA1% AND ERR = "EF" THEN BEGIN                       \! Si encuentra fin de 
            BAN.PRG$ = "1"				   																				! archivo
            Resume
         ENDIF
         If ERRF% = 19 AND ERR = "EF" OR ERR="OE" THEN BEGIN               \! Valida la lectura
            BAN.PRG$ = "1"				       																		! del archivo de 
            Resume 				       																					! help del aplicativo
         ENDIF						       																						!
         Call TRADUCE.ERROR																									!
         MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$						!
         LOCATE 24,1:PRINT MSG$                          										! Presenta Error pantalla
!--- Fin despliegue de errores

!--- Fin del programa FECOCOL.BAS
