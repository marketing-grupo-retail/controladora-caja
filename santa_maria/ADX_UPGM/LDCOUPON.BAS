!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDCOUPON.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Articulos    *
!                para generacion cupones          *
!**************************************************
! Version 1.0 --- 20Nov2023 ---
! Modificaciones:
!--------------------------------------------------
! Mod 13/Feb/2025
! Se controla máximo numero de cupones a entregar
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ue.Data2$, FileOut$, Param2$, \
                  XParam$, Ifile$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
END Function


Sub Apertura.Archivos
    Ifile$ = "PROMODAT:"+Ifile$
    Open Ifile$ As 1																				 ! Carga archivos parámetros bono recompra
    Create "ADX_UDT1:ERRBONOS.TXT" As 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String Rbuffer$, XReg$, XFini$, XFfin$, Xsku$, xvlr$, xfven$
String Xcla$, Xind$, Xnum$, Xtip$, Xcon$, Xmsg$
String Xfinib$, Xfvenb$, Xtcomp$, XvlrMin$, Xtipo$, Xcup$, Xmcup$
Integer*2 Xloc%
  U.Qty% = 0
  While(1)
      Read #1; Line XReg$																										! Lectura de registro
      U.Qty% = U.Qty% + 1																										!
      Locate 11,26 : Print Str$(U.Qty%)																			!
      U.Error% = -1 																												!
      If U.Qty% = 1 Then Begin           																		! Registro de parametros
         If Len(Xreg$) = 43 Then Begin																			! Si registro cabecera OK
         	  Open "TF:ITMCPN" KEYED RECL 9  As 4                  						! Lista items / seccion compra
         	  Open "TF:PCUPON" KEYED RECL 23 As 3                  						! Parametros cupones
            XCup$  = Mid$(Xreg$, 1,6)                                       ! Codigo cupon
            XFini$ = Mid$(Xreg$, 7,8)                                       ! Fecha inicial del evento
            XFfin$ = Mid$(Xreg$,15,8)                                       ! Fecha final   del evento
            Xcla$  = Mid$(Xreg$,23,1)                                       ! Tipo operacion aplicar 1.Valida Item 2. Valida estruc comercial 3. toda la tienda
            Xnum$  = Mid$(Xreg$,24,2)                                       ! Numero de cupones a generar por evento
            XvlrMin$ = Mid$(Xreg$,26,10)                                    ! Valor compra para generar bonos
            Xmsg$  = Mid$(Xreg$,36,6)                                       ! Numero del mensaje a generar
            Xmcup$ = Mid$(Xreg$,42,2)                                       ! Numero maximo cupones a generar
            Xcup$    = (Right$("000000"+Xcup$,6))                           ! Arma llave registro
            XvlrMin$ = (Right$("0000000000"+Xvlrmin$,10))                   ! Valor minimo compra
            xfvenb$  = (Right$("00000000"+Xfvenb$,8))                       ! Fecha vencimiento
            xfinib$  = (Right$("00000000"+Xfinib$,8))                       ! Fecha inicio
            Xnum$    = (Right$("00"+Xnum$,2))                               ! Numero cupones a emitir
            Xmsg$    = (Right$("000000"+Xmsg$,6))                           ! Numero mensaje imprimir
            Xmcup$   = (Right$("00"+Xmcup$,2))                              ! Numero maximo cupones a emitir
            Write Form "C3 2C4 C1 C2 C5 C3 C1" ; #3;                       \! Graba el registro
               Pack$(Xcup$),                                               \! Codigo cupon           PD  3
               Pack$(Xfini$),                                              \! Fecha inicial          PD  4
               Pack$(Xffin$),                                              \! Fecha Final            PD  4
               Xcla$,                                                      \! Tipo validacion        Asc 1
               Xnum$,                                                      \! Numero bonos x evento  Asc 2 
               Pack$(XvlrMin$),                                            \! Valor minimo compra    PD  5 
               Pack$(Xmsg$),                                               \! Numero del mensaje     PD  3
               Pack$(Xmcup$)                                                ! Numero maximo cupones  PD  1

         EndIf Else Begin
      	 	  Write #2; "Linea :"+Str$(U.Qty%),Xreg$,"FALLA REGISTRO CABECERA",FINR$
            UE.MSG$ = "FALLA REGISTRO CABECERA"
            Call MSG.LOG
      	 	  Close 2
      	 	  Stop 
         EndIf
      EndIf
      If U.Qty% >= 2 Then Begin           																	! Lista items / secciones 
      	 Xsku$ = Pack$(Xcup$ + Right$("000000000000"+Xreg$,12))							! Proceso validacion
      	 Write Form "C9" ; #4; Xsku$                                        ! Graba el registro
      EndIf   																															!
      If U.Error% = -1 Then Begin																						! Si proceso OK
         U.QtyOk% = U.QtyOk% + 1 
         Locate 12,27 : Print Str$(U.QtyOk%)
      EndIf Else Begin																											! Proceso fallido
         U.QtyNok% = U.QtyNok% + 1																					!
         Locate 13,27 : Print Str$(U.QtyNok%)																!
         Write #2; "Linea :"+Str$(U.Qty%),Xreg$,UE.MSG$,FINR$								!
      EndIf    																															!
  Wend
End Sub


Sub Auditoria.Bonos
Integer*4 X.Len%
String    X.Lec$, X.data$
    FileOut$ = "ADX_UDT1:CT"+DATE$+".TXT"														! Auditoria cupones tienda
    Open FileOut$ As 51 Append																			! Acceso al archivo
    X.data$ = "CUPON TIENDA APLICADO : 20"+Left$(DATE$,2)+"/"+    \! Reporta la fecha y 
              Mid$(DATE$,3,2)+"/"+Right$(DATE$,2) + " - " +        \! hora de aplicacion
              Left$(TIME$,2)+":"+Mid$(TIME$,3,2)+":"+Right$(TIME$,2)! del evento

    X.Len% = Len(X.data$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; X.data$, Finr$            							! Graba registro

    X.Len% = Len(Xparam$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; Xparam$, Finr$            							! Graba registro
    
    Xparam$ = String$(77,"-")
    X.Len% = Len(Xparam$)						  					  								  ! Toma longitud del registro
    X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
    Write form X.Lec$; #51 ; Xparam$, Finr$            							! Graba registro
    
    Close 51
End Sub 

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Cupones, Verifique ADX_UDT1:ERRBONOS.TXT"
    Call Msg.Log
    Call Auditoria.Bonos
    Close 4 : Close 1 : Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** CARGA MASIVA CUPONES PROMOCIONALES V 1.0**"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Febrero   13 2025 GR.**"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : PROMODAT:"+Ifile$
   Locate 9, 1: Print "Actualizando Archivo   : TF:COMPRA   "
   Locate 10,1: Print "Actualizando Parametros: TF:PBONOS   "
   Locate 11,1: Print "Registros Procesados   : "
   Locate 12,1: Print "Registros Actualizados : " 
   Locate 13,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
Ifile$ = command$
If Ucase$(Ifile$) = "VERSION" Then Begin
  Print "Grupo Retail Ltda"
  Print "CARGA MASIVA CUPONES PROMOCION  Ver 1.1"
  Print "Ultima Revision Software Febrero   13 2025"
  Stop 
EndIf
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion
Call Fin.Proceso
Stop 


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 

  If ERR = "EF" Then Resume 

  If ERRF% = 51 AND (ERR = "OE" OR ERR = "FU") Then Begin
    Create FileOut$ As 51
    Resume
  EndIf 
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO "+Ifile$+" NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 

  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     Create POSFILE "TF:ITMCPN" KEYED 9,,,50000 RECL  9 AS 4
     U.Error% = -1
     Resume 
  EndIf 

  If ERRF% = 3 AND (ERR = "OE" OR ERR = "FU") Then Begin
     Create Posfile "TF:PCUPON" KEYED 3,,,10000 RECL 23 AS 3
     U.Error% = -1
     Resume 
  EndIf 

  CALL MSG.LOG
  Stop 
