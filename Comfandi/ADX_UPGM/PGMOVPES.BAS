!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PgMovPes.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Generación interface PES         *
!**************************************************
! Modificaciones:
! Mod 29Jul2021
! Se toman los logs de movimientos pes para generar
! interface de conciliacion PES                
! Desarrollado por Grupo Retail - OVS
!--------------------------------------------------

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ifile$, Ue.Data2$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

%Include ADX_UPGM:BASROUT.J86

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
End Function

Sub Apertura.Archivos
String Xfile$
    Xfile$ = "C:\SAPCORE\"+Ifile$
    Open Xfile$                    AS 1								       ! LOG DE MOVIMIENTOS
    Open "C:\SAPCORE\SALIDA.TXT"   AS 2 Append               ! Archivo Salida
End Sub

Sub Write.Data
String Rbuffer$, X.Lec$, X.Costo$, X.Recibo$, X.Vlr$, X.Signo$
String X.Pago$, X.Caja$, X.Trx$, X.Tmp$, X.Forma$
Integer*4 X.Len%, X.J%, X.Comp%
      Rbuffer$ = Ue.Data2$ + finr$ 
      X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
      X.Lec$ = "C"+Str$(X.len%)								  							  ! Arma estructura de grabacion
      Write form X.Lec$; #2 ; Rbuffer$                  							! Graba registro
End Sub 

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$
String X.Sku$, X.Ean$, X.Status$, XReg$
String Xmonto$, Xced$
  While(1)

      Read #1; XReg$
      If match("RTA:1043",Xreg$,1) > 0 Then Begin												  ! Rta Pago PES
      	 Xmonto$ = Str$(Val(Mid$(Xreg$,73,10)))															! Valor del subsidio
      EndIf

      If match("RTA:1032",Xreg$,1) > 0 Then Begin												  ! Rta Pago PES
      	 Xmonto$ = Str$(Val(Mid$(Xreg$,73,10)))															! Valor del subsidio
      EndIf

      If match("MSG:10381",Xreg$,1) > 0 And                                \!
         Val(Xmonto$) > 0 Then Begin												                ! Confirmacion pago PES
      	 Xced$ = Str$(Val(Mid$(Xreg$,115,18)))
      	 Ue.Data2$ = "2;PES;" +                                            \! Registro pago
                     Mid$(Xreg$,31,4) + ";" +                              \! Centro de costo
                     Mid$(Xreg$,35,6) + ";" +                              \! Nro de terminal
                     Mid$(Xreg$,41,10) + ";" +                             \! Nro de cajero
                     Mid$(Xreg$,57,6) + ";" +                              \! Nro de trx
                     "20" + Mid$(Xreg$,63,10) + "00;" +                    \! AAAAMMDDHHMMS del pago
                     Xced$ + ";" +                                         \! Cedula beneficiario
                     Xmonto$ + ";" + "PAGADO"
         U.Qty% = U.Qty% + 1
         Locate 12,27 : Print Str$(U.Qty%)
         U.Error% = -1 
         Call Write.Data
      EndIf																																	!


     
      If match("MSG:10501",Xreg$,1) > 0 And                                \!
         Val(Xmonto$) > 0 Then Begin												                ! Confirmacion pago PES
      	 Xced$ = Str$(Val(Mid$(Xreg$,115,18)))
      	 Ue.Data2$ = "2;PES;" +                                            \! Registro pago
                     Mid$(Xreg$,31,4) + ";" +                              \! Centro de costo
                     Mid$(Xreg$,35,6) + ";" +                              \! Nro de terminal
                     Mid$(Xreg$,41,10) + ";" +                             \! Nro de cajero
                     Mid$(Xreg$,57,6) + ";" +                              \! Nro de trx
                     "20" + Mid$(Xreg$,63,10) + "00;" +                    \! AAAAMMDDHHMMS del pago
                     Xced$ + ";" +                                         \! Cedula beneficiario
                     Xmonto$ + ";" + "PAGADO"
         U.Qty% = U.Qty% + 1
         Locate 12,27 : Print Str$(U.Qty%)
         U.Error% = -1 
         Call Write.Data
      EndIf																																	!
       
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Conciliacion Pes, Verifique Errores en SAPCORE:SALIDA.TXT"
    Call Msg.Log
    Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** CARGA MASIVA CONCILIACION PES  Ver.1.1****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Julio   29 2021 G.R.***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : SAPCORE:"+Ifile$
   Locate 9, 1: Print "Actualizando Archivo   : SAPCORE:SALIDA.TXT"
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores

Ifile$   = COMMAND$                                             					! Dato del SO

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
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDITMPFZ.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 

  If ERRF% = 2 AND (ERR = "OE" OR ERR = "FU") Then Begin
     Create "C:\SAPCORE\SALIDA.TXT"   AS 2
     Resume 
  EndIf 

  CALL MSG.LOG
  Stop 
