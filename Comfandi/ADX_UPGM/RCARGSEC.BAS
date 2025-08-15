!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RCARGSEC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Reverso Dscto x Depto            *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, IFILE$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%

Sub MSG.LOG
    Locate 23,1 : Print String$(78," ") 
    Locate 23,1 : Print "Msg: "+UE.MSG$
End Sub 

FUNCTION TRADUCE.ERROR                                       !
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
END FUNCTION


Sub Apertura.Archivos
    Open IFILE$ AS 1												              								! Plano maestro de carga
    Open "MKTP:SECCION.DAT" KEYED RECL 84 AS 4            								! Descuento por seccion 
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                   ! Errores proceso de carga
End Sub

Function Validar.Ean(X.Tmp$)
Integer*1 Validar.Ean
Integer*4 U.I%
String    X.tmp$, x.a$
   If Len(X.tmp$) > 12 Then Begin
      Validar.Ean = 0 
      Exit Function      
   EndIf 
   
   For U.I% = 1 to Len(X.tmp$)
       X.A$ = MID$(X.tmp$,U.I%,1) 
       If Match(X.A$,"0123456789",1) <= 0 Then Begin 
          Validar.Ean = 0 
          Exit Function
       EndIf Else Validar.Ean = -1
   Next U.I%
   
End Function 
!--- Validacion del EAN

Sub Carga.Masiva.Datos
 String U.Secc$, U.Cupon$, U.dat$, UE.DIA$(1), UE.HORA$(2), Xlec$, U.Promo$, U.Clte$
  Dim UE.DIA$(10)
  Dim UE.HORA$(10,2)
  U.Qty2% = 0 
  While (1)																												! Mientras no EOF
    Read #1; U.SECC$, U.Cupon$, U.dat$, \
             UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \ Lunes
             UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \ Martes 
             UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \ Miercones
             UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \ Jueves
             UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \ Viernes
             UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \ Sabado
             UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2), \ Domingo
             U.Clte$, U.Promo$                         ! Codigo de la promocion 
            
     U.Qty2% = U.Qty2% + 1
     Locate 12,27 : Print Str$(U.Qty2%)
     U.Clte$  = (Right$("0000"+U.Clte$,4))                             ! Cliente dirigido Add 12 Jun 2011
     U.SECC$  = Pack$("1"+Right$("000000000000000"+U.SECC$,15)) + U.Clte$		! Arma llave
     U.Error% = -1
     DelRec 4 ; U.SECC$                                                     ! Borrado Descuento por Seccion 
     If U.Error% = -1 Then Begin
       U.QtyOk% = U.QtyOk% + 1 
       Locate 13,27 : Print Str$(U.QtyOk%)
     EndIf Else Begin
       U.QtyNok% = U.QtyNok% + 1
       Locate 14,27 : Print Str$(U.QtyNok%)
       Write #2; Unpack$(U.Secc$),UE.MSG$,FINR$
     EndIf    
  Wend 																													  ! Fin del ciclo de carga

End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Articulos, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 4 : Delete 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   REVERSO CARGA  DESCUENTOS POR DEPARTAMENTO    ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Agosto 12  2.011   G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : SECCION.DAT"
   Locate 12,1: Print "Registros Procesados   : "
   Locate 13,1: Print "Registros Actualizados : " 
   Locate 14,1: Print "Registros No Procesados: " 
   Locate 23,1: Print "Msg : "
   FINR$ = CHR$(13) + CHR$(10)   ! Marca de fin de registro
End Sub


!---
!------- Bloque Principal
!---

On Error GoTo Errores.Aplicativo																					! Control de proceso de errores
IFILE$ = COMMAND$																												  ! Nombre archivo de carga
Call Pantalla.Principal																									  ! Pantalla Principal
Call Apertura.Archivos																										! Apertura de archivos
Call Carga.Masiva.Datos																										! Carga Masiva de Informacion


Errores.Aplicativo:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  
  If ERR = "IH" Then Resume 
  If ERR = "IO" Then Resume
  
  If ERRF% = 1 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO "+IFILE$+" NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "ARCHIVO DSCTO POR DEPTO NO EXISTE... "
     Call Msg.Log
     Stop        
  EndIf 
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume
  EndIf   
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop

