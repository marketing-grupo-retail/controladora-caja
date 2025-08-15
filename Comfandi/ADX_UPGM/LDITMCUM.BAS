!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : LDITMCUM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion masiva Articulos    *
!                Codido CUM                       *
!**************************************************
! Modificaciones:
! Mod 21Oct2012
! Se ajusta archivo de entrada para lectura de 
! longitud fija.
! desarrollado por grupo Retail - OVS
!--------------------------------------------------
! Mod 29Ene2013
! Se modifica programa, para que cada vez que se ejecute
! se cree el archivo TF:ITMCUM en ceros, proceso
! solicitado por Freddy Guerrero
! Desarrollado Grupo Retail - OVS
!-------------------------------------------------

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$
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
    Open "ADX_UDT1:LDITMCUM.TXT"   AS 1								       ! Plano maestro de carga

    Create PosFile "TF:ITMCUM" KEYED 6,,,100000 RECL 78 AS 4 COMPOUND PERUPDATE     ! Add 29Ene2013

!    Open "TF:ITMCUM" KEYED RECL 78 AS 4				  					   ! Apertura Articulos controlados Mod 29ene2013

    Create "ADX_UDT1:CARGACUM.TXT" AS 2                      ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
String U.buffer$, U.Iva$, U.Dsct$, Lectura$
String X.Itm$, X.NAME$, X.Status$, X.CUM$
  While(1)
      !Read #1; X.Itm$, X.NAME$, X.CUM$, X.Status$
      
      Read #1; Line Lectura$
      
      X.Itm$    = Mid$(Lectura$,1,7)    ! Plu SMA
      X.NAME$   = Mid$(Lectura$,8,60)   ! Descripcion larga
      X.CUM$    = Mid$(Lectura$,68,20)  ! Codigo CUM
      X.Status$ = Mid$(Lectura$,88,1)   ! Accion a realizar
      
      U.Qty% = U.Qty% + 1
      Locate 12,27 : Print Str$(U.Qty%)
      U.Error% = -1 
      
      X.Itm$   = Str$(Val(X.Itm$))
      X.Itm$   = Pack$(Right$("000000000000"+X.Itm$,12))
      X.NAME$  = Left$(X.NAME$+String$(60," "),60)
      X.CUM$   = Left$(X.CUM$+String$(12," "),12)
      
      If Val(X.Status$) <> 3 Then Begin 
        WRITE FORM "C6 C12 C60" ; #4;          \! Graba el registro en archivo de notas
          X.Itm$ ,                             \! Articulo controlado        UPD 6
          X.Cum$,                              \! Codigo CUM articulo        Asc 12
          X.Name$                               ! Descripcion del producto   Asc 60
       EndIf
       
       If Val(X.Status$) = 3 Then Begin 
       	  DelRec 4 ; X.Itm$
       EndIf 
       
       If U.Error% = -1 Then Begin
         U.QtyOk% = U.QtyOk% + 1 
         Locate 13,27 : Print Str$(U.QtyOk%)
       EndIf Else Begin
         U.QtyNok% = U.QtyNok% + 1
         Locate 14,27 : Print Str$(U.QtyNok%)
         Write #2; "Linea :"+Str$(U.Qty%),Unpack$(X.Itm$),UE.MSG$,FINR$
       EndIf    
  Wend
End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Notas, Verifique Errores en ADX_UDT1:CARGACUM.TXT"
    Call Msg.Log
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA MASIVA CODIGOS CUM MEDICAMENTOS  Ver.1.1****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Febrero    04 2013 G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : ADX_UDT1:LDITMCUM.TXT"
   Locate 9, 1: Print "Actualizando Archivo   : TF:ITMCUM   "
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
     UE.MSG$ = "ARCHIVO ADX_UDT1:LDITMCUM.TXT NO EXISTE... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 4 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 AND (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "FALLA CREACION ARCHIVO MAESTRO DE CODIGO MEDICAMENTOS..."
     Call Msg.Log
     Stop
  EndIf 
  CALL MSG.LOG
  Stop 
