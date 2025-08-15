!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RIFASDEF.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion Definicion Rifas    *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, IFILE$, Prm.Clte$, Xfile$
Integer*1  Global U.Error%, Terror%, Topen%
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
    TOpen% = 0
    Again.Procesa:
    Terror% = -1
    Open IFILE$ As 1
    If Terror% = 0 Then Begin 
 	     Wait ; 1800
 	     TOpen% = TOpen% + 1 
 	     If TOpen% > 5 Then Stop 
 	     GoTo Again.Procesa
    Endif 

    Open "TF:A0004" KEYED RECL 8 AS 44            			                ! Definicion de Rifas
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                 ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
 String U.Cod$, U.Name$, U.Fec1$, U.Fec2$, U.Vlr$, U.Bol$, U.Con$, U.Msg$, U.Stat$, Key$, LEC$, U.Promo$
 Integer*1 xi%
  U.Qty2% = 0
  LEC$="C8" 
  While (1)																												! Mientras no EOF
    Read #1; U.Cod$, U.Name$
      U.Qty2% = U.Qty2% + 1
      U.Error% = -1
      U.Cod$ = Ucase$(U.Cod$)
!     If MATCH("TC",U.Cod$,1) <= 0 Then U.Cod$ = "0000"     ! Para todos los clientes 

      U.Cod$  = Right$("0000"+U.Cod$,4)									    ! Codigo promocion
      U.Name$ = Right$("000000000000"+U.Name$,12)           ! Codigo del producto 
      Key$ = Pack$(U.cod$ + U.Name$)
      
      Write FORM LEC$;#44 ; KEY$      											! Articulo Promocionado

     If U.Error% = -1 Then Begin
       U.QtyOk% = U.QtyOk% + 1 
       Locate 13,27 : Print Str$(U.QtyOk%)
     EndIf Else Begin
       U.QtyNok% = U.QtyNok% + 1
       Locate 14,27 : Print Str$(U.QtyNok%)
       Write #2; Unpack$(Key$),UE.Msg$,FINR$
     EndIf    
  Wend 																													  ! Fin del ciclo de carga

End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Rifas, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****   CARGA   MASIVA PRODUCTOS RELACIONADOS A RIFAS ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Septiembre 12 2010 G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : TF:A0004"
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
Xfile$ = Mid$(Ifile$,15,4)
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
  	 Terror% = 0 
     UE.MSG$ = "ARCHIVO "+IFILE$+" NO EXISTE... "
     Call Msg.Log
     Resume 
  EndIf 
  If ERRF% = 44 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 
  If ERRF% = 4 And (ERR = "OE" OR ERR = "FU") Then Begin
     UE.MSG$ = "MAESTRO DE ARTICULOS NO EXISTE ... "
     Call Msg.Log
     Stop
  EndIf 
  If ERRF% = 44 AND (ERR = "OE" OR ERR = "FU") Then Begin
     CREATE POSFILE "TF:P0004" KEYED 8,,,150000 RECL 8   \! si no existe lo crea.
                           AS 44 compound perupdate 		   ! 
     Resume        
  EndIf 
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume
  EndIf   
  If ERRF% = 56 AND ERR = "EF" THEN Begin
     Resume
  EndIf   

  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop

