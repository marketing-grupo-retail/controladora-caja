!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : PROMOFPG.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Promocion formas de pago         *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, IFILE$
Integer*1  Global U.Error%, Terror%, Topen%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%

%INCLUDE ADX_UPGM:BASROUT.J86

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

    Open "TF:PFPAGO" KEYED RECL 16 AS 44          			                ! Definicion Formas de pago
    Open "TF:AFPAGO" KEYED RECL 07 AS 45          			                ! Definicion Articulos relacionados
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                 ! Errores proceso de carga
End Sub

Sub Carga.Masiva.Datos
 String U.Cod$, U.Name$, U.Fec1$, U.Fec2$, U.Vlr$, U.Bol$, U.Con$, U.Msg$, U.Stat$, Key$, LEC$, U.Clte$, U.Promo$, U.Promo2$
 Integer*1 xi%
 Integer*4 Xl%
  U.Qty2% = 0
  LEC$="C1 C1 2C3 C2 C4 C2" 
  Xl% = 0
  While (1)																											! Mientras no EOF
    Read #1; U.Cod$, U.Name$, U.Fec1$, U.Fec2$, U.Bol$, U.Promo$, U.promo2$
    Xl% = Xl% + 1 
    If Xl% = 1 Then begin                                       ! Cabecera de control 
    	 U.Qty2% = U.Qty2% + 1                                    !
    	 Key$    = Pack$(Right$("00"+U.Cod$,2))                   ! Tipo y variedad de pago
    	 U.Name$ = Pack$(Right$("00"+U.name$,2))                  ! Tipo de promocion 1 Dscto sobre total trx 2 Dscto sobre articulos
    	 U.Fec1$ = Pack$(Right$("000000"+U.fec1$,6))              ! Fecha inicial de la promocion
    	 U.Fec2$ = Pack$(Right$("000000"+U.fec2$,6))              ! Fecha final de la promocion
    	 U.Bol$  = Pack$(Right$("0000"+U.bol$,4))                 ! Porcentaje de descuento
    	 U.Promo$ = (Right$("0000"+U.Promo$,4))                   ! Dirigido a lista de cliente
    	 U.Promo2$ = Pack$(Right$("0000"+U.Promo2$,4))            ! Codigo de la promocion 

       Write Form LEC$;#44 ; KEY$,                             \! Grabacion registro 
             U.Name$,                                          \! Tipo de promocion
             U.Fec1$,                                          \! Fecha inicial de promocion 
             U.Fec2$,                                          \! Fecha final de promocion 
             U.Bol$,                                           \! Ptg dscto aplicar
             U.Promo$,                                         \! Dirigido a lista de clientes
             U.Promo2$                                          ! Codigo de la promocion 
             
       If U.Error% = -1 Then Begin
          U.QtyOk% = U.QtyOk% + 1 
          Locate 13,27 : Print Str$(U.QtyOk%)
          U.Msg$ = UnPack$(U.Msg$)
       EndIf Else Begin
          U.QtyNok% = U.QtyNok% + 1
          Locate 14,27 : Print Str$(U.QtyNok%)
          Write #2; Unpack$(Key$),UE.Msg$,FINR$
       EndIf    

    EndIf Else Begin
     U.Qty2% = U.Qty2% + 1
     U.Error% = -1
   	 Key$    = Right$("00"+U.Cod$,2)                   ! Tipo y variedad de pago
   	 U.Name$ = Right$("000000000000"+U.name$,12)       ! Codigo del articulo
   	 Key$    = Pack$( Key$ + U.Name$ )
     Write Form "C7";#45 ; KEY$
     If U.Error% = -1 Then Begin
       U.QtyOk% = U.QtyOk% + 1 
       Locate 13,27 : Print Str$(U.QtyOk%)
       U.Msg$ = UnPack$(U.Msg$)
     EndIf Else Begin
       U.QtyNok% = U.QtyNok% + 1
       Locate 14,27 : Print Str$(U.QtyNok%)
       Write #2; Unpack$(Key$),UE.Msg$,FINR$
     EndIf    
    Endif  
  Wend 																													  ! Fin del ciclo de carga

End Sub

Sub Fin.Proceso
    UE.MSG$ = "Fin Carga Masiva Formas Pago, Verifique Errores en ADX_UDT1:CARGAERR.TXT"
    Call Msg.Log
    Close 44: Close 45
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****  CARGA MASIVA PARAMETROS PROMOCION FORMAS PAGO  ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Septiembre 12 2010 G.R. ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 8, 1: Print "Cargando     Archivo   : "+IFILE$
   Locate 9, 1: Print "Actualizando Archivo   : TF:PFPAGO"
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
  	 Terror% = 0 
     UE.MSG$ = "ARCHIVO "+IFILE$+" NO EXISTE... "
     Call Msg.Log
     Resume 
  EndIf 
  If ERRF% = 44 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 

  If ERRF% = 45 AND ERR = "KF" THEN Begin
     Resume 
  EndIf 


  If ERRF% = 44 AND (ERR = "OE" OR ERR = "FU") Then Begin
     CREATE POSFILE "TF:PFPAGO" KEYED 1,,,500 RECL 14   \! si no existe lo crea.
                           AS 44 compound perupdate 		   ! 
     Resume        
  EndIf 

  If ERRF% = 45 AND (ERR = "OE" OR ERR = "FU") Then Begin
     CREATE POSFILE "TF:AFPAGO" KEYED 7,,,45000 RECL 7   \! si no existe lo crea.
                           AS 45 compound perupdate 		   ! 
     Resume        
  EndIf 

 
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop

