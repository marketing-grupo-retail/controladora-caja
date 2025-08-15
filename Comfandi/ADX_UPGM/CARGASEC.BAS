!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : CARGASEC.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Alimentacion Dscto x Depto       *
!**************************************************
! Modificaciones:


%ENVIRON C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, IFILE$
Integer*1  Global U.Error%, Terror%, Topen%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty2%
%INCLUDE EAMITEMR.J86
%INCLUDE EXIRTSVA.J86


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
    Open "MKTP:SECCION.DAT" KEYED RECL 84 AS 44            								! Descuento por seccion 
    Open "EAMITEMR" KEYED RECL 77 AS 4                    								! Maestro de articulos 
    Create "ADX_UDT1:CARGAERR.TXT" AS 2                                   ! Errores proceso de carga
End Sub


Function GRABA.CUPON(C.ITEM$,C.DESC$,C.VALOR$,C.METODO$,C.LIGA$,ID1%,ID1A%,C.FAMILY$,C.LINKEDTO$) ! 
 String C.ITEM$, C.VALOR$, C.METODO$, C.LIGA$,	       \!
        C.ITEMNAME$, C.DESC$, C.ITEM$, C.SALEQUAN$,    \!
        C.INDI2$, C.FAMILY$, C.MPGROUP$, C.LINKEDTO$,  \!
        C.DEPARTME$					            !

    IR.ITEMNAME$ = C.DESC$				                            ! Descripcion cupon
    IR.SALEPRIC$ = PACK$(RIGHT$("0000000000"+C.VALOR$,10))    ! Valor del cupon
    IR.ITEMCODE$ = RIGHT$(STRING$(4,"0")+C.ITEM$,4)           ! Item del cupon
    IR.ITEMCODE$ = Pack$("44000004"+IR.ITEMCODE$)
    IR.SALEQUAN$ = PACK$(RIGHT$("91",2))				              ! OVS 12-SEP-01 DE 00 A 99
    IR.INDICAT2$ = PACK$(C.METODO$)			                      ! 
    IR.FAMILYNU$ = PACK$(C.FAMILY$)			                      !
    IR.DEPARTME$ = PACK$("0001")  !
    IR.MPGROUP$  = PACK$("00")				                        !
    IR.LINKEDTO$ = PACK$(RIGHT$("0000"+C.LINKEDTO$,4))        ! Cupon ligado
    IR.INDICAT0 = 0
    IR.INDICAT1 = 0
    IR.INDICAT1A = 0
    IR.USEREXIT1 = 0					                          !
    IR.USEREXIT2 = 0					                          !
    ASC.IR.DATA$ = PACK$(String$(40,"0"))
    Write Form "C6 3I1 C1 C2 C3 2C1 C5 C2 C18 2I2 C31";#4;    \
               IR.ITEMCODE$,         \ ITEM CODE                  UPD 6
               IR.INDICAT0,          \ INDICATOR FLAG BYTE 0      INT 1
               IR.INDICAT1,          \ INDICATOR FLAG BYTE 1      INT 1
               IR.INDICAT1A,         \ INDICATOR FLAG BYTE 2      INT 1
               IR.INDICAT2$,         \ INDICATOR BYTE 3           UPD 1
               IR.DEPARTME$,         \ DEPARTMENT NUMBER          UPD 2
               IR.FAMILYNU$,         \ FAMILY COUPON NUMBERS      UPD 3
               IR.MPGROUP$,          \ MULTIPRICING GROUP NUMBER  UPD 1
               IR.SALEQUAN$,         \ DEAL QUANTITY              UPD 1
               IR.SALEPRIC$,         \ PRICE                      UPD 5
               IR.LINKEDTO$,         \ LINKED ITEM                UPD 2
               IR.ITEMNAME$,         \ ITEM DESCRIPTION           ASC 18
               IR.USEREXIT1,         \ RESERVED FOR USER DATA     INT 2
               IR.USEREXIT2,         \ RESERVED FOR USER DATA     INT 2
               ASC.IR.DATA$          ! DATOS ADICIONALES          Asc 31       
End Function						    !

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
 String U.Secc$, U.Cupon$, U.dat$, UE.DIA$(1), UE.HORA$(2), Xlec$, U.Desc$, U.Promo$, U.Clte$
 Integer*1 xi%
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
     U.dat$   = Str$(Val(U.Dat$)) + "0"
     U.Desc$  = "Dscto "+U.Dat$+"%"
     U.Clte$  = (Right$("0000"+U.Clte$,4))                             ! Cliente dirigido Add 12 Jun 2011
     U.SECC$  = Pack$("1"+Right$("000000000000000"+U.SECC$,15)) + U.Clte$		! Arma llave
     U.CUPON$ = Right$("0000"+U.CUPON$,4)       									          ! Arma Cupon
     U.dat$   = Right$("000"+U.dat$,3)
     U.Promo$ = Pack$(Right$("0000"+U.Promo$,4)) 									! Codigo de la promocion
     U.Error% = -1
     
     For XI% = 0 TO 6
         UE.HORA$(XI%,1) = Right$("0000"+UE.HORA$(XI%,1),4)
         UE.HORA$(XI%,2) = Right$("0000"+UE.HORA$(XI%,2),4)
     Next XI%
     
     
     XLEC$ = "C12 C4 C3 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C1 C4 C4 C2"  !
     Write Form XLEC$;#44;                                        \! Grabar Portafolio      
             U.SECC$, U.Cupon$, U.dat$, \
             UE.DIA$(1), UE.HORA$(1,1), UE.HORA$(1,2), \ Lunes
             UE.DIA$(2), UE.HORA$(2,1), UE.HORA$(2,2), \ Martes 
             UE.DIA$(3), UE.HORA$(3,1), UE.HORA$(3,2), \ Miercones
             UE.DIA$(4), UE.HORA$(4,1), UE.HORA$(4,2), \ Jueves
             UE.DIA$(5), UE.HORA$(5,1), UE.HORA$(5,2), \ Viernes
             UE.DIA$(6), UE.HORA$(6,1), UE.HORA$(6,2), \ Sabado
             UE.DIA$(0), UE.HORA$(0,1), UE.HORA$(0,2), \ Domingo
             U.Promo$
     
     If U.Error% = -1 Then Begin
       U.QtyOk% = U.QtyOk% + 1 
       Locate 13,27 : Print Str$(U.QtyOk%)
       U.dat$ = Right$("00"+Str$(Val(unpack$(U.dat$))),2)
       U.dat$ = "99" + U.dat$ + "0"
       
       !Call GRABA.CUPON(U.CUPON$,U.Desc$,U.dat$,"74","0000",00000000B,0,"000000","0000")  ! Grabacion del cupon  
       
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
    Close 4 : Close 1: Close 2
    Stop
End Sub 

Sub Pantalla.Principal
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "****     CARGA MASIVA DESCUENTOS POR DEPARTAMENTO    ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software Septiembre 12 2010 G.R. ***"
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
Stop 

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
     CREATE POSFILE "MKTP:SECCION.DAT" KEYED 12,,,15000 RECL 84   \! si no existe lo crea.
                           AS 4 compound perupdate 		   ! 
     Resume        
  EndIf 
  If ERRF% = 4 AND ERR = "EF" THEN Begin
     Resume
  EndIf   
  If ERRF% = 1 AND ERR = "EF" THEN Begin
     Call Fin.Proceso												   														! Fin de ejecucion del programa     
  EndIf 
  Call MSG.LOG
  Stop

