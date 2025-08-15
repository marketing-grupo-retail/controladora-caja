!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRNEWBON.BAS                     *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion nuevas estructuras      *
!                para bonos recompra              *
!**************************************************
!-- Version 1.0 8 Ene 2020 - GR/OVS

%Environ C						   																		 ! Ambiente de controlador

String     Global UE.MSG$, ERRFX$, Finr$, Ue.Data2$, FileOut$, Param2$, \
                  XParam$, Ifile$
Integer*1  Global U.Error%
Integer*4  Global U.QtyOk%, U.QtyNok%, U.Qty%

%INCLUDE BASROUT.J86

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


On Error GoTo Salida
   CLEARS
   Locate 2, 4: Print CHR$(218)+STRING$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "*** CREACION ESTRUCTURAS BONOS RECOMPRA           ***"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revision Software ENERO     08 2020 GR.**"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+STRING$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   
   Locate 7,1 : Print "Creando estructuras bono recompra cliente, Espere por favor..."

!--- Creación bonos recompra
  Create POSFILE "TF:COMPAG" KEYED 6,,,50000 RECL  6 AS 4 COMPOUND PERUPDATE  ! Creacion Articulos bonos recompra
  Create Posfile "TF:PBONPG" KEYED 4,,,56    RECL 73 AS 3 COMPOUND PERUPDATE  ! Creacion control proyecto
  Create Posfile "TF:BPGLOC" KEYED 9,,,50000 RECL 17 AS 5 COMPOUND PERUPDATE  ! Creacion contingencia
  Close 3 : Close 4 : Close 5
  Locate 8,1 : Print "Estructuras Creadas..."
  wait ; 2000
  Locate 9,1 : Print "Creando estructuras bono recompra forma pago, Espere por favor..."
!--- Creación bonos recompra forma de pago
  Create POSFILE "TF:COMPRA" KEYED 6,,,50000 RECL  6 AS 4 COMPOUND PERUPDATE  ! Creacion Articulos bonos recompra
  Create Posfile "TF:PBONOS" KEYED 4,,,50    RECL 63 AS 3 COMPOUND PERUPDATE ! Creacion control proyecto
  Create Posfile "TF:BLOCAL" KEYED 9,,,50000 RECL 17 AS 5 COMPOUND PERUPDATE ! Creacion contingencia
  Close 3 : Close 4 : Close 5
  Locate 10,1 : Print "Estructuras Creadas..."
  wait ; 2000
  Locate 9,1 : Print "Cargando información base bonos Espere por favor..."
  Call osshell("ADX_UPGM:LDITBONO.286 BNCLTNEW.TXT")
  Call osshell("ADX_UPGM:LDITBNPG.286 BNPGSNEW.TXT")
  Locate 22,1 : Print "Procedimiento Terminado Satisfactoriamente."
  Stop 

Salida:
  U.Error% = 0
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  CALL MSG.LOG
  Stop 
