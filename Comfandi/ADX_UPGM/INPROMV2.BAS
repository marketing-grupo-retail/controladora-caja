!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : INPROMV2.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Creacion Estructuras Motor       *
!                Promociones V2                   *
!**************************************************
! Modificaciones:

String Global UE.MSG$, ERRFX$

Sub MSG.LOG																																	!
    Locate 23,1 : Print String$(78," ") 																		!
    Locate 23,1 : Print "Msg: "+UE.MSG$																			!
End Sub 																																		!

Function TRADUCE.ERROR                                       								!                       
Integer*4 HX%, S%, SX%, SUM%                                                                        
String    Z$                                                                                        
    HX% = ERRN                                                              ! Traduccion de los     
    ERRFX$=""                                                               ! codigos de error      
    FOR S% = 28 TO 0 STEP -4                                                ! del sistema operativo 
        SX% = SHIFT(HX%,S%)                                                 !                       
        SUM% = SX% AND 000FH                                                !                       
        IF SUM% > 9 THEN SUM% = SUM% + 55                                  \!                       
        ELSE SUM% = SUM% + 48                                               !                       
        Z$ = CHR$(SUM%)                                                     !                       
        ERRFX$ = ERRFX$ + Z$                                                !                       
    NEXT S%                                                                 !                       
END Function


On Error GoTo FALLA.PROCESO
Clears
Print "Grupo Retail Ltda "
Print "Creando Estructuras Motor Promociones - Ver. 2.0 29/May/2018"
Print " "
!-- Creacion control carga archivos promociones
     Print "Creando Estructura Control promociones TF:LSTCAR, Espere..."
     CREATE POSFILE "TF:LSTCAR" KEYED 12,,,400000 RECL 13   		 					 \! Creacion de estructura
                           AS 4 compound perupdate 		            					! 
     Close 4 																																!

!-- Creacion Listas de promociones
     Print "Creando Estructura Listas de promociones TF:LISTAS, Espere..."
     CREATE POSFILE "TF:LISTAS" KEYED 6,,,500000 RECL 6      		 					 \! Creacion de estructura
                           AS 5 compound perupdate 		            					! 
     Close 5 																																!

!-- Creacion Lista x Items
     Print "Creando Estructura Lista x Item TF:ITMLST, Espere..."
     CREATE POSFILE "TF:ITMLST" KEYED 6,,,50000 RECL 508    		 					 \! Creacion de estructura
                           AS 6 compound perupdate 		            					! 
     Close 6 

!-- Creacion Mensajes promocionales
    Print "Creando Estructura Mensajes TF:MSGPRM, Espere..."
    Create PosFile "TF:MSGPRM" KEYED 5,,,50000 RECL 43                 		 \! archivo de mensajes
                           AS 22 compound perupdate 		     								! 
    Close 22

!-- Creacion Estructuras Bonos recompra forma de pago
!    Print "Creando Estructuras Bono Recompra Forma de Pago, Espere..."
!    Create POSFILE "TF:COMPAG" KEYED 6,,,50000 RECL  6 AS 4 COMPOUND PERUPDATE  ! Creacion Articulos bonos recompra
!    Create Posfile "TF:PBONPG" KEYED 4,,,56    RECL 66 AS 3 COMPOUND PERUPDATE  ! Creacion control proyecto
!    Create Posfile "TF:BPGLOC" KEYED 9,,,50000 RECL 17 AS 5 COMPOUND PERUPDATE  ! Creacion contingencia
!    Close 4 : Close 3 : Close 5

Print " "
Print "Procedimiento Estructura Promociones V2 Finalizado"
     
Stop      

FALLA.PROCESO:
UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
Print UE.MSG$
Stop 
