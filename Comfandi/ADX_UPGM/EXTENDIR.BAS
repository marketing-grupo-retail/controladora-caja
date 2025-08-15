!**************************************************
!Empresa       : Grupo Retail Lta                 *
!Programa      : EXTENDIR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Mantenmiento Expansion del       *
!		             Eamitemr - Comfandi              *
!**************************************************
!* Modificaciones:

%ENVIRON C							                ! Modulo controlador
%INCLUDE EAMIRRFG.J86						        ! Definicion variables
%INCLUDE EAMCFINC.J86         					! Data entry global variables
%INCLUDE EAMITEMR.J86         					! Item record definition
%INCLUDE EXIRTSVA.J86						        ! Definicion variables
%INCLUDE DMEXTR.J86           					! Display Manager external routines

!------------------------------------------------------------------------------
!
!                             GETN1/GETN2/GETN4
!      Las siguientes rutinas extraen una/dos/cuatro bytes enteros de un
!      string. P2 es el offset dentro de la cadena de string
!
!------------------------------------------------------------------------------
 
  FUNCTION GETN1(P1$,P2) EXTERNAL                       
     INTEGER*1 GETN1
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN2(P1$,P2) EXTERNAL
     INTEGER*2 GETN2
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
  END FUNCTION

!------------------------------------------------------------------------------
!
!                            PUTN1/PUTN2/PUTN4
!
!       These routines insert a one/two/four byte integer into a string.
!       P2 is the offset within the string and P3 is the source integer
!
!------------------------------------------------------------------------------
  
  FUNCTION PUTN1(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*1 P3
  END FUNCTION
  
  FUNCTION PUTN2(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2,P3
  END FUNCTION
  
  FUNCTION PUTN4(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*4 P3
  END FUNCTION

 FUNCTION SUBSTR (P1$,P2,P3$,P4,P5) EXTERNAL
 STRING P1$,P3$
 INTEGER*2 P2,P4,P5
 END FUNCTION


 FUNCTION ADXERROR(TERM,GRP,NUM,SVRTY,EVENT,UNQ$) EXTERNAL
 INTEGER*2 TERM,NUM
 INTEGER*1 SVRTY,GRP,EVENT
 STRING    UNQ$
 END FUNCTION

 SUB MMCFEC04(ENTRY.FIELD$) EXTERNAL
     STRING ENTRY.FIELD$
 END SUB

SUB MMCFEC05(RET.VAL,DM.CMD$) EXTERNAL   ! Handle display error
   INTEGER*2 RET.VAL
   STRING    DM.CMD$
END SUB

SUB EXTMIR01.011  PUBLIC
  IF IRRF.RECL NE 77 THEN BEGIN 
     CALL ADXERROR( 0,                              \ 0 for controller
                   ASC("B"),                        \ msg displayed
                   007,                             \ msg number
                     3,                             \ severity level
                     2,                             \ 2 = checkout support
                   "Err Long. Registro")	    ! Mensaje rutina error
  ENDIF
END SUB

SUB EXTMIR06.011  PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    User Exit  :  EXTNIR06.011                                         **
!   **    Inclu¡r en :  EAMIUC06.J86                                         **
!   **    Programa   :  EAMIRUEC.BAS                                         **
!   **    Executable :  EAMDB55L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!   
!------------------------------------------------------------------------------
  ASC.IR.DATA$       = IR.USERDATA$												! Datos expandidos
  ASC.IR.NDEPTO$     = Left$(ASC.IR.DATA$,16)							! ESTRUCTURA COMERCIAL
  ASC.IR.CONSUMO%    = GETN4(ASC.IR.DATA$,16)					    ! IMPUESTO AL CONSUMO
  ASC.IR.FECHA%      = GETN4(ASC.IR.DATA$,20)					    ! FECHA DE ACTUALIZACION REGISTRO

END SUB
!--- Fin lectura datos adicionales

SUB EXTMIR07.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    User Exit  :  EXTNIR07.011                                         **
!   **    Incluir en :  EAMIUC07.J86                                         **
!   **    Programa   :  EAMIRUEC.BAS                                         **
!   **    Executable :  EAMDB55L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!   
!------------------------------------------------------------------------------

  IR.USERDATA$ = STRING$(15,"0")												! Init user data
  IR.USERDATA$ = ASC.IR.NDEPTO$ + IR.USERDATA$
  CALL PUTN4(IR.USERDATA$, 16,ASC.IR.CONSUMO%)		   		! Impuesto al consumo
  CALL PUTN4(IR.USERDATA$, 20,ASC.IR.FECHA%)		    		! Fecha de actualizacion 
  ASC.IR.DATA$ = IR.USERDATA$

END SUB
!--- Fin de asignacion datos adicionales EAMITEMR
