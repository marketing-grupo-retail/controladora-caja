\------------------------------------------------------------------------------
\                   NOMBRE DEL MODULO : NFFMODBG.BAS      
\          Modulo de Rutinas de Front-Store - NFF Fiscal 
\  
\               VERSION : General Consolidada Mayo del 2000
\
\       Primer Revision : Mayo del 2000 by Global DataTel
\  
\  Resumen de Cambios
\ 
!-----------------------------------------------------------------------------
!  14/03/98     Consolidar rutinas para version fiscal            
!-----------------------------------------------------------------------------
\
!-----------------------------------------------------------------------------
!  FAVOR  ORGANIZAR LOS TS.LINETYPE EN LA USER-20 DE ACUERDO AL 
!         SIGUIENTE ORDEN:
!-----------------------------------------------------------------------------
\
\         TS.LINETYPE        ETAPA TRANSACCION
\         -----------       -------------------------------------------
\             18             Header  (Detectar "HEADER DE DETECCION 1")     
\              1             Venta de articulos
\              7             Balance due  =  Calculo de saldo
\              2             Pagos
\              4             Cambio despues del pago
\             22             Anulacion en el trailer
\             31             Suspension de transaccion
\              6             Trailer de la Tienda
\             18             Trailer  (Detectar "TRAILER DE DETECCION 2")    
\
\
\ NOTA: Favor avisar en caso de incluir otros TS.LINETYPE
\ 
!-----------------------------------------------------------------------------
!               DEFINICION DE VARIABLES DE ARCHIVOS
!-----------------------------------------------------------------------------

     %ENVIRON C                                        !

\ REM!!**************** INCLUDE FILES *********************************

 %INCLUDE EAMCSXPT.J86
 %INCLUDE EAMCSGLO.J86
 %INCLUDE EAMTLIST.J86
 %INCLUDE EAMTPROD.J86
 %INCLUDE EAMMTRNC.J86
 %INCLUDE EAMCUSTA.J86            ! customer account status file  IR68987
 %INCLUDE EAMTRANS.J86            ! transaction summary log       IR68987
 %INCLUDE EAMSOPTS.J86            ! store options                 IR68987
 %INCLUDE EAMCSUVA.J86            ! user exit global variables
 %INCLUDE EAMIWORK.J86            ! item movement work file       IR77860
 %INCLUDE EAMCSCF1.J86            ! checkout support cntl file    IR92597
                                  !
 %INCLUDE EAMXXCPY.J86            ! Copyright Statement
 %INCLUDE EAMASMCT.J86            ! assembler routines
 %INCLUDE EAMADXRT.J86            ! system services interface     IR68987

                                                       !
!------------------------------------------------------------------------------
!                 Subrutinas del Modulo Fiscal NFFMODBG.BAS
!------------------------------------------------------------------------------
FUNCTION ADXSTART (NAME$,PARM$,MESS$) EXTERNAL         ! Funcion de Inicio de
    INTEGER*2 ADXSTART                                 ! Tareas de Background
    STRING    NAME$,   PARM$,      MESS$               ! Variables
END FUNCTION                                           !
                                                       !
!-----------------------------------------------------------------------------
SUB TSCSEC03 EXTERNAL                                  ! Despliega Error
END SUB                                                !

SUB NFFCSU16.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Julio de 1998                                        **
!   **    Project    :  Control de Numeracion Fiscal                        **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMCSU16.J86                                         **
!   **    Program    :  EAMCSUPC.BAS                                         **
!   **    Executable :  EAMCSMLL.286                                         **
!   **        Ajuste :                                                       **
!   **                                                                       **
!   **    Estas lineas se insertaran en el archivo EAMCSU16.J86 para         **
!   **    Indicarle a la Aplicacion de Background que se va a iniciar        **
!   **    la ejecuci¢n AUTOMATICA del Proceso de Extracci¢n de la            **
!   **    numeraci¢n Fiscal incluida el la String '99' del LOG Diario        **
!   **                                                                       **
!   **    El programa NFFEXTRA.286 se encarga de EXTRAER esta Inf.          **
!   **                                                                       **
!******************************************************************************
  WORK1I2 = PACKBIN2(CS.INPUT$,CS.FLD.POS(2),         \! Toma Indicat de 
                               CS.FLD.LEN(2))          ! Cierre 
  IF WORK1I2 = 1 OR                                   \! Cierre Largo
     WORK1I2 = 2 THEN BEGIN                            ! Cierre Corto
     WORK1$ = "ADX_UPGM:NFFEXTRA.286"                  ! Progr de Flags UExit
     WORK2$ = "NFFEXTRA"                               ! Archivo que interviene
     WORK3$ = "** Extracci¢n de Numeraci¢n Fiscal **"  ! Mens para Background
     WORK1I2 = ADXSTART(WORK1$,WORK2$,WORK3$)          ! Inicio de Progr Batch
     IF WORK1I2 <> 0 THEN BEGIN                        ! Si hubo error
        WORK3$ = "*ERR" + "," + STR$(WORK1I2)          ! Log del Error
        CALL ADXERROR(0,83,9,2,1,WORK3$)               !
     ENDIF                                             !
  ENDIF                                                ! 
!------------------------------------------------------------------------------
END SUB                                                !
!------------------------------------------------------------------------------
!                    Fin de las User Exits de Usuario
!------------------------------------------------------------------------------
