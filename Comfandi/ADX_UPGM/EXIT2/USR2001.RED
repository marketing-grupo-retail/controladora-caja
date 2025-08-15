!   ***************************************************************************
!   **    Date       :  Agosto de 1966                                       **
!   **    Project    :  IBM - CALI - COMFANDI                                **
!   **    User Exit  :  USR2001.011                                          **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS11L.286                                         **
!   **        Ajuste :  SOLSER49                                             **
!   **                                                                       **
!   **    Instrucciones para evitar la Linea de Iva de la SMA/4690 en la     **
!   **    Tira del Cliente (CR)                                              **
!   **                                                                       **
!   **    Code for printing IVA on the customer receipt station.             **
!   **    Report descriptor 3058 needs to be updated to reflect User Option  **
!   **                                                                       **
!   ***************************************************************************
if mid$((ts.prtbuf$),4,4) ="****" and \
mid$((ts.prtbuf$),20,2) = "TOT" THEN \
ts.prtbuf$ = " "

!CALL FORMAT.AMOUNT(TS.COUP.AMT*-1)
!FONPH$ = CHR$(27)+CHR$(69)
!TS.PRTFRU$  = "**TOTAL DESCUENTOS --->    $  -" + TS.TEMP1$     !bufer imp
 !          IF TS.COUP.AMT <> 0    AND                          \!     !DCTO CUPON 
  !           TS.LINETYPE =  7 THEN BEGIN                        ! 
   !          WRITE FORM "C38  A1";#34;FONPH$+TS.PRTFRU$         !      
    !       ENDIF
!-------------------------------------------------------------------------
AJUS1% = AJUS%
!TOTAL1 = VAL(VR$) - (AJUS1%)
!VR$ = STR$(TOTAL1)


!VR$ = str$(total1)
IF AJUS1% <> 0 OR SL.TE.AMTTENDE <> 0 THEN BEGIN
IF TS.LINETYPE = 7 THEN  BEGIN                        ! Si es linea TOTAL
    VR$ = RIGHT$(TS.PRTBUF$,12)                      ! Toma el Valor Total
!TS.LINETYPE = 0    
!TOTAL1 = VAL(VR$) -(iv%* (AJUS1%))
!VR$ = STR$(TOTAL1)
    TS.PRTBUF$ = "**SUBTOTAL/TOTAL --->  $ "+VR$     ! Arma STRING de nuevo
      USR.ITEMS$(450) = TS.PRTBUF$                 ! Guarda Subtotal
ENDIF ELSE BEGIN
IF TS.LINETYPE = 7 THEN \
TS.PRTBUF$ = " "
ENDIF 
ENDIF 

!------------------------------------------------------------------------------

IF TO.USEROPTS(4) <> 0 THEN BEGIN                    ! Si Opt Usuario=4 Iva 
   IF TS.LINETYPE = 18 THEN UE.NIT = 1               ! Si es Header de Inicio
   IF TS.LINETYPE = 1 OR                            \! Si es Vta Item
      TS.LINETYPE = 5 THEN BEGIN                     ! 
         IF UE.NIT = 1 THEN BEGIN                    ! y es Cabecera
!           WRITE FORM "C38 A1"; #34 ;               ! Escribe Mensaje
!           "         AGENTE RETENEDOR DE IVA"       !
!           WRITE FORM "C38 A1"; #34 ;               ! y Nit
!           "           NIT 123.456.789-0"           !
            AUX$ = "    FACTURA DE VENTA # "      + \! Letrero de Factura
            RIGHT$("000"+TS.STORE$,3)             + \! con Num Almac
            RIGHT$("000"+STR$(SL.HD.TERMINAL),3)  + \! la terminal
            RIGHT$("0000"+STR$(SL.HD.TRANSNUM+1),3)  ! y la transaccion
!------------------------------------------------------------------------------
!      Se documemtan las DOS sig l¡neas por M¢dulo NFF    Mayo/2000
!
!            WRITE FORM "C38 A1"; #34 ; AUX$          ! Y lo Imprime en CR
!            WRITE FORM "C38 A1"; #36 ; AUX$          ! Y lo Imprime en SJ
!------------------------------------------------------------------------------
            AUX$ = "   FECHA DE EXPEDICION:  "    + \! Letrero de Fecha
            LEFT$(UNPACK$(SL.HD.DATETIME$),2)+"/" + \! compaginada
            MID$(UNPACK$(SL.HD.DATETIME$),3,2)+"/"+ \! para mejor
            MID$(UNPACK$(SL.HD.DATETIME$),5,2)       ! presentacion 
!------------------------------------------------------------------------------
!      Se documemta la sig l¡nea por M¢dulo NFF    Mayo/2000
!
!            WRITE FORM "C38 A2"; #34 ; AUX$          ! y lo Imprime
!------------------------------------------------------------------------------
            UE.NIT = 0                               ! Reset de Inicio
         ENDIF                                       ! Fin de Cabecera
   ENDIF                                             ! Fin de Impresion Plu
ENDIF                                                !

!WRITE FORM "C38 A1";#34;STR$(CA.PPOINTS)
!WRITE FORM "C38 A1";#34;STR$(CA.TR.POINTS)
!----------------------------------------------------------------------------
