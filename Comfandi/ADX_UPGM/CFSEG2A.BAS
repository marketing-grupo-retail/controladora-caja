\------------------------------------------------------------------------------
\                   NOMBRE DEL MODULO :
\            Modulo de Rutinas de Front-Store -
\
\               VERSION : General Consolidada Marzo de 1999
\
\       Primer Revision : Febrero de 1999 by Global DataTel
\
\  Resumen de Cambios
\
!-----------------------------------------------------------------------------
!  14/03/98     Consolidar rutinas para version LA 14 S.A.
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
\ Se ajusta el modulo para el procedimiento de recuperacion de trx cuando
\ tiene presente un cupon de descuento evitando que la aplicacion se
\ finalice por un fin anormal de aplicacion.
\ Ajustado el 20 de octubre de 2.005 OVS - ASIC CONSULTING GROUP
\-------------------------------------------------------------------------
\ Se ajusta el modulo para reportar al nuevo modulo de impuestos en las
\ anchetas el IVA correspondiente.
\ Ajustado el 10 de mayo de 2.010 Grupo Retail - OVS
!-----------------------------------------------------------------------------
! Se ajusta modulo en la definicion de variables y calculo del IVA para evitar
! fin anormal de aplicacion en el registro de anchetas y uso de formas de pago
! con franqueo.
! Ajustado el 14 de Septiembre de 2.010 Grupo Retail - OVS
!-----------------------------------------------------------------------------
! Mod 28May2012
! Se modifica la impresion de los items que componen una oferta definida
! para que no se refleje en la impresion el valor y el flag de impuesto
! solicitado por Freddy Guerrero.
! Desarrollado por Grupo Retail - OVS
!-----------------------------------------------------------------------------
! Mod 28May2012
! Se adiciona en la generación de la venta de una canasta familiar, que se
! acumule la venta para la generación de boletas
! desarrollado por Grupo Retail - OVS
!-----------------------------------------------------------------------------
!                   RUTINAS GENERALES
!-----------------------------------------------------------------------------
!
! E1    SAVE.PRINT              Salvar variables de Impresion
! E2    RESTORE.PRINT           Restaurar variables de Impresion
! E3    SAVE.DISPLAY            Salvar variables del DISPLAY
! E4    RESTORE.DISPLAY         Restaurar variables del DISPLAY
!
!-----------------------------------------------------------------------------

%Environ T                                        !

Integer*1 Global FIS.TARIFA, FIS.ITEM, FIS.SIGNO, FT%, SIGNOF%, USER.SIGNO, OFERTA%, \
                 SUBTL%

Integer*2 Global LIG2%, LIG1%, MOTO%, MOTOP%,FIS.LIN%
Integer*4 Global UE.TOTALS(1), USER.SAVE(1), FIS.FIV%,UE.II%, FIS.TOTV(1),FIS.IMP.TOT(1)            ! Total Impuestos
Integer*4 Global FIS.VRU%, VTA.TO%, VTOT.OFE%, OF.END, FIS.IVA%(1)
String    Global USER.SAVE$(1), FIS.A$, NFF.S34$, FONPH$, NOM.OFE$,PREFOF$, PLUOF$, \
                 OF.STR$(1), NFF.CNT$, NFF.AMT$, FRA.TIM$, FRA.H$, FRA.M$, FRA.S$, \
                 FRA.FEC$, FRA.AA$, FRA.MM$, FRA.DD$, FRA.LIN$, FIS.DL$,FONPHN$,FONCR$,FONLF$,FCORT$, \
                 FIS.EQ$(1), PREFALI$, OFERT$, KEYOF$, FRA.FAC$,LIN.CRE$,DESOFE$,ULT.PLU$,PLU.OF$

Integer*4 Global Gr.Mkp.VlrCmpBol%, Asc.Tmp.Apun%      ! Vta para generacion de boletas
Integer*4 GLOBAL  UE.TABLAIVA(1)																							! Lista tarifas IVA

%INCLUDE EAMTSWKG.J86                                  ! working storage
%INCLUDE EAMETWKA.j86                                  !
%INCLUDE EAMTRANS.j86                                  !
%INCLUDE EAMTERMS.J86                                  !
%INCLUDE EAMITEMR.J86                                  !
%INCLUDE EAMTSUVA.J86                                  ! working user storage
!------------------------------------------------------------------------------

%INCLUDE EAMTOPTS.J86                                  !
%INCLUDE EAMSOPTS.J86                                  !
%INCLUDE EAMASMRT.J86                                  ! assembler SUBroutines
%INCLUDE EAMADXRT.J86                                  ! EXTERNAL SUBroutines
%INCLUDE EAMTSXHC.J86                                  ! TSLOG Functions
%INCLUDE SVRSTSSU.011
!------------------------------------------------------------------------------
!                 Subrutinas del Modulo CFSEG2.BAS
!------------------------------------------------------------------------------
FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL                  !
  INTEGER*1 FORMAT.AMOUNT                              !
  INTEGER*4 AMT1                                       !
END FUNCTION                                           !
                                                       !
SUB VER EXTERNAL                                       !
END SUB                                                !

FUNCTION U.EXPLUSADI EXTERNAL
END FUNCTION

FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL                !
    STRING PRTLINE
END FUNCTION                                           !

SUB FIN.LOOP EXTERNAL                                  ! Toma Datos Cheque
END SUB                                                !
                                                       !
SUB DATOS.CHEQUE EXTERNAL                              ! Toma Datos Cheque
END SUB                                                !
                                                       !
SUB DATOS.BONO EXTERNAL                                ! Toma Datos Bonos
END SUB                                                !
                                                       !
SUB DATOS.DEBITO EXTERNAL                              ! Toma Datos T-DB
END SUB                                                !
                                                       !
SUB DATOS.CREDITO EXTERNAL                             ! Toma Datos T-CR
END SUB                                                !
                                                       !
SUB DATOS.CONCES EXTERNAL                              ! Toma Datos Conces
END SUB                                                !
                                                       !
SUB FIS.LEACON EXTERNAL                                ! Lectura Ctrls Almac
END SUB                                                !
!------------------------------------------------------------------------------
!                  Rutina para Asignar Tarifas de IVA
!------------------------------------------------------------------------------

SUB FIS.BUSTARIFA PUBLIC                               !
   IF (FIS.FIV% AND 01H) THEN                         \!
      FIS.A$ = "1" ELSE FIS.A$ = "0"                   !
   FOR UE.II% = 1 TO 7                                    !
       FIS.FIV% = SHIFT(FIS.FIV%,1)                    ! Inicializa sig. BIT
       IF (FIS.FIV% AND 01H) THEN FIS.A$="1"+FIS.A$   \! Arma String de 1's
           ELSE FIS.A$ ="0"+FIS.A$                     ! y 0's
   NEXT UE.II%                                            !
   TS.SDESC$(160) = "."                                ! Modif 'z' para SJ
   FIS.TARIFA = 8                                      ! Exento por Default
   IF LEFT$(FIS.A$,4)="1000" THEN FIS.TARIFA=1 ELSE   \! Si hay Plan A
   IF LEFT$(FIS.A$,4)="0100" THEN FIS.TARIFA=2 ELSE   \! Si hay Plan B
   IF LEFT$(FIS.A$,4)="0010" THEN FIS.TARIFA=3 ELSE   \! Si hay Plan C
   IF LEFT$(FIS.A$,4)="0001" THEN FIS.TARIFA=4 ELSE   \! Si hay Plan D
   IF LEFT$(FIS.A$,4)="1100" THEN FIS.TARIFA=5 ELSE   \! Si hay Plan AB(E)
   IF LEFT$(FIS.A$,4)="0110" THEN FIS.TARIFA=6 ELSE   \! Si hay Plan BC(F)
   IF LEFT$(FIS.A$,4)="0011" THEN FIS.TARIFA=7 ELSE   \! Si hay Plan CD(X)
   IF LEFT$(FIS.A$,4)="1001" THEN FIS.TARIFA=8         !

   !TS.SDESC$(160) = FIS.EQ$(FIS.TARIFA)                ! Modif 'z' para SJ

   TS.SDESC$(160) = MID$("ABCDEFGH",FIS.TARIFA,1)      ! Coloca Flag indicador IVA GR-OVS 10 May 2010

END SUB
!------------------------------------------------------------------------------
!                Subrutina de Calculo del IVA
!------------------------------------------------------------------------------

!SUB FIS.CALIVA PUBLIC                                  !
! IF FIS.ITEM THEN BEGIN                                ! Si es item con Iva
!    FIS.ITEM = 0                                       ! Reset Flag
!    FIS.TOTV(0)=FIS.TOTV(0)+FIS.VRU% * FIS.SIGNO       ! Acumula Gran Tot Vta
!    IF FT% = 0 THEN BEGIN                              ! Si es primera vez
!       FT% = 1                                         ! marca Flag
!       FIS.TOTV(FIS.TARIFA)=FIS.TOTV(FIS.TARIFA) +    \!
!           FIS.VRU% * FIS.SIGNO                        ! Acumula Vta por Plan
!       FIS.IVADEF% = FIS.VRU% * FIS.SIGNO   -         \! Deflacta el Iva segun
!           ROUND(FLOAT(FIS.VRU% * FIS.SIGNO)/         \! Tarifa y obtiene IVA
!           (1+FLOAT(FIS.IVA%(FIS.TARIFA))/100),0,0)    ! definitivo por Item
!       FIS.IMP.TOT(FIS.TARIFA) =                      \! Acumula IVA segun Tarifa
!           FIS.IMP.TOT(FIS.TARIFA) + FIS.IVADEF%       ! por cada Item
!       FIS.IMP.TOT(0) = FIS.IMP.TOT(0)+FIS.IVADEF%     ! Acumul Impuesto Total
!    ENDIF ELSE BEGIN                                   ! Si NO es 1er Vez
!       FT% = 0                                         ! Restaura Flag
!    ENDIF                                              !
! ENDIF                                                 !
!END SUB                                                !

!--- Nueva rutina para almacenar IVA
!--- Desarrollado por Grupo Retail - OVS 10 Mayo 2010
Sub FIS.CALIVA PUBLIC                                  !
Integer*4 XQty%
 If FIS.ITEM THEN BEGIN                                ! Si es item con Iva
 	  FIS.ITEM = 0                                       ! Reset Flag
 	  XQty% = SL.IE.QTYORWGT                             ! Cantidad

!    FIS.TOTV(0)=FIS.TOTV(0)+FIS.VRU% * FIS.SIGNO       ! Acumula Gran Tot Vta

    FIS.TOTV(0)=FIS.TOTV(0)+FIS.VRU%                   ! Acumula Gran Tot Vta

    IF FT% = 0 THEN BEGIN                              ! Si es primera vez
       FT% = 1                                         ! marca Flag
       
       On FIS.TARIFA GoSub AIV1,AIV2,AIV3,AIV4,AIV5,AIV6,AIV7,AIV8
       GoTo ANC.SIGA

       AIV1:
       UE.TOTALS(1) = UE.TOTALS(1) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV2:
       UE.TOTALS(2) = UE.TOTALS(2) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV3:
       UE.TOTALS(3) = UE.TOTALS(3) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV4:
       UE.TOTALS(4) = UE.TOTALS(4) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV5:
       UE.TOTALS(5) = UE.TOTALS(5) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV6:
       UE.TOTALS(6) = UE.TOTALS(6) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV7:
       UE.TOTALS(7) = UE.TOTALS(7) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub
       AIV8:
       UE.TOTALS(8) = UE.TOTALS(8) +  (FIS.VRU% * SL.IE.QTYORWGT)  : Exit Sub

       ANC.SIGA:
 	   ENDIF Else FT% = 0
 ENDIF
End SUB                                                !

!------------------------------------------------------------------------------
!          Subrutina para explosionar Ofertas en LOG de Transacciones
!------------------------------------------------------------------------------

SUB ITEM.COLLAPSE PUBLIC                             !
String LIGA$, FIS.KIT$,RELE1$,VALOF$,LIGO$,RELE2$,RELE3$,UNO$,DOS$, \
       OFER.PLU$, CCC$, INDIC1$, INDIC2$, INDIC3$, Udata$
Integer*1 IND1OF%, ANCHO%, XJ%
String Grlt$, Grname$, GrAlpha$
    TS.ER.RETURN = -1                                ! Retorna si NO Abre OK
    VTOT.OFE% = 0                                    !
    VTA.TO%   = 0                                    !
!    WRITE FORM "C38";#34;" "                        ! Escribe inicio de
!    WRITE FORM "C2 C5 C30 A1";#34;CHR$(27)+        \!
!          CHR$(69)," ","** "+NOM.OFE$+" **"         !
!------------------------------------------------------------------------------
    NFF.S34$=" " : CALL U.IMPRIME(NFF.S34$,4100H)   !        Ses-34 Cambio 1
    NFF.S34$=FONPH$+"    ** "+NOM.OFE$+" **"         !
    CALL U.IMPRIME(NFF.S34$,4100H)                  !
    NFF.S34$="    ** "+NOM.OFE$+" **"                !
    CALL U.IMPRIME(NFF.S34$,2100H)                  !
!------------------------------------------------------------------------------
    LIG2% = 0                                        ! Ind para Str 02
    FOR LIG1% = 1 TO 99                              ! Explosiona la Oferta
!        IF TS.XXMOD = 8 THEN SIGNOF% = -1            ! Si VOID,invierte Signo

        LIGA$=RIGHT$("00"+STR$(LIG1%),2)             ! Arma llave para
        FIS.KIT$ = PACK$(PREFOF$ + LIGA$)            ! lectura de Regs MP5
        READ FORM "C16 C6 C2 C18 C35";              \! asociadas desde  Mod GR-OVS 8 Abr 2010 Exp. Itemr
             #4 KEY FIS.KIT$;                       \! EAMITEMR
             RELE1$,VALOF$,LIGO$,DESOFE$,RELE2$      !
        If TS.ER.RETURN <> -1 Then GoTo SALE.COLLAPSE
        PLUOF$ = UNPACK$(VALOF$)                     !
        READ FORM "C7 I1 C9 C5 C2 C18 C35";         \! Ahora lee loas ALIAS Mod GR-OVS 8 Abr 2010 Exp. Itemr
             #4 KEY VALOF$;                         \! verdaderos de Oferta
             RELE1$,IND1OF%,RELE3$,                 \!
             VALOF$,LIGA$,DESOFE$,RELE2$             !
        If TS.ER.RETURN <> -1 Then GoTo SALE.COLLAPSE
        FIS.FIV% = IND1OF%                           !
        CALL FIS.BUSTARIFA                           ! Busca Iva
        FIS.ITEM = -1  :  FT% = 0                    !
        FIS.VRU% = VAL(UNPACK$(VALOF$))*SIGNOF%      !
        FIS.SIGNO = SIGNOF%                          !
        USER.SIGNO = SIGNOF%                         !
        CALL FIS.CALIVA                              !
        VTA.TO% = FIS.VRU% * SL.IE.QTYORWGT          !
        VTOT.OFE% = VTOT.OFE% + VTA.TO%              ! Calcula Neto Oferta
        TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0)+VTA.TO%  !
        IF SIGNOF% = 1 THEN BEGIN                    !
           SL.HD.GROSSPOS=SL.HD.GROSSPOS+VTA.TO%     !
              TS.GROSSPOS=   TS.GROSSPOS+VTA.TO%     !
        ENDIF ELSE BEGIN                             !
           SL.HD.GROSSPOS=SL.HD.GROSSNEG+VTA.TO%     !
              TS.GROSSNEG=   TS.GROSSNEG+VTA.TO%
        ENDIF                                        !
!   write form "c38";#34;str$(sl.ie.qtyorwgt)+" "+str$(ts.totals(0,0,0))
        IF FIS.TARIFA > 0 AND                       \!
           FIS.TARIFA < 5 THEN BEGIN                 !
              TS.TOTALS(FIS.TARIFA,0,0) =           \!
              TS.TOTALS(FIS.TARIFA,0,0) + VTA.TO%    !
        ENDIF                                        !
        UNO$ = PACK$("01")                           ! Tipo String 01
        DOS$ = PACK$("02")                           ! Tipo String 02
        Udata$ = PACK$("99")                         ! Tipo String 99
        OFER.PLU$ = STR$(VAL(PLUOF$))                ! String de No. PLU
        CCC$ = CHR$(34) + CHR$(44) + CHR$(34)        ! Comilla,Coma,Comilla
        FIS.FIV%=LEN(OFER.PLU$)                      ! Calcula Long PLU
        FIS.FIV%=MOD(FIS.FIV%,2)                     ! y Verific Cant Dig
        IF FIS.FIV% = 0 THEN                        \! Si long es PAR, se
           OFER.PLU$ = PACK$(OFER.PLU$)             \! empaqueta como viene
        ELSE                                        \! si no, ajusta a PAR
           OFER.PLU$ = PACK$("0" + OFER.PLU$)        ! Valores PD --> IND1
        IF FIS.TARIFA = 1 THEN INDIC1$ = "0129"      ! Iva Tarifa 1   8  1
        IF FIS.TARIFA = 2 THEN INDIC1$ = "0065"      ! Iva Tarifa 2   4  1
        IF FIS.TARIFA = 3 THEN INDIC1$ = "0033"      ! Iva Tarifa 3   2  1
        IF FIS.TARIFA = 4 THEN INDIC1$ = "0017"      ! Iva Tarifa 4   1  1
        IF FIS.TARIFA = 5 THEN INDIC1$ = "0193"      ! Iva Tarifa 5   C  1
        IF FIS.TARIFA = 6 THEN INDIC1$ = "0097"      ! Iva Tarifa 6   6  1
        IF FIS.TARIFA = 7 THEN INDIC1$ = "0049"      ! Iva Tarifa 7   3  1
        IF FIS.TARIFA = 8 THEN INDIC1$ = "0145"      ! Iva Tarifa 8   9  1
                                                     ! Valrs PD-> IND2
        INDIC2$ = "2050"                             ! DigExt+,2050 Dig+,2048
        IF USER.SIGNO= -1 THEN INDIC2$ = "2178"      ! DigExt-,2178 Dig-,2176
        INDIC3$ = "01"                               ! Item Norm Digit
        IF USER.SIGNO= -1 THEN INDIC3$ = "81"        ! Item Norm Dig y Void
        TS.TEMP5$ = "00"
        IF USER.SIGNO= -1 Then TS.TEMP5$ = "01"
        TS.TEMP4$ = "0"
        If FIS.TARIFA = 7 Then TS.TEMP4$ = "0"		   ! Si es excento
        If FIS.TARIFA = 8 Then TS.TEMP4$ = "1"			 ! Si es exclusiv
        GrAlpha$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"        	
        GrName$ = ""
        For XJ% = 1 To Len(DESOFE$)																			! Recorre descripcion Item
         If match(Ucase$(Mid$(DESOFE$,XJ%,1)),GrAlpha$,1) <= 0 Then           \! Caracter especial
         	GrLt$ = " " Else GrLt$ = Mid$(DESOFE$,XJ%,1)                 ! Elimina char especial cambia x espacio
         GrName$ = GrName$ + GrLt$																					! Arma descripcion 
        Next XJ%
        
        OF.STR$(LIG1%+LIG2%) = Udata$  + ":" +      \! Arma String 99
               Pack$("20170119") + ":" +            \! Codigo proyecto
               OFER.PLU$         + ":" +            \! Codigo producto
               Pack$(Str$(UE.TABLAIVA(FIS.TARIFA))) + ":" + \! Tarifa de IVA 
               Pack$(TS.TEMP5$)     + ":" +         \! Signo del movimiento 
               TS.TEMP4$            + ":" +         \! Si Es excento o exclusivo
               GRNAME$                               ! Nombre del prodcuto 
                               
        OF.STR$(LIG1%+LIG2%+1)=UNO$+":"+OFER.PLU$   + \! Arma String 01
                ":" + PACK$(STR$(ABS(VTA.TO%)))   + \! segun formato de Reg
                ":" + MID$(RELE3$,3,2) + ":"  +     \! de SA/Prog Guide
                PACK$("000000")+":"+PACK$(INDIC1$)+ \!
                ":" + PACK$(INDIC2$) + ":" +        \!
                PACK$(INDIC3$)                       !

        OF.STR$(LIG1%+LIG2%+2)=DOS$ + "::::"  +     \! Arma String 02
                PACK$("1") + ":"              +     \! segun formato de Reg
                PACK$(STR$(ABS(FIS.VRU%))) + ":" +  \! de SA/Program Guide
                PACK$(STR$(SL.IE.QTYORWGT))   +     \!
                ":" + PACK$("32")                    !

        LIG2% = LIG2% + 2                            ! Incr Ind String 02
        
        IF VTA.TO% <> FIS.VRU%  THEN BEGIN           !
!          RELE1$ = "PIC(####-) C1 PIC(##,###-)"     !
!          WRITE FORM RELE1$;#34;SL.IE.QTYORWGT,    \!
!                     "x",FIS.VRU%                   !
!          WRITE FORM RELE1$;#36;SL.IE.QTYORWGT,    \!
!                      "x",FIS.VRU%                  !
!----------------------------------------------------v-------------------------
           CALL FORMAT.AMOUNT(SL.IE.QTYORWGT)        !      Ses-34/36 Cambio 2
           NFF.CNT$ = RIGHT$("     "+TS.TEMP1$,5)    !
           CALL FORMAT.AMOUNT(FIS.VRU%)              !
           NFF.AMT$ = RIGHT$("       "+TS.TEMP1$,8)  !
           NFF.S34$ = NFF.CNT$ + "x" + NFF.AMT$      !
!           CALL U.IMPRIME(NFF.S34$,6100H)           ! Elimina Impresion 28May2012 GR-OVS
!----------------------------------------------------!-------------------------
        ENDIF                                        !
        ANCHO% = MATCH("00000",PLUOF$,1)             !
        IF ANCHO% = 0 THEN BEGIN                     !
!          WRITE FORM "C38";#34;PLUOF$               !
!          WRITE FORM "C38";#36;PLUOF$               !
!----------------------------------------------------v-------------------------
           NFF.S34$ = PLUOF$                         !      Ses-34/36 Cambio 3
           CALL U.IMPRIME(NFF.S34$,6100H)           !
!----------------------------------------------------!-------------------------
!          RELE1$="C8 C18 PIC(######,###-) C1"       !
!          WRITE FORM RELE1$;#34;" ",DESOFE$,       \!
!                VTA.TO%,TS.SDESC$(160)              !
!          RELE1$="C8 C18 PIC(######,###-) C1"       !
!          WRITE FORM RELE1$;#36;" ",DESOFE$,       \!
!                VTA.TO%,TS.SDESC$(160)              !
!----------------------------------------------------v-------------------------
           CALL FORMAT.AMOUNT(VTA.TO%)               !      Ses-34/36 Cambio 4
           NFF.AMT$=RIGHT$("          "+TS.TEMP1$,11)!
           NFF.S34$="        "+DESOFE$+NFF.AMT$  +  \!
                    TS.SDESC$(160)                   !
           CALL U.IMPRIME(NFF.S34$,6100H)           !
!----------------------------------------------------!-------------------------
        ENDIF                                        !
        IF ANCHO% > 0 THEN BEGIN                     !
!          RELE1$ =                                 \!
!          "PIC(#######-) C18 PIC(######,###-) C1"   !
!          WRITE FORM RELE1$;#34;                   \!
!                VAL(PLUOF$),DESOFE$,VTA.TO%,       \!
!                TS.SDESC$(160)                      !
!          WRITE FORM RELE1$;#36;                   \!
!                VAL(PLUOF$),DESOFE$,VTA.TO%,       \!
!                TS.SDESC$(160)                      !
!----------------------------------------------------v-------------------------
           NFF.CNT$ = RIGHT$("        " +           \!
                      STR$(VAL(PLUOF$)),7) + " "     !      Ses-34/36 Cambio 5
           CALL FORMAT.AMOUNT(VTA.TO%)               !
           NFF.AMT$=RIGHT$("          "+TS.TEMP1$,11)!
           NFF.AMT$=RIGHT$("            ",12)        ! Se elimina precio y flag iva impresion 28 May 2012
!           NFF.S34$=NFF.CNT$+DESOFE$+NFF.AMT$   +   \!
!                    TS.SDESC$(160)                   !

           NFF.S34$=NFF.CNT$+DESOFE$+NFF.AMT$        ! Mod 28May2012

           CALL U.IMPRIME(NFF.S34$,6100H)           !
!           CALL U.IMPRIME(NFF.S34$,2100H)           !
!----------------------------------------------------!-------------------------
        ENDIF                                        !
        TS.SDESC$(160) = " "                         !
        IF VAL(UNPACK$(LIGO$)) = 0 THEN BEGIN        !
           OF.END = LIG1% + LIG2%                    ! Cantidad de Strings
           LIG1%=99                                  !
           PREFOF$ = ""                              !
        ENDIF                                        !
!        write #34;"Liga va en:"+str$(lig1%)
    NEXT LIG1%                                       !
!   WRITE FORM "C2 C6 C21 PIC(#####,###-)";#34;     \!
!         CHR$(27)+CHR$(69)," ",                    \!
!         "**Subtotal Oferta ->",VTOT.OFE%           !
!----------------------------------------------------v-------------------------
    TS.TEMP1I4 = ( VTOT.OFE% / SL.IE.QTYORWGT )      ! Vlr Unit Oferta
    Call Format.Amount(TS.TEMP1I4)                   ! Formatea Valor
    TS.TEMP1$ = Str$(SL.IE.QTYORWGT) + " x " + TS.TEMP1$
    TS.TEMP1$ = Left$(TS.TEMP1$+STRING$(38," "),38)
    CALL U.IMPRIME(TS.TEMP1$,6100H)                  !

    CALL FORMAT.AMOUNT(VTOT.OFE%)                    !      Ses-34 Cambio 6
    NFF.AMT$ = RIGHT$("          "+TS.TEMP1$,10)     !
    NFF.S34$ = FONPH$ +                             \!
               "      **Subtotal Oferta ->"+NFF.AMT$ !
    CALL U.IMPRIME(NFF.S34$,2100H)                  !
!------------------------------------------------------------------------------
 SALE.COLLAPSE:
END SUB                                              !
!------------------------------------------------------------------------------
SUB TSTPEC01 EXTERNAL                                  ! Rut. escritura en TSL
END SUB                                                !
                                                       !
SUB TSTPEC06 EXTERNAL                                  ! Rut. Escrit XLOG y TSL
END SUB                                                !
                                                       !
SUB TSHIECET EXTERNAL                                  ! Rut. para mandar tono
END SUB                                                !
                                                       !
SUB TSCSEC03 EXTERNAL                                  ! Despliega Error
END SUB                                                !

SUB TSCSEC08 EXTERNAL                                  ! Despliega Error
END SUB                                                !
!------------------------------------------------------------------------------
!                 Subrutinas del Modulo CFSEG2.BAS de la LA 14 S.A.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!           Rutina para armar Fecha y Hora de Transaccion
!------------------------------------------------------------------------------
SUB TOMA.FECHORA                                       !
    FRA.TIM$ = TIME$                                   ! Toma Hora Sistema
    FRA.H$  = LEFT$(FRA.TIM$,2)                        ! Arma horas
    FRA.M$  = MID$(FRA.TIM$,3,2)                       ! Arma minutos
    FRA.S$  = RIGHT$(FRA.TIM$,2)                       ! Arma segundos
    FRA.FEC$= DATE$                                    ! Toma Fecha Sistema
    FRA.AA$ = LEFT$(FRA.FEC$,2)                        ! Arma ano
    FRA.MM$ = MID$(FRA.FEC$,3,2)                       ! Arma mes
    FRA.DD$ = RIGHT$(FRA.FEC$,2)                       ! Arma dia
    FRA.LIN$= "Hora: "+FRA.H$+":"+FRA.M$+             \!
              ":"+FRA.S$+" Fecha: "+FIS.DL$            ! Arma la Linea
END SUB                                                !
!------------------------------------------------------------------------------
!                         FIN DE RUTINAS COMUNES
!------------------------------------------------------------------------------


SUB CFTSU07.011 PUBLIC
!------------------------------------------------------------------------------
!         **  PROYECTO IBM -  LA 14 S.A.   **
!
!     USER EXIT          =  UT0701A.ARC
!     Punto de Entrada   =  EAMTSU07.J86
!     Fecha de Revisi¢n  =  Feb. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!-----------------------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!    Esta linea se inserta para abrir el archivo
!    ADX_IDT1:CUPOS.DAT, EMPRE.DAT incluyendo su contenido
!    en el archivo tipo INCLUDE EAMTSU07.J86
!
!-----------------------------------------------------------------------------
!    create "r::log4610" as 81 buffsize 32000
  FONPH$  = CHR$(1BH)+CHR$(64H)+CHR$(1)
  FONPHN$ = CHR$(1BH)+CHR$(64H)+CHR$(0)
  FONCR$  = CHR$(1BH)+CHR$(47H)+CHR$(1)
  FONLF$  = CHR$(0AH)
  FCORT$  = CHR$(1BH)+CHR$(6DH)
  DIM OF.STR$(100)                                     !
  DIM FIS.TOTV(8)                                      ! Reset de Totales
  DIM FIS.IMP.TOT(8)                                   ! Declara Arreglo Iva
  DIM FIS.EQ$(8)                                       ! Define Equiv Tarifas
  FIS.EQ$(1) = "A"    :    FIS.EQ$(2) = "B"            !
  FIS.EQ$(3) = "C"    :    FIS.EQ$(4) = "D"            !
  FIS.EQ$(5) = "E"    :    FIS.EQ$(6) = "F"            !
  FIS.EQ$(7) = "X"    :    FIS.EQ$(8) = "*"            !
  DIM FIS.IVA%(8)                                      !
    FIS.IVA%(1) = 19                                   ! Toma Plan IVA A
    FIS.IVA%(2) = 19                                   ! Toma Plan IVA B
    FIS.IVA%(3) = 05                                   ! Toma Plan IVA C
    FIS.IVA%(4) = 08                                   ! Toma Plan IVA D
    FIS.IVA%(5) = 00                                   ! Toma Plan IVA AB
    FIS.IVA%(6) = 08                                   ! Toma Plan IVA BC
    FIS.IVA%(7) = 00                                   ! Toma Plan IVA EXCL
    FIS.IVA%(8) = 00                                   ! Toma Plan IVA EXEN
  PREFOF$  = ""                                        !
  PREFALI$ = "  "                                      !

  MOTO% = 0   :   MOTOP% = 0                           !
END SUB

SUB CFTSU08.011 PUBLIC
!-------------------------------------------------------------------------------
!         **  PROYECTO IBM -  LA 14 S.A.   **
!
!     USER EXIT          =  UT0801A.VRF
!     Punto de Entrada   =  EAMTSU08.J86
!     Fecha de Revisi¢n  =  Feb. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!------------------------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
! Estas lineas se insertan en la USER EXIT 08  para controlar la
! venta de Articulos  con Indicativos especiales de Verificacion
! interceptando y procesando los PLU's de Drogueria y Cosmeticos
! como si fueran Verificables.
!
! Como guia documentativa, se INCLUYE la siguiente informacion
! para tener en cuenta en la defincion de "Opciones de Usuario"
! en el Capitulo "Personalizacion" de TERMINAL OPTIONS = 19:
!
!                                   Descrip/Reporte
!                                      3058-3064
!
! El Display del Operador le Indica con B105 el comienzo de la
! Verificacion
!------------------------------------------------------------------

  PREFALI$ = RIGHT$(UNPACK$(IR.ITEMCODE$),10)            !
!  SUBTL% = 0                                             !
!  IF PREFOF$ = PREFALI$ THEN BEGIN                       !
!     FIS.KIT$ = PACK$(PREFALI$)+RIGHT$(IR.LINKEDTO$,1)   !
!     IR.LINKEDTO$ = FIS.KIT$                             ! Cambia Cod Nueva Lect
!  ENDIF                                                  !

!------------------------------------------------------------------------------

!%INCLUDE EXIT2\USR0801.011

END SUB
                                                       !
SUB CFTSU14.011 PUBLIC
String RELE1$,VALOF$,LIGO$,DESOFE$,RELE2$
Integer*4 NUM.OFE%, VAL.OFE%
!   ***************************************************************************
!   **    Project    :  IBM - LA 14                                          **
!   **    Date       :  Febrero de 1999                                      **
!   **    User Exit  :  UT1401A.KGS                                          **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  SOLSER49                                             **
!   **                                                                       **
!   **            ***    Descripci¢n del Fuente      ***                     **
!   **                                                                       **
!   **   Restaura Lectura de Controles Desc, Claves etc al DESASEGURARSE     **
!   **                                                                       **
!   **   Separa el Codigo de la Clave en las secuencias de Autorizacion      **
!   **   de la Supermarket Application                                       **
!   **                                                                       **
!   **   Coloca indicativo de ANULACION a FRUVER                             **
!   **                                                                       **
!   **   Actualiza Fecha de Cambio de Clave si el Cajero Cambia su CLAVE     **
!   **                                                                       **
!   **   Torna VERIFICABLES LOS PLU's de la Funcion NOSALE #PLU ENTER        **
!   **   para permitir con un nuevo ENTER el registro de la Venta.           **
!   **   Permitida en PLU's normales y en Ventas por PESO                    **
!   **                                                                       **
!   **   Respuesta a la secuencia de digitaci¢n:                             **
!   **                                                                       **
!   **        .------.     .--------.                                        **
!   **        | PESO |     | BORRAR |                                        **
!   **         ------       --------                                         **
!   **                                                                       **
!   **   Para exhibir el peso de cualquier producto. Ver complemento con     **
!   **   User Exit 23 de Despliegue sobre los Visores....                    **
!   **                                                                       **
!   **   Maneja el Prefijo 777777 para Fruver                                **
!   **                                                                       **
!   ***************************************************************************
!------------------------------------------------------------------------------

IF TS.IO.STATE    = 10 AND                            \! Si estado MAIN y
   TS.IO.MOTORKEY = 73 AND                            \! con MOTOR BORRAR y
   TS.IO.KEYS(6)  = 72 AND                            \! con tecla PESO y
   TS.IO.KEYS(2)  = 0  AND                            \! sin ENTER y
   TS.IO.KEYS(0)  = 0 THEN BEGIN                       ! sin FC BORRAR
      TS.XXMOD = 0                                     ! Borra Ident XX="PS"
      TS.IO.KEYS(5)  = 74                              ! Coloca FC=PRECIO
      TS.IO.DATA$(5) = "0"                             ! con CERO
ENDIF                                                  !

!------------------------------------------------------------------------------

! IF TS.PROCEDURE < 1 THEN BEGIN                        ! Si esta en Venta
!   IF TS.IO.KEYS(6) = 72 AND                          \! y se Digita PESO
!      TS.IO.MOTORKEY > 0 AND                          \! y el Motor esta OK
!      LEN(TS.IO.DATA$(2)) < 7 THEN BEGIN               ! con Plu Fruver
!         TS.IO.DATA$(2)=RIGHT$("000000"+              \!
!                        TS.IO.DATA$(2),6)              ! Inserta Ceros
!         TS.IO.DATA$(2)="777777" + TS.IO.DATA$(2)      ! Inserta Prefijo 777777
!         TS.IO.MOTORKEY=80                             ! Asigna Tecla ENTER
!         TS.IO.KEYS(2) =80                             ! con MOTOR Enter
!         ANUL% = 1                                     ! Reset de Anular
!         IF TS.IO.KEYS(1) = 70 THEN BEGIN              ! y se Digita VOID
!            ANUL% = -1                                 ! se torna negativo
!         ENDIF                                         ! Fin de Anular
!   ENDIF                                               ! Fin de Balanza
! ENDIF                                                 ! Fin Procedm Vta


!---------------------------------------------------------------------------
!         **  PROYECTO LA 14  **
!
!   **  USER EXIT          :  USR1402A.011
!   **  Punto de Entrada   :  EAMTSU14.J86
!   **  Date               :  Febrero de 1999
!   **  Unidad Responsable :  T‚cnicos de Sistemas POS
!-----------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!    Estas lineas se insertan en la USER EXIT 14 para manejar la
!    secuencia de Venta de Ofertas y/o Promociones
!
!    Los PLU's de Ofertas/Promociones estan en el rango de 1 a 9999
!    Este RANGO de Ofertas/Promociones existen en EAMITEMR con el
!    PREFIJO '99999999' para conformar la siguiente estructura:
!
!               99999999pppp, donde:    pppp = No. de Of/Promoc (1-9999)
!
!    Adicionalmente todos los PLU's de los Articulos que conforman la Promocion
!    se numeran de 01-99. Tambien utilizan un PREFIJO y se identifican de la
!    siguiente manera:
!
!               999999ppppnn, donde:    pppp = No. de Of/Promoc   (1-9999)
!                                         nn = No. de PLU Of/Prom (1-99)
!
!    La caracteristica principal es: PRICING METHOD = 5 con un 'Alias Code'
!    que identifica el verdadero PLU de Venta.
!
!    Toda la CADENA de 'Linked Items' se hace a traves de los PLU's con
!    PRFIJO 999999pppp para evitar el encadenamiento en la Venta Individual
!    de cualquiera de los Items de la Cadena.
!------------------------------------------------------------------------------

IF TS.IO.STATE    = 10   AND                        \! Si esta en MAIN y
   TS.IO.MOTORKEY = 80  THEN BEGIN                   ! con ENTER
   IF LEFT$(TS.IO.DATA$(02),8)="88888888" THEN BEGIN ! Si oferta Dig/Scanned
      TS.IO.MOTORKEY = 251                           ! Asigna Motor Oferta
      TS.IO.KEYS(05) = 251                           ! Asigna Tecla Oferta
      TS.IO.KEYS(02) = 0                             ! Borra ENTER
      TS.IO.DATA$(5) = RIGHT$(TS.IO.DATA$(02),      \! No. de la Oferta
                       LEN(TS.IO.DATA$(02)) - 8)     ! eliminando 8's
      TS.IO.DATA$(5) = LEFT$(TS.IO.DATA$(5),4)       ! Elimina Dig Ctrl
      TS.IO.DEVICE = 1                               ! Simula entrada digitada
      TS.IO.DATA$(2) = ""                            ! Borra No. Oferta
      OFERTA% = 1
   ENDIF                                             !
ENDIF                                                !

      IF SUBTL% = 1 AND                             \!
         NOT TS.BAL.TAKEN THEN BEGIN                 ! y no ha totalizado
             TS.GUIDANCE = 1020                      !
             TS.IO.MOTORKEY = 0                      !
      ENDIF                                          !


IF SUBTL% = 1 THEN BEGIN                             ! Subtotal Ofertas
   SUBTL% = 0                                        !
   IF RIGHT$(TS.PRTBUF$,12) <>                      \!
      "            " THEN BEGIN                      !
!     WRITE FORM "C2 C8 C18 PIC(######,###-) A1";   \!
!           #34;CHR$(27)+CHR$(69)," ",              \!
!           "VR/NETO OFERTA -->",VTOT.OFE%           !  *SIGNOF%
!------------------------------------------------------------------------------
    CALL FORMAT.AMOUNT(VTOT.OFE%)                    !      Ses-34 Cambio 7
    NFF.AMT$ = RIGHT$("          "+TS.TEMP1$,11)     !
    NFF.S34$ = FONPH$ + "        "  +               \!
               "VR/NETO OFERTA -->" + NFF.AMT$       !

!-- Mod 29May2012

		If (TS.IO.KEYS(1) = 70) OR (TS.IO.KEYS(8) = 67) Then \										! Es una anulacion producto
		    Gr.Mkp.VlrCmpBol% = Gr.Mkp.VlrCmpBol% - VTOT.OFE% Else \
		    Gr.Mkp.VlrCmpBol% = Gr.Mkp.VlrCmpBol% + VTOT.OFE%   									! Si es una venta de producto


    CALL U.IMPRIME(NFF.S34$,4100H)                  !
!------------------------------------------------------------------------------
   ENDIF                                             !
ENDIF                                                !

IF TS.IO.STATE    = 10   AND                        \! Si esta en MAIN y
   TS.IO.MOTORKEY = 251  THEN BEGIN                  ! Tecla de Oferta
      FIS.LIN% = VAL(TS.IO.DATA$(5))                 ! No. de la Oferta
      OFERTA% = 1
      IF FIS.LIN% < 1 OR FIS.LIN% > 99 THEN BEGIN    ! Valida No Oferta
         FIS.LIN% = 0                                ! Si hay error
         TS.GUIDANCE = 1003                          ! en rango, avisa
         TS.IO.MOTORKEY = 0                          ! con sec Inval
      ENDIF                                          !
      SIGNOF% = 1                                    ! Coloca Signo
      IF TS.IO.KEYS(1) = 70 THEN BEGIN               ! y se Digita VOID
         SIGNOF% =  -1                               ! se torna negativo
      ENDIF                                          ! Fin de Anular
      OFERT$ = RIGHT$("0000"+STR$(FIS.LIN%),4)       ! arma Cod Oferta
      KEYOF$ = PACK$("99999999" + OFERT$)            ! y adiciona prefijo
      TS.ER.RETURN = -1                              ! Retorna si NO Abre OK
      READ FORM "C16 C6 C2 C18 C35";                \! Lectura EAMITEMR Oferta
             #4 KEY KEYOF$;                         \!
             RELE1$,VALOF$,LIGO$,DESOFE$,RELE2$      !
      IF TS.ER.RETURN <> -1  THEN BEGIN              ! Si NO Existe Oferta
         LOCATE #30;1,1,OFF                          ! Notifica al Operador
         WRITE FORM "2C20";#30;                     \!
               " NO EXISTE OFERTA",                 \!
               " Num = "+STR$(FIS.LIN%)+"  REVISE.!" !
         TS.IO.MOTORKEY = 0                          ! Suprime proceso
      ENDIF ELSE BEGIN                               ! Si lectura Ok
         NUM.OFE% = FIS.LIN%                         ! Toma Oferta
         NOM.OFE$ = DESOFE$                          ! Toma Descripcion
         VAL.OFE% = VAL(UNPACK$(VALOF$))             ! Toma Vr Oferta
         LIG1% = 0                                   ! Simula el ingreso
         TS.IO.KEYS(5) = 0                           ! de un PLU Normal
         TS.IO.DATA$(5) = ""                         !
         PREFOF$ = "999999" + OFERT$                 ! Prefijo ofertas
         TS.IO.DATA$(2) = "999999" + OFERT$ + "99"   ! Codigo Total del
         TS.IO.KEYS(2) = 80                          ! Desc de la Oferta
         IF TS.IO.DATA$(6) = "" THEN BEGIN           ! Si no hay QTY
            TS.IO.KEYS(6) = 75                       ! asigna QTY
            TS.IO.DATA$(6) = "1"                     ! y la unidad
         ENDIF                                       !
         TS.IO.MOTORKEY = 80                         ! con ENTER normal
      ENDIF                                          !
ENDIF                                                !
!------------------------------------------------------------------------------


END SUB

SUB CFTSU20.011 PUBLIC
String BUF$,CAN.CRE$, VAL.UNI$, LF$, COD$, ULTIMO$, VR$, SUPER$
Integer*2 POSX%, TYPE5%, POSL%, POSA%, FLAG.SR%, UIND1%
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Febrero de 1999                                      **
!   **    Project    :  IBM - ** LA 14 **                                    **
!   **    User Exit  :  UT2001A.IVA                                          **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  SOLSER49                                             **
!   **                                                                       **
!   **    Instrucciones para mostrar la Linea de Iva de la SMA/4690 en la    **
!   **    Tira del Cliente (CR). Para HABILITARLA, ACTIVAR Opt/User 1 = Y    **
!   **                                                                       **
!   **    Se agregan las siguientes instrucciones para dejar una Linea       **
!   **    en blanco despues de la linea de TOTAL                             **
!   **                                                                       **
!   **    Code for printing IVA on the customer receipt station.             **
!   **    Report descriptor 3058 needs to be updated to reflect User Option  **
!   **                                                                       **
!   **    Imprime datos adicionales con algunos mensajes exigidos por        **
!   **    la Ley, como Nit, No. de Factura y Fecha de Expedicion.            **
!   **                                                                       **
!   ***************************************************************************


!%INCLUDE EXIT2\USR2006.011

IF TS.LINETYPE = 1 OR                               \!
   TS.LINETYPE = 5 THEN BEGIN                        !
      PREFALI$ = RIGHT$(UNPACK$(IR.ITEMCODE$),10)    !
      IF PREFOF$+"99" = "99"+PREFALI$ THEN BEGIN     ! Si es PLU Oferta
         CALL ITEM.COLLAPSE                          !
      ENDIF
EndIf                                                !

!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Marubre de 19 96                                      **
!   **    Project    :  IBM - ** LA 14 **                                    **
!   **    Responsible:  IBM DE COLOMBIA                                      **
!   **    User Exit  :  UT2003A.VTA                                          **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  SOLSER49                                             **
!   **                                                                       **
!   **    Estas lineas se insertaran en el archivo EAMTSU20.J86              **
!   **    a manera  de INCLUDE para mostrar el  PLU en la linea              **
!   **    de impresion, respetando  los  7  primeros caracteres              **
!   **    cuando la aplicacion los tenga.                                    **
!   **                                                                       **
!   **    Adicionalmente analiza cada linea de Impresion que se              **
!   **    registra en CR, buscando caracteres de multiplicaci¢n,             **
!   **    abreviaturas de Autorizaci¢n para reintegros, Cambios              **
!   **    precio, L¡mites etc. (PR, AS, CL, VF,etc)                          **
!   **                                                                       **
!   ***************************************************************************

CONTUSEG20:                                            !
If FRA.FAC$ = "" THEN BEGIN                            !
   FRA.FEC$ = DATE$                                    ! Toma Fecha Sistema
   FRA.AA$  = LEFT$(FRA.FEC$,2)                        ! Arma ano
   FRA.MM$  = MID$(FRA.FEC$,3,2)                       ! Arma mes
   FRA.DD$  = RIGHT$(FRA.FEC$,2)                       ! Arma dia
   FRA.FAC$ = FRA.AA$+"   "+FRA.MM$+"   "  +          \! Arma Linea Franq
              FRA.DD$+"     "+TS.STORE$                ! con Fecha y C/Costo
ENDIF                                                  !

IF TS.LINETYPE = 5 THEN BEGIN                          ! Si Cant/Prcio Item
   FT% = 0                                             ! Restaura 1er Vez IVA
   BUF$ = TS.PRTBUF$                                   ! Toma Buffer
   TYPE5% = 1                                          ! Marca de CANT y VAL
   POSX% =  MATCH(CHR$(64),BUF$,1) - 2                 ! Busca "x" para CANT
   IF POSX%<1 THEN POSX% = MATCH("x",BUF$,1) - 2       ! Busca "x" para CANT
   If POSX% < 2 Then POSX% = 3                         ! Add OVS 19 Oct 2005
   POSL% =  MATCH("# ",BUF$,POSX% + 2)                 ! Busca Long VrUnit
   POSL% =  POSL% - POSX% - 2                          ! Calcula Long VrUni
   POSX% = POSX% - 1                                   !
   If POSL% <= 0 Then POSL% = 1
   If POSX% <= 0 Then POSX% = 1
   CAN.CRE$=RIGHT$("        "        +                \!
            MID$(BUF$,2,POSX%),8)                      ! Inicia con Cant
   VAL.UNI$ = RIGHT$("          "    +                \!
         MID$(BUF$,POSX%+4,POSL%),10)+"  "             ! Guarda Vr.Unit
ENDIF                                                  !

IF TS.LINETYPE = 1 THEN BEGIN                          ! Si es Item Sale
   FT% = 0                                             ! Restaura 1er Vez IVA
   BUF$ = TS.PRTBUF$                                   ! Toma Buffer
!        write form "c60 c2";#81;"1-"+ts.prtbuf$,chr$(13)+chr$(10)
   LF$  = LEFT$(TS.PRTBUF$,7)                          ! Halla contenido
   POSX% =  MATCH(CHR$(64),BUF$,1) - 1                 ! Busca "x" para CANT
   If POSX% < -2 Then POSX% = 0                        ! Add OVS 19 Oct 2005
   POSL% =  MATCH("# ",BUF$,POSX% + 2)                 ! Busca Long VrUnit
   COD$ = UNPACK$(IR.ITEMCODE$)                        ! Desempaqueta Codigo
   POSA% = MATCH("000",COD$,1)                         !
   IF POSA% = 1 THEN BEGIN                             !
      COD$ = RIGHT$(COD$,9)                            !
      COD$ = STR$(VAL(COD$))                           ! y quita CEROS Izq
   ENDIF                                               !
   IF FLAG.SR% = 0 AND LEN(COD$) >=7 THEN BEGIN        ! Si Cod > 7 Digitos
      COD$ = ITEMCODE$                                 ! Variable PLU EAMTSWKG
      COD$ = STR$(VAL(UNPACK$(COD$)))                  ! y desempaqueta
   ENDIF                                               ! Fin de Condic
   IF RIGHT$(COD$,7) = "9999999" THEN BEGIN            ! Si recupera 9's
      COD$ = STR$(VAL(UNPACK$(IR.SALEPRIC$)))          ! y desempaqueta
   ENDIF                                               ! Cod ALIAS desde Precio
   COD$ = RIGHT$("       " + COD$ ,7 )                 ! Justifica a la Derecha
   IF LEFT$(COD$,7) = " 999000"  THEN BEGIN            ! Si es Modo Autonomo
      COD$ = RIGHT$("       "+TS.IO.DATA$(10),7)       ! lo reemplaza por DATA
   ENDIF                                               !
   IF LEFT$(COD$,5) = " 9999" THEN BEGIN               ! Si es Tecla Deptal
      COD$ = "       "                                 ! No muestra el PLU
   ENDIF                                               !

   IF LEFT$(UNPACK$(IR.ITEMCODE$),6) =                \!
      "999999"  THEN BEGIN                             ! Si es PLU de Oferta
!        write form "c60 c2";#81;"2-"+ts.prtbuf$,chr$(13)+chr$(10)
      IF RIGHT$(TS.PRTBUF$,12) <>                     \!
         "            " THEN BEGIN                     !
         IF RIGHT$(UNPACK$(IR.ITEMCODE$),2)="99" THEN BEGIN
            SUBTL% = 1                                 ! Indicativo Subtotal
            VTOT.OFE%=VTOT.OFE%-SL.IT.XPRICE*SIGNOF%   ! Calcula Neto Oferta
            SL.HD.GROSSNEG=SL.HD.GROSSNEG-SL.IT.XPRICE*SIGNOF%
            TS.GROSSNEG=TS.GROSSNEG-SL.IT.XPRICE*SIGNOF%
         ENDIF                                         !
      ENDIF                                            !
      COD$ = "*of:"                 +                 \!
      RIGHT$(UNPACK$(IR.ITEMCODE$),3)                  ! reemplaza por Ind
   ENDIF                                               !
   IF LEFT$(COD$,1) = "0"  THEN BEGIN                  ! Si es PLU VALE/SUB
      COD$ = " " + RIGHT$(COD$,6)                      ! le suprime prefijo
   ENDIF                                               !
   ULTIMO$ = RIGHT$(STRING$(12," ") +                 \!
             STR$(VAL(UNPACK$(IR.ITEMCODE$))),12)      !
   IF LEFT$(ULTIMO$,6) = "      " THEN                \!
      ULTIMO$ = "   " + RIGHT$(ULTIMO$,6) + "   "      !
   ULTIMO$ = ULTIMO$ + MID$(TS.PRTBUF$,29,8)           !
   IF LF$ <> "       " THEN BEGIN                      ! Si hay Datos Item
!      write form "c60 c2";#81;"3-"+ts.prtbuf$+" Ln="+str$(len(ts.prtbuf$)),chr$(13)+chr$(10)
      IF RIGHT$(TS.PRTBUF$,20) <>                     \! Con Descrip/Vr
         STRING$(20," ") THEN BEGIN                    ! en Blancos
           VR$ = RIGHT$(TS.PRTBUF$,30)                 ! Toma vlr y descrip
           !TS.PRTBUF$ = COD$+" "+VR$                   ! Arma string de nuevo
           SUPER$ = LEFT$(BUF$,7)                      ! Salva Tipo Autoriz
           POSA% = MATCH("!",SUPER$,1)                 ! Busca cualq Letra
           IF POSA% = 0 THEN  : POSA% = 1              ! Rest a la Posic 1
           IF CAN.CRE$ = "        " THEN BEGIN         !
              CAN.CRE$ = "       1"                    ! *-*
              VAL.UNI$ = MID$(TS.PRTBUF$,28,10)        ! Agrega Total
              VAL.UNI$ = VAL.UNI$ + " "                !
           ENDIF                                       !
           LIN.CRE$=COD$ + "    "           +         \! Agrega Codigo
               MID$(TS.PRTBUF$,9,18)+"   "  +         \! Descripcion
               MID$(SUPER$,POSA%,2) +" "    +         \! y Autoriz
               CAN.CRE$+"   " + VAL.UNI$    +         \! Cant y Vr Unitario
               "        "+MID$(TS.PRTBUF$,28,10)       ! Agrega Total
           UIND1% = UIND1% + 1                         ! Incrementa Cont/Lin
           CAN.CRE$ = "        "                       !
           VAL.UNI$ = STRING$(12," ")                  !
!      write form "c60 c2";#81;"4-"+ts.prtbuf$+" Ln="+str$(len(ts.prtbuf$)),chr$(13)+chr$(10)
      ENDIF                                            !
   ENDIF ELSE BEGIN                                    !
!      write form "c60 c2";#81;"5-"+ts.prtbuf$+" Ln="+str$(len(ts.prtbuf$)),chr$(13)+chr$(10)
      VR$ = RIGHT$(TS.PRTBUF$,30)                      ! Toma vlr y descrip
      !TS.PRTBUF$ = COD$+" "+VR$                        ! Arma string de nuevo
      POSA% = MATCH("!",SUPER$,1)                      ! Busca cualq Letra
      IF POSA% = 0 THEN  : POSA% = 1                   ! Rest a la Posic 1
      IF CAN.CRE$ = "        " THEN BEGIN              !
         CAN.CRE$ = "       1"+ " "                    ! *.*
         VAL.UNI$ = MID$(TS.PRTBUF$,28,10)             ! Agrega Total
         VAL.UNI$ = VAL.UNI$ + " "                     !
      ENDIF                                            !
      LIN.CRE$=COD$ + "    "                +         \! Agrega Codigo
               MID$(TS.PRTBUF$,9,18)+"   "  +         \! Descripcion
               MID$(SUPER$,POSA%,2) +" "    +         \! y Autoriz
               CAN.CRE$+"   " + VAL.UNI$    +         \! Cant y Vr Unitario
               "        "+MID$(TS.PRTBUF$,28,10)       ! Agrega Total
      UIND1% = UIND1% + 1                              ! Incrementa Cont/Lin
      CAN.CRE$ = "        "                            !
      VAL.UNI$ = STRING$(12," ")                       !
   ENDIF                                               !
   IF LEFT$(UNPACK$(IR.ITEMCODE$),5)="08888" THEN BEGIN! Si Concesion, escribe
!      WRITE FORM "C38 A0";#34;DESOFE$                 ! Conces, Dpto y Fact
!      WRITE FORM "C38 A1";#34;DESOFE$                 !
!------------------------------------------------------v-----------------------
      NFF.S34$ = FONPH$ + DESOFE$                      !     Ses-34 Cambio 8
      CALL U.IMPRIME(NFF.S34$,4100H)                  !
!------------------------------------------------------!-----------------------
   ENDIF                                               !
   IF LEFT$(UNPACK$(IR.ITEMCODE$),5) <>               \!
      "08888" THEN BEGIN                               !
      ULT.PLU$ = TS.PRTBUF$                            ! Si NO es Plu Conces
   ENDIF                                               !
 ENDIF                                                  !

OFERTA% = 0                                            !

!%INCLUDE EXIT2\USR2006.011

End Sub

Sub CFTSU68.011 Public
Integer*2 DOSP%,SIGUE.OF%
!   ***************************************************************************
!   **    Date       :  Septiembre de 1999                                   **
!   **    Project    :  IBM - COMFANDI                                       **
!   **    Responsible:  IBM DE COLOMBIA                                      **
!   **    User Exit  :  UT6801A.OFE                                          **
!   **    Inclu¡r en :  EAMTSU68.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Estas lineas se insertaran en el archivo EAMTSU68.J86              **
!   **    para Inclu¡r NUEVAS Strings Tipo 01 como resultado de              **
!   **    la descomposicion de los PLU's de OFERTAS.                         **
!   **                                                                       **
!   **    No OLVIDAR incluir EAMTSXHC.J86 en EAMTSUPC.BAS para               **
!   **    poder invocar la rutina WRITE.SL.STR                               **
!   **                                                                       **
!   ***************************************************************************

!  if recolecta% = 0 then begin
!     create "r::recolec" as 77 buffsize 5180
!     recolecta% = 1
!  endif

!    write FORM "C38";#34;UNPACK$(PLU.OF$)+"*"+STR$(ELEMENT)+"*"+STR$(SL.END)+"*"+STR$(UE.II%)

    IF ENTRA$ <> "" THEN BEGIN                            ! Si hay String y es
       IF UNPACK$(LEFT$(ENTRA$,1))="00" THEN BEGIN        ! String tipo 00
          IF SIGNOF% = 1 THEN BEGIN                       !
             SL.HD.GROSSPOS=SL.HD.GROSSPOS+VTA.TO%        !
                TS.GROSSPOS=   TS.GROSSPOS+VTA.TO%        !
          ENDIF ELSE BEGIN                                !
             SL.HD.GROSSPOS=SL.HD.GROSSNEG+VTA.TO%        !
                TS.GROSSNEG=   TS.GROSSNEG+VTA.TO%
          ENDIF                                           !
       ENDIF                                              !
       IF UNPACK$(LEFT$(ENTRA$,1))="01" THEN BEGIN        ! String tipo 01
          DOSP% = MATCH(":",ENTRA$,3)                     ! Busca nuevo 3A
          If (DOSP%-3) <= 0 Then  DOSP% = 4               ! Add OVS 19 Oct 2005
          PLU.OF$ = MID$(ENTRA$,3,DOSP%-3)                ! Extrae Plu

          IF LEFT$(UNPACK$(PLU.OF$),6)="999999" THEN BEGIN! Si es oferta
             SIGUE.OF% = 1                                !
          ENDIF                                           !
       ENDIF                                              !
       IF UNPACK$(LEFT$(ENTRA$,1))="02" AND              \! String tipo 02
          SIGUE.OF% = 1 THEN BEGIN                        !
             SIGUE.OF% = 0                                !
             FOR UE.II% = 1 TO OF.END                     !
                 SL.END = SL.END + 1                      ! Restaura Num Strngs
                 SL.STR$(SL.END) = OF.STR$(UE.II%)        ! por los Items
             NEXT UE.II%                                  !
             DIM OF.STR$(100)                              ! y variables de la
             OF.END  = 0                                  ! explosion de PLU's
       ENDIF                                              !
    ENDIF                                                 !
!-------------------------------------------------------------------------
END SUB

!-- En proceso de recuperacion
Sub CFTSU53.011 Public

End Sub