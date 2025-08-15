\------------------------------------------------------------------------------
\                   NOMBRE DEL MODULO : RTIMODUL.BAS      
\     Modulo de Rutinas de Front-Store - Retefuente IVA Pafgos Electr¢nicos
\  
\               VERSION : General Consolidada Marzo de 2001
\
\       Primer Revision : Global DataTel      Marzo del 2001
\  
\    Resumen de Cambios : Cambio String '99' Decreto 633 de Oct 12/2000
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
!                   RUTINAS GENERALES
!-----------------------------------------------------------------------------
!
! E1    SAVE.PRINT              Salvar variables de Impresion     
! E2    RESTORE.PRINT           Restaurar variables de Impresion
! E3    SAVE.DISPLAY            Salvar variables del DISPLAY
! E4    RESTORE.DISPLAY         Restaurar variables del DISPLAY
! 
!-----------------------------------------------------------------------------

     %ENVIRON T                                        !

String    Global UE.LISTA.TARIFAS$, PRTFIS, GR.USERDATA$
Integer*4 Global UE.LISTA.BASES%(1)
Integer*4 Global UE.TOTALS(1), UE.TABLAIVA(1),TVRTPAG% ! Total Impuestos
Real      Global UE.BASE.428%, UE.BASE.IVA%

%INCLUDE EAMTSWKG.J86                                  ! working storage
%INCLUDE EAMETWKA.j86                                  !
%INCLUDE EAMTRANS.j86                                  !
%INCLUDE EAMTERMS.J86                                  !
%INCLUDE EAMITEMR.J86                                  !
%INCLUDE EAMTSUVA.J86                                  ! working user storage
%INCLUDE EAMTOPTS.J86                                  !
%INCLUDE EAMSOPTS.J86                                  !
%INCLUDE EAMASMRT.J86                                  ! assembler SUBroutines
%INCLUDE EAMADXRT.J86                                  ! EXTERNAL SUBroutines
%INCLUDE SVRSTSSU.011
!------------------------------------------------------------------------------
!                  Subrutina de Despliegue de ARRAY Datos
!---------------- -------------------------------------------------------------
 
Sub VER PUBLIC                                       !

!   PRTLINE ="ts.io.device:"+str$(ts.io.device)       !
!   CALL U.IMPRIME(PRTLINE,4100H)                    !
!   CALL U.IMPRIME("Estado:"+STR$(TS.IO.STATE) +    \!
!         "  Proc:"+STR$(TS.PROCEDURE)      +        \!
!         " Motor:"+STR$(TS.IO.MOTORKEY)    +        \!
!         " Varie:"+STR$(SL.TE.TENDTYPE)    +        \!
!                   STR$(SL.TE.TENDVAR),4100H)        !
!   FOR RR% = 0 TO 10                                 !
!       PRTLINE = "TECLA "        +                  \!
!                 RIGHT$("  "+STR$(RR%),2) + " " +   \!
!                 "   " +STR$(TS.IO.KEYS(RR%))   +   \!
!                 "  DATO "+TS.IO.DATA$(RR%)          !
!       CALL U.IMPRIME(PRTLINE,4100H)                !
!	   WRITE #35;CHR$(1BH)+CHR$(6DH)             ! Eject paper
!   NEXT RR%                                          !

 END SUB                                             !
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

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$,U.D.TIEMPO%,U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$						! Variables de
Integer*2 U.d.visor%, U.d.tiempo%   							! trabajo
String U.D.Linea1$, U.D.Linea2$		
End Function
!--- Fin manejo de visores

FUNCTION ASIC.DATOS$(UMSG1$,UMSG2$) External   						! Captura de datos
STRING UMSG1$, UMSG2$, ASIC.DATOS$     							! Definicion de variables
End Function
!--- Fin Asic.Datos

Function U.CORTACR External								! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

Sub DECRETO428
  UE.LISTA.TARIFAS$ = "0"
  TS.ER.RETURN = -1
  Open "R::ASCNTRL" AS 94
  If TS.ER.RETURN = -1 THEN BEGIN		       ! Ok en la apertura
     IF END #94 THEN UE.BASE.FINAL		       ! Si es EOF
     While (1)															  
      Read #94; TS.TEMP1$			       ! Lectura registro
      If TS.TEMP1$ = "[DECRETO 428]" Then Begin	       ! 
        Read #94; TS.TEMP1$			       ! Lectura registro
        UE.LISTA.TARIFAS$ = Mid$(TS.TEMP1$,30,(LEN(TS.TEMP1$) - 29))
      Endif
     Wend
     UE.BASE.FINAL:
        Close 94
  EndIf Else Begin
    UE.LISTA.TARIFAS$ = "0"
    Call TSHIECET				             ! Tono de Alerta
    Call VISORES4690(1,"ERROR PARAMETROS EN ","LA CARGA DECRETO 428",1200,"L")
    Wait ; 1200
EndIf

End Sub 
!-- Fin de la carga del decreto 428


!Sub CALCULO.428                                  ! Calculo de Base iva a reportar
!Real TMP.428%, TMP.TOTAL%, tmp.calculo%
!Integer*1 Tmp.Pos%
!Integer*4 tmp.vlr.pago%
!
!    Dim UE.LISTA.BASES%(8)                       ! Init parametros
!    UE.BASE.428% = 0
!    For I% = 1 TO Len(UE.LISTA.TARIFAS$)
!
!      tmp.pos% = VAL(MID$(UE.LISTA.TARIFAS$,I%,1))
!
!      If MID$(UE.LISTA.TARIFAS$,I%,1) = "1" Then \
!        UE.LISTA.BASES%(1) = TS.TOTALS(1,0,0) - TVRTIV1%
!        
!      If MID$(UE.LISTA.TARIFAS$,I%,1) = "2" Then \
!        UE.LISTA.BASES%(2) = TS.TOTALS(2,0,0) - TVRTIV2%  
!        
!      If MID$(UE.LISTA.TARIFAS$,I%,1) = "3" Then \
!        UE.LISTA.BASES%(3) = TS.TOTALS(3,0,0) - TVRTIV3%        
!
!      If MID$(UE.LISTA.TARIFAS$,I%,1) = "4" Then \
!        UE.LISTA.BASES%(4) = TS.TOTALS(4,0,0) - TVRTIV4%
!
!      UE.BASE.428% = UE.BASE.428% + UE.LISTA.BASES%(tmp.pos%)
!
!    Next I%
!
!    If TVRTPAG% > TS.TOTALS(0,0,0) THEN \
!       Tmp.Vlr.Pago% = TS.TOTALS(0,0,0) ELSE  \
!       Tmp.Vlr.Pago% = TVRTPAG%
!       
!    tmp.428%     = ue.base.428% / 1000                    ! Se disminuyen los valores  en 1000
!    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
!    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
!    TMP.428% = tmp.calculo% * tmp.428%                    !
!    TMP.428% = TMP.428% / TMP.TOTAL%                      !
!    UE.BASE.428% = (TMP.428% * 1000)                      !
!End Sub

Sub CALCULO.428                                  ! Calculo de Base iva a reportar
Real TMP.428%, TMP.TOTAL%, tmp.calculo%
Integer*1 Tmp.Pos%
Integer*4 tmp.vlr.pago%

    Dim UE.LISTA.BASES%(8)                       ! Init parametros
    UE.BASE.428% = 0
    UE.BASE.IVA% = 0
    For I% = 1 TO Len(UE.LISTA.TARIFAS$)
      tmp.pos% = VAL(MID$(UE.LISTA.TARIFAS$,I%,1))
      UE.BASE.428% = UE.BASE.428% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.),0,0)
      UE.BASE.IVA% = UE.BASE.IVA% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.)*FLOAT(UE.TABLAIVA(tmp.pos%))/100.,0,0)
    Next I%
    TVRTPAG% = Val(TS.IO.DATA$(05))                       ! Toma valor capturado de pago
    
    If TVRTPAG% > TS.TOTALS(0,0,0) THEN \
       Tmp.Vlr.Pago% = TS.TOTALS(0,0,0) ELSE  \
       Tmp.Vlr.Pago% = TVRTPAG%
    
    tmp.428%     = ue.base.428% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    TMP.428% = TMP.428% / TMP.TOTAL%                      !
    UE.BASE.428% = (TMP.428% * 1000)                      ! Retorna Valor del Calculo

    tmp.428%     = ue.base.iva% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    TMP.428% = TMP.428% / TMP.TOTAL%                      !
    UE.BASE.IVA% = (TMP.428% * 1000)                      ! Retorna Valor del Calculo


End Sub


!----------------------------------------------------------------------------
!            Funci¢n para Solicitar Servicios de la Aplicaci¢n
!----------------------------------------------------------------------------
! SUB ADXSERVE(FIS.RET,FUNC,PARM1,PARM2$) EXTERNAL     !
!     STRING       PARM2$                              !
!     INTEGER*4    FIS.RET                             !
!     INTEGER*2    FUNC,   PARM1                       !
! END SUB                                              !
!------------------------------------------------------------------------------
!                 Subrutinas del Modulo Fiscal RTIMODUL.BAS
!----------------------------------------------------------------------------
!            Funci¢n para Conversion de Fecha y Hora
!----------------------------------------------------------------------------
SUB FIS.FEC(KFH$) PUBLIC                               ! 
    STRING KFH$,  HH$,  ME$,  FIS.FE$                  !
    M$ = "EneFebMarAbrMayJunJulAgoSepOctNovDic"        ! Tabla Meses
    IF VAL(MID$(KFH$,3,2)) = 0 THEN BEGIN              ! Si es Mes Invalido
       FIS.FE$ = "           ****"                     ! coloca stars
       EXIT SUB                                        ! y sale de rutina
    ENDIF                                              !
    HH$ =  MID$(KFH$,7,2) + ":"  +                    \! Compagina Hora
           MID$(KFH$,9,2) + ":"+ RIGHT$(KFH$,2)        !
    ME$ = MID$(M$,VAL(MID$(KFH$,3,2))*3-2,3)  + " "    ! Traduce Mes
    FIS.FE$ = "/19"+ LEFT$(KFH$,2)                     ! Coloca siglo XX
    IF VAL(LEFT$(KFH$,2)) < 98 THEN BEGIN              ! Coloca siglo XXI
       FIS.FE$ = "/20"+ LEFT$(KFH$,2)                  !
    ENDIF                                              !
    KFH$=ME$+RIGHT$(" "+STR$(VAL(MID$(KFH$,5,2))),2)+ \! Compagina Fecha
         FIS.FE$ + "     " + HH$                       !
END SUB                                                ! 
!------------------------------------------------------------------------------
!             Subrutina para Incluir IVA en Linea de Franqueo
!----------------------------------------------------------------------------
SUB FORMATO.FRANK                                      !
FOR RR% = 1 TO TO.NUMTNDR                              ! Lopp Pagos Definidos
    IF TO.TENDOPTS(RR%,0)=SL.TE.TENDTYPE THEN BEGIN    ! Si encuentra Tipo
       IF TO.TENDOPTS(RR%,1)=SL.TE.TENDVAR THEN BEGIN  ! y Variedad
          TS.TEMP1I1 = TO.TENDOPTS(RR%,5)              ! Toma Num Formato
          IF TS.TEMP1I1 <> 0 THEN BEGIN                ! Si hay Fmto definido
             TO.FRANK.TXT$(TS.TEMP1I1,3) =            \! Coloca IVA declarado
             LEFT$(TO.FRANK.TXT$(TS.TEMP1I1,3),       \! en Linea TRES del
             25) + RIGHT$("      " + "*IVA:" +        \! formato
             STR$(TVRTIVT%) + "*",13)                  !
             RR% = TO.NUMTNDR                          ! Reset del Loop
             TIVADEC% = TIVADEC% + TVRTIVT%            ! Acumula Iva Decl Trx
          ENDIF                                        !
       ENDIF                                           !
    ENDIF                                              !
NEXT RR%                                               !
END SUB                                                !
!------------------------------------------------------------------------------
!          Subrutina de Escritura de Variables para ReteFuente de IVA
!               Prepara informacion para String 99 de RetIVA
!------------------------------------------------------------------------------
SUB RTI.GRABA PUBLIC                                   !
! call ver
    KTIVAR$=STR$(SL.TE.TENDTYPE)+STR$(SL.TE.TENDVAR)   !
    IF (TS.IO.MOTORKEY = 90  OR                       \!
        TS.IO.MOTORKEY = 80) AND                      \!
        TS.IO.KEYS(09) = 90  AND                      \!
        TS.IO.DEVICE   = 1   THEN BEGIN                !
           TVRTPAG% = VAL(TS.IO.DATA$(05))             ! Se cambia de posicion de la 7 a la 5 GR - OVS 10May2010
           KNUMTAR$ = PACK$(TS.IO.DATA$(9))            !
    ENDIF                                              !
    TS.TEMP1I1 = MATCH(KTIVAR$,C4$,1)                  !    
    IF TS.TEMP1I1 <> 0 THEN BEGIN                      !
       CALL FORMATO.FRANK                              !
       TS.TEMP1I1 = 99                                 ! Envia al LOG Num.Fact
       RTI.AUX$ = "RT"                                 !
       RTI.AUX$ = RTI.AUX$ + "+"                       !
       IF (TS.GROSSPOS - TS.GROSSNEG) < 0 THEN        \!
          RTI.AUX$ = LEFT$(RTI.AUX$,2) + "-"           !
! write form "c10 pic(######,###)";#34;"Br(+):",ts.grosspos
! write form "c10 pic(######,###)";#34;"Br(-):",ts.grossneg
! write form "c10 4pic(###,###)";#34;"Iva(1-4):",     \!
!       val(nff.it1$),val(nff.it2$),val(nff.it3$),val(nff.it4$)
! write form "c10 4pic(###,###)";#34;"Ivas(5-8)",     \!
!       val(nff.it5$),val(nff.it6$),val(nff.it7$),val(nff.it8$)
       RTI.NET$ = STR$(ABS(TS.GROSSPOS-TS.GROSSNEG))   ! Vr Neto Trans
       RTI.IT1$ = PACK$(STR$(TVRTPAG%))                ! Vr Pago Electron
       RTI.IT2$ = PACK$(STR$(TVRTIVT%))                ! Iva declarado
       RTI.IT3$ = PACK$(STR$(TVRTDUE%))                ! Vta Total Trans
       RTI.IT4$ = PACK$(STR$(TVRTGRA%))                ! Vr Gravado
       RTI.IT5$ = PACK$(STR$(TVRTIMX%))                ! Vr Total IVA
       RTI.IT6$ = PACK$(STR$(TOTIMPC%))                ! Imp al Consumo
       ALTETROP$ = RIGHT$(TS.STORE$,3)       +        \!
             TS.TERMINAL$ + RIGHT$("0000"    +        \!
             STR$(SL.HD.TRANSNUM+1),4)       +        \!
             RIGHT$("000000"                 +        \!
             UNPACK$(SL.HD.OPERATOR$),6)               ! 
       IF FIS.TRLOG$ = "" THEN                        \!
          FIS.TRLOG$ = MID$(ALTETROP$,4,7)             !
       TS.USERDATA$=PACK$("21")+":"+RTI.AUX$ +        \!
          ":"+PACK$(DATE$+TIME$)             +        \! Arma Str Usuario con
          ":"+PACK$(ALTETROP$)+":"           +        \! Fec,Hr,Alm,Ter,Trn,Op
          PACK$(KTIVAR$)+":"+KNUMTAR$+":"    +        \!
          PF1$+FIS.TRLOG$+":"+PACK$(RTI.NET$)+        \! #Fact, Vr/Vta Neto
          ":" + RTI.IT1$ + ":" + RTI.IT2$    +        \! y Vrs de Iva
          ":" + RTI.IT3$ + ":" + RTI.IT4$    +        \!
          ":" + RTI.IT5$ + ":" + RTI.IT6$              !
       CALL TSTPEC01                                   ! Invoca Subrut WR-LOG
       FIS.IT1$ = "" : FIS.IT2$ = "" : FIS.IT3$ = ""   !
       FIS.IT4$ = "" : FIS.IT5$ = "" : FIS.IT6$ = ""   !
       FIS.IT7$ = "" : FIS.IT8$ = ""                   !
       TVRTPAG% = 0  : TVRTIVT% = 0  : TVRTTOT% = 0    !
       TVRTGRA% = 0  : TVRTIMX% = 0  : TOTIMPC% = 0    !
       TS.USERDATA$ = ""                               !
    ENDIF                                              !
END SUB                                                !
!------------------------------------------------------------------------------
!          Subrutina de Toma de Variables para ReteFuente de IVA
!------------------------------------------------------------------------------
SUB DATOS.RETIVA PUBLIC                                   !
    KEY$ = PACK$("100010001000")                          ! Plu temporizador
    TS.ER.RETURN = -1                                     !
    READ FORM "C8 I1 C8 C5 C2 C18 2I2 C31";#4 KEY KEY$;  \! Mod GR-OVS 8 Mzo 2010
         KEY$,KOPC%,C1$,C2$,C3$,C4$,KUE1%,KUE2%,GR.USERDATA$ !
    IF TS.ER.RETURN THEN BEGIN                            !
       C2$ = STR$(VAL(UNPACK$(C2$)))                      !
       KIV1 = 1 + FLOAT(VAL(MID$(C2$,1,2))) / 100.0       !
       KIV2 = 1 + FLOAT(VAL(MID$(C2$,3,2))) / 100.0       ! 
       KIV3 = 1 + FLOAT(VAL(MID$(C2$,5,2))) / 100.0       !
       KIV4 = 1 + FLOAT(VAL(MID$(C2$,7,2))) / 100.0       !
       KIVA% = VAL(UNPACK$(C3$))                          ! Motor para ReteIVA
       TS.SAVPRT$ = "%'s RetIva: "+MID$(C2$,1,2) + "% "+ \!
                    MID$(C2$,3,2)+"% "+MID$(C2$,5,2) +   \!
                    "% "+MID$(C2$,7,2)+"%"                !
!       WRITE FORM "C12 5PIC(#.##-)";#34;TS.SAVPRT$        !
!---------------------------------------------------------v--------------------

       PRTFIS = TS.SAVPRT$                                !

!       CALL U.IMPRIME(PRTFIS,4100H)                      !
!------------------------------------------------------------------------------
    ENDIF ELSE BEGIN                                      !
       Call TSHIECET				             ! Tono de Alerta
       Call VISORES4690(1,"NO APTA PARA VENTA.!"," ..FALTA 3xMIL..!   ",1200,"L")
       STOP                                               ! Suprime proceso
    ENDIF                                                 !
END SUB                                                   !
!------------------------------------------------------------------------------
!             Subrutina de Impresion para Soporte de ReteFuente       
!------------------------------------------------------------------------------
SUB TIQUETE.IVA                                        !
    IF TVRTGRA% <> 0 THEN BEGIN                        !
       FINLIN$ = CHR$(13)                              !+ CHR$(10)
       !LFEED$ = CHR$(1BH)+CHR$(33H)+CHR$(204)
       LFEED$  = CHR$(0AH)
       FONPH$  = CHR$(1BH)+CHR$(47H)+CHR$(1)
       FONPHN$ = CHR$(1BH)+CHR$(47H)+CHR$(00)
       CALL SAVE.PRINT
       WRITE #35;(FONPH$+" C O M F A N D I "+FINLIN$)       !
       WRITE #35;(FONPHN$+" NIT :890-303-208-0"+FINLIN$)    !
       WRITE #35;( "RETE/FUENTE IVA PAGOS ELECTRONICOS"+FINLIN$)!
       CALL RESTORE.PRINT
       CALL U.IMPRIME(" "+" Venta Exenta.."+STR$(TVRTEXE%),1100H)  !
       CALL U.IMPRIME(" "+"Venta Gravada.."+STR$(TVRTGRA%),1100H)  !
       CALL U.IMPRIME(" "+"  TOTAL Venta.."+STR$(TVRTTOT%),1100H)  !
       CALL U.IMPRIME(" "+STRING$(12,"-"),1100H)           !
       CALL U.IMPRIME(" "+"  TOTAL IVA -->"+STR$(TVRTIMX%),1100H)  !
       CALL U.IMPRIME(" "+STRING$(12,"-"),1100H)           !
       IF TIVADEC% <> 0 THEN BEGIN                          !
       CALL U.IMPRIME(" "+"Acum/Declarado:"+STR$(TIVADEC%),1100H)  
       ENDIF                                                !
       CALL U.IMPRIME(" "+"Pago T/DB-T/CR-->"+STR$(TVRTPAG%),1100H)!
       DEFI$ = "* IVA *"                                    !
       FORT$ = "C1 C2 C7 PIC(#####,###) A1"                 !
       IF KOPC% = 15 THEN BEGIN                             !
          DEFI$ = " "                                       ! 
          FORT$ = "C10 C2 C1 PIC(######,###) A1"            !
       ENDIF                                                !
       IF KOPC% AND 08H THEN BEGIN                          !
          CALL U.IMPRIME("    TOTAL:"+DEFI$+STR$(TVRTIMX%),1100H)!
       ENDIF                                                !
       IF KOPC% AND 04H THEN BEGIN                          !
          VRPLANA% = VRPLANA%                               !
          CALL U.IMPRIME("   PROPOR:"+DEFI$+STR$(VRPLANA%),1100H)!
          TVRTIVT% = VRPLANA%                               !
       ENDIF                                                !
       IF KOPC% AND 02H THEN BEGIN                          !
          NORETEN% = NORETEN%                               !
          CALL U.IMPRIME(" PARC/EXE:"+DEFI$+STR$(NORETEN%),1100H)!
          TVRTIVT% = NORETEN%                               !
       ENDIF                                                ! 
       IF KOPC% AND 01H THEN BEGIN                          !
          TVRTIVP% = TVRTIVP%                               !
               CALL U.IMPRIME( RIGHT$("        " +        \!
                STR$(KUE1%)+"%:",10)+                      \!
                DEFI$+STR$(TVRTIVP%),1100H)                 !
          TVRTIVT% = TVRTIVP%                               !
       ENDIF                                                !
       KFH$ = DATE$ + TIME$                                 !
       CALL  FIS.FEC(KFH$)                                  !
       C1$ = LEFT$(KFH$,12) + LEFT$(TIME$,2)  +            \!
             ":" + MID$(TIME$,3,2) + " "      +            \!
             RIGHT$(TS.STORE$,3) + " "        +            \!
             TS.TERMINAL$+" "+RIGHT$("0000"   +            \!
             STR$(SL.HD.TRANSNUM+1),4)        +            \!
             " " + UNPACK$(SL.HD.OPERATOR$)                 ! 
       CALL U.IMPRIME(C1$,1100H)                           !
    ENDIF                                                   !
    WRITE #35;CHR$(1BH)+CHR$(6DH)                           ! Eject paper
END SUB                                                     !
!------------------------------------------------------------------------------
!             Subrutina de Consulta de IVA para ReteFuente       
!------------------------------------------------------------------------------
SUB CONSULTA.RETIVA PUBLIC                                !
    Exit Sub 																							! Proceso anulado FECO 25Oct2024 GR-OVS
    IF TS.IO.MOTORKEY = KIVA% THEN BEGIN                  !
       IF VAL(TS.IO.DATA$(5)) = 0 THEN BEGIN              !
          Call VISORES4690(1,"INDIQUE EL MONTO DEL"," PAGO ELECTRONICO.! ",0,"L")
          TS.IO.MOTORKEY = 0                              ! Suprime proceso
       ENDIF ELSE BEGIN                                   !
          IF KIVA% > 199 AND KIVA% < 240 THEN BEGIN       ! 
             IF VAL(TS.IO.DATA$(5)) = 1 THEN BEGIN        ! Opc adic Impres
                CALL TIQUETE.IVA                          ! Repite ant liq
                TS.IO.MOTORKEY = 0                        ! Suprime proceso
                EXIT SUB                                  !
             ENDIF ELSE BEGIN                             !
                TVRTPAG% = VAL(TS.IO.DATA$(5))            ! Toma nuevo Pago
             ENDIF                                        !
             TVRTDUE% = TS.BALDUE(0)                      !
             TVRTTOT% = TS.TOTALS(0,0,0)                  ! Toma Total Vta
             TVRTEXE% = TS.TOTALS(0,0,0) -               \!
                        TS.TOTALS(1,0,0) -               \! Calcula Vta Exenta
                        TS.TOTALS(2,0,0) -               \!
                        TS.TOTALS(3,0,0) -               \!
                        TS.TOTALS(4,0,0)                  !  
             TVRTGRA% = TVRTTOT% - TVRTEXE%               ! Calcula Vta Grav
             IF TS.BALDUE(0) = 0 THEN BEGIN               !
                Call VISORES4690(1,"TOTALICE POR FAVOR.!","CUENTA VA EN=$0.oo  ",0,"L")
                TS.IO.MOTORKEY = 0                        ! Suprime proceso
                EXIT SUB                                  !
             ENDIF                                        !
             TOTIMPC% = IPOTAA%+IPOTBB%+IPOTCC%+IPOTDD%   !
             TVRTIV1% = (TS.TOTALS(1,0,0) - IPOTAA%) -   \!
                        (TS.TOTALS(1,0,0) - IPOTAA%) / KIV1
             TVRTIV2% = (TS.TOTALS(2,0,0) - IPOTBB%) -   \!
                        (TS.TOTALS(2,0,0) - IPOTBB%) / KIV2
             TVRTIV3% = (TS.TOTALS(3,0,0) - IPOTCC%) -   \!
                        (TS.TOTALS(3,0,0) - IPOTCC%) / KIV3 
             TVRTIV4% = (TS.TOTALS(4,0,0) - IPOTDD%) -   \!
                        (TS.TOTALS(4,0,0) - IPOTDD%) / KIV4  
             TVRTIVT% = TVRTIV1%+TVRTIV2%+TVRTIV3%+TVRTIV4%
             TVRTIMX% = TVRTIVT%                          !
             VRPLANA% = 0                                 !
             IF TVRTDUE% <> 0 THEN BEGIN                  !
                FRACRTIV = FLOAT(TVRTPAG%) / TVRTDUE%     !
                VRPLANA% = FRACRTIV * (TVRTIVT%-TIVADEC%) !
             ENDIF                                        !
             IF TVRTPAG% > TVRTDUE% THEN                 \! 
                           VRPLANA%=TVRTIVT%-TIVADEC%     !
             IF TVRTPAG% <= TVRTEXE% THEN BEGIN          \!
                NORETEN% = 0                              !
             ENDIF ELSE BEGIN                             !
                IF TVRTPAG% > TVRTDUE% THEN BEGIN         !
                   FRACRTIV = FLOAT(TVRTDUE%-TVRTEXE%) / \!
                              TVRTDUE%                    !
                ENDIF ELSE BEGIN                          !
                   FRACRTIV = FLOAT(TVRTPAG%-TVRTEXE%) / \!
                              TVRTDUE%                    !
                ENDIF                                     !
                NORETEN% = FRACRTIV * TVRTIVT%            !
                IF NORETEN% > TVRTIVT% THEN              \!
                   NORETEN%=TVRTIVT%                      !
             ENDIF                                        !
             TVRTIVP% = TVRTIVT% * KUE1% / 100            !
             IF KOPC% AND 04H THEN TVRTIVT% = VRPLANA%    !
             IF KOPC% AND 02H THEN TVRTIVT% = NORETEN%    !
             IF KOPC% AND 01H THEN TVRTIVT% = TVRTIVP%    !
             IF KOPC% AND 80H THEN CALL TIQUETE.IVA       !
!            TVRTIVT% = TVRTIVT% - TIVADEC%               ! Resta Iva/Ant Decl


!             LOCATE #30;1,1,OFF                           ! Notifica al Oper
!             WRITE FORM "C20 2PIC(######,###)";#30;      \!
!                   " PAGO RED:      IVA:",TVRTPAG%,TVRTIVT% 


!--- Se adiciona rutina para el calculo de la Base de impuesto
!--- requerido en el decreto 428 de 2004
!--- desarrollado por OVS. 10 Jun 2004

             Call CALCULO.428                             ! Calculo datos para decreto 428
             
!             WRITE FORM "C20 2PIC(######,###)";#30;      \! Muestra datos display operador
!             "      IVA:     BASE:",TVRTIVT%,UE.BASE.428% !

             WRITE FORM "C20 2PIC(######,###)";#30;      \! Muestra datos display operador
             "      IVA:     BASE:",UE.BASE.IVA%,UE.BASE.428% !

             TS.IO.MOTORKEY = 0                           ! Suprime proceso
          ENDIF                                           !
       ENDIF                                              !
    ENDIF                                                 !
END SUB                                                   !
!-----------------------------------------------------------------------------
!                Subrutina para Desempaquetar Datos    
!------------------------------------------------------------------------------
SUB GETUNPK PUBLIC                                     !
    K% = MATCH(":",B$,J%)                              ! Busca separador Campo
    A$ = UNPACK$(MID$(B$,J%,K%-J%))                    ! y lo desempaqueta
    J% = K% + 1                                        ! Marca inicio del 
END SUB                                                ! siguiente campo 
!------------------------------------------------------------------------------
!                  Subrutina para Obtener Flags 
!------------------------------------------------------------------------------
SUB GETFLAG PUBLIC                                     !
    FIS.FLAG = VAL(A$)                                 ! Conv. FIS.FLAG a INTEGER
                                                       ! FLAGS individuales
    IF (FIS.FLAG AND 00000001H) THEN                  \!
       A$ = "1" ELSE A$ = "0"                          !
    FOR I% = 1 TO 15                                   !
      FIS.FLAG = SHIFT(FIS.FLAG,1)                     ! Inic siguiente BIT
      IF (FIS.FLAG AND 00000001H) THEN                \!
         A$ = "1" + A$                                \!
      ELSE                                            \!
        A$ = "0" + A$                                  !
    NEXT I%                                            !
END SUB                                                ! 

!------------------------------------------------------------------------------
!               Restaura Variables en Recovery/Retreieve STR-1
!------------------------------------------------------------------------------

SUB FIS.STRING1 PUBLIC                                 !
   J% = 3                                              ! Inicio de Datos
   CALL GETUNPK                                        ! Codigo del Producto
   FIS.PLU$  = A$                                      !
   CALL GETUNPK                                        ! Precio de Venta
   RTI.VRU% = VAL(A$)                                  !
   CALL GETUNPK                                        ! Codigo del Dpto.
   FIS.DEPA$ = A$                                      !
   CALL GETUNPK                                        ! Cupones Fam. Corriente
   CALL GETUNPK                                        ! Indicativo 1
   CALL GETFLAG                                        ! Busca BITS en 1
   FIS.INDICAT1 = VAL(MID$(A$,15,1))                   ! Toma Cond Descto
   CALL GETUNPK                                        ! Indicativo 2
   FIS.VOREF% = VAL(A$)                                ! Indica Void/Refund
   CALL GETFLAG                                        ! Busca BITS
   CALL GETUNPK                                        ! Indicativo 3
   FIS.ITIPO = VAL(LEFT$(A$,1))                        ! Tipo de Articulo
   IF FIS.ITIPO < 6 THEN                              \! Si es item de venta 
         RTI.ITEM = -1 ELSE FIS.COUP = -1              !
END SUB                                                !
!------------------------------------------------------------------------------
!               Restaura Variables en Recovery/Retreieve STR-99
!------------------------------------------------------------------------------

SUB FIS.STRING99 PUBLIC                                !
   J% = 3                                              ! Inicio de Datos
   CALL GETUNPK                                        ! Tipo de string
   FIS.STYPE = VAL(A$)                                 !
   IF FIS.STYPE = 21 THEN BEGIN                        ! String Nro Transaccion
      FIS.TRTYPE$ = MID$(B$,J%,2)                      ! Datos de Transaccion 
      J% = J% + 2                                      !
      FIS.PRF$ = MID$(B$,J%,4)                         ! Prefijo
      J% = J% + 4                                      !
      FIS.NTRA = VAL(UNPACK$(MID$(B$,J%,8)))           ! Nro de Factura
   ENDIF                                               !
END SUB                                                !
!------------------------------------------------------------------------------
!        Subrutina para presentar la Fecha en Formato MMM/DD/AAAA
!------------------------------------------------------------------------------

SUB FIS.ANO2000 PUBLIC                                 !
                                                       !
    M$ = "EneFebMzoAbrMyoJnoJloAgoSepOctNovDic"        ! Tabla Meses
    RTI.DL$ = "20"                                     ! Siglo XXI
    IF LEFT$(DATE$,2) = "99" THEN RTI.DL$ = "19"       ! Siglo XX
    RTI.DL$ = RTI.DL$ + LEFT$(DATE$,2) + " "           ! Fecha para Store
    RTI.DL$ = MID$(M$,VAL(MID$(DATE$,3,2))*3-2,3) +   \! Data Line de
              "/" + STR$(VAL(RIGHT$(DATE$,2)))    +   \! DI and CR
              "/" + RTI.DL$                            ! 
    IF TS.LINETYPE = 6 THEN BEGIN                      ! Si Store Data/Trans
       FIS.SLA% = MATCH("/",TS.PRTBUF$,1)              ! Busca SLASH
       IF FIS.SLA% <> 0 THEN BEGIN                     ! Si lo encuentra
          FIS.SLA% = MATCH(" ",TS.PRTBUF$,FIS.SLA%)    ! ubica sig espacio
          IF FIS.SLA% < 10 THEN BEGIN                  ! Si hay espacio
             TS.PRTBUF$=RTI.DL$+MID$(TS.PRTBUF$, 10 , \! reformatea parte
                        LEN(TS.PRTBUF$)-FIS.SLA%-3)    ! izq de PRTBUF$
          ENDIF ELSE BEGIN                             ! y termina
             TS.PRTBUF$=LEFT$(TS.PRTBUF$,FIS.SLA%-3)+ \!
                        RTI.DL$ + RIGHT$(TS.PRTBUF$,  \! reformatea parte
                          LEN(TS.PRTBUF$)-FIS.SLA%-11) ! izq de PRTBUF$
          ENDIF                                        !
       ENDIF                                           !
       FIS.SLA% = MATCH("/",TS.SJDATA$,1)              ! Busca SLASH
       IF FIS.SLA% = 3 THEN BEGIN                      ! Si lo encuentra
          FIS.SLA% = MATCH(" ",TS.SJDATA$,FIS.SLA%)    ! ubica sig espacio
          IF FIS.SLA% < 10 THEN BEGIN                  ! Si hay espacio
             RTI.DL$ = "20"                            ! Siglo XXI
             IF LEFT$(DATE$,2)="99" THEN RTI.DL$="19"  ! Siglo XX
             RTI.DL$=RTI.DL$ + LEFT$(DATE$,2) + " "    ! Fecha para Store
             RTI.DL$=                                 \!
             MID$(M$,VAL(MID$(DATE$,3,2))*3-2,2)+     \! Data Line de
             "/"+RIGHT$(DATE$,2) + "/" + RTI.DL$       ! DI and SJ
             TS.SJDATA$=RTI.DL$+MID$(TS.SJDATA$,10,10) ! reformatea parte
          ENDIF                                        !
       ENDIF                                           !
    ENDIF                                              !
END SUB                                                !

!------------------------------------------------------------------------------
!          Funcion para Recuperar Codigo de Error Compatible BASIC
!--- ---------------------------------------------------------------------------
FUNCTION ERRNSTR$(ERRNUM)                              !
    INTEGER*1 I                                        ! Denfine Variables
    INTEGER*4 ERRNUM,WORK                              !
    STRING HEX$,ERRNSTR$,WORK$                         !
    HEX$="0123456789ABCDEF"                            ! 
    ERRNSTR$="":WORK$=""                               !
    FOR I=1 TO 8                                       ! Inicia Loop
        WORK=ERRNUM AND 0000000FH                      ! Todos OFF menos Ultimo
        WORK$=MID$(HEX$,WORK+1,1)+WORK$                ! Adic Vr Hex a String
        ERRNUM=SHIFT(ERRNUM,4)                         ! Asigna sig Ultimo
    NEXT I                                             ! Continua Loop
    ERRNSTR$=WORK$                                     ! Toma Cod Error Definitivo
FEND                                                   !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSUSU.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    User defined variables for printing IVA on Customer Receipt        **
!   ***************************************************************************
!
FUNCTION FORMAT.AMOUNT(AMT1) EXTERNAL                  !
  INTEGER*1 FORMAT.AMOUNT                              !
  INTEGER*4 AMT1                                       !
END FUNCTION                                           ! 
                                                       !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    Responsible:                                                       **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSUSU.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   ***************************************************************************

!------------------------------------------------------------------------------
!                   Subrutina para Intercalar I*4 en STRING  PUTN4
!------------------------------------------------------------------------------
!FUNCTION PUTN4(P1$,P2,P3) EXTERNAL                    !
!   STRING P1$                                         !
!   INTEGER*2 P2                                       !
!   INTEGER*4 P3                                       !
!END FUNCTION                                          !

!------------------------------------------------------------------------------
!                Subrutina para Armar String XLog & TLog
!------------------------------------------------------------------------------

SUB FIS.ARMA PUBLIC                                    ! Intercala una String
  CALL SUBSTR(SL.LOGDATA$,FIS.DIR% + FIS.D% -         \! en Otra String con
              LEN(FIS.X$),FIS.X$,0,LEN(FIS.X$))        ! long predefinida
  FIS.DIR% = FIS.DIR% + FIS.D%                         ! Asigna Sig posic de
END SUB                                                ! inicio de Intercalac
!----------------------------------------------------------------------------
!       Funci¢n para Desplegar Mensajes y hacer Entradas al LOG
!----------------------------------------------------------------------------
! FUNCTION ADXERROR(TERM,MSGGRP,MSGNUM,SEVERITY,EVENT,UNIQUE) EXTERNAL
!    INTEGER*1    TERM,   MSGNUM,  ADXERROR            !
!    INTEGER*2    EVENT,  MSGGRP,  SEVERITY            !
!    STRING       UNIQUE                               !
! END FUNCTION                                         !

SUB RTITSU02.011 PUBLIC                                !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS ELECTRONICOS                          **
!   **    User Exit  :  RTI0201.011                                          **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Indata Ltda                                          **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
    TIVADEC% = 0                                       !

END SUB                                                !

SUB RTITSU07.011 PUBLIC                                !
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    User Exit  :  RTI0701.TER                                          **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!
  FUNC = 4                                             ! Funcion Appl Serv
  CALL ADXSERVE(FIS.RET,FUNC,PARM1,PARM2$)             ! Invoca Datos Term
  FIS.AL$  = MID$(PARM2$,1,4)                          ! Toma No. Almacen
  FIS.TER$ = MID$(PARM2$,8,3)                          ! Toma No. Terminal
  FIS.OFF$ = MID$(PARM2$,11,1)                         ! Toma Status Online
  FIS.RET$ = MID$(PARM2$,12,1)                         ! Toma Status Tot.Ret
  FIS.MAT$ = MID$(PARM2$,52,1)                         ! Toma Tipo de Terminal 

  CALL DATOS.RETIVA                                    ! 
  CALL DECRETO428				       ! Decreto 428 2004
END SUB                                                !

SUB RTITSU08.011 PUBLIC                                !
!------------------------------------------------------------------------------
!         **  PROYECTO RETEIVA PAGOS EFT        **
!
!     User Exit          =  RTI081A.VRF
!     Punto de Entrada   =  EAMTSU08.J86
!     Fecha de Revisi¢n  =  Mar. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!------------------------------------------------------------------------------
!            ***    Descripci¢n del Fuente      ***
!
!     Estas lineas se insertan en la USER EXIT 08  para detectar los
!     PLU's que son Items de Vta Normal ¢ los que son Cupones
!------------------------------------------------------------------------------
   RTI.FIV% = IR.INDICAT1                              ! Asigna Indicats IVA
   FIS.ITIPO=VAL(LEFT$(UNPACK$(IR.INDICAT2$),1))       ! Toma Tipo Item
   IF IR.INDICAT1A AND 02H THEN BEGIN                  ! Flag de User 3
      RTI.ITEM = 0                                     ! Reset de Item
      FIS.COUP = 0                                     ! Reset Coupon
   ENDIF                                               !
   IF FIS.ITIPO < 6 THEN                              \! Si es item de venta 
         RTI.ITEM = -1 ELSE FIS.COUP = -1              !
!------------------------------------------------------------------------------
END SUB                                                !

SUB RTITSU14.011 PUBLIC                                !
!------------------------------------------------------------------------------
!         **  PROYECTO RETEIVA PAGOS EFT        **
!
!     User Exit          =  RTITSU14.011
!     Punto de Entrada   =  EAMTSU14.J86
!     Fecha de Revisi¢n  =  Mar. de 1999
!     Unidad Responsable =  T‚cnicos de Sistemas POS
!-----------------------------------------------------------------------------
!                    ***    Descripci¢n del Fuente      ***       
!
!     El siguiente Control permite el manejo de las Siguientes Operaciones
! 
!         1 - Consulta del Monto Total del IVA hasta este momento
!             aplicando los criterios definidos para la Declaracion 
!             del IVA a las entidades Financieras calificadas como
!             Retenedoras de IVA.
!
!----------------------------------------------------------------------------
CALL CONSULTA.RETIVA                                   !
!    write form "c38";#34;ktivar$+" Dev:"+str$(ts.io.device)+" Mot"+str$(ts.io.motorkey)+ \
!               " Dat:"+ts.io.data$(09)+" St:"+str$(ts.io.state)

!------------------------------------------------------------------------------
END SUB                                                !

SUB RTITSU20.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    User Exit  :  RTITSU20.011                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Imprime datos adicionales con algunos mensajes exigidos por        **
!   **    la Ley, como Nit, No. de Factura y Fecha de Expedicion.            **
!   **                                                                       **
!   ***************************************************************************

   IF TS.LINETYPE = 1 OR                              \! Si es Vta Item
      TS.LINETYPE = 5 THEN BEGIN                       ! 
         FIS.ITIPO=VAL(LEFT$(UNPACK$(IR.INDICAT2$),1)) ! Toma Tipo Item
         FIS.VOREF% = SL.IT.INDICAT2                   ! Chequea Si Cancelac
   ENDIF                                               !
   CALL FIS.ANO2000                                    !
   IF NOT TS.TRAINING THEN BEGIN                       ! Si no es Ses. Entrenam
      IF TS.LINETYPE = 2 THEN BEGIN                    ! Si linea Pago
         CALL RTI.GRABA                                ! Rutina Verif Pag EFT
      ENDIF                                            ! End if taxes > 0 
   ENDIF                                               !
!------------------------------------------------------------------------------
END SUB                                                !

SUB RTITSU21.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Nov. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    User Exit  :  RTITSU21.011                                         **
!   **    Inclu¡r en :  EAMTSU21.J86                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Imprime fecha con ajuste Siglo XXI                                 **
!   **                                                                       **
!   ***************************************************************************

!  CALL FIS.ANO2000                                     ! Fecha Siglo XXI
                                                       !
END SUB                                                !


SUB RTITSU53.011 PUBLIC                                !
!   ***************************************************************************
!   **    Date       :  Mar. de 1999                                         **
!   **    Project    :  RETEIVA PAGOS EFT                                    **
!   **    User Exit  :                                                       **
!   **    Inclu¡r en :  EAMTSU53.J86                                         **
!   **    Program    :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **        Ajuste :  Global DataTel                                       **
!   **                                                                       **
!   **    Se agregan las  siguientes instrucciones para recuperar los        **
!   **    posibles Datos de Usuario, cuando el sistema efect£a               **
!   **    Transaction Recovery                                               **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

B$ = SL.STR.ENTRY$ + ":"                               ! Agrega Separador STR
IF UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "01" THEN BEGIN   ! Si DATA/ENTRY
   CALL FIS.STRING1                                    ! Extraer Datos Str 1
ENDIF                                                  !
IF UNPACK$(LEFT$(SL.STR.ENTRY$,1)) = "99" THEN BEGIN   ! Si String Datos 99
   CALL FIS.STRING99                                   ! Extraer Datos Str 99
ENDIF                                                  !
END SUB                                                !

!------------------------------------------------------------------------------
!                    Fin de las User Exits de Usuario
!------------------------------------------------------------------------------

