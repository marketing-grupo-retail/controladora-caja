!************************************************** 
!Empresa       : Comfamiliar ANDI                 *
!Programa      : CFMEDRUT.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Rutinas Planilla Fiscal          *
!**************************************************
! Mod 26Mzo2009
! Se adiciona la forma de pago 46 al reporte concepto B032
! Familias en Accion, desarrollado por Grupo Retail - OVS
!---------------------------------------------------

%Environ C

%INCLUDE CFMEDPAG.VAR   ! Definicion de Variables

Sub STRING.FISCAL Public                               !
  PRE99$ = "TR"                                        !
  STR99% = MATCH(PRE99$,INAREA$,1)                     !
  IF STR99% > 0 THEN BEGIN                             ! Ident TransVta
     VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0        !
              IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0        !
     PRETR$ = MID$(INAREA$,STR99%+3,4)                 !
     NUFIS$ = UNPACK$(MID$(INAREA$,STR99%+7,8))        !
     NUFIS$ = RIGHT$(NUFIS$,11)                        !
  ENDIF                                                !
  EXIT SUB
        J = 12                                         !
        ESFIS% = 1                                     !
        GOSUB GETUNPKA                                 ! Desempaq sig UPD
        TRAF$ = RIGHT$(STRING$(15,"0")+A$,15)          !
        LOCATE 20,55                                   !
        PRETRAF$ = MID$(B$,5,7)+" "+RIGHT$(TRAF$,7)    !
        !PRINT PRETRAF$                                 !
        SIGN$ = MID$(B$,7,1)                           !
        PREF$ = MID$(B$,8,4)                           !
        ALTE$ = PACK$(RIGHT$("0"+LEFT$(TRAF$,7),8))    !
        TRAF% = VAL(RIGHT$(TRAF$,8))                   !
        GOSUB GETUNPKA                                 ! Vr Neto Vta
        VTA% = VAL(A$)                                 !
        IF SIGN$ = "-" THEN VTA% = -1 * VTA%           !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 1
        IV1% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 2
        IV2% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 3
        IV3% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 4
        IV4% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 5
        IV5% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Iva Tarifa 6
        IV6% = VAL(A$)                                 !
        GOSUB GETUNPKA                                 ! Vr Exento
        IV7% = VAL(A$)                                 !
        IV7% = VTA% - VRVTA%                           ! Resta Aj/Cmb Dsc
        GOSUB GETUNPKA                                 ! Impo Consumo
        IV8% = VAL(A$)                                 !
        IF OPC$ = "2" THEN BEGIN                       !
           FORTR$="C15 PIC(########) 8PIC(######) C2"  !
           WRITE FORM FORTR$;#3;                      \!
              PRETRAF$,VRVTA%,IV1%,IV2%,IV3%,IV4%,      \!
                       IV5%,IV6%,IV7%,IV8%,FR$         !
        ENDIF                                          !
        IF IV1% > VTA% THEN IV1% = 0                   !
        IF IV2% > VTA% THEN IV2% = 0                   !
        IF IV3% > VTA% THEN IV3% = 0                   !
        IF IV4% > VTA% THEN IV4% = 0                   !
        IF IV5% > VTA% THEN IV5% = 0                   !
        IF IV6% > VTA% THEN IV6% = 0                   !
        IF IV7% > VTA% THEN IV7% = 0                   !
        IF IV8% > VTA% THEN IV8% = 0                   !
GETUNPKA:                                              !
  K  = MATCH(":",B$,J)                                 ! Busca sep de Campo
  A$ = UNPACK$(MID$(B$,J,K-J))                         ! y lo desempaqueta
  A$ = TRANSLATE$(A$,"=","-")                          ! Signo Negativo
  H$ = MID$(B$,J,K-J)                                  !
  J  = K + 1                                           ! Marca el inicio de sigui-
RETURN                                                 ! ente campo
END SUB

!------------------------------------------------------------------------------
!             Subrutina para la Impresion de Comprobante Contable
!------------------------------------------------------------------------------
SUB IMPRI.COMPROBANTE Public                         !
  WRITE FORM "2C2";#FIL5;CHR$(12),FR$                !
!       WRITE FORM "C31 C2";#FIL5;S8CND$,FR$         ! Salto;1/8's;Conds
  WRITE FORM "C17 C63 C2";#FIL5;" ",                \!
        "* CAJA COMPENSACION FAMILIAR"   +          \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL5;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL5;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  ORIG$ = "Alm:" + ALM$+NOAL$+"          "        + \!
          "Elaborado: "+ TM$(VAL(MID$(DATE$,3,2)))+ \!
          " "+STR$(VAL(RIGHT$(DATE$,2))) + "/20"  + \!
          LEFT$(DATE$,2)+"   "+LEFT$(TIME$,2)     + \!
          ":"+MID$(TIME$,3,2)                        !
  WRITE FORM "C80 C2";#FIL5;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "DEP  DESCRIPCION       %IVA       " + \!
             "VR/BASE      VR/IVA    VR/IMPOC   " + \!
             "TOT/VENTA",FR$                         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOR II% = 1 TO 998                                 !
      MACH$ = "DP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,DEPA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+"  DEPTO SIN DEFINIR"!
      IF I%<>0 THEN DEPI$=MID$(DEPA$,I%+2,27)        !
      IF IVAD%(II%,1) <> 0 THEN BEGIN                !
         IF NOSU% <> 1 THEN BEGIN                     
         IVAD%(II%,4)=IVAD%(II%,1)+IVAD%(II%,2)+    \!
                      IVAD%(II%,3)                   !
         ENDIF                                          
         IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)          !
         IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)          !
         IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)          !
         IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)          !
         IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)          !
         WRITE FORM "C29 4PIC(########,###) C2";    \!
               #FIL5;DEPI$,IVAD%(II%,1),            \!
               IVAD%(II%,2),IVAD%(II%,3),           \!
               IVAD%(II%,4),FR$                      !
      ENDIF                                          !
  NEXT II%                                           !

  Write Form "C29 PIC(########,###) C2"; #FIL5;     \! Add OVS 12 Ene 2006
        "VENTA DOMICILIOS",Asc.Vta.Domic%,FR$        !

  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL5;     \!
        "    ** TOTAL --->",IVAD%(0,1),             \!
        IVAD%(0,2),IVAD%(0,3),IVAD%(0,4),FR$         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL5;                        \!
        "                     " +                   \!
        "**  T O T A L   T R I B U T A D O  **",FR$  !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "                %IVA             " +  \!
             " VR/BASE      VR/IVA    VR/IMPOC " +  \!
             "  TOT/VENTA",FR$                       !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOR I% = 1 TO 5                                    !
      IV = VAL(MID$(TARIF$,I%*3-2,2))                !
      IVAD%(0,0) = IV                                !
      IVAD%(0,1) = 0                                 !
      IVAD%(0,2) = 0                                 !
      IVAD%(0,3) = 0                                 !
      IVAD%(0,4) = 0                                 !
      IVAD%(0,5) = 0                                 !
      FOR II% = 1 TO 998                             !
          IF IVAD%(II%,0)=IV THEN BEGIN              !
             IF NOSU% <> 1 THEN BEGIN                !
                IVAD%(999,1)=IVAD%(999,1)+IVAD%(II%,1)
                IVAD%(999,2)=IVAD%(999,2)+IVAD%(II%,2)
                IVAD%(999,3)=IVAD%(999,3)+IVAD%(II%,3)
                IVAD%(999,4)=IVAD%(999,4)+IVAD%(II%,4)
                IVAD%(999,5)=IVAD%(999,5)+IVAD%(II%,5)
             ENDIF                                   !
             IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)      !
             IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)      !
             IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)      !
             IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)      !
             IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)      !
          ENDIF                                      !
      NEXT II%                                       !
      WRITE FORM "C29 4PIC(########,###) C2";#FIL5; \!
            "                 "+STR$(IVAD%(0,0))+   \!
            "%",IVAD%(0,1),IVAD%(0,2),IVAD%(0,3),   \!
            IVAD%(0,4),FR$                           !
  NEXT I%                                            !      
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL5;     \!
        "    ** TOTAL --->",IVAD%(999,1),           \!
        IVAD%(999,2),IVAD%(999,3),IVAD%(999,4),FR$   !
  IVAD%(0,1) = 0                                     !
  IVAD%(0,2) = 0                                     !
  IVAD%(0,3) = 0                                     !
  IVAD%(0,4) = 0                                     !
  IVAD%(0,5) = 0                                     !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "2C2";#FIL5;CHR$(12),FR$                !
  WRITE FORM "C13 C67 C2";#FIL5;" ",                \!
        "* CAJA DE COMPENSACION FAMILIAR DEL"   +   \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL5;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL5;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  WRITE FORM "C80 C2";#FIL5;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "TPV  N/SERIE   #INI   #FIN #CLI    "+ \!
             "SN/IVA     VR/BASE   VR/IVA "       + \!
             "VR/IMPOC  DSC/CUP",FR$                 !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  IF HAY99% = 1 THEN BEGIN                           !
     FOR II% = 1 TO 100                              !
         IVAT%(II%,0) = TRF%(II%,0)                  !
         IVAT%(II%,1) = TRF%(II%,1)                  !
         IVAT%(II%,2) = TRF%(II%,2)                  !
     NEXT II%                                        !
  ENDIF                                              !
  FOR II% = 1 TO 100                                 !
      MACH$ = "TP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,TPVA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+" SIN DEFINIR"       !
      IF I%<>0 THEN DEPI$=MID$(TPVA$,I%+2,23)        !
      LOCATE 1 , 1                                   !
      IF IVAT%(II%,2) <> 0 OR                       \!
         IVAT%(II%,3) <> 0 THEN BEGIN                !
            IF NOSU% <> 1 THEN BEGIN                 !
               IVAT%(0,2)=IVAT%(0,2)+IVAT%(II%,2)    !
               IVAT%(0,3)=IVAT%(0,3)+IVAT%(II%,3)    !
               IVAT%(0,4)=IVAT%(0,4)+IVAT%(II%,4)    !
               IVAT%(0,5)=IVAT%(0,5)+IVAT%(II%,5)    !
               IVAT%(0,6)=IVAT%(0,6)+IVAT%(II%,6)    !
               IVAT%(0,7)=IVAT%(0,7)+IVAT%(II%,7)    !
            ENDIF                                    !
         FOIM$="C12 2PIC(#######) PIC(#####)"   +   \!
               " 2PIC(###########) 3PIC(#########) C2"
         WRITE FORM FOIM$;                          \!
               #FIL5;LEFT$(DEPI$,3)+MID$(DEPI$,5,9),\!
               IVAT%(II%,0),IVAT%(II%,1),           \!
               IVAT%(II%,2),IVAT%(II%,5),           \!
               IVAT%(II%,3),IVAT%(II%,4),           \!
               IVAT%(II%,6),IVAT%(II%,7),FR$         !
      ENDIF                                          !
  NEXT II%                                           !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM FOIM$;                                 \!
        #FIL5;"** TOTAL --->",0,0,IVAT%(0,2),       \!
        IVAT%(0,5),IVAT%(0,3),IVAT%(0,4),           \!
        IVAT%(0,6),IVAT%(0,7),FR$                    !
  WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL5;                        \!
        "              " + "**  T O T A L   "    +  \!
        "M E D I O S    D E    P A G O  **",FR$      !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL5;                        \!
             "     "                             +  \!
             "       TIPO            VARIEDAD   " + \!
             "        Cant   V A L O R ",FR$         !
  WRITE FORM "C80 C2";#FIL5;STRING$(78,"-"),FR$      !
  FOIM$ = "C10 2C18 PIC(####) PIC(#########,###) C2" !
  FOR I% = 1 TO 6                                    !
      IF TPG%(I%,0,1) <> 0 THEN BEGIN                !
         FOR II% = 1 TO 6                            !
             IF TPG%(I%,II%,1) <> 0 THEN BEGIN       !
                WRITE FORM FOIM$;#FIL5;" "," ",     \!
                TI$(I%,II%),TPG%(I%,II%,0),         \!
                            TPG%(I%,II%,1),FR$       !
             ENDIF                                   !
             IF II% = 6 THEN BEGIN                   !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"-"),FR$        !
                WRITE FORM FOIM$;#FIL5;" ",         \!
                TI$(I%,0)," ",TPG%(I%,0,0),         \!
                TPG%(I%,0,1),FR$                     !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"-"),FR$        !
             ENDIF                                   !
         NEXT II%                                    !
      ENDIF                                          !
  NEXT I%                                            !
  WRITE FORM FOIM$;#FIL5;" ",                       \!
        " ","** TOTAL PAGOS **",TPG%(0,0,0),        \!
                TPG%(0,0,1),FR$                      !
                WRITE FORM "C10 C54 C2";#FIL5;      \!
                      " ",STRING$(54,"="),FR$        !
End Sub                                              !

