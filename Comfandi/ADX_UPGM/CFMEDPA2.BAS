!------------------------------------------------------------------------------
!    PROYECTO              : COMFAMILIAR ANDI 
!    Nombre del Fuente     : CFMEDPAG.BAS
!    Fecha de Modificaci¢n : MARZO DE 2007       
!    Responsables          : Tec Sistemas POS   
!    Archivos que utiliza  : C:\ADX_IDT4\EAMTRANx.DAT
!                                   ( x = A, B, ¢ C )
!------------------------------------------------------------------------------
!    Genera archivos de medios de pago, pagos y compras
!    para Cr‚dito y Cobranzas. Prepara movimiento  para
!    el Sistema Central de Inventarios. "Z para reportes diarios "
!------------------------------------------------------------------------------
!
!    Las Strings '99' del LOG de Transacciones con tipo de Registro=21
!    contienen los siguientes datos que van a conformar el Archivo ANUAL   
!    de Uso de Numeracion Fiscal ADX_UDT1:NFFHISyy.DAT:
! 
!      *   Tipo de String            '99'
!      *   Tipo de Registro          '21'        (String Cierre Transaccion)
!      A   Identificador Transac     'TR'
!      A   Signo de los Valores      '+'  ¢  '-' (Positivo/Negativo)
!      A   Prefijo autorizado DIAN   '0000'      (Default)
!      *   Num-Almacen/Num-Terminal  aaaaattt
!      *   Num consecutivo Fiscal    nnnnnnnn    (1-99999999)
!      *   Valor Neto Transac        v8
!      *   Valor Iva Tarifa 1        v8
!      *   Valor Iva Tarifa 2        v8
!      *   Valor Iva Tarifa 3        v8
!      *   Valor Iva Tarifa 4        v8
!      *   Valor Iva Tarifa 5        v8
!      *   Valor Iva Tarifa 6        v8
!      *   Valor Iva Tarifa 7        v8
!      *   Valor Iva Tarifa 8        v8
!
!      Equivalencias:   * = UPD Field  ;  A = ASCII Field
!-----------------------------------------------------------------------------
! Se adiciona al reporte el informe de movimientos por 
! venta a Domicilios, desarrollado por Asic Consulting
! OVS 12 Ene 2006 
!-----------------------------------------------------------------------------
! Se adiciona al reporte el informe de movimientos por 
! venta a FONEDE    , desarrollado por Asic Consulting
! OVS 11 Jul 2006 
! Nota: En la grabacion de los conceptos BXXX se hace informando
!       la linea donde debe grabar, tener esto en cuenta para 
!       futuras adiciones.
!-----------------------------------------------------------------------------
! Se adiciona en el reporte fiscal la forma de pago 6,6 para el
! manejo de FinAmerica. desarrollado por Grupo Retail 
! OVS 5 Oct. 2007
!------------------------------------------------------------------------
! Mod 25Ago2011
! Se retiran los ceros a la izquierda en la numeracin fiscal para todas las interfaces
! de la aplicación. desarrollado Grupo Retail - OVS
!------------------------------------------------------------------------
! Mod 22Nov2011
! Se adiciona detalle de forma de pago 66 para manejo de anticipo subdisio
! se reporta al concepto B033
! Desarrollado por Grupo Retail Ltda - OVS
!---------------------------------------------------------------------------
! Mod 17 Jul 2012
! Se modifica forma de pago tarjeta comfandi x Credito Credi Uno 
! en el tipo 5 variedad 3, ademas se retira de plantilla la forma
! de pago 26 anteriormente definida para crediuno
! Desarrollado por Grupo Retail - OVS 
!----------------------------------------------------------------------------
! Mod 08 Ene 2013
! Se modifican las tarifas del IVA para manejo de nueva reforma tributaria
! 2013, reportandolas asi 16,10,05,08 la cuarta tarifa corresponde al impto
! nacional del consumo INC
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 16 Dic 2013
! Se adiciona el concepto C040, descuentos otorgados por el motor de 
! promociones.
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 05 abr 2016
! Se modifica plantilla financiera con la nueva definición y reorganizacion
! de las formas de pago.
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 18 Abr 2016
! Se retira de plantilla financiera el concepto 070 Descuento Cotraim
! solicitado por Comfandi 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 05 Ene 2017
! Se modifican las tarifas del IVA para manejo de nueva reforma tributaria
! 2017, reportandolas asi 16,19,05,08 la cuarta tarifa corresponde al impto
! nacional del consumo INC
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 26 Jun 2018
! Se adiciona concepto B047 BONO KIT ESCOLAR ligado al tipo y variedad 22
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 15 May 2020
! Se adiciona concepto B049 UNIDOS POR EL VALLE ligado al tipo y variedad 14
! Se modifica concepto B048 LIFEMILES pasa a la forma de pago 54
! Solicitado por Freddy Guerrero - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 29/Jun/2020
! Se define la forma de pago 55 como LifeMiles manual acumulando al concepto
! B048
! Solicitado por Freddy Guerrero - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 11/aGO/2020
! Se adiciona concepto B050 tienda virtual
! Solicitado por Freddy Guerrero - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 27/May/2021
! Se adiciona el concepto B051 Bonos Emergencia 
! Solicitado por Freddy Guerrero - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 27/Sep/2021
! Se adiciona modifica el concepto B051 Bonos Emergencia a
! B051 BONOS KUPI, Solicitado por Freddy Guerrero - Andrea Guzman 
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 29Sep2022
! Se ajusta impresion de plantilla financiera archivo Z separandolo en 2 
! archivos Z y N
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 25Oct2022
! Se modifica el concepto de bonos kupi, reemplazando por el concepto 987
! según requerimiento de Comfandi:
! B051 BONOS KUPI
! C987 VENTA BONOS VIRTUAL
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 15Nov2022
! Se adiciona concepto credito brilla
! B052 CREDITO BRILLA
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 15/Dic/2021
! Se reversa cambio concepto bonos Kupi pasando de nuevo a B051
! B051 BONOS KUPI
! C987 VENTA BONOS VIRTUAL
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 11/May/2023
! Se define concepto B056 - ALKOSTO reportando los pagos registrados
! a la forma de pago 16
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 14/Jul/2023
! Ajuste para reporte impuesto a la bolsa
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------
! Mod 20/Oct/2023
! Reporte impuesto ICUI (impuesto consumo ultraprocesados industriales)
! Desarrollado por Grupo Retail - OVS 
!-----------------------------------------------------------------------------

%Environ C

%INCLUDE CFMEDPAG.VAR
Integer*4 Global Pagos(2), Grind2%, Rtap%, Gtdcto%
String    Global Rutina$, Ssgn$
String    Global Bines(1), Gpago$, Gversion$, Gdpto$
Integer*2 Global Qty.Bines%
Integer*1 Global Terror%, DctMotor%
String    Global Gsobrante$, Gfaltante$

!-- Variables para manejo impuesto ICUI
Integer*1 Global Gr.Iva.IndIcui%	 		  		! Activación proyecto ICUI
integer*4 Global Gr.Iva.TaxIcui%,          \! Valor impuesto Icui en trx
                 Gr.Iva.PtgIcui%            ! Porcentaje impuesto ICUI
String    Global Gr.Iva.flgIcui$,          \! Flag ICUI
                 Gr.Iva.DscIcui$,          \! Descriptor ICUI
                 Gr.Iva.DptoIcui$           ! Departamento reporte recaudo icui

Integer*4 Global UE.TABLAIVA(1)

 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function

Sub Parametros.Bines External
End Sub

Sub Carga.Iva.Planilla External
End Sub 

Function Tarifa.Iva.Item(Xind1%) External
Integer*4 Xind1%
Integer*1 Tarifa.Iva.Item
End Function 

Function Valida.String.Tef ( TRX$ ) External
String Valida.String.Tef, TRX$
End Function

!------------------------------------------------------------------------------
!                Busca campos de String Fiscal   Oct. 21 de 2002 JRS.
!------------------------------------------------------------------------------
Sub String.FISCAL																			 !
Integer*4 Xpos%                                        !
  PRE99$ = "TR+"                                        !
  Xpos% = MATCH(PRE99$,INAREA$,1)                      !
  If Xpos% > 0 Then BEGIN                              ! Ident TransVta
     VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0        !
              IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0        !
     PRETR$ = Mid$(INAREA$,Xpos%+3,4)                  !
     NUFIS$ = Str$(Val(UNPACK$(Mid$(INAREA$,Xpos%+7,8)))) ! Retira ceros de la izquierda
     NUFIS$ = RIGHT$("           "+NUFIS$,11)          !
  EndIf                                                !

End Sub
! ---------------------------------------------------------------------
!------------------------------------------------------------------------------
!             Subrutina para la Impresion de Comprobante Contable
!------------------------------------------------------------------------------
Sub IMPRI.COMPROBANTE                                !
  WRITE FORM "2C2";#FIL13;CHR$(12),FR$                !
!       WRITE FORM "C31 C2";#FIL5;S8CND$,FR$         ! Salto;1/8's;Conds
  WRITE FORM "C17 C63 C2";#FIL13;" ",                \!
        "* CAJA COMPENSACION FAMILIAR"   +          \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL13;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL13;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL5;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  ORIG$ = "Alm:" + ALM$+NOAL$+"          "        + \!
          "Elaborado: "+ TM$(VAL(Mid$(DATE$,3,2)))+ \!
          " "+STR$(VAL(RIGHT$(DATE$,2))) + "/20"  + \!
          LEFT$(DATE$,2)+"   "+LEFT$(TIME$,2)     + \!
          ":"+Mid$(TIME$,3,2)                        !
  WRITE FORM "C80 C2";#FIL13;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;                        \!
             "DEP  DESCRIPCION       %IVA       " + \!
             "VR/BASE      VR/IVA    VR/IMPOC   " + \!
             "TOT/VENTA",FR$                         !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  FOR II% = 1 TO 998                                 !
      MACH$ = "DP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,DEPA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+"  DEPTO SIN DEFINIR"!
      IF I%<>0 THEN DEPI$=Mid$(DEPA$,I%+2,27)        !
      IF IVAD%(II%,1) <> 0 THEN BEGIN                !
         IF NOSU% <> 1 THEN BEGIN                     
         IVAD%(II%,4)=IVAD%(II%,1)+IVAD%(II%,2)+    \!
                      IVAD%(II%,3)                   !
         EndIf                                          
         IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)          !
         IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)          !
         IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)          !
         IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)          !
         IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)          !
         WRITE FORM "C29 4PIC(########,###) C2";    \!
               #FIL13;DEPI$,IVAD%(II%,1),            \!
               IVAD%(II%,2),IVAD%(II%,3),           \!
               IVAD%(II%,4),FR$                      !
      EndIf                                          !
  NEXT II%                                           !

  Write Form "C29 PIC(########,###) C2"; #FIL13;     \! Add OVS 12 Ene 2006
        "VENTA DOMICILIOS",Asc.Vta.Domic%,FR$        !

  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL13;     \!
        "    ** TOTAL --->",IVAD%(0,1),             \!
        IVAD%(0,2),IVAD%(0,3),IVAD%(0,4),FR$         !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL13;                        \!
        "                     " +                   \!
        "**  T O T A L   T R I B U T A D O  **",FR$  !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;                        \!
             "                %IVA             " +  \!
             " VR/BASE      VR/IVA    VR/IMPOC " +  \!
             "  TOT/VENTA",FR$                       !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  FOR I% = 1 TO 5                                    !
      IV = VAL(Mid$(TARIF$,I%*3-2,2))                !
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
             EndIf                                   !
             IVAD%(0,1)=IVAD%(0,1)+IVAD%(II%,1)      !
             IVAD%(0,2)=IVAD%(0,2)+IVAD%(II%,2)      !
             IVAD%(0,3)=IVAD%(0,3)+IVAD%(II%,3)      !
             IVAD%(0,4)=IVAD%(0,4)+IVAD%(II%,4)      !
             IVAD%(0,5)=IVAD%(0,5)+IVAD%(II%,5)      !
          EndIf                                      !
      NEXT II%                                       !
      WRITE FORM "C29 4PIC(########,###) C2";#FIL13; \!
            "                 "+STR$(IVAD%(0,0))+   \!
            "%",IVAD%(0,1),IVAD%(0,2),IVAD%(0,3),   \!
            IVAD%(0,4),FR$                           !
  NEXT I%                                            !      
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C29 4PIC(########,###) C2";#FIL13;     \!
        "    ** TOTAL --->",IVAD%(999,1),           \!
        IVAD%(999,2),IVAD%(999,3),IVAD%(999,4),FR$   !
  IVAD%(0,1) = 0                                     !
  IVAD%(0,2) = 0                                     !
  IVAD%(0,3) = 0                                     !
  IVAD%(0,4) = 0                                     !
  IVAD%(0,5) = 0                                     !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "2C2";#FIL13;CHR$(12),FR$                !
  WRITE FORM "C13 C67 C2";#FIL13;" ",                \!
        "* CAJA DE COMPENSACION FAMILIAR DEL"   +   \!
        " VALLE DEL CAUCA *",FR$                     !
  WRITE FORM "C22 C58 C2";#FIL13;" ",                \!
        "*COMFAMILIAR ANDI-NIT 890.303.208-5*",FR$   !
  WRITE FORM "C80 C2";#FIL13;"  ",FR$                 !
  WRITE FORM "C16 C29 C30 C2";#FIL13;"  ",           \!
             "COMPROBANTE INFORME DIARIO -",        \!
             TM$(MEST%) + " " + STR$(DIAT%) + "/" + \!
             STR$(ANOT%),FR$                         !
  WRITE FORM "C80 C2";#FIL13;ORIG$,FR$                !
  WRITE FORM "C80 C2";#FIL13;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;                        \!
             "TPV  N/SERIE   #INI   #FIN #CLI    "+ \!
             "SN/IVA     VR/BASE   VR/IVA "       + \!
             "VR/IMPOC  DSC/CUP",FR$                 !
  WRITE FORM "C80 C2";#FIL13;STRING$(80,"-"),FR$      !
  IF HAY99% = 1 THEN BEGIN                           !
     FOR II% = 1 TO 100                              !
         IVAT%(II%,0) = TRF%(II%,0)                  !
         IVAT%(II%,1) = TRF%(II%,1)                  !
         IVAT%(II%,2) = TRF%(II%,2)                  !
     NEXT II%                                        !
  EndIf                                              !
  FOR II% = 1 TO 100                                 !
      MACH$ = "TP"+RIGHT$("000"+STR$(II%),3)         !
      I% = MATCH(MACH$,TPVA$,1)                      !
      IF I%= 0 THEN DEPI$=MACH$+" SIN DEFINIR"       !
      IF I%<>0 THEN DEPI$=Mid$(TPVA$,I%+2,23)        !
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
            EndIf                                    !
         FOIM$="C12 2PIC(#######) PIC(#####)"   +   \!
               " 2PIC(###########) 3PIC(#########) C2"
         WRITE FORM FOIM$;                          \!
               #FIL13;LEFT$(DEPI$,3)+Mid$(DEPI$,5,9),\!
               IVAT%(II%,0),IVAT%(II%,1),           \!
               IVAT%(II%,2),IVAT%(II%,5),           \!
               IVAT%(II%,3),IVAT%(II%,4),           \!
               IVAT%(II%,6),IVAT%(II%,7),FR$         !
      EndIf                                          !
  NEXT II%                                           !
  WRITE FORM "C80 C2";#FIL13;STRING$(80,"-"),FR$      !
  FOIM$="C12 2PIC(#######) PIC(#####)"   +   \!
        " 2PIC(###########) 3PIC(#########) C2"
  WRITE FORM FOIM$;                                 \!
        #FIL13;"** TOTAL --->",0,0,IVAT%(0,2),       \!
        IVAT%(0,5),IVAT%(0,3),IVAT%(0,4),           \!
        IVAT%(0,6),IVAT%(0,7),FR$                    !
  WRITE FORM "C80 C2";#FIL13;STRING$(80,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;"  ",FR$                 !
  WRITE FORM "C80 C2";#FIL13;                        \!
        "              " + "**  T O T A L   "    +  \!
        "M E D I O S    D E    P A G O  **",FR$      !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  WRITE FORM "C80 C2";#FIL13;                        \!
             "     "                             +  \!
             "       TIPO            VARIEDAD   " + \!
             "        Cant   V A L O R ",FR$         !
  WRITE FORM "C80 C2";#FIL13;STRING$(78,"-"),FR$      !
  FOIM$ = "C10 2C18 PIC(####) PIC(#########,###) C2" !
  FOR I% = 1 TO 6                                    !
      IF TPG%(I%,0,1) <> 0 THEN BEGIN                !
         FOR II% = 1 TO 7                            !
             IF TPG%(I%,II%,1) <> 0 THEN BEGIN       !
                WRITE FORM FOIM$;#FIL13;" "," ",     \!
                TI$(I%,II%),TPG%(I%,II%,0),         \!
                            TPG%(I%,II%,1),FR$       !
             EndIf                                   !
             IF II% = 7 THEN BEGIN                   !
                WRITE FORM "C10 C54 C2";#FIL13;      \!
                      " ",STRING$(54,"-"),FR$        !
                WRITE FORM FOIM$;#FIL13;" ",         \!
                TI$(I%,0)," ",TPG%(I%,0,0),         \!
                TPG%(I%,0,1),FR$                     !
                WRITE FORM "C10 C54 C2";#FIL13;      \!
                      " ",STRING$(54,"-"),FR$        !
             EndIf                                   !
         NEXT II%                                    !
      EndIf                                          !
  NEXT I%                                            !
  WRITE FORM FOIM$;#FIL13;" ",                       \!
        " ","** TOTAL PAGOS **",TPG%(0,0,0),        \!
                TPG%(0,0,1),FR$                      !
                WRITE FORM "C10 C54 C2";#FIL13;      \!
                      " ",STRING$(54,"="),FR$        !
End Sub                                              !

!------------------------------------------------------------------------------
!      Rutina para Acumular Totales de Control Medios de Pago 
!------------------------------------------------------------------------------

Function Graba.Planilla(Xpos%,F1$, F2$, F3$, I1%, I2%, I3%, I4%, F4$)
String F1$, F2$, F3$, F4$
Integer*4 I1%, I2%, I3%, I4%, Xpos%
  Write Form "C12 C4 C20 4PIC(-999999999) C2";#FIL10,        \!
             Xpos%;F1$, F2$, F3$, I1%, I2%, I3%, I4%, F4$
End Function 

SUB ACUDIA                                                    !
    NOSU% = 0                                                 ! 
    CALL IMPRI.COMPROBANTE                                    !
    NOSU% = 1                                                 ! 
    
    !CALL IMPRI.COMPROBANTE                                    !
    
    TT.SOB = 0   : TT.FAL = 0                                 !
    OPEN "EAMACCTP" DIRECT RECL 512 AS 11 NOWRITE NODEL       !
    SREC% = SREC% + 1                                         !
    READ FORM "T43 I4 I2 T55 I2 17I4 C388";#11,SREC%;        \! HEADER DATA 
               BLK.NUM,REC.LEN,KEY.LEN,CHA.TRE,              \!
               LON.CHA,RES.UNO,TOT.REA,REA.UPD,              \!
               TOT.ADD,WRI.UPD,RES.DOS,TOT.DEL,              \!
               TWR.1DE,TWR.2DE,TWR.3DE,TWR.MAS,              \!
               TOT.OPE,TOT.CLO,TOT.CLW,TOT.REL,H$             ! FOR KEYED FILE
    ASIZE = 508/REC.LEN * BLK.NUM                             ! MAX A$ RECORDS 
    !LOCATE 1,25                                               !
    !Print "Contabilidad de ";BLK.NUM;" Bloques.."             ! Mod OVS 06-Abr-2016
FOR II% = 2 TO BLK.NUM                                        ! Loop Lec EAMACC
    READ FORM                                                \! segun formato
    "C4 C1 2C5 C7 4I4 C28 I4 C28 I4 C28 8I4 C44 I4 C302 ";   \! Lect directa
         #11,II%;KEYS$,HD.TYPE,HD.OPERATOR,HD.DATETIME,      \! segun formato
                 A$,HD.GROSSPOS,HD.GROSSNEG,HD.NUMTRANS,     \! de la Contab
                 LO.AMTCASH,B$,PK.AMTCASH,C$,CT.AMTCASH,     \! del dia anter
                 D$,SO.AMTCASH,SO.AMTCHECK,SO.AMTFOODS,      \! para tomar
                 SO.AMTMISC1,SO.AMTMISC2,SO.AMTMISC3,        \! Falt/Sob
                 SO.AMTMANUF,SO.AMTSTORE,E$,AMTFEES,F$        ! y tipo de Reg
    IF HD.TYPE = "2" THEN BEGIN                               ! Si es Cajera
       LOCATE 20,15                                           ! Ubica cursor
       Print "     DATOS CONTABLES  ";HD.TYPE;"  ";STR$(II%);\! y muestra dat
             " Op. ";STR$(VAL(UNPACK$(HD.OPERATOR)))          ! Operador
       BASES% = BASES% + LO.AMTCASH                           ! Acumula Bases
       TT.COM = TT.COM + AMTFEES                              ! Acumula Comis
       TT.CLI = TT.CLI + HD.NUMTRANS                          ! Acumula Clientes
       IF SO.AMTCASH  > 0 THEN TT.FAL = TT.FAL + SO.AMTCASH   ! Acum Falt Ef
       IF SO.AMTCHECK > 0 THEN TT.FAL = TT.FAL + SO.AMTCHECK  ! Acum Falt Ch
       IF SO.AMTMISC1 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC1  ! Acum Falt Mis1
       IF SO.AMTMISC2 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC2  ! Acum Falt Mis2
       IF SO.AMTMISC3 > 0 THEN TT.FAL = TT.FAL + SO.AMTMISC3  ! Acum Falt Mis3
       IF SO.AMTFOODS > 0 THEN TT.FAL = TT.FAL + SO.AMTFOODS  ! Acum Falt Mis2
       IF SO.AMTSTORE > 0 THEN TT.FAL = TT.FAL + SO.AMTSTORE  ! Acum Falt CupAl
       IF SO.AMTMANUF > 0 THEN TT.FAL = TT.FAL + SO.AMTMANUF  ! Acum Falt CupFa
       IF SO.AMTCASH  < 1 THEN TT.SOB = TT.SOB + SO.AMTCASH   ! Acum Falt Ef
       IF SO.AMTCHECK < 1 THEN TT.SOB = TT.SOB + SO.AMTCHECK  ! Acum Falt Ch
       IF SO.AMTMISC1 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC1  ! Acum Falt Mis1
       IF SO.AMTMISC2 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC2  ! Acum Falt Mis2
       IF SO.AMTMISC3 < 1 THEN TT.SOB = TT.SOB + SO.AMTMISC3  ! Acum Falt Mis3
       IF SO.AMTFOODS < 1 THEN TT.SOB = TT.SOB + SO.AMTFOODS  ! Acum Falt Mis2
       IF SO.AMTSTORE < 1 THEN TT.SOB = TT.SOB + SO.AMTSTORE  ! Acum Falt CupAl
       IF SO.AMTMANUF < 1 THEN TT.SOB = TT.SOB + SO.AMTMANUF  ! Acum Falt CupFa
    EndIf                                                     ! 

NEXT II%                                                      !

FINALIZA:                                                     !
     TT.SOB = Val(Gsobrante$)
     TT.FAL = Val(Gfaltante$)        	
     TT.SOBFAL = TT.SOB + TT.FAL                              !
     Print "                   ";                             !
     
     Print "VR.TOTAL SOBRA(-)/FALTA(+) ";STR$(TT.SOBFAL)      ! 

     Call GRABA.PLANILLA( 1,RESP$, "B001","EFECTIVO            ",Pagos(11,1),(Pagos(11,2)-TT.SOBFAL),0,0,FR$)
     Call GRABA.PLANILLA( 2,RESP$, "B002","CH/BANCOS LOCALES   ",Pagos(21,1),Pagos(21,2),0,0,FR$)
     Call GRABA.PLANILLA( 3,RESP$, "B003","TARJETAS DEB/CRED   ",Pagos(41,1)+Pagos(51,1),Pagos(41,2)+Pagos(51,2),0,0,FR$)
     Call GRABA.PLANILLA( 4,RESP$, "B005","CREDITO COMFANDI    ",(Pagos(36,1)+Pagos(61,1)),(Pagos(36,2)+Pagos(61,2)),0,0,FR$)
     Call GRABA.PLANILLA( 5,RESP$, "B023","BONOS VECINO FIEL   ",Pagos(33,1),Pagos(33,2),0,0,FR$)
     Call GRABA.PLANILLA( 6,RESP$, "B025","PAGO ELEC. FOSFEC   ",Pagos(44,1),Pagos(44,2),0,0,FR$)
     Call GRABA.PLANILLA( 7,RESP$, "B029","PAGO ELECTRONICO    ",Pagos(43,1),Pagos(43,2),0,0,FR$)
     Call GRABA.PLANILLA( 8,RESP$, "B035","REMISIONES          ",Pagos(12,1),Pagos(12,2),0,0,FR$)
     Call GRABA.PLANILLA( 9,RESP$, "B036","BONO EMPLEADO       ",Pagos(42,1),Pagos(42,2),0,0,FR$)
     Call GRABA.PLANILLA(10,RESP$, "B038","CREDITO EMPRESAS    ",Pagos(13,1),Pagos(13,2),0,0,FR$)
     Call GRABA.PLANILLA(11,RESP$, "B040","CREDIAMIGO          ",Pagos(56,1),Pagos(56,2),0,0,FR$)
     Call GRABA.PLANILLA(12,RESP$, "B041","BONOS SOD/BIG/FABR. ",Pagos(31,1)+Pagos(32,1),Pagos(31,2)+Pagos(32,2),0,0,FR$)
     Call GRABA.PLANILLA(13,RESP$, "B043","BONO  DESCUENTAZOS  ",Pagos(35,1),Pagos(35,2),0,0,FR$)
     Call GRABA.PLANILLA(14,RESP$, "B044","BONOS ELECTROFIESTA ",Pagos(34,1),Pagos(34,2),0,0,FR$)
     Call GRABA.PLANILLA(15,RESP$, "B045","TARJ. BONO COMFANDI ",Pagos(46,1),Pagos(46,2),0,0,FR$)
     Call GRABA.PLANILLA(16,RESP$, "B046","TARJ.BONO INCENTIVO ",Pagos(53,1),Pagos(53,2),0,0,FR$)
     Call GRABA.PLANILLA(17,RESP$, "B047","BONOS KIT ESCOLAR   ",Pagos(22,1),Pagos(22,2),0,0,FR$)
     Call GRABA.PLANILLA(18,RESP$, "B048","LIFEMILES           ",Pagos(54,1)+Pagos(55,1),Pagos(54,2)+Pagos(55,2),0,0,FR$)
     Call GRABA.PLANILLA(19,RESP$, "B049","UNIDOS POR EL VALLE ",Pagos(14,1),Pagos(14,2),0,0,FR$)
     Call GRABA.PLANILLA(20,RESP$, "B050","TIENDA VIRTUAL      ",Pagos(15,1),Pagos(15,2),0,0,FR$)
     Call GRABA.PLANILLA(21,RESP$, "B051","BONOS KUPI          ",Pagos(63,1),Pagos(63,2),0,0,FR$)

     !Call GRABA.PLANILLA(21,RESP$, "C987","VENTA BONOS VIRTUAL.",Pagos(63,1),Pagos(63,2),0,0,FR$)
     Call GRABA.PLANILLA(22,RESP$, "B052","CREDITO BRILLA      ",Pagos(64,1),Pagos(64,2),0,0,FR$)
     Call GRABA.PLANILLA(23,RESP$, "B053","TARJ. VISA COMFANDI ",Pagos(52,1),Pagos(52,2),0,0,FR$)
     Call GRABA.PLANILLA(24,RESP$, "B056","ALKOSTO             ",Pagos(16,1),Pagos(16,2),0,0,FR$)
     
     TT.CONTEO = Pagos(11,2) + Pagos(12,2) + Pagos(13,2) + Pagos(14,2) + Pagos(15,2) + Pagos(16,2) + \! Suma Tipo 1
                 Pagos(21,2) + Pagos(22,2) + Pagos(23,2) + Pagos(24,2) + Pagos(25,2) + Pagos(26,2) + \! Suma Tipo 2
                 Pagos(31,2) + Pagos(32,2) + Pagos(33,2) + Pagos(34,2) + Pagos(35,2) + Pagos(36,2) + \! Suma Tipo 3
                 Pagos(41,2) + Pagos(42,2) + Pagos(43,2) + Pagos(44,2) + Pagos(45,2) + Pagos(46,2)    ! Suma Tipo 4
     TT.CONTEO = TT.CONTEO +                                                                         \! Siguiente grupo pagos
                 Pagos(51,2) + Pagos(52,2) + Pagos(53,2) + Pagos(54,2) + Pagos(55,2) + Pagos(56,2) + \! Suma Tipo 5
                 Pagos(61,2) + Pagos(62,2) + Pagos(63,2) + Pagos(64,2) + Pagos(65,2) + Pagos(66,2)    ! Suma Tipo 6


     Call GRABA.PLANILLA(25,RESP$, "B011","TOTAL CONTEO        ",0,TT.CONTEO-TT.SOBFAL,0,0,FR$)
     Call GRABA.PLANILLA(26,RESP$, "B012","TOTAL *ZETA*        ",0,TT.CONTEO,0,0,FR$)
     Call GRABA.PLANILLA(27,RESP$, "B013","TOTAL DE FALTANTES  ",0,TT.FAL,0,0,FR$)
     Call GRABA.PLANILLA(28,RESP$, "B014","TOTAL CONTABILIDAD  ",0,TT.CONTEO,0,0,FR$)
     Call GRABA.PLANILLA(29,RESP$, "B015","TOTAL DE SOBRANTES  ",0,(TT.SOB),0,0,FR$)
     Call GRABA.PLANILLA(30,RESP$, "C001","DESCUENTO CH/SUBS   ",0,IVAD%(001,1),0,0,FR$)
     Call GRABA.PLANILLA(31,RESP$, "C029","DESC/TEMPO ESCOLAR  ",0,IVAD%(029,1),0,0,FR$)
     Call GRABA.PLANILLA(32,RESP$, "C032","DESCUENTO ANCHETAS  ",0,IVAD%(032,1),0,0,FR$)
     Call GRABA.PLANILLA(33,RESP$, "C033","DESCUENTO FRUVER 15%",0,IVAD%(033,1),0,0,FR$)
     Call GRABA.PLANILLA(34,RESP$, "C038","DSCTO/CARRO ABANDONA",0,IVAD%(038,1),0,0,FR$)
     Call GRABA.PLANILLA(35,RESP$, "C039","DESCUENTO CARNES 10%",0,IVAD%(039,1),0,0,FR$)
     Call GRABA.PLANILLA(36,RESP$, "C041","DESCUENTO MERCADOS  ",0,IVAD%(041,1),0,0,FR$)
     Call GRABA.PLANILLA(37,RESP$, "C042","DESCUENTO 10 % VF   ",0,IVAD%(042,1),0,0,FR$)
     Call GRABA.PLANILLA(38,RESP$, "C060","DESCUENTO MEDICAM.  ",0,IVAD%(060,1),0,0,FR$)
     Call GRABA.PLANILLA(39,RESP$, "C049","DESC. BONOS COMFABU ",0,IVAD%(049,1),0,0,FR$)
     Call GRABA.PLANILLA(40,RESP$, "C051","DES/TEMP. ESCOLAR   ",0,IVAD%(051,1),0,0,FR$)
     
     TOTDES% = (IVAD%(001,1)+TT.COM+IVAD%(028,1)+        \!
                IVAD%(031,1)+IVAD%(032,1)+IVAD%(033,1)+  \!
                IVAD%(038,1)+IVAD%(039,1)+IVAD%(041,1)+  \!
                IVAD%(043,1)+IVAD%(048,1)+IVAD%(049,1) + \!
                IVAD%(029,1)+IVAD%(042,1)+IVAD%(060,1) + \!
                IVAD%(051,1))   

!               IVAD%(070,1))
              
     Call GRABA.PLANILLA(41,RESP$, "C000","TOTAL DESCUENTOS    ",0,TOTDES%,0,0,FR$)
     Call GRABA.PLANILLA(42,RESP$, "C003","TOTAL CLIENTES    ",TT.CLI,0,0,0,FR$)
     SREC% = 42
     FOR II% = 1 TO 998                                       !
         IF IVAD%(II%,1) <> 0 THEN BEGIN                      !
         IF IVAD%(II%,6) = 0 THEN BEGIN                       !
            SREC% = SREC% + 1                                 !
            PREF$ = "C"                                       !
            IF II% > 779 THEN BEGIN                           !
               CANCON% = CANCON% + IVAD%(II%,5)               !
               VARCON% = VARCON% + IVAD%(II%,1)               !
               PREF$ = "F"                                    !
            EndIf                                             !
            MACH$ = "DP"+RIGHT$("000"+STR$(II%),3)            !
            I% = MATCH(MACH$,DEPA$,1)                         !
            IF I%= 0 THEN DEPI$=MACH$+"  DEPTO SIN DEFINIR"   !
            IF I%<>0 THEN DEPI$=Mid$(DEPA$,I%+7,19)           !
            PREF$ = PREF$ + RIGHT$("000" + STR$(II%),3)       !
            Call GRABA.PLANILLA(SREC%,RESP$,PREF$,DEPI$,     \!
                         IVAD%(II%,5),IVAD%(II%,1)  +        \!
                         IVAD%(II%,2) + IVAD%(II%,3),        \!
                         IVAD%(II%,3) , IVAD%(II%,2),FR$)     !
            ESPERADO% = ESPERADO% + (IVAD%(II%,1) + IVAD%(II%,2) + IVAD%(II%,3))
         EndIf                                                !
         EndIf                                                !
    NEXT II%                                                  !
    
    Call GRABA.PLANILLA(SREC%+1,RESP$,"F998","TOTAL CONCESIONES",CANCON%,VARCON%,0,0,FR$)
    
    !ESPERADO%=IVAD%(999,1)+IVAD%(999,2)+IVAD%(999,3)-TOTDES%  !
    
    Call GRABA.PLANILLA(SREC%+2,RESP$,"F000","TOTAL VENTAS DEPTO",IVAD%(999,5),ESPERADO%, \!
                        IVAD%(999,3),IVAD%(999,2),FR$)        !
  FOR$   = "###,###"                                          !
  FORMA$ = "$$####,###,###"                                   !
  INICIMP:                                                    !
  I% = 0                                                      !
  LPRINTER                                                    !
  IF END #10 THEN CERRAR                                      ! 
LECDIR:                                                       ! Lectura Directa
  I% = I% + 1                                                 ! Incr Cont
  READ FORM "C78";#10,I%;REGI$                                ! Lectura Cuadre dia
  IF Mid$(REGI$,37,1) = " " THEN BEGIN                        ! Si es blanco en Cant
     REGI$=LEFT$(REGI$,36) + "+" + RIGHT$(REGI$,41)           ! coloca signo +
  EndIf                                                       ! 
  IF Mid$(REGI$,47,1) = " " THEN BEGIN                        ! Si es Blanco en Vr
     REGI$=LEFT$(REGI$,46) + "+" + RIGHT$(REGI$,31)           ! coloca signo +
  EndIf 
  IF Mid$(REGI$,13,4) = "F000" THEN BEGIN                     ! ARREGLO PULGA SUMATO
     VALIC$ = Mid$(REGI$,48,9)                                ! coloca signo +
     TT.VALDEP = VAL(VALIC$)                                  !
     PUL% = 1                                                 !
  EndIf                                                       !  
  WRITE FORM "C78";#10,I%;REGI$                               ! Graba segun Fmto
  FECH$ = Mid$(REGI$,3,6)                                     ! Toma Fecha
  CONC$ = Mid$(REGI$,13,4)                                    ! Concepto
  DESC$ = Mid$(REGI$,17,20)                                   ! Descriptor
  SIGN$ = Mid$(REGI$,37,1)                                    ! Signo
  CANT$ = Mid$(REGI$,38,9)                                    ! Cantidad 
  VALO$ = Mid$(REGI$,47,10)                                   ! y Valor
  IMPO$ = Mid$(REGI$,57,10)
  VIVA$ = Mid$(REGI$,67,10)

  IF SI% = 1 THEN SI% = 2                                     ! Bloquea el Flag
  IF CONLI% > 58 THEN BEGIN                                   ! Si salto de Pag
     CONLI% = 3                                               ! Asigna cont
     WRITE FORM "2C2";#FIL5;CHR$(12),FR$                      !
     WRITE FORM "C53 C27 C2";#FIL5;                          \!
           "Alm:"+ALM$+" "+NOAL$,"Procesado el : "+DATE$,FR$  ! 
     WRITE FORM "C23 C57 C2";#FIL5;                          \!
           " ","CIFRAS DE CUADRE DEL DIA: "+FECH$,FR$         !
     WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$            !
     WRITE FORM "C80 C2";#FIL5;                              \!
           "    CODIGO  CONCEPTO           "+                \!
           "CANTIDAD    VR/VENTA    I/CONSUMO "+             \!
           "      VR/IVA ",FR$                                !
     WRITE FORM "C80 C2";#FIL5;STRING$(80,"-"),FR$            !
  EndIf                                                       !
  
  IF TOTRED% > 0 AND CONC$ = "C048" THEN FORMA$ ="+"+ "$$###,###,###"                   !
  IF TOTRED% < 0 AND CONC$ = "C048" THEN FORMA$ ="-"+ "$$###,###,###"                   !
  
  IF CONC$ <> "C048" THEN FORMA$ = "$$####,###,###"                                   !
  IF SI%  = 0 AND LEFT$(CONC$,1) = "F" OR                    \! Si 1er Vez y Conces 
     CONC$ = "F999" THEN BEGIN                                ! o Total Concesiones
       SI% = 1                                                ! Indica Flag
  EndIf                                                       !
  IF LEFT$(CONC$,1) = "C" AND                                \!
     VAL(CANT$)     =  0  AND                                \!
     VAL(VALO$)     =  0  THEN GOTO LECDIR                    !
  IF CONC$ = "B015" OR                                       \!
     CONC$ = "C000" OR                                       \!
     CONC$ = "C015" OR                                       \!
     CONC$ = "F998" OR                                       \!
     CONC$ = "F000" OR                                       \!
     CONC$ = "B011" OR                                       \!
     SI%   = 1 THEN BEGIN                                     ! o Flag de Totales
     CONLI% = CONLI% + 1                                      ! Incr Cont de Lineas
     WRITE FORM "C26 C54 C2";#FIL5;                          \!
     " ","       ------- ------------" +                     \! Subtotal
     "   -----------   ----------- ",FR$                      ! Subtotal
  EndIf                                                       !
  WRITE FORM "C5 C6 C20 PIC(###,###) 3PIC($$#######,###) C2";\!
        #FIL5;" ",CONC$,DESC$,VAL(CANT$),VAL(VALO$),         \!
              VAL(IMPO$),VAL(VIVA$),FR$                       !
  CONLI% = CONLI% + 1                                         !
  GOTO LECDIR                                                 !
CERRAR:                                                       !
  WRITE FORM "C26 C54 C2";#FIL5;                             \!
  " ","       ------- ------------" +                        \! Subtotal
  "   -----------   ----------- ",FR$                         ! Subtotal
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                          !
  WRITE FORM "C80 C2";#FIL5;"  ",FR$                          !
  WRITE FORM "C80 C2";#FIL5;                                 \!
        "      ---------------------------"+                 \!
        "     ----------------------------",FR$               !
  WRITE FORM "C80 C2";#FIL5;                                 \!
        "        VoBo del Administrador   "+                 \! 
        "     Recibido Centro Inf Mercadeo ",FR$              !
  IF CANLIS% = 0 THEN BEGIN                                   ! Si solo 1er listado
     CONLI% = 60                                              ! Condic de Overflow
     CANLIS% = 1                                              ! Indica 2o Listado
     CLOSE 10                                                 ! Cierra Cuadre

     !OPEN AR10$ DIRECT RECL 78 AS 10 BUFFSIZE 1780            ! Abre Cuadre
     !GOTO INICIMP                                             ! Vuelve a empezar
     
  EndIf                                                       ! Fin Condic
DESCU    = TT.CONTEO-(TT.VALDEP+(SA.AMTDISCO-TO.DISVOID))     !
DESCU$   = STR$(DESCU)                                        !
If TT.CONTEO = ESPERADO% + TOTDES% THEN BEGIN                 !
    CLEARS                                                    !
    CONSOLE                                                   !
    CUADRE = 1                                                !
    Print " "                                                 ! 
    Print "      ****************************************************"   !
    Print "               P L A N I L L A   C U A D R A D A"  !
    Print "                                           "       !
    Print "          VALOR POR DEPARTAMENTOS :";              !
    Print USING FORMA$ ; ESPERADO%+TOTDES%                    !
    Print USING FORMA$ ; ESPERADO%                            !
    Print USING FORMA$ ; TOTDES%                              !
    Print "          VALOR POR DOMICILIOS    :";              !
    Print USING FORMA$ ; Asc.Vta.Domic%                       !
    Print "                                           "       !
    Print "      VALOR ZETA O MEDIOS DE PAGO :";              !
    Print USING FORMA$;      TT.CONTEO                        !
    Print "                                           "       !
    Print "          Continue con los procesos y con la copia"!
    Print "          de archivos.                        "    !
    Print "          Este es un mensaje informativo.     "    !
    Print "      ****************************************************"
EndIf ELSE BEGIN                                              !
    CLEARS                                                    !
    CUADRE = 0                                                !
    CONSOLE                                                   !
    Print " "                                                 !
    Print "      ****************************************************" 
    Print "      ****************************************************"
    Print "                                                 " !
    Print "             P L A N I L L A   N O  C U A D R A  " !
    Print " "                                                 !
    Print "             ERROR; ERROR; ERROR; ERROR; ERROR ; " !
    Print "                                              "    !
    Print "             VALOR POR DEPARTAMENTOS :";           !
    Print USING FORMA$ ; ESPERADO% + TOTDES%                  !
    Print USING FORMA$ ; ESPERADO%                            !
    Print USING FORMA$ ; TOTDES%                              !
    Print "          VALOR POR DOMICILIOS    :";              !
    Print USING FORMA$ ; Asc.Vta.Domic%                       !    
    Print "                                              "    !
    Print "         VALOR ZETA O MEDIOS DE PAGO :";           !
    Print USING FORMA$ ; TT.CONTEO                            !
    Print " "                                                 !
    Print "   ********************************************************  " !
    Print "   *   Revise los valores de los medios de pago           *  " !
    Print "   *   y las partidas por departamentos porque es         *  " !
    Print "   *   muy posible que haya un error, comuniquese con     *  " !
    Print "   *   el técnico de APOYO        !LO MAS PRONTO POSIBLE..*  " !
    Print "   *                                                      *  " !
    Print "   *   Y POR FAVOR NO ENVIE EL ARCHIVO NI LA PLANILLA     *  " !
    Print "   *   GRACIAS....................                        *  " !
    Print "   *                                                      *  " !
    Print "   *          ESTE ES UN MENSAJE INFORMATIVO.....!        *  " !
    Print "   ********************************************************  " !
LPRINTER
EndIf
END SUB                                                       ! 

Sub ACUMULA                                                 !

      If SIGNO$ = "+" Then Begin                              ! Pago con signo +
         Pagos(Val(TIPPAG$),1) = Pagos(Val(TIPPAG$),1) + 1    ! Acumula bono
         Pagos(Val(TIPPAG$),2) = Pagos(Val(TIPPAG$),2) + VAL(VALPAG) ! Acum Vr bono 
      EndIf Else Begin                                        ! de lo contrario
         Pagos(Val(TIPPAG$),2) = Pagos(Val(TIPPAG$),2) - VAL(VALPAG) ! Resta Acum Vr bono 
      EndIf                                                   ! Fin de Condic
     
End Sub 

! -------------  Rutina para crear compaginar Hora y Fecha --------------

    SUB CREAFECHA Public 
      FRA.TIM$ = TIME$                                        ! Toma Hora Sistema
      FRA.H$   = LEFT$(FRA.TIM$,2)                            ! Arma horas
      FRA.M$   = Mid$(FRA.TIM$,3,2)                           ! Arma minutos
      FRA.S$   = RIGHT$(FRA.TIM$,2)                           ! Arma segundos
      FRA.FEC$ = DATE$                                        ! Toma Fecha Sistema
      FRA.AA$  = LEFT$(FRA.FEC$,2)                            ! Arma a¤o
      FRA.MM$  = Mid$(FRA.FEC$,3,2)                           ! Arma mes
      FRA.DD$  = RIGHT$(FRA.FEC$,2)                           ! Arma dia
      MES      = VAL(FRA.MM$)                                 ! Toma Mes
      SIGLO$   = "20"
      FHOY$    = TM$(MES)+" "+STR$(VAL(FRA.DD$))             \! Arma Fecha Hoy
                 + " " +SIGLO$ + FRA.AA$                      !
      FRA.LIN$ =FRA.H$+":"+FRA.M$+":"+FRA.S$     +           \! Arma la Linea
               "    de "+FHOY$                                ! con Hora y Fecha
      FRA.MD$  = RIGHT$(FRA.FEC$,4)                           ! Salva Mes y Dia 
  END Sub

!------------------------------------------------------------------------------
!           Subrutina para Tomar IVAS, Deptos y Terminales
!------------------------------------------------------------------------------
SUB TOMA.IVAS                                                 !
    DIM IVAD%(999,6)                                          !
    DIM IVAT%(100,7)                                          !
    OPEN "EAMITEMR" KEYED RECL 77 AS 22                       !
    FOR II% = 1 TO 999                                        !
        KEY$ = PACK$("000099999" + RIGHT$("000"+STR$(II%),3)) !
        EOF$ = "  "                                           !
        READ FORM "C7 2I1 C15 C18 C4 C31";#22 KEY KEY$;      \!
                   KEY$,IND1,IND1A,DATO4$,RESP$,DATO4$,GR.DATA$ !
        IF EOF$ = "  " THEN BEGIN                             !
           IVGR$ = " 0"                                       !
           IF IND1  AND 80H THEN IVGR$ = "16"                 !
           IF IND1  AND 40H THEN IVGR$ = "19"                 !
           IF IND1  AND 20H THEN IVGR$ = "05"                 !
           IF IND1  AND 10H THEN IVGR$ = "08"                 !
           IF IND1A AND 01H THEN IND1A = 1 ELSE IND1A = 0     ! Si user Option 4 On
           IF IND1A = 1 THEN CADE% = CADE% + 1                !
           IVAD%(II%,0) = VAL(IVGR$)                          !
           IVAD%(II%,6) = IND1A
           IVGR$ = IVGR$ + "%"                                !
           DEPA$ = DEPA$ + "DP"+RIGHT$("000"+STR$(II%),3)+   \!
                   "  "  + RESP$ + " " + IVGR$                !
        EndIf                                                 !
    NEXT II%                                                  !
    IF CADE% = 0 THEN BEGIN                                   !
       CLEARS                                                 !
       LOCATE 10,15                                           !
       Print "A L E R T A : En este Sistema NO se ha Definido"!
       LOCATE 11,35                                           !
       Print "ESQUEMA DE descuentos..!"                       !
       LOCATE 14,25                                           !
       Print "El Proceso se   S U S P E N D E..!"             !
       STOP                                                   !
    EndIf                                                     !
    FOR II% = 1 TO 050                                        !
        KEY$ = PACK$("000098989" + RIGHT$("000"+STR$(II%),3)) !
        EOF$ = "  "                                           !
        READ FORM "C7 I1 C16 C18 C35";#22 KEY KEY$;           \!
                   KEY$,IND1,DATO4$,RESP$,DATO4$              !
        IF EOF$ = "  " THEN BEGIN                             !
           TPVA$ = TPVA$ + "TP" + RIGHT$("000"+STR$(II%),3)+ \!
                   "  " + RESP$                               !
        EndIf                                                 !
    NEXT II%                                                  !
END SUB                                                       !
!------------------------------------------------------------------------------
!             Liquidacion de Bases, Ivas e ImpoConsumo por Transac
!------------------------------------------------------------------------------
SUB LIQUIVA.MED                                               !
  DETRA1$ = DETRA$                                            !
  WHILE LEN(DETRA1$) > 0                                      !
     DEPI%      = VAL(Mid$(DETRA1$,3,3))                      !
     DETRA1$    = RIGHT$(DETRA1$,LEN(DETRA1$)-5)              !
     FOR TFI%   = 0 TO 4                                      !
         BASE%  = IV.TF%(DEPI%,TFI%,0) -                     \! Calc Base sin
                  IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         IV     = IVAD%(DEPI%,0)                              ! Tarifa IVA/DEP
         IV     = 1 + FLOAT(IV)/100                           ! Calc Fctr Iva
         BASE%  = ROUND(BASE% / IV, 0, 0)                     ! Deflc Iva Base
         ICO%   = IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         CAN%   = IV.TF%(DEPI%,TFI%,2)                        ! Cantidades
         IVATR% = IV.TF%(DEPI%,TFI%,0) - ICO% - BASE%         ! Calc IVA
         IF IV < 1.01 THEN BEGIN                              ! Ventas Exentas
            IVATRA%(0) = IVATRA%(0) + BASE%                   ! Suma Base Ex-Transac
         EndIf ELSE BEGIN                                     !
           IF IV = 1.16 THEN IVATRA%(1) = IVATRA%(1) + BASE%
           IF IV = 1.19 THEN IVATRA%(2) = IVATRA%(2) + BASE% 
           IF IV = 1.05 THEN IVATRA%(3) = IVATRA%(3) + BASE%
           IF IV = 1.08 THEN IVATRA%(4) = IVATRA%(4) + BASE% 
           IVATRA%(5) = IVATRA%(5) + IVATR%                   ! Suma Ivas-Transac
         EndIf                                                !
         IVATRA%(6) = IVATRA%(6) + ICO%                       ! Suma ImpCon-Transac
      NEXT TFI%                                               !
WEND                                                        !

END SUB                                                       !

!------------------------------------------------------------------------------
!             Liquidacion de Bases, Ivas e ImpoConsumo por Transac
!------------------------------------------------------------------------------
SUB LIQUIVA                                                   !
  SILIQ% = 0                                                  !
  WHILE LEN(DETRA$) > 0                                       !
     BLA$ = "              "                                  !
     DEPI%      = VAL(Mid$(DETRA$,3,3))                       !
     DETRA$     = RIGHT$(DETRA$,LEN(DETRA$)-5)                !
     FOR TFI%   = 0 TO 4                                      !
         BASE%  = IV.TF%(DEPI%,TFI%,0) -                     \! Calc Base sin
                  IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         IV     = IVAD%(DEPI%,0)                              ! Tarifa IVA/DEP
         IV     = 1 + FLOAT(IV)/100                           ! Calc Fctr Iva
         BASE%  = ROUND(BASE% / IV, 0, 0)                     ! Deflc Iva Base
         ICO%   = IV.TF%(DEPI%,TFI%,1)                        ! ImpConsumo
         CAN%   = IV.TF%(DEPI%,TFI%,2)                        ! Cantidades
         IVATR% = IV.TF%(DEPI%,TFI%,0) - ICO% - BASE%         ! Calc IVA
         IVAD%(DEPI%,1) = IVAD%(DEPI%,1) + BASE%              !
         IVAD%(DEPI%,2) = IVAD%(DEPI%,2) + IVATR%             !
         IVAD%(DEPI%,3) = IVAD%(DEPI%,3) + ICO%               !
         IVAD%(DEPI%,5) = IVAD%(DEPI%,5) + CAN%               !
         IF IV = 1 THEN BEGIN                                 ! Ventas Exentas
            IVATRA%(0) = IVATRA%(0) + BASE%                   ! Suma Base Ex-Transac
            IVAT%(NT%,5) = IVAT%(NT%,5)+BASE%                 ! suma solo en Tot/Terminal
            IVAT%(NT%,4) = IVAT%(NT%,4)+IVATR%                ! Suma Ivas Terminal
         EndIf ELSE BEGIN                                     !
            IVATRA%(TFI%) = IVATRA%(TFI%) + BASE%             ! Suma Base Iva-Transac
            IVAT%(NT%,3) = IVAT%(NT%,3)+BASE%                 ! suma solo en Tot/Terminal
            IVAT%(NT%,4) = IVAT%(NT%,4)+IVATR%                ! Suma Ivas Terminal
            IVATRA%(5) = IVATRA%(5) + IVATR%                  ! Suma Ivas-Transac
         EndIf                                                !
         IVAT%(NT%,6) = IVAT%(NT%,6)+ICO%                     ! Suma ImpoC Terminal
         IVATRA%(6) = IVATRA%(6) + ICO%                       ! Suma ImpCon-Transac
      NEXT TFI%                                               !
  WEND                                                        !
  DIM IV.TF%(999,4,2)                                         !
  DIM IV.TF2%(999,4,1)                                        !
END SUB                                                       !

SUB GRABA.OTROIVA                                             !
    CALL LIQUIVA.MED                                          !
    SG$ = "+"                                                 !
    REGMED9$ = LEFT$(REGMED$,74)                              !
    EXEIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(0))),9)   !
    PLAIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(1))),9)   !
    PLBIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(2))),9)   !
    PLCIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(3))),9)   !
    PLDIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(4))),9)   !
    GRTIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(5))),9)   ! 
    ICOIVA$ = RIGHT$("000000000" + STR$(ABS(IVATRA%(6))),9)   ! 
    FOR FI% = 0 TO 6                                          !
        TOTTRAN% = IVATRA%(FI%) + TOTTRAN%                    !
        IF TOTTRAN% < 0 THEN SG$ = "-"                        !
    NEXT FI%                                                  !
    TOTTRAN$ = RIGHT$("000000000" + STR$(ABS(TOTTRAN%)),9)    ! 
    IF TOTTRAN%=0 AND Mid$(REGMED9$,21,1)="-" Then SG$ = "-"  !
    DESCUE$ = STR$(SA.AMTDISCO*-1)                               !
    DESCUE$ =RIGHT$(STRING$(6,"0")+DESCUE$,6)                 !
    Call String.Fiscal
    !FISCO$ = "TR"+"+"+LEFT$(PRETR$,2)+ "00"+"-"+RIGHT$(NUFIS$,7)
    
    NUMFIS$ = RIGHT$("           "+NUFIS$,11)
    FISCO$ = LEFT$(PRETR$,4) + NUMFIS$

    !FISCO$ = RIGHT$(STRING$(15,"0")+FISCO$,15)                ! Oct. 21 de 2002
    !FISCO$ = PRENUF$                                          !
    
    If Gr.Ind.Rem% = -1 Then Begin 
    	 FISCO$ = String$(15,"X")
    EndIf 
    REGMED9$=REGMED9$+EXEIVA$+SG$+"16"+PLAIVA$     +         \!
             SG$+"19"+PLBIVA$+SG$+"05"+PLCIVA$     +         \!
             SG$+"08"+PLDIVA$+SG$+GRTIVA$+SG$      +         \!
             ICOIVA$+SG$+TOTTRAN$+SG$                         !
    WRITE FORM "C162 C15 C6 C2";#FIL9,POSI%;                 \! 
               REGMED9$,FISCO$,DESCUE$,FR$                    !
    POSI% = POSI% + 1                                         !
    DESCUE$ = ""
    SA.AMTDISCO = 0
    DIM IVATRA%(9)                                            !
    TOTTRAN% = 0                                              !
    CREDI% = 0 
END SUB                                                       !
!------------------------------------------------------------------------------
!                 Acumulados de IVA e Impuesto al Consumo
!------------------------------------------------------------------------------

SUB ACU.IVAICO                                   														! 
Integer*4 Xvlrbol%, Xdpt%, XICUI%, Xpos%, xtax%, Gbase%											!
Integer*1 XIND0%, XIND1%, XIND1A%, Xiva% 																	!
String  XTMP$																																!
  EOF$ = "  "                                    														! Inicia Flag de Error
  LLAVE$ = RIGHT$("000000000000" + COPLU$,12)    														! Arma llave             
  LLAVE$ = PACK$(LLAVE$)                                                    ! y la empaqueta         
  TFI$   = RIGHT$("00"+STR$(IVAD%(DEPI%,0)),2)                              ! Convierte a Str long 2 
  TFI%   = MATCH(TARIF$,TFI$,1)                                             ! Busca Posic Tarifa IVA 
  TFI%   = TFI% / 3 + 1                                                     ! Calcula Ocorrencia     
  ICON%  = 0                                                                ! ImpoConsumo Default    
  READ FORM "C6 3I1 C1 C2 C4 C1 C5 C20 2I2 C31";#22 									     \! Lee EAMITEMR
       KEY LLAVE$;REGI$,XIND0%, XIND1%, XIND1A%, IND2$,SEC$,FAGR$,         \! para localizar
           CONT$,ALI$,LINO$,ICON%,FECR%,GR.DATA$ 														! Impuesto al Consumo
  ICON% = GETN4(GR.DATA$,16)                     														! Valor del impoconsumo
  IF EOF$ = "  " THEN BEGIN                      														! Si hay error Lectura
     IF RIGHT$(UNPACK$(IND2$),1)="5" THEN BEGIN  														! Si em Metodo/Precio 5
        SECCI$ = SEC$                            														! toma depto y
        LLAVE$ = PACK$("00")+ALI$                														! extrae Llave ALIAS
        READ FORM "C9 C1 C2 C4 C1 C5 C20 2I2 C31";                         \! Lee nuevemente 
             #22 KEY LLAVE$;REGI$,IND2$,                                   \! EAMITEMR
                 SEC$,FAGR$,CONT$,ALI$,                                    \!
                 LINO$,ICON%,FECR%,GR.DATA$      													  ! para comparar Deptos
     EndIf                                       														!
  EndIf 																																		!
  ICON% = GETN4(GR.DATA$,16)                     														! Valor del impoconsumo
  IF CAN% = 0 THEN CAN% = 1                      														!
  ICO%  = ICON% * (CAN% * SGN%)                  														! Calcula Imp Consumo

!-- Calculo ICUI
  If (XIND1A% AND 08H) Then Begin 							 														! Producto con ICUI
  	 If Val(Gdpto$) > 800 Then GoTo Acum.Dpto.Vta                           ! 
     Xdpt% = Val(Gr.Iva.DptoIcui$)																					! Dpto acumulacion icui
     Xpos% = MATCH(Str$(Xdpt%),DETRA$,1)           													!
     IF Xpos% = 0 THEN                                                     \!
        DETRA$ = DETRA$ + "DP" + Right$("000"+Gr.Iva.DptoIcui$,3)						!
     Xiva% = Tarifa.Iva.Item(XIND1%)
     xiva% = ue.tablaiva(xiva%)
     Xtax% = Gr.Iva.PtgIcui% + xiva%                    								    ! Tarifa IVA + % ICUI
     XTMP$ = Str$(ROUND(FLOAT(BB.PRECIO)/(1.+FLOAT(Xtax%)/100.),0,0))   		! Base de la compra
     Gbase% = Val(XTMP$)                                              			! Valor Base compra
     XICUI% = (Gbase% * Gr.Iva.PtgIcui%) / 100                            	! Valor del ICUI
     BB.PRECIO = BB.PRECIO - XICUI%																					! Ajusta el precio
     IV.TF%(Xdpt%,TFI%,0) = IV.TF%(Xdpt%,TFI%,0)+ XICUI% 										! Acumula Vta/PLU
     IV.TF%(Xdpt%,TFI%,2) = 1      																					! Acumula Cantidad
     IV.TF%(Xdpt%,TFI%,1) = IV.TF%(Xdpt%,TFI%,1)+ 0      										! Acumula ImpCon/PLU
     IV.TF2%(Xdpt%,TFI%,1) = IV.TF2%(Xdpt%,TFI%,1)+ Xicui%									! Acumula ICUI
     GoTo Acum.Dpto.Vta
  EndIF																					 														! Fin calculo ICUI
!--- Fin calculo ICUI

!--- Calculo impto bolsas 14Jul2023
!  If Val(COPLU$) = Gr.Iva.PluBol% Then Begin                                ! Venta Bolsa
!   	  Xvlrbol% = Gr.Iva.VlrBol% * (CAN% * SGN%)   													! Total bolsas
!  	  BB.PRECIO = BB.PRECIO - Xvlrbol%            													! Vta Sin impuesto bolsa
!  	  Xdpt% = Val(Gr.Iva.DptBol$)                 													! Reporta dpto venta bolsas
!      IV.TF%(Xdpt%,1,0) = IV.TF%(Xdpt%,1,0)+ BB.PRECIO               				! Acumula Vta/PLU
!      IV.TF%(Xdpt%,1,2) = IV.TF%(Xdpt%,1,2)+  1                       		  ! Acumula Cantidad
!      IV.TF%(Xdpt%,1,1) = IV.TF%(Xdpt%,1,1)+  0                       		  ! Acumula ImpCon/PLU
!      BB.PRECIO = Xvlrbol%
!  EndIf
!--- Fin calculo impto bolsas  


Acum.Dpto.Vta:
  
  IV.TF%(DEPI%,TFI%,0) = IV.TF%(DEPI%,TFI%,0)+  \!
                         BB.PRECIO               ! Acumula Vta/PLU
  IV.TF%(DEPI%,TFI%,1) = IV.TF%(DEPI%,TFI%,1)+  \!
                         ICO%                    ! Acumula ImpCon/PLU
  IV.TF%(DEPI%,TFI%,2) = IV.TF%(DEPI%,TFI%,2)+  \!
                         1                       ! Acumula Cantidad
End SUB                                          !
!------------------------------------------------------------------------------
!----                              PROGRAMA  PRINCIPAL                     ----
!------------------------------------------------------------------------------
ON ERROR GOTO ERRTN                                           !
   Dim Pagos(90,2)                                    ! Formas de pago registradas
   Call Parametros.Bines                              ! Definicion bines TEF
   Dim UE.TABLAIVA(10)
   Call Carga.Iva.Planilla                            ! Carga tarifas de IVA
   FMTO1$="C74 PIC(-9999999) "              +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) PIC(-9999999)"          +        \!
          " PIC(99) 4PIC(-9999999) C2"                !
   OCH$   = CHR$(27)+CHR$(48)                         ! Impr a 1/8's 1B30
   CND$   = CHR$(27)+CHR$(15)                         ! Condensado 1B0F
   S8CND$ = CHR$(12)+OCH$+CND$                        ! Salto;1/8's;Condens
   SEX$   = CHR$(27)+CHR$(50)                         ! Impr a 1/6's 1B32
   DIS$   = CHR$(27)+CHR$(80)                         ! 10 CPI(NO Cond) 1B50
   S6NCN$ = SEX$+DIS$                                 ! Salto;1/6's;NO-Condens
   DIM TRF%(999,2)                                    !
   DIM IVATRA%(9)                                     !
   DIM IV.TF%(999,4,2)                                !
   DIM IV.TF2%(999,4,1)                               !
   DIM TPG%(7,7,1)                                    !
   DIM TI$(7,7)                                       !
   DIM TM$(12)                                        !
   CIENP = 100.0                                      !
   POSI% = 3
   FECHA$  = DATE$                                    ! Toma Fecha 
   MESPRO  = VAL(Mid$(FECHA$,3,2))                    ! Mes
   DIAPRO$ = RIGHT$(FECHA$,2)                         ! Dia
   ANOPRO$ = LEFT$(FECHA$,2)                          !
   FINR$   = CHR$(13) + CHR$(10)                      ! Caracter "0D0A"
   FR$     = CHR$(13) + CHR$(10)                      ! Fin de Reg 0D0A
!----------------------   Llena arreglo de meses   ---------------------------
   TM$(1)="ENERO"     :  TM$(2)="FEBRERO"    :  TM$(3)="MARZO"
   TM$(4)="ABRIL"     :  TM$(5)="MAYO"       :  TM$(6)="JUNIO"
   TM$(7)="JULIO"     :  TM$(8)="AGOSTO"     :  TM$(9)="SEPTIEMBRE"
   TM$(10)="OCTUBRE"  :  TM$(11)="NOVIEMBRE" :  TM$(12)="DICIEMBRE"

   TI$(1,0)="EFECTIVO"  :  TI$(2,0)="CHEQUES"    :  TI$(3,0)="OTROS PAG/(MFac)"
   TI$(4,0)="RED MULTIC":  TI$(5,0)="TARJETAS CR":  TI$(6,0)="CREDITO COMFANDI"

   TI$(1,1)="Efectivo            "  :  TI$(1,2)="Remision           "
   TI$(1,3)="Credito Empresas    "  :  TI$(1,4)="                   "
   TI$(1,5)="                    "  :  TI$(1,6)="                   "

   TI$(2,1)="Bancos Locales      "  :  TI$(2,2)="                   "
   TI$(2,3)="                    "  :  TI$(2,4)="                   "
   TI$(2,5)="Ch Comfandi         "  :  TI$(2,6)="                   "

   TI$(3,1)="Bonos Sodexho       "  :  TI$(3,2)="Bonos Fabricante   "
   TI$(3,3)="Redencion Puntos    "  :  TI$(3,4)="Bonos Big Pass     "
   TI$(3,5)="Bonos Descuentazo   "  :  TI$(3,6)="CR Comfandi(mf)    "
    
   TI$(4,1)="Pagos Tarjetas/DB   "  :  TI$(4,2)="Bono Electronico   "
   TI$(4,3)="Pagos Electronico   "  :  TI$(4,4)="Fosfec             "
   TI$(4,5)="Capitacion          "  :  TI$(4,6)="Tarj. Bono Amigo   "
   
   TI$(5,1)="Pagos Tarjetas/CR   "  :  TI$(5,2)="Bono Pre. Comfandi "
   TI$(5,3)="                    "  :  TI$(5,4)="                   "
   TI$(5,5)="                    "  :  TI$(5,6)="CrediAmigo         "
   
   TI$(6,1)="CR Comfandi(mf)     "  :  TI$(6,2)="                   "
   TI$(6,3)="Credito Interno     "  :  TI$(6,4)="                   "
   TI$(6,5)="                    "  :  TI$(6,6)="                   "
!----------------------------------------------------------------------------

TAMA% = VAL(COMMAND$)                                         ! Procesar cierto
Gversion$ = Command$
IF TAMA% = 0 THEN TAMA% = 99999999                            ! tamano de LOG
CLEARS                                                        !
If Ucase$(Gversion$) = "VERSION" Then Begin
   Print "CAJA DE COMPENSACION FAMILIAR DEL VALLE - COMFANDI    "
   Print "Generacion Movimientos Diarios de Venta"               !
   !rint "Version: 17/Mzo/2020 03:15 p.m."                       ! Avanza Linea
   !Print "Version: 21/Myo/2020 08:43 a.m."                       ! Avanza Linea
   !Print "Version: 25/Jun/2020 06:20 p.m."                       ! Avanza Linea
   !Print "Version: 11/Ago/2020 10:20 a.m."                       ! Avanza Linea
   !Print "Version: 27/Myo/2021 12:02 p.m."                       ! Avanza Linea
   !Print "Version: 01/Ago/2021 10:30 p.m."                       ! Avanza Linea
   !Print "Version: 27/Sep/2021 12:30 p.m."                       ! Avanza Linea
   !Print "Version: 15/Feb/2022 04:30 p.m."                       ! Avanza Linea
   !Print "Version: 30/Mzo/2022 02:30 p.m."                       ! Avanza Linea
   !Print "Version: 14/Sep/2022 10:30 a.m."                       ! Avanza Linea
   !Print "Version: 29/Sep/2022 05:20 p.m."                       ! Avanza Linea
   !Print "Version: 25/Oct/2022 11:50 a.m."                       ! Avanza Linea
   !Print "Version: 15/Nov/2022 05:00 p.m."                       ! Avanza Linea
   !Print "Version: 15/Dic/2022 05:00 p.m."                       ! Avanza Linea
   !Print "Version: 11/May/2023 07:00 p.m."                       ! Avanza Linea
   !Print "Version: 07/Nov/2023 08:15 p.m."                       ! Avanza Linea
    Print "Version: 20/Nov/2023 10:30 a.m."                       ! Avanza Linea
    Stop 
EndIf
Print "Ingresar datos para proceso de cuadre"
Input "Valor Sobrante:";Gsobrante$
Input "Valor Faltante:";Gfaltante$
CLEARS                                                        !

TITULO$ = "        ** CREACION DE LOS MEDIOS DE PAGO (MED.DAT) **"
TITULO$ = "CAJA DE COMPENSACION FAMILIAR DEL VALLE - COMFANDI    "
LOCATE 12,20
Print "Toma Iva de PLU's Departamentales..."
CALL TOMA.IVAS
CALL CREAFECHA                                                !
FEC1$ = FRA.LIN$                                              !
CONLI% = 60                                                   !  
EMPEZAR:                                                      !
AR0$ = "EAMCSCF1"                                             ! Arch Cotrl Alm
AR1$ = "C:\ADX_UDT1\MED.DAT"                                  ! Medios de Pago
AR2$ = "C:\ADX_UDT1\PAGCOM.DAT"                               ! Pagos Compbte
AR3$ = "C:\ADX_UDT1\CHQPOS.DAT"                               ! Cheques Postfec
AR4$ = "C:\ADX_UDT1\PAGO.DAT"                                 ! Pagos Hoy
AR5$ = "C:\ADX_UDT1\Z"                                        ! Comprob Diario
AR6$ = "C:\ADX_UDT1\PG.DAT"                                   ! Pagares
AR7$ = "C:\VEND1.DAT"                                         ! Vendedores
AR8$ = "C:\ADX_UDT1\R"                                        ! Rebajas
AR9$ = "C:\ADX_UDT1\IVACRED.DAT"                              ! Medios de Pago
AR10$= "C:\C"                                                 ! Medios de Pago
AR11$= "C:\ADX_UDT1\F"                                        ! VTA EMPRESA
AR12$= "C:\ADX_UDT1\G"                                        ! GASES OCCIDE
AR13$= "C:\ADX_UDT1\N"                                        ! PARTE UNO INTERFAZ Z

AR14$= "C:\ADX_UDT1\A"                                        ! BONOS SUBSIDO
!Open AR0$ KEYED RECL 36 AS 1 UNLOCKED NOWRITE NODEL          ! Arch. Control

Open AR0$ KEYED RECL 36 AS 1 NOWRITE NODEL                    ! Arch. Control

KEYCF1$=PACK$("9998")                                         ! Reg. de Cierre
READ FORM "C36";#1 KEY KEYCF1$;REGCF1$                        ! Lectura
B$="ADX_IDT4:" + Mid$(REGCF1$,11,8) + ".DAT"               ! TSLOG Previo
IF Mid$(REGCF1$,11,8)="        " THEN B$="EAMTRANA"           ! TSLOG 1st Vez
CLOSE 1                                                       ! Cierra EAMCSCF1
OPEN "C:\ADXALMA.DAT" AS 24 BUFFSIZE 80                       ! Arch Ctrl Alm
READ FORM "C80";#24;AL$                                       ! Lee Ctrl Alm
WHILE TIP$ <> "5"                                             ! Planes Iva
      READ FORM "C80";#24;ALL$                                ! Lee Cotrl Alm
      TIP$ = Mid$(ALL$,78,1)                                  !
      IF TIP$ = "5" THEN TARIF$ = "00 "+ LEFT$(ALL$,12)       !
WEND                                                          !
CLOSE 24                                                      ! Lo Cierra 

b$ = Gversion$																								! Procesa Log capturado

AL$     = AL$ + STRING$(20," ")                               ! Ajusta Reg Ctrl
RESP$   = Mid$(AL$,5,8)                                       ! Toma Fecha Control
NUAL$   = STR$(VAL(LEFT$(AL$, 4)))                            ! Guarda Num Almacen
CCOSTO$ = "0"+NUAL$                                           ! Guarda Centro Costo
NOAL$   = Mid$(AL$, 13, 30)                                   ! Guarda Nombre
ALM$    = NUAL$                                               ! 
RESP$   = RESP$+"0"+NUAL$                                     ! Arma Reg Ctrl 
ANOT% = VAL(LEFT$(RESP$,4))                                   !
MEST% = VAL(Mid$(RESP$,5,2))                                  !
DIAT% = VAL(Mid$(RESP$,7,2))                                  !
LETMES$ = "EFMAYJLGSOND"                                      ! Equival Meses
LM$     = Mid$(LETMES$,VAL(Mid$(RESP$,5,2)),1)                !
AR10$   = AR10$ + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
AR8$    = AR8$  + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
!AR5$    = AR5$  + NUAL$ + LM$+Mid$(RESP$,7,2) + ".DAT"        !
AR5$    = AR5$  + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
AR11$   = AR11$ + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
AR12$   = AR12$ + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
AR13$   = AR13$ + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !

AR14$   = AR14$ + NUAL$ + Mid$(RESP$,5,4) + ".DAT"            !
IF FRA.AA$ > Mid$(AL$,7,2) THEN BEGIN                         ! Informa Cambio de A¤o
    CLEARS                                                    !
    Print "        Registramos Complacidos un "               !
    Print                                                     !
    Print "        **** N U E V O   A ¥ O ***"                !
    Print                                                     !
    Print "   Las  CON	ES  de las ventas diarias"
    Print "   a  partir  de  la Fecha, quedar n con el"
    Print "   NUEVO a¤o. Hoy se MODIFICA  este dato en"
    Print "   forma AUTOMATICA en C:\ADXALMA.DAT."            !
    Print                                                     !
    Print "   Este es un mensaje INFORMATIVO..! "             ! 
    Print                                                     !
    !INPUT "   ..Feliz A¤o Nuevo..!    Una vez m s con.... INTRO.." ; CONTRO$
    Print                                                     !
EndIf                                                         !
CLEARS                                                        !
Print TITULO$                                                 !
Print "Generacion Movimientos Diarios de Venta"               !
Print "Version: 20/Nov/2023 10:30 a.m."                       ! Avanza Linea
Print String$(78,"-")
Print "Proceso del Almacen No. ";ALM$;"  ";NOAL$              !
Print "Inicio Proceso: ";FEC1$                                ! Indica comienzo
Print                                                         ! Avanza Linea
Print                                                         ! Avanza Linea
!Print "    *(";STR$(CADE%);")    Se esta procesando   : ";B$  ! Archivo en Proceso
Print "Procesando Log Transacciones : ";B$  ! Archivo en Proceso
Print "Se Generaran los siguientes Archivos "                 !
Print "MEDIOS DE PAGO               : ";AR1$             !
Print "RESUMEN CONTABLE             : ";AR10$            !
Print "CUOTAS MODERADORAS           : ";AR9$             !
Print "VENTAS POR EMPRESA           : ";AR11$            !
Print "REBAJAS Y DESCUENTOS         : ";AR8$             !
Print "COMPROBANTE INFORME DIARIO   : ";AR5$             !
Print "                               ";AR13$                 ! Avanza Linea
Print "                        Por Favor Esperar..."          !
Print "                         "                             !

TLOG = 25  :  FIL1 = 26    :     FIL2 = 27                    ! N£meros de
FIL3 = 28  :  FIL4 = 29    :     FIL5 = 30                    ! Sesi¢n Usados
FIL6 = 31  :  FIL7 = 32    :     FIL8 = 33                    ! por el Programa
FIL9 = 34  :  FIL10 = 10   :     FIL11 = 35                   !
FIL12 = 36 :  FIL13 = 37   :     FIL14 = 38                   !  
CREATE AR1$  DIRECT RECL 76  AS FIL1 BUFFSIZE 7680            ! Creaci¢n de
CREATE AR10$ DIRECT RECL 78  AS FIL10 BUFFSIZE 1780           !
CREATE AR9$  DIRECT RECL 185 AS FIL9 BUFFSIZE 5120            !   
CREATE AR2$  AS FIL2 BUFFSIZE 2560                            ! Archivos en
CREATE AR3$  AS FIL3 BUFFSIZE 2560                            ! ADX_UDT1
CREATE AR4$  AS FIL4 BUFFSIZE 2560                            ! 
CREATE AR5$  AS FIL5 BUFFSIZE 2560                            !
CREATE AR6$  AS FIL6 BUFFSIZE 2560                            ! 
CREATE AR7$  AS FIL7 BUFFSIZE 1780                            !
CREATE AR8$  AS FIL8 BUFFSIZE 1780                            !
CREATE AR11$ AS FIL11 BUFFSIZE 1780                           !
CREATE AR12$ AS FIL12 BUFFSIZE 1780                           !
CREATE AR13$ AS FIL13 BUFFSIZE 1780                           !

CREATE AR14$ AS FIL14 BUFFSIZE 1780                           !
TIREG$  =  "1"                                                !
NITEM$  =  STRING$(14,"0")                                    !

Open B$ AS TLOG BUFFSIZE 5120 NOWRITE NODEL                   ! 
B$ = ""                                                       !
WRITE FORM " C1 C4 C8 C2";#FIL11; "1",CCOSTO$,RESP$,FR$       !
WRITE FORM " C1 C4 C8 C2";#FIL12; "1",CCOSTO$,RESP$,FR$       !
!Write FORM " C1 C4 C8 C2";#FIL13; "1",CCOSTO$,RESP$,FR$       !

WRITE FORM " C1 C4 C8 C2";#FIL14; "1",CCOSTO$,RESP$,FR$       !
                                                              !
NXTRCD:                                                       !
    DATOFOR$ = ("00000000000000000000") 
    FORMULA$ = ("00000000")
    DIAGN$ = ("0000")
    MEDICO$ = ("00000000")
    Doc.Crd$ = "0"
    Gr.Ind.Pago% = 0
    !EDUCA$ = "00000000000"
DIM    BB.AR.KEYTV$(100),   BB.AR.PRECIO(100),               \!
       BB.AR.COMPR(100),    BB.AR.FINAL(100),                \!
       BB.AR.AGENC(100),    NETVEN$(100),    CODART$(700),   \!
       UNIVTA$(700),        MTOVTA$(700),   NITCLI$(700),    \!
       DEPART$(700),        SGNVTA$(700)
       
PTRART = 0
Q    = 1                                                      !
FT   = 0                                                      !
FTV  = 0                                                      !
FLAG = 0                                                      !
MTOTRAN = 0                                                   !
TIPPAG$="11"                                                  !
BB.FLAG = 0                                                   !
IF SILIQ% = 1 THEN CALL LIQUIVA                               !
READ #TLOG; LINE INAREA$                                      !
YY% = YY% + LEN(INAREA$) + 2                                  !
If YY% > TAMA% THEN CALL ACUDIA
LOCATE 23,01                                                  !
Print " Msg: Offset del Log : ";YY%                                 !
INAREA$ = INAREA$ + ","                                       !
WHILE (Q < LEN(INAREA$))                                      !
  P = MATCH (",",INAREA$,Q)                                   ! Busca Delimit
  IF (P-Q) < 3 THEN BEGIN                                     ! Verif STRING Inv
    Q  =  P+1                                                 ! SETUP Sig STRING
    GOTO AGAIN                                                !
  EndIf                                                       !
  B$ =  Mid$(INAREA$,Q+1,(P-Q)-2)                             ! Quita Comillas
  B$ =  B$+":"                                                ! Adiciona :
  Q  =  P+1                                                   ! SETUP Sig STRING
  A  =  VAL(UNPACK$(LEFT$(B$,1)))                             ! Determina String
  IF A = 0 THEN GoSub S0 : GOTO AGAIN                         !
  IF (A < 0) OR (A > 11 AND A < 99) THEN GOTO AGAIN           !

  IF A  =  1  THEN GoSub S1                                   ! Trans item entry  
  IF A  =  2  THEN GoSub S2                                   ! Trans entry ext
  IF A  =  3  THEN GoSub S3                                   ! Descu
  IF A  =  4  THEN GoSub S4                                   ! Anul Desc
  IF A  =  5  THEN GoSub S5                                   ! Medios de pago
  IF A  =  6  THEN GoSub S6                                   ! Corr medios pago
  IF A  =  9  THEN GoSub S9                                   ! Camb food stamp
  IF A  =  11 THEN GoSub S11                                  ! Entrada datos
  IF A  =  99 THEN GoSub S99                                  ! String 99
AGAIN:                                                        !
WEND                                                          !
IF FT = 1 AND FLAG = 0 THEN BEGIN                             !
  IF VALPAG <> "0000000000" THEN BEGIN                        !
    NREC% = VAL(CONSEC$) + 2                                  !
    CALL ACUMULA                                              !
!   Print "FCTA GR : ";CHEQUE$,NOCUE$
    REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+          \!
              CHEQUE$+CTACTE$+NUT+NTR+                       \! TPV/TRANS
              NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$               !
    IF LEN(REGMED$) = 76 THEN BEGIN                           !
       WRITE FORM "C76";#FIL1,NREC%;REGMED$                   !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !Print TOTCRE$
    EndIf ELSE BEGIN                                          !
       Print "ER ",TIREG$," ",CONSEC$                         !
    EndIf                                                     !
!------------------------------------------------------------------------------
        IF VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        EndIf                                             !
!------------------------------------------------------------------------------

  PORCEN = 0                                                  !
  IF MTOTRAN <> 0 THEN BEGIN                                  !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                   ! 
  EndIf                                                       !

!  Print "#FACTURA: ";CHEQUE$,NOCUE$
    PORCEN = 0                                                !
    IF MTOTRAN <> 0 THEN BEGIN                                !
       PORCEN = VAL(VALPAG) / MTOTRAN * 100                   ! 
    EndIf                                                     !
    IF TIPPAG$ = "43" THEN BEGIN                             \! Cheque PostFechado
       WRITE FORM "C7 C8 C2 C1 C7 C10 C7 C13 C19 C2";        \! Graba Reg Pago
       #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",                   \!
       "2","0000000",VALPAG,"0000000",                       \!
       "             ","0000000000000000000",FR$              !
    EndIf                                                     !
  EndIf                                                       !
EndIf                                                         !
!------------------------------------------------------------------------------
!             Grabamos registros de pagos
!------------------------------------------------------------------------------
FOR I = 0 TO D - 1
   TIPTRA1 = "00"
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=41 THEN TIPTRA1 = "06" 
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=42 THEN TIPTRA1 = "07" 
   IF VAL(LEFT$(UNPACK$(BB.AR.KEYTV$(I)),2))=43 THEN TIPTRA1 = "00" 
   IF TIPTRA1 <> "00" THEN BEGIN
      NOCUE$=STRING$(7,"0")+UNPACK$(RIGHT$(BB.AR.KEYTV$(I),4))
      IF VAL(VALCHQ) < 1 THEN VALCHQ = "000000000000"
        NUMPAG$=BB.AR.COMPR(I)
        FIN$=BB.AR.FINAL(I)
        AGENC$=BB.AR.AGENC(I)
        VALPAG=RIGHT$(STRING$(10,"0")+STR$(BB.AR.PRECIO(I)),9)
        IF BB.AR.PRECIO(I) > 0 THEN BEGIN
           SIGNO$ = "+"
        EndIf ELSE BEGIN
           SIGNO$ = "-"
        EndIf
        IF VAL(VALPAG) > 0 THEN BEGIN
           VALPAG = RIGHT$(VALPAG,9)+"00"
        EndIf ELSE BEGIN
           VALPAG = RIGHT$(VALPAG,9)
        EndIf
        WRITE FORM "C7 C8 C2 C2 C1 C7 C11 C12 C1 C1 C7 C2";#FIL2;      \
              RIGHT$(NOCUE$,7),FECTRA,"00","51","2",COMPRO,VALPAG,     \
              VALCHQ,FIN$,AGENC$,NUMPAG$,FR$   
        WRITE FORM "C7 C2 C8 C2 C2 C1 C7 C11 C12 C1 C1 C7 C2";#FIL4;   \
              RIGHT$(NOCUE$,7),TIPTRA1,FECTRA,"00","51","2",COMPRO,VALPAG,\
              VALCHQ,FIN$,AGENC$,NUMPAG$,FR$   
     EndIf
  NEXT I
  IF FTV=1 THEN BEGIN
     FOR E=1 TO F
        WRITE FORM "C3 C10 C2";#FIL7;VEND$,NETVEN$(E),FR$   
     NEXT E
  EndIf
  DIM      BB.AR.KEYTV$(0),  BB.AR.PRECIO(0),           \!
           BB.AR.COMPR(0),   BB.AR.FINAL(0),            \!
           CODART$(0),	     UNIVTA$(0),    MTOVTA$(0), \!
           NITCLI$(0),	     DEPART$(0),    SGNVTA$(0), \!
           NETVEN$(0)                                    ! Reset de arreglos
  D=0
  F=0
GOTO NXTRCD                                              !
WRITE FORM " C39 ";#FIL11;                              \!
"9999999999999999999999999999999999999999"               !

S0:                                                  !  Header de Transac

  CREDI% = 0
  Gr.Ind.Rem% = 0   
  Gr.Remi% = 0
  DctMotor% = 0
  J = 3                                              !
  VALCHQ="0"                                         !
  GoSub GetUnpk                                      !
  NUT  =  RIGHT$(STRING$(3,"0")+A$,3)                ! N.Terminal
  NT% = VAL(NUT)                                     !
  GoSub GetUnpk                                      !
  NTR =  RIGHT$(STRING$(4,"0")+A$,4)                 ! N.Transacc
  TR% = VAL(NTR)                                     !
  GoSub GetUnpk                                      !
  
  Locate 22,1 : Print "Procesando Trx :"+NUT+ " -- "+NTR
  
  FECTRA  =  Mid$(A$,3,8)                            !
  COMPRO  =  "1"+Mid$(FECTRA,5,2)+NTR                !
  HORATR$ =  Mid$(A$,7,4)                            !
  GoSub GetUnpk                                      !
  TIPTRA  =  RIGHT$(STRING$(1,"0")+A$,1)             ! Tipo Transac
  IF VAL(TIPTRA) = 0 OR VAL(TIPTRA) = 2 THEN BEGIN   ! Trans de Vta
     DIM IVATRA%(9)                                  !
     IF IVAT%(NT%,0) <= 0   THEN IVAT%(NT%,0) = TR%  ! Trans Inicial
     IF IVAT%(NT%,1) <  TR% THEN IVAT%(NT%,1) = TR%  ! Trans Final
     IVAT%(NT%,2) = IVAT%(NT%,2) + 1                 ! Incr No Clientes
     IF VAL(TIPTRA) = 0 THEN BEGIN                   ! Oct. 21 de 2002
        CALL STRING.FISCAL                           ! Ajuste para obtener
        IF STR99% > 0 THEN BEGIN                     ! datos String 99
           PRENUF.ANT$ = PRENUF$                     ! TR Fiscal
           PRENUF$ =  PRETR$ + NUFIS$                !
           IF PRENUF.ANT$="" THEN PRENUF.ANT$=PRENUF$!
        EndIf                                        !
     EndIf                                           !
  EndIf                                              !
  TIPTRA1 =  "00"
  BANCO$  =  "00"
  CTACTE$ =  STRING$(14,"0")
  CEDULA$ =  CTACTE$
  CHEQUE$ =  STRING$(8,"0")
  FACTURA$ = CHEQUE$
  GoSub GetUnpk
  GoSub GetUnpk
  NUMOPE$  =  RIGHT$(STRING$(6,"0")+A$,6)
  GoSub GetUnpk
  GoSub GetUnpk
  GROSPOSS$  =  RIGHT$(STRING$(8,"0")+A$,8)
  GoSub GetUnpk
  GROSNEGS$  =  RIGHT$(STRING$(8,"0")+A$,8)
  MONTOTR$   =  STR$(VAL(GROSPOSS$)-VAL(GROSNEGS$))
  GoSub GetUnpk
  Return


S1:
  Rutina$ = "EN S1"
  J = 3
  PTRART = PTRART + 1
  GoSub GetUnpk
  COPLU$ = A$                                           ! PLU para Liq IVA
  ANCHE$ = STRING$(12,"0")                              !
  CAN% = 0
  IF LEN(A$) = 12 THEN BEGIN                            ! Si PLU 12 Digitos
     ANCHE$ = LEFT$(A$,6)                               ! Prefijo Ancheta
     ANCME$ = Mid$(A$,7,4)                              ! Ancheta o Mercado
     REBAJ$ = RIGHT$(A$,2)                              !
  EndIf                                                 !
  COD$   = RIGHT$("000000" + A$,6)                      ! Cod Item para rebajas
  CODART$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)           ! 
  GoSub GetUnpk                                         ! Toma PrecTotal
  MTOVTA$(PTRART)=RIGHT$(STRING$(10,"0")+A$,10)         !
  BB.PRECIO = VAL(A$)                                   ! Valor del pago
  NETOPO$=RIGHT$(STRING$(10,"0")+A$,10)                 ! Vr Item vendedores
  GoSub GetUnpk                                         !
  If Val(A$) = 935 Then A$ = "18"                       ! Recargas a Papeleria Excluida
  Gdpto$ = A$																						! Dpto de venta
  DEP$=RIGHT$("000"+A$,3)                               ! Dep para rebajas
  MACH$ = "DP" + DEP$                                   !
  DEPI% = MATCH(MACH$,DETRA$,1)                         !
  IF DEPI% = 0 THEN                                    \!
     DETRA$ = DETRA$ + "DP" + DEP$                      !
  DEPART$(PTRART)=RIGHT$(STRING$(3,"0")+A$,3)           !
  UNIVTA$(PTRART)= "000001"                             !
  DEPI% = VAL(DEP$)                                     !
  GoSub GetUnpk                                         ! Toma Flags Fam
  GoSub GetUnpk                                         ! Indic1 del Log
  GoSub GetFlag                                         ! Flags de Indic1
  PESA% = 0                                             !
  IF Mid$(A$,2,1) = "1" THEN CAN%=1  :  PESA%=1         ! Si es Pesable
  GoSub GetUnpk                                         ! Indic2 del Log
  Grind2% = Val(A$)
  GoSub GetFlag                                         ! Flags del Indic2
  SGNVTA$(PTRART) = "+"                                 !
  SGN% = 1                                              !
  VRUNPLU% = 0
  IF Mid$(A$,15,1) = "1" AND                           \! Si hay Extension
     PESA% = 0 THEN BEGIN                               !
     PP = MATCH(",",INAREA$,Q)                          !
     BB$ = Mid$(INAREA$,Q+1,(PP-Q)-2)                   !
     BB$ = BB$ + ":"                                    !
     JJ = 3                                             !
     GoSub GETEXTE                                      ! Grupo Multiprec
     GoSub GETEXTE                                      ! Items en Deal
     GoSub GETEXTE                                      ! Metodo Precio
     GoSub GETEXTE                                      ! Deal Quant
     CONT% = VAL(AA$)
     GoSub GETEXTE                                      ! Precio de Vta
     VRUNPLU% = VAL(AA$)                                !
     GoSub GETEXTE                                      ! Unidades o Peso
     CAN% = VAL(AA$)                                    !
  EndIf                                                 !
  IF Mid$(A$,09,1) = "1" OR                            \! Si Item Cancel
     Mid$(A$,10,1) = "1" OR                            \! or Refund
     Mid$(A$,11,1) = "1" OR                            \! or Cupon Almac
     Mid$(A$,12,1) = "1" THEN BEGIN                     ! or Cupon Provee
     SGN% = -1                                          !
     SGNVTA$(PTRART) = "-"                              !
     SIGNO1$ = "-"
     BB.FLAG = 1:BB.PRECIO=BB.PRECIO * -1               ! Si es anulacion
     NETOPO$ = STR$(VAL(NETOPO$)*-1)                    ! Vendedores
     IF BB.FLAG < 0 THEN SIGNO1$ = "-"
  EndIf                                                 !
  IF DEP$ ="861" THEN BEGIN
      IF BB.PRECIO > 0 THEN SIGNO1$ = "+"
      IF BB.PRECIO < 0 THEN SIGNO1$ = "-"
      VALEDU$ = STR$(BB.PRECIO)
  !   Print DEP$+VALEDU$
  EndIf
  
  IF ANCHE$ = "999999" AND                             \!
     REBAJ$ = "99"     THEN BEGIN                       !
        IF VAL(ANCME$) < 50 THEN DEPI% = 32             !
        IF VAL(ANCME$) > 49 THEN DEPI% = 41             !
  EndIf                                                 !

!------------------------------------------------------------------------------
!                Equivalencias para Valores de DEPI%
!
!      ------------------   ---     --------------------------------------
!        DESCRIPTOR         ddd           O b s e r v a c i o n e s
!      ------------------   ---     --------------------------------------
!      Desc Ch/Subsid    =  001  
!      Retefuente        =  009
!      Descto COOTRAIM   =  028
!      Descto 10% COMFAN =  031                                            
!      Descto Anchetas   =  032     NOTA: Cada uno de estos conceptos debe
!      Descto Fruver     =  033           tener su respectivo PLU Deptal y
!      Descto Vecino Fiel=  038           el 'User Flag 4' = Y
!      Descto Carnes     =  039           
!      Descto Mercados   =  041 
 !     Descto Mercados   =  042          DEC VF MEDICA 
!      Descto Bonos      =  043
!      Redondeo Trans    =  048
!      Descto 8% Carnes  =  049
!      Dcto 3 % cotraim  =  070
!------------------------------------------------------------------------------
  IF IVAD%(DEPI%,6) = 1 THEN BEGIN                      ! Si el Depto esta
        IVAT%(NT%,7) = IVAT%(NT%,7) + BB.PRECIO * (-1)    ! asociado a un
        IVAD%(DEPI%,1) = IVAD%(DEPI%,1)+BB.PRECIO * (-1)  ! DESCUENTO,no Liquida 
        IVAD%(DEPI%,5) = IVAD%(DEPI%,5)+CAN%            ! Incrementa Cant
  EndIf ELSE BEGIN                                      !
     SILIQ% = 1 
     CALL ACU.IVAICO                                    !
  EndIf                                                 !
  !MTOTRAN = MTOTRAN + BB.PRECIO                        ! prueba trn miscel 
GoSub GetUnpk
IF LEFT$(A$,1) = "4" OR LEFT$(A$,1) = "5" THEN BEGIN 
   MTOTRAN = MTOTRAN-BB.PRECIO
   DESC4 = DESC4+BB.PRECIO
EndIf 
Return

S2:
  Rutina$ = "EN S2"
  J = 3
  GoSub GetUnpk                                        ! Grupo Multiprec
  GoSub GetUnpk                                        ! Items en Deal
  GoSub GetUnpk                                        ! Metodo Precio
  GoSub GetUnpk                                        ! Deal Quant
  GoSub GetUnpk                                        ! Precio de Vta
  BB.PRECIO = VAL(A$)                                  !
  GoSub GetUnpk                                        ! Unidades o Peso
  UNIVTA$(PTRART)=RIGHT$(STRING$(6,"0")+A$,6)          !
  MTOTRAN = MTOTRAN + BB.PRECIO * VAL(UNIVTA$(PTRART)) !
Return
  
S3:
  Rutina$ = "EN S3"
  J = 3
  DctMotor% = -1
Return

S4:
  Rutina$ = "EN S4"
  J = 3
  DctMotor% = -1
Return

S5:
  Rutina$ = "EN S5" 
  J = 3

     PORCEN = 0                                               !
     IF MTOTRAN <> 0 THEN BEGIN                               !
        PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                ! 
        POR = PORCEN -100
     EndIf                                                  !
     IF TIPPAG$ = "43" THEN BEGIN                          \! Cheque Postfechado
        WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";   \! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
     EndIf                                                  !
 
  GoSub GetUnpk                                              ! Tip/Vrdad Pg 
  TIPPAG$  =  RIGHT$("00" + A$,2)                            ! Guarda Tip/Var
  IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
  IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                      ! Conv Otr/Pg a CR
  If TIPPAG$ = "12" And Gr.Remi% = 1 THEN TIPPAG$ = "61"    ! Remision Interna 9 Ene 2013
  OLD.PAGO$ = TIPPAG$
  IF TIPPAG$ = "45" THEN Begin 
    OLD.PAGO$ = TIPPAG$
  	TIPPAG$ = "61"                      ! Conv Otr/Pg a CR
  EndIf 
  If Gpago$ <> "" Then \ 
  	 TIPPAG$ = Gpago$ : Gpago$ = ""                          ! Reporta bines de TEF
  TIPPAW$  =  TIPPAG$                                        !
  GoSub GetUnpk                                              ! Valor Pago 
  VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                    ! Guarda Vr Pag
!------------------------------------------------------------------------------
  TI% = VAL(LEFT$(TIPPAG$,1))                                ! Acumula Pagos
  VA% = VAL(RIGHT$(TIPPAG$,1))                               ! para Compr
  PAGO% = VAL(A$)                                            ! Diario de IVA
  TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%                  !
  TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                      !
  TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                    !
  TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                        !
  TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                      !
  TPG%(0,0,0)     = TPG%(0,0,0) + 1                          !
  If OLD.PAGO$ = "45" THEN Begin 
     TIPPAG$ = OLD.PAGO$
  EndIf 
!------------------------------------------------------------------------------
  IF VAL(A$) < 0 THEN BEGIN                                  !
     SIGNO$ = "-"                                            !
      VALPAG  =  RIGHT$(STRING$(10,"0")+STR$(VAL(A$)*-1),9)  !
  EndIf ELSE BEGIN                                           !
     SIGNO$ = "+"                                            !
  EndIf                                                      !
  GoSub GetUnpk                                              ! Vr Comisiones
  GoSub GetUnpk                                              ! No. Cliente
  NRWRK$  =  A$                                              ! Guarda Cliente
  PORCEN = 0                                                 !
  IF MTOTRAN <> 0 THEN BEGIN                                 !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                  ! 
  EndIf                                                      !
  
  PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                    !
  BANCO = 0                                                  !
  BANCO = (100 - VAL(BANCO$))                                !
  BAN$ = RIGHT$("000" + STR$(BANCO),3)                       !
  NIT$ = RIGHT$("00000000000000" + NRWRK$,14)                !
  MEDICO$ = RIGHT$("00000000" + MEDICO$,8)                   !
  FORMULA$ = RIGHT$("00000000" + FORMULA$,8)                 !
  DIAGN$ = RIGHT$("0000" + DIAGN$,4)                         !

  IF TIPPAG$ = "61" OR TIPPAG$ = "60" OR                    \!
     TIPPAG$ = "65" Then BEGIN                               ! Se retira la forma pago 66 de la grabacion
     Doc.Crd$ = factura$
     Nit.Crd$ = Nit$
     Gr.Ind.Pago% = 1

     If Gr.Ind.Pago% = 1 Then Begin
      Call String.Fiscal
      !PREF2$ = "     TR"+"+"+LEFT$(PRETR$,4)+"-"+RIGHT$(NUFIS$,7)
      PREF2$ = "     "+LEFT$(PRETR$,4)+RIGHT$(NUFIS$,11)
      NO.FACTURA$=  AUTO$+PREF2$                             ! 
      FOR INDART = 1 TO PTRART                                 !
         WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C14 C14 C3 C8 C8 C4 C26 C6 C2 C4 C4 C2 ";    \!
         #FIL11;                                               \! 
         "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
         UNIVTA$(INDART),MTOVTA$(INDART),CHEQUE$,CTACTE$,      \!
         Nit.Crd$,BAN1$,MEDICO$,Doc.Crd$,DIAGN$,NO.FACTURA$,CUOMOD$, \!
         "**",NUT,NTR,FR$                                       !
      Next INDART                                              !  
     EndIf 
  EndIf                                                      !


      IF TIPPAG$ = "26" THEN BEGIN
         FOR INDART = 1 TO PTRART                             !
      IF VAL(NIT$) > 99999999 THEN NIT$ = Mid$(NIT$,1,7): NIT$ = \
                                RIGHT$("00000000"+NIT$,8)                     !
         WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C9 C2";#36;"2", CODART$(INDART),\!
                DEPART$(INDART),SGNVTA$(INDART),UNIVTA$(INDART),\!
                MTOVTA$(INDART),RIGHT$(NIT$,8),VALPAG,FR$
         NEXT INDART   
      EndIf

      FOR INDART = 1 TO PTRART                                !
          IF DEPART$(INDART) = "861" THEN BEGIN
              SIGNO1$ = SGNVTA$(INDART)
              VALEDU$ = MTOVTA$(INDART)
         EndIf
      NEXT INDART

  LONG      =  LEN(A$)
  NOCUE$  =  RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14) ! N§ Cuenta 
  POS% = MATCH("##############",NOCUE$,1)                   ! Mira 14 Dig-->
  IF POS% = 0 THEN BEGIN                                    ! Hubo Caract Inv
     NOCUE$=RIGHT$(STRING$(24,"0")+LEFT$(NRWRK$,LONG-1),14) ! N§ Cuenta 
  EndIf
  CONSE   =  CONSE + 1                                      ! Acum Consec
  CONSEC$  =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
  FT      =  1 
  GoSub GRABA
  Return

S6:
  Rutina$ = "EN S6"
  IF FT = 1 THEN BEGIN                                      !
     IF VALPAG <> "000000000" THEN BEGIN                    !
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NRWRK$+FR$         !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
        
           TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
        EndIf ELSE BEGIN                                    !
           Print "ER ",TIREG$," ",CONSEC$                   !
        EndIf                                               !
        !IF VAL(TIPPAG$) >= 60 THEN BEGIN                     !
        	
        IF VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        EndIf                                               !
        PORCEN = 0                                          !
        IF MTOTRAN <> 0 THEN BEGIN                          !
           PORCEN = (VAL(VALPAG) / MTOTRAN) * 100           ! 
        EndIf                                               !
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
        EndIf                                               !
     EndIf ELSE BEGIN                                       !
        CONSE  = CONSE - 1                                  !
        CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)      ! Consecutivo
     EndIf
     J = 3
     FT      =  1                                           !
     GoSub GetUnpk                                          ! Toma Tipo/Var
     TIPPAG$  =  RIGHT$("00" + A$,2)                        ! en 2 Posic
     IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GoSub GetUnpk                                          ! Toma Vr Pago
     VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                ! Guarda Vr Pag
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%              !
     TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                  !
     TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                !
     TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                    !
     TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                  !
     TPG%(0,0,0)     = TPG%(0,0,0) + 1                      !
!------------------------------------------------------------------------------
     SIGNO$ = "-"                                           ! Asigna signo
     GoSub GetUnpk                                          ! Toma Comision
     GoSub GetUnpk                                          ! Toma No Cta
     IF A$ = "" THEN A$ = "0"                               !
     NRWRK$  =  A$                                          ! y lo guarda 
     IF LEFT$(TIPPAG$,1) <> "2" THEN BEGIN                  ! Id cheque 
        VALCHQ=RIGHT$(VALPAG,9)+"00"                        ! coloca cvs
        FECHEQUE  =  "0"                                    !
        BANCO$     =  "00"                                  !
        LONG      =  LEN(A$)                                ! Log del No Cta
     EndIf                                                  !
     NOCUE$ = RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14)! Num de Cuenta 
     CONSE  = CONSE + 1                                     ! Suma Consec
     CONSEC$= RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
  EndIf ELSE BEGIN                                          !
     J       =  3                                           !
     FLAG    =  0                                           !
     GoSub GetUnpk                                          ! Toma Tipo/Var
     TIPPAG$  =  RIGHT$("00" + A$,2)                        ! en 2 Posic
     IF TIPPAG$ = "36" AND CREDI% = 1 THEN TIPPAG$ = "60" : CREDI% = 0             ! Conv Otr/Pg a CR
     IF TIPPAG$ = "36" THEN TIPPAG$ = "61"                  ! Conv Otr/Pg a CR
     TIPPAW$  =  TIPPAG$                                    ! y lo guarda
     GoSub GetUnpk                                          ! Toma Vr Pago
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1) = TPG%(TI%,VA%,1) + PAGO%              !
     TPG%(TI%,VA%,0) = TPG%(TI%,VA%,0) + 1                  !
     TPG%(TI%,0,1)   = TPG%(TI%,0,1) + PAGO%                !
     TPG%(TI%,0,0)   = TPG%(TI%,0,0) + 1                    !
     TPG%(0,0,1)     = TPG%(0,0,1) + PAGO%                  !
     TPG%(0,0,0)     = TPG%(0,0,0) + 1                      !
!------------------------------------------------------------------------------
     VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)                ! Guarda Vr Pag
     SIGNO$ = "-"                                           ! Asigna signo
     GoSub GetUnpk                                          ! Toma Comision
     GoSub GetUnpk                                          ! Toma No Cta
     IF A$ = "" THEN A$ = "0"                               !
     NRWRK$  =  A$                                          ! y lo guarda 
     IF LEFT$(TIPPAG$,1) <> "2" THEN BEGIN                  ! Id cheque 
        VALCHQ=RIGHT$(VALPAG,9)+"00"                        ! coloca cvs
        FECHEQUE  =  "0"                                    !
        BANCO$     =  "00"                                  !
        LONG      =  LEN(A$)                                ! Log del No Cta
     EndIf                                                  !
     NOCUE$ = RIGHT$(STRING$(24,"0")+RIGHT$(NRWRK$,LONG),14)! Num. Cuenta 
     CONSE  = CONSE + 1                                     !
     CONSEC$= RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
     IF TIPPAG$ > "10" THEN BEGIN                           ! Estaba NO=11
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$         !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !Print TOTCRE$
        EndIf ELSE BEGIN                                    !
           Print "ER ",TIREG$," ",CONSEC$                   !
        EndIf                                               !
!------------------------------------------------------------------------------
        !IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
        	
        IF VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        EndIf                                               !
        
!------------------------------------------------------------------------------
!Print "#FACTURA: ";CHEQUE$,NOCUE$
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
               #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",         \!
               "2","0000000",VALPAG,SIGNO$,"0000000",      \!
               "             ","0000000000000000000",FR$    !
        EndIf                                               !
    EndIf                                                   !
  EndIf                                                     !
  PORCEN = 0                                                !
  IF MTOTRAN <> 0 THEN BEGIN                                !
     PORCEN = (VAL(VALPAG) / MTOTRAN) * 100                 ! 
  EndIf                                                     !
  PORCEN$= RIGHT$("000" + STR$(PORCEN),3)                   !
  BANCO = 0                                                 !
  BANCO = (100 - VAL(BANCO$))                               !
  BAN$ = RIGHT$("000" + STR$(BANCO),3)                      !
  NIT$ = RIGHT$("00000000000000" + NRWRK$,14)               !
  MEDICO$ = RIGHT$("00000000" + MEDICO$,8)                  !
  FORMULA$ = RIGHT$("00000000" + FORMULA$,8)                !
  DIAGN$ = RIGHT$("0000" + DIAGN$,4)                        !

  IF TIPPAG$ = "61" OR TIPPAG$ = "64"                      \!
  OR TIPPAG$ = "65"     THEN BEGIN                          ! se retira la forma de pago 66 de la grabacion
     Doc.Crd$ = formula$
     Nit.Crd$ = Nit$
     Gr.Ind.Pago% = 2

 If Gr.Ind.Pago% = 2 Then Begin
    Call String.Fiscal
    !PREF2$ = "     TR"+"+"+LEFT$(PRETR$,4)+"-"+RIGHT$(NUFIS$,7)
    PREF2$ = "     "+LEFT$(PRETR$,4)+RIGHT$(NUFIS$,11)
    NO.FACTURA$=  AUTO$+PREF2$                             ! 
    FOR INDART = 1 TO PTRART                                 !
        WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C14 C14 C3 C8 C8 C4 C26 C6 C2 C4 C4 C2 ";    \!
        #FIL11;                                               \! 
        "2",CODART$(INDART),DEPART$(INDART),SGNVTA$(INDART),  \!
        UNIVTA$(INDART),MTOVTA$(INDART),CHEQUE$,CTACTE$,      \!
        Nit.Crd$,BAN1$,MEDICO$,Doc.Crd$,DIAGN$,NO.FACTURA$,CUOMOD$, \!
        "**",NUT,NTR,FR$                                       !
    Next INDART
 EndIf
  EndIf                                                      !
     
      IF TIPPAG$ = "26" THEN BEGIN

        IF VAL(NIT$) > 99999999 THEN NIT$ = Mid$(NIT$,1,7):NIT$= \
                                RIGHT$("00000000"+NIT$,8)                     !
         FOR INDART = 1 TO PTRART                             !
         WRITE FORM "C1 C6 C3 C1 C6 C10 C8 C9 C2";#36;"2", CODART$(INDART),\!
                DEPART$(INDART),SGNVTA$(INDART),UNIVTA$(INDART),\!
                MTOVTA$(INDART),RIGHT$(NIT$,8),VALPAG,FR$
         NEXT INDART   
      EndIf


    FOR INDART = 1 TO PTRART                                !
         IF DEPART$(INDART) = "861" THEN BEGIN
         SIGNO1$ = SGNVTA$(INDART) 
         VALEDU$ = MTOVTA$(INDART) 
        EndIf
    NEXT INDART                                              !  
  Return                                                    !
Print "NUT " + NUT + " TRAN  " + NTR 

S9:
  Rutina$ = "EN S9"
  J = 3                                                     !
  IF FT = 1 AND FLAG = 0 THEN BEGIN                         !
     IF VALPAG <> "000000000" THEN BEGIN                    !
        NREC% = VAL(CONSEC$) + 2                            !
        CALL ACUMULA                                        !
        REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+    \!
                  CHEQUE$+CTACTE$+NUT+NTR+                 \! TPV/TRANS
                  NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$         !
        IF LEN(REGMED$) = 76 THEN BEGIN                     !
           WRITE FORM "C76";#FIL1,NREC%;REGMED$             !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
   ! Print TOTCRE$
        EndIf ELSE BEGIN                                    !
           Print "ER ",TIREG$," ",CONSEC$                   !
        EndIf                                               !
!------------------------------------------------------------------------------
        !IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
        	
        If VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     
           CALL GRABA.OTROIVA                               !
        EndIf                                               !
!------------------------------------------------------------------------------
        IF TIPPAG$ = "43" THEN BEGIN                       \! Cheque Postfechado
           WRITE FORM "C7 C8 C2 C1 C7 C9 C1 C7 C13 C19 C2";\! Graba Reg Pago
              #FIL3;RIGHT$(NOCUE$,7),FECTRA,"01",          \!
              "2","0000000",VALPAG,SIGNO$,"0000000",       \!
              "             ","0000000000000000000",FR$     !
        EndIf                                               !
     EndIf ELSE BEGIN                                       !
        CONSE  =  CONSE - 1                                 !
        CONSEC$ =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)     ! Consecutivo
     EndIf
  EndIf
  FLAG    =  1
  GoSub GetUnpk                                             ! Tipo y Vrdad
  GoSub GetUnpk                                             ! Cambio Efectivo
  VALPAG  =  RIGHT$(STRING$(10,"0")+A$,9)
  TIPPAG$ =  "11"
  BANCO$  =  "00"
  !NOCUE$  =  STRING$(14,"0")
  CHG     =  VAL(VALPAG)*-1
  VALPAG  =  STR$(CHG)
!------------------------------------------------------------------------------
     TI% = VAL(LEFT$(TIPPAG$,1))                            ! Acumula Pagos
     VA% = VAL(RIGHT$(TIPPAG$,1))                           ! para Compr
     PAGO% = VAL(A$) * (-1)                                 ! Diario de IVA
     TPG%(TI%,VA%,1)   = TPG%(TI%,VA%,1) + PAGO%            !
!    TPG%(TI%,VA%,0)   = TPG%(TI%,VA%,0) + 1                !
     TPG%(TI%,0,1)     = TPG%(TI%,0,1) + PAGO%              !
!     TPG%(TI%,0,0)     = TPG%(TI%,0,0) + 1                  !
     TPG%(0,0,1)       = TPG%(0,0,1) + PAGO%                !
!     TPG%(0,0,0)       = TPG%(0,0,0) + 1                    !
!------------------------------------------------------------------------------
  IF VAL(VALPAG) < 0 THEN BEGIN
     SIGNO$ = "-"
     VALPAG  =  RIGHT$(STRING$(10,"0")+STR$(CHG*-1),9)
  EndIf ELSE BEGIN
     SIGNO$ = "+"
  EndIf
  CONSE   =  CONSE+1
  CONSEC$ =  RIGHT$(STRING$(6,"0")+STR$(CONSE),6)            ! Consecutivo
      NREC% = VAL(CONSEC$) + 2                               !
      CALL ACUMULA                                           !
      REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+       \!
                CHEQUE$+CTACTE$+NUT+NTR+                    \! TPV/TRANS
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$            !
      IF LEN(REGMED$) = 76 THEN BEGIN                        !
         WRITE FORM "C76";#FIL1,NREC%;REGMED$                !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
   ! Print TOTCRE$
      EndIf ELSE BEGIN                                       !
         Print "ER ",TIREG$," ",CONSEC$                      !
      EndIf                                                  !
!------------------------------------------------------------------------------
        !IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
        	
        IF VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     !        	
           CALL GRABA.OTROIVA                               !
        EndIf                                               !
!------------------------------------------------------------------------------
  Return                                                     !


S11:
  Rutina$ = "EN S11"
  J = 3                                                      !
  NUMCS$ = RIGHT$("0000000000" + STR$(NSU%) + "+", 10)
  GoSub GetUnpk                                              !
     IF A$ = "?" OR A$ = ">>" OR                  \
        A$ = "<<" OR A$ = "=="  THEN Return

         CTACTE$  = RIGHT$(STRING$(20,"0")+A$,14)            !
         BENE$    = RIGHT$(STRING$(2,"0")+A$,2) 
         APROB$   = CTACTE$    
         EDUCA$ = "00000000000"

         IF LEFT$(A$,8) ="20040510" THEN BEGIN 
            EDUCA$   = RIGHT$(STRING$(19,"0")+A$,19)    
            EDUCA$   = RIGHT$(EDUCA$,11)
            VALEDU$=RIGHT$(STRING$(10,"0")+VALEDU$,10)
         !   WRITE FORM"C10 C1 C11 C8 C3 C4 C2 C2";#37;VALEDU$,\
         !              SIGNO1$,EDUCA$,RESP$,NUT,NTR,TIPPAG$,FR$
         EndIf

!--- Se reemplaza >> por 99
!         Print "valida en s11: "+Mid$(CTACTE$,12,2)
!         Input A$
         
         IF Mid$(CTACTE$,12,2) = "99" THEN BEGIN             
            BENE$   = "00"
            CTACTE$ = Mid$(CTACTE$,2,10)                        
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            CTACTE$ = CTACTE$+BENE$                           
         EndIf ELSE BEGIN
           IF Mid$(CTACTE$,11,2) = "99" THEN BEGIN             
            CTACTE$ = Mid$(CTACTE$,1,10)                        
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            CTACTE$ = CTACTE$+BENE$                           
           EndIf ELSE BEGIN
            CTACTE$ = RIGHT$("000000000000"+(CTACTE$),12)     
            BENE$   = "00"
            CTACTE$ = CTACTE$+BENE$                           
           EndIf
         EndIf
         CEDULA$  = CTACTE$
         DATOFOR$ = A$                                       !
        IF LEN(DATOFOR$) = 21  THEN BEGIN 
          DATOFOR$ = RIGHT$(STRING$(21,"0")+A$,21)           !
          DIAGN$   = Mid$(DATOFOR$,17,5)
          MEDICO$  = Mid$(DATOFOR$,1,8)
          FORMULA$ = Mid$(DATOFOR$,9,8)
        EndIf
        IF LEN(DATOFOR$) = 8  AND \
           VAL(DATOFOR$) = 14052004 THEN BEGIN 
           !Print DATOFOR$
           CREDI% = 1 
        EndIf
! Print DATOFOR$
  GoSub GetUnpk
        CHEQUE$  = RIGHT$("00000000000000000000"+(A$),8)
        IF Mid$(CHEQUE$,7,2) = "--" THEN BEGIN
            CHEQUE$ = "00000000"  
         EndIf 

  GoSub GetUnpk
        BANCO1$   = RIGHT$(STRING$(20,"0")+A$,9)
        GASOCC$   = RIGHT$(BANCO1$,8)
        BAN1$     = LEFT$(BANCO1$,3)
        CUOMOD$   = RIGHT$(BANCO1$,6)
        BANCO$    = RIGHT$(STRING$(20,"0")+BANCO1$,2)
  GoSub GetUnpk
        DATO4$   = RIGHT$(STRING$(20,"0")+A$,20)
        FACTURA$ = Mid$(DATO4$,1,7)
        FACTURA$ = RIGHT$(STRING$(8,"0")+FACTURA$,8)
        IF FACTURA$ > "00000000" THEN CHEQUE$  = FACTURA$   
        
  GoSub GetUnpk
        AUTO$    = RIGHT$(STRING$(20,"0")+A$,10)
        DATO5$   = LEFT$(STRING$(20,"0")+A$,6)
!        Print AUTO$
        IF VAL(AUTO$) > 111111 THEN BEGIN                       ! Busca 
           AUTO$ = RIGHT$("000000"+(AUTO$),6) EndIf ELSE BEGIN  ! autorizacion
           AUTO$ = "000000"                                     ! en credito
        EndIf                                                   !y lo guarda
  GoSub GetUnpk
        DATO6$   = RIGHT$(STRING$(20,"0")+A$,6)
        DAT6    = VAL(DATO6$)
        IF DATO6$ <> "000000" THEN BEGIN
           REINT$ = CTACTE$   :   ZETA$  = CHEQUE$
           NCAJA$ = BANCO$    :   DOCUM$ = DATO4$
           ACTIV$ = DATO5$    :   NCLIE$ = DATO6$
        EndIf
  Return

!------------------------------------------------------  
!    El resto del p rrafo S11 sirve para manejar 
!    pagos para Sistema de Cr. Ver User Exits para BG
!                                                Dic/94
!------------------------------------------------------  

  IF LEFT$(A$,1)<>"<" THEN BEGIN
    IF  VAL(A$) > 150 AND VAL(A$) < 200 THEN BEGIN
       VEND$=RIGHT$(A$,3)
       FTV=1
    EndIf
  EndIf
  CUOT$ = Mid$(A$,2,2)
  CPR$ = Mid$(A$,4,7)
  AUT$=RIGHT$(A$,4)
  IF TIPPAG$ = "41" THEN BEGIN
     IF CUOT$ > "24" THEN CUOT$ = "24"
  EndIf
  IF TIPPAG$ = "42" THEN BEGIN
     IF CUOT$ > "36" THEN CUOT$ = "36"
  EndIf
  IF VAL(VALPAG) > 0 THEN BEGIN
     VALPAG = RIGHT$(VALPAG,9) + "00"
  EndIf ELSE BEGIN
     VALPAG = RIGHT$(VALPAG,9) 
  EndIf
  IF TIPPAW$ = "41" OR TIPPAW$ = "42" THEN BEGIN\
     WRITE FORM "C7 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";            \
           #OUTFIL5;RIGHT$(NOCUE$,7),FECTRA,"00","01","2",CPR$,  \
           VALPAG,"0"+AUT$,CUOT$,"              ",FR$
     WRITE FORM "C7 C2 C8 C2 C2 C1 C7 C11 C5 C2 C14 C2";         \
           #OUTFIL6;RIGHT$(NOCUE$,7),TIPPAG$,FECTRA,"00","01","2",\
           CPR$,VALPAG,"0"+AUT$,CUOT$,"              ",FR$
     TIPPAW$ = "00"
EndIf ELSE BEGIN
  BB.KEYTV$=PACK$(Mid$(A$,5,26))
  BB.FINAL=Mid$(A$,32,1)
  BB.AGENC=Mid$(A$,33,1)
  BB.COMPR=RIGHT$(A$,7)
  IF BB.FLAG THEN BEGIN                                  ! Si fue anulacion
     BB.FLAG = 0
     BB.FLAG2 = 0
     FOR I = 0 TO D - 1                                  ! Busca ANUL o DEV
         IF BB.KEYTV$ = BB.AR.KEYTV$(I) THEN            \!
            BB.FLAG2=-1   :    I=D-1                     ! Anulacion
     NEXT I
     IF BB.FLAG2 THEN BEGIN                              ! Si hay PLU en Transac
        IF D > 1 THEN BEGIN                              ! Arregla elementos
           FOR I = 0 TO D - 1                            ! del arreglo para
              IF BB.KEYTV$ = BB.AR.KEYTV$(I) THEN BEGIN  ! sacar Pago anulado
                 FOR J = I TO D - 1                      !
                     BB.AR.KEYTV$(J) = BB.AR.KEYTV$(J+1) !
                     BB.AR.PRECIO(J) = BB.AR.PRECIO(J+1) !
                     BB.AR.FINAL(J) = BB.AR.FINAL(J+1)   !
                     BB.AR.AGENC(J) = BB.AR.AGENC(J+1)   !
                     BB.AR.COMPR(J) = BB.AR.COMPR(J+1)   !
                 NEXT J
                 D = D - 1
                 Return
              EndIf
            NEXT I
         EndIf ELSE BEGIN
            D = 0
         EndIf
     EndIf ELSE BEGIN 
        BB.AR.KEYTV$(D) = BB.KEYTV$
        BB.AR.PRECIO(D) = BB.PRECIO
        BB.AR.FINAL(D) = BB.FINAL
        BB.AR.AGENC(D) = BB.AGENC
        BB.AR.COMPR(D) = BB.COMPR
        D = D + 1
     EndIf
  EndIf ELSE BEGIN                       
     BB.AR.KEYTV$(D) = BB.KEYTV$
     BB.AR.PRECIO(D) = BB.PRECIO
     BB.AR.FINAL(D)  = BB.FINAL
     BB.AR.AGENC(D)  = BB.AGENC
     BB.AR.COMPR(D)  = BB.COMPR
     D = D + 1
EndIf
EndIf
  Return
!-------------------------------------------------------------

GRABA:
  Rutina$ = "EN GRABA"
  IF FT = 1 AND FLAG = 0 THEN BEGIN                         !
    IF VALPAG <> "000000000" THEN BEGIN                     !
      NREC% = VAL(CONSEC$) + 2                              !
       CALL ACUMULA                                         !
       REGMED$ = TIREG$+CONSEC$+HORATR$+VALPAG+SIGNO$+     \!
                CHEQUE$+CTACTE$+NUT+NTR+                   \! TPV/TRANS
                NUMOPE$+TIPPAG$+BANCO$+NOCUE$+FR$           !
      IF LEN(REGMED$) = 76 THEN BEGIN                       !
         WRITE FORM "C76";#FIL1,NREC%;REGMED$               !
    TOTCRE$ = RIGHT$("000000000"+STR$(MTOTRAN),9)
    !Print TOTCRE$
      EndIf ELSE BEGIN                                      !
         Print "ER ",TIREG$," ",CONSEC$                     !
      EndIf                                                 !
!------------------------------------------------------------------------------
        !IF VAL(TIPPAG$) > = 60 THEN BEGIN                     !
        	
        If VAL(TIPPAG$) > = 60 And TIPPAG$ <> "66" THEN BEGIN                     !
           CALL GRABA.OTROIVA                               !
        EndIf                                               !
!------------------------------------------------------------------------------
   EndIf ELSE BEGIN                                          !
     CONSE  = CONSE - 1                                      ! 
     CONSEC$ = RIGHT$(STRING$(6,"0")+STR$(CONSE),6)          ! Consecutivo
   EndIf                                                     !
 EndIf                                                       ! 
FT = 0                                                       !
Return                                                       !  

S99:                                                         !
  Rutina$ = "EN S99"
  J = 3                                                      ! Posic 3
  GoSub GetUnpk                                              ! Desempaq sig UPD
!--- Add GR-OVS 28 ENE 2010  
  If A$ = "97"  And DctMotor% = -1 Then Begin                ! Si es descuento motor promociones v2
  	 GoSub GetUnpk                                           ! Codigo del producto
  	 CodPlu$ = A$
  	 GoSub GetUnpk                                           ! Valor  del descuento
  	 BB.PRECIO = Val(A$) * -1
  	 CAN% = 1
  	 GoSub GetUnpk                                           ! Codigo promocion
  	 GoSub GetUnpk                                           ! descriptor producto o flag iva nuevo Str
  	 Terror% = -1
  	 Rtap% = Val(a$) 
  	 If Terror% = 0 Then Begin
  	   GoSub GetUnpk                                         ! flag Iva
  	 EndIf 
  	 GoSub GetUnpk                                           ! cod depto
  	 DEPI% = Val(A$)
  	 GoSub GetUnpk                                           ! estructura comercial
  	 GoSub GetUnpk                                           ! Signo de la operacion
  	 If A$ = "01" Then BB.PRECIO = BB.PRECIO * -1            !
  	 Call ACU.IVAICO
  	 A$ = "97"
  EndIf 																										 ! Fin dscto motor promociones v2
  !--- Add GR-OVS 28 ENE 2010  
  If A$ = "45"  Then Begin                                   ! Si es una remision de mercancia
    Gr.Ind.Rem% = -1
  EndIf 

  If A$ = "20110114"  Then Begin                             ! Si es una remision de mercancia 
     GoSub GetUnpk                                           ! Numero de remision
     GoSub GetUnpk                                           ! Tipo de remision
     Gr.Remi% = Val(a$)                                      ! 
  EndIf 


!-- Add OVS 12 Ene 2006  
  If A$ = "20060104" Then Begin                              ! Movimiento Domicilios 
     Asc.Vta.Domic% = Asc.Vta.Domic% + Val(MontoTr$)         ! Monto Ventas Domicilios
  EndIf 
  IF A$ = "21" AND                                          \! Str Cierre/Trans
     Mid$(B$,5,2) = "TR" THEN BEGIN                          ! Ident TransVta
        HAY99% = 1                                           !
        VTA%=0 : IV1%=0 : IV2%=0 : IV3%=0 : IV4%=0           !
                 IV5%=0 : IV6%=0 : IV7%=0 : IV8%=0           !
        IF LEN(B$) < 25 THEN BEGIN                           !
           J = 11                                            !
           GoSub GetUnpk                                     ! Desempaq sig UPD
           TRAF$ = A$                                        !
           LOCATE 20,55
           Print Mid$(B$,5,7);" ";STR$(VAL(TRAF$))           !
           PREF$ = Mid$(B$,7,4)                              !
           ALTE$ = PACK$(RIGHT$("00000000"+TRAF$,8))         !
           TRFI% = VAL(RIGHT$(TRAF$,8))                      !
           IF TRF%(NT%,0) <= 0 THEN TRF%(NT%,0)=TRFI%        ! Trans Inicial
           IF TRF%(NT%,1) < TRFI% THEN TRF%(NT%,1)=TRFI%     ! Trans Final
           TRF%(NT%,2) = TRF%(NT%,2) + 1                     ! Incr No Clientes
           VTA% = VRVTA%                                     !
           Return                                            ! Si String sin Vrs
        EndIf                                                !
        J = 12                                               !
        GoSub GetUnpk                                        ! Desempaq sig UPD
        TRAF$ = RIGHT$(STRING$(15,"0")+A$,15)                !
        TRFI% = VAL(RIGHT$(TRAF$,8))                         !
        LOCATE 20,55                                         !
        !Print Mid$(B$,5,7);" ";TRAF$                         !
          !INPUT "  ";ZZZ$                                    !
        SIGN$ = Mid$(B$,7,1)                                 !
        PREF$ = Mid$(B$,8,4)
        TRAF2$  = RIGHT$(STRING$(7," ")+TRAF$,07)
        PREF2$ = "     TR"+"+"+LEFT$(PREF$,2)+ "00"+"-"+TRAF2$
        ! Print PREF2$                                       ! Mod OVS 06-Abr-2016
        ALTE$ = PACK$(RIGHT$("0"+LEFT$(TRAF$,7),8))          !
        IF TRF%(NT%,0) <= 0 THEN TRF%(NT%,0)=TRFI%           ! Trans Inicial
        IF TRF%(NT%,1) < TRFI% THEN TRF%(NT%,1)=TRFI%        ! Trans Final
        TRF%(NT%,2) = TRF%(NT%,2) + 1                        ! Incr No Clientes
!        FISCO$ = PREF$+RIGHT$(STRING$(11,"0")+TRAF$,11)
        GoSub GetUnpk                                        ! Vr Neto Vta
        VTA% = VAL(A$)                                       !
        IF SIGN$ = "-" THEN VTA% = -1 * VTA%                 !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 1
        IV1% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 2
        IV2% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 3
        IV3% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 4
        IV4% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 5
        IV5% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 6
        IV6% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 7
        IV7% = VAL(A$)                                       !
        GoSub GetUnpk                                        ! Vr Iva Tarifa 8
        IV8% = VAL(A$)                                       !
        IF IV1% > VTA% THEN IV1% = 0                         !
        IF IV2% > VTA% THEN IV2% = 0                         !
        IF IV3% > VTA% THEN IV3% = 0                         !
        IF IV4% > VTA% THEN IV4% = 0                         !
        IF IV5% > VTA% THEN IV5% = 0                         !
        IF IV6% > VTA% THEN IV6% = 0                         !
        IF IV7% > VTA% THEN IV7% = 0                         !
        IF IV8% > VTA% THEN IV8% = 0                         !

  EndIf                                                      !
  Gpago$ = Valida.String.Tef ( B$ )													 ! Valida String de TEF
Return                                                       !

! ---------------------------------------------------------------------
GetUnpk:                                               !
  K  = MATCH(":",B$,J)                                 ! Busca sep de Campo
  If (K-J) > 0 Then \
   A$ = UNPACK$(Mid$(B$,J,K-J)) Else A$ = ""           ! y lo desempaqueta
   A$ = TRANSLATE$(A$,"=","-")                          ! Signo Negativo
!  If K > 0 Then \
!     H$ = Mid$(B$,J,K-J)                               !
  J  = K + 1                                           ! Marca el inicio de sigui-
Return                                                 ! ente campo

! ---------------------------------------------------------------------

GetFlag:                                               !
 FLAG = VAL(A$)                                        ! Conv FLAG a INT
                                                       ! Inicia STRING con
                                                       ! FLAGS individuales
  IF (FLAG AND 00000001H) THEN                        \!
      A$ = "1" ELSE A$ = "0"                           !
  FOR I = 1 TO 15                                      ! 
    FLAG = SHIFT(FLAG,1)                               ! Inicializa sig BIT
    IF (FLAG AND 00000001H) THEN                      \!
        A$ = "1" + A$                                 \!
    ELSE                                              \!
        A$ = "0" + A$                                  !
  NEXT I                                               !
  Return                                               !

! ---------------------------------------------------------------------
GETEXTE:
  KK = MATCH(":",BB$,JJ)            ! Busca separador de Campo
  If (KK-JJ > 0) Then Begin
    AA$ = UNPACK$(Mid$(BB$,JJ,KK-JJ)) ! y lo desempaqueta
    HH$ = Mid$(BB$,JJ,KK-JJ)          !
  EndIf Else Begin
  	AA$ = ""
  	HH$ = ""
  EndIf
  JJ=KK+1                           ! Marca el inicio de sigui-
  Return                            ! ente campo


ERRTN:
  EOF$ = ERR
  Terror% = 0
  IF ERR = "IH" THEN Resume
  If ERR = "SN" THEN GoTo FINAL
  IF ERRF% = 22 THEN RESUME                            !
  IF ERRF% = 24 THEN BEGIN
     CLEARS
     Print "    ERROR..!   NO EXISTE EL ARCHIVO DE CONTROL DE ALMACEN..!"
     Print "               Consulte con el Depto de Sistemas."
     Print "                 Crear  C:\ADXALMA.DAT"
     STOP
  EndIf
  IF ERR <> "EF" THEN  BEGIN                          ! Si NO es EOF
    LOCATE 22,1
    Print " ERR="+ERR+" ARCH="+STR$(ERRF%)+" ERRN="+Str$(ERRN)+" "+Rutina$+" "+str$(PTRART) + " :"+NUT+" - "+NTR
    !INPUT "  ";ZZZ$                                   !

    RESUME NXTRCD:                                    !
  EndIf ELSE BEGIN                                   \! Si es EOF TSLOG
    
   CABECER$ = STRING$(109,"0")
   WRITE FORM "C1 C12 6C10 C1 C2";                   \! Graba Reg Control
    #FIL1,1;"4",RESP$,TOTEF$,TOTCH$,TOTOT$,          \! de Medios de Pago
                   TOTRM$,TOTTC$,TOTCR$,"*",FR$       !
   WRITE FORM "C1 C12 6C10 C1 C109 C2";               \! Graba Reg Control
    #FIL9,1;"4",RESP$,TOTEF$,TOTCH$,TOTOT$,          \! de Medios de Pago
                    TOTRM$,TOTTC$,TOTCR$,"*",CABECER$,FR$       !
    WRITE FORM "C1 C12 6C10 C1 C2";                  \! Graba Reg Control
    #FIL1,2;"5",RESP$,NUMEF$,NUMCH$,NUMOT$,          \! de Cantidades
                   NUMRM$,NUMTC$,NUMCR$,"*",FR$       !
    WRITE FORM "C1 C12 6C10 C1 C109 C2";             \! Graba Reg Control
    #FIL9,2;"5",RESP$,NUMEF$,NUMCH$,NUMOT$,          \! de Cantidades
                  NUMRM$,NUMTC$,NUMCR$,"*",CABECER$,FR$       !
    CALL ACUDIA                                       ! 
    HORA$=TIME$                                       !
    HORA$=LEFT$(HORA$,2)+":"+                        \!
          Mid$(HORA$,3,2)+":"+RIGHT$(HORA$,2)         !
    LPRINTER
   IF CUADRE = 1 THEN BEGIN                           !
      WRITE FORM "C80 C2";#FIL5;" ",FR$               !
      WRITE FORM "C10 C70 C2";#FIL5;                 \!
            "  "," PLANILLA DE CUADRE OK.! "+HORA$    !
    EndIf ELSE BEGIN
      WRITE FORM "C80 C2";#FIL5;" ",FR$               !
      WRITE FORM "C10 C35 PIC($$#######,###) C2";    \!
            #FIL5;" ",                               \!
            " ** ERROR EN PLANILLA POR VALOR DE:",   \!
            ESPERADO% + TOTDES% - TT.CONTEO,FR$
    EndIf                                            !
    WRITE FORM "C31 C2";#FIL5;S6NCN$,FR$             ! Salto;1/6's;NO-Conds
    WRITE FORM "C31 C2";#FIL5;"** Fin **",FR$        ! Reset de Impres
  EndIf                                              !
  STOP                                               !
! ---------------------------------------------------------------------
FINAL:                                               !
  Print                                              !
  Print "EL PROGRAMA FINALIZA..."                    !
  STOP

FILERR:
  Print " Error de Aplicacion "
  Print " ERR = "+ERR+" ERRN = "+Str$(ERRN)+NUT$+"-"+NTR$
  Print "EL PROGRAMA FINALIZA..."
  Stop
! ---------------------------------------------------------------------
!           ***     FIN  DEL  PROGRAMA  PRINCIPAL      ***
! ---------------------------------------------------------------------
