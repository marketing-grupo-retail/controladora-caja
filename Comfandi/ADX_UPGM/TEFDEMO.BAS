!**************************************************
!Empresa       : Grupo Retail Ltda                *
!Programa      : GRMTEFII.BAS                     *
!Desarrollo    : Unidad Retail                    *
!Lenguaje      : Basic 4690 IBM                   * 
!Observaciones : Modulo Librerias para TEF II     *
!**************************************************
!* Modificaciones:

%ENVIRON C 

String    GLOBAL UE.LISTA.TARIFAS$
Integer*4 Global UE.TOTALS(1), UE.TABLAIVA(1),TVRTPAG% ! Total Impuestos
Real      Global UE.BASE.428%, UE.BASE.IVA%
Integer*4 GLOBAL UE.IVA2%, UE.IVA%, compra%

%INCLUDE EAMTSWKG.J86

Function USR.TARJ.IVA(UE.VALUE4%) Public
Integer*4 UE.VALUE4%, USR.TARJ.IVA 
Real TMP.428%, TMP.TOTAL%, tmp.calculo%
Integer*1 Tmp.Pos%
Integer*4 tmp.vlr.pago%
    UE.BASE.428% = 0
    UE.BASE.IVA% = 0
    USR.TARJ.IVA = 0
    For I% = 1 TO Len(UE.LISTA.TARIFAS$)
      tmp.pos% = VAL(MID$(UE.LISTA.TARIFAS$,I%,1))
      UE.BASE.428% = UE.BASE.428% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.),0,0)
      UE.BASE.IVA% = UE.BASE.IVA% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.)*FLOAT(UE.TABLAIVA(tmp.pos%))/100.,0,0)
    Next I%
    TVRTPAG% = UE.VALUE4%                                 ! Toma valor capturado de pago
    
    If TVRTPAG% > TS.TOTALS(0,0,0) THEN \
       Tmp.Vlr.Pago% = TS.TOTALS(0,0,0) ELSE  \
       Tmp.Vlr.Pago% = TVRTPAG%
    
    tmp.428%     = ue.base.428% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    If TMP.TOTAL% > 0 Then \
     TMP.428% = TMP.428% / TMP.TOTAL%  Else              \!
     TMP.428% = 0
    UE.BASE.428% = (TMP.428% * 1000)                      ! Retorna Valor del Calculo

    tmp.428%     = ue.base.iva% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    
    If TMP.TOTAL% > 0 Then \
     TMP.428% = TMP.428% / TMP.TOTAL%  Else              \!
     TMP.428% = 0

    UE.BASE.IVA% = (TMP.428% * 1000)
    USR.TARJ.IVA = UE.BASE.IVA%        
FEND 

Function USR.TARJ.IVA2( PAGO ) Public
Integer*4 USR.TARJ.IVA2
Integer*4 PAGO
Real TMP.428%, TMP.TOTAL%, tmp.calculo%
Integer*1 Tmp.Pos%
Integer*4 tmp.vlr.pago%
    UE.BASE.428% = 0
    UE.BASE.IVA% = 0
    USR.TARJ.IVA2 = 0
    For I% = 1 TO Len(UE.LISTA.TARIFAS$)
      tmp.pos% = VAL(MID$(UE.LISTA.TARIFAS$,I%,1))
      UE.BASE.428% = UE.BASE.428% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.),0,0)
      UE.BASE.IVA% = UE.BASE.IVA% + ROUND(FLOAT(UE.TOTALS(tmp.pos%))/(1.+FLOAT(UE.TABLAIVA(tmp.pos%))/100.)*FLOAT(UE.TABLAIVA(tmp.pos%))/100.,0,0)
    Next I%
    TVRTPAG% = PAGO                                      ! Toma valor capturado de pago
    
    If TVRTPAG% > TS.TOTALS(0,0,0) THEN \
       Tmp.Vlr.Pago% = TS.TOTALS(0,0,0) ELSE  \
       Tmp.Vlr.Pago% = TVRTPAG%
    
    tmp.428%     = ue.base.428% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    If TMP.TOTAL% > 0 Then \
     TMP.428% = TMP.428% / TMP.TOTAL%  Else              \!
     TMP.428% = 0
    UE.BASE.428% = (TMP.428% * 1000)                      ! Retorna Valor del Calculo
    USR.TARJ.IVA2 = UE.BASE.428%
    
    tmp.428%     = ue.base.iva% / 1000                    ! Se disminuyen los valores  en 1000
    tmp.total%   = ts.totals(0,0,0) / 1000                ! para evitar problemas en el calculo
    tmp.calculo% = Tmp.Vlr.Pago% / 1000                   ! con datos de overflow
    TMP.428% = tmp.calculo% * tmp.428%                    !
    If TMP.TOTAL% > 0 Then \
     TMP.428% = TMP.428% / TMP.TOTAL%  Else              \!
     TMP.428% = 0

    UE.BASE.IVA% = (TMP.428% * 1000)


FEND

On Error GoTo Paila
Clears
Input "Compra ";Compra%
Dim ts.totals(10,10,10)
TS.TOTALS(0,0,0) = Compra%
UE.IVA% = USR.TARJ.IVA(Compra%)
UE.IVA2% = USR.TARJ.IVA2(Compra%)
Print "RTA UE.IVA%  ",ue.iva%
Print "RTA UE.IVA2% ",ue.iva2%
Stop 

Paila:
  Print "puto error ",err
  Stop 