!**************************************************
!Empresa       : ASIC S.A.        Unidad  Retail  *
!Programa      : FISMECDR.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   *
!Fecha         : Julio 14 del 2.004               *
!Observaciones : Modulo Numeracion Fiscal         *
!                Legislacion Tributaria Ecuador   *
!**************************************************
! Mod01Dic2005
! Se desarrolla la funcion Validacion.Fecha.Fiscal
! para controlar que la fecha de la terminal este
! entre el rango de operacion fiscal definido por
! el SRI. Desarrollado el 1 de Dic de 2005 - OVS
! --------------------------------------------------
! Mod06May2008
! Sea adiciona la rutina de impresion de cuadre de
! cajeros segun requerimiento de Santa Maria
! Desarrollador por OVS 6 Mayo 2008
!---------------------------------------------------
! Mod07May2008
! Se controla que si dentro de una transaccion se captura
! un cliente que sea empresa estatal no permita la captura
! y si es al inicio de una trx todos los productos que se
! registren van Sin IVA, ajustado 7 Mayo 2008 OVS
!------------------------------------------------------
! Mod07Jul2008
! Se adiciona control de numeracion en la NVRAM de las terminales
! Desarrollado el 7 de julio de 2008 OVS
!------------------------------------------------------
! Mod11Ago2008
! Se cambia el proceso de actualizacion del archivo de procesos
! para la generacion de notas credito para que estas sean
! procesadas por la checkout support.
! desarrollado por Grupo Retail Ltda - OVS
!--------------------------------------------------------
! Mod13Ago2008
! Se ajusta la grabacion de datos de la nota credito
! controlando que no se almacene basura de otras operaciones
! en la transaccion.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 20Oct2015
! Ajuste modulo fiscal para manejo de facturacion Electronica
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------
! Mod 19Oct2016
! Se adiciona en la grabacion del UD 20050822 el nombre del 
! cliente segun requerimiento de santa maria.
! Se retira la validación de la fecha final de rango de 
! autorizacion expedida por el SRI segun requerimiento de 
! santa maria.
! Se adiciona definicion de formas de pago, segun requerimientos
! del SRI.
! Desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------


%Environ T                 ! Ambiente de terminal

Integer*1 Global Asc.Ind.Open.Fiscal%, \
                 ASIC.LLAVE%,          \
                 UE.ER.RETURN,         \
                 f.rprint,             \
                 carga.inicial%,       \
                 INDEX.FACT,           \
                 lines%,               \
                 flag.paso,            \
                 fRef%,                \
                 injecta.trx.tot,      \
                 INYECTA.UNITARIO,     \
                 rh.busca.mov,         \
                 NC.IN%,               \
                 rh.flag.usuario%,     \
                 rh.user%,             \
                 rh.dIf%,              \
                 rh.ex.feria%,         \
                 rh.flag.nc.total%,    \
                 rh.ya.imprimi%,       \
                 rh.sus,               \
                 Gr.Fiscal.ImpDoc%,    \
                 Gr.Fiscal.TipDoc%,    \
                 Gr.Graba.Dato%,       \
                 enable.Imptot%

Integer*2 Global Busca1,          \
                 Busca2,          \
                 Busca3,          \
                 Busca4,          \
                 Busca5,          \
                 Busca6,          \
                 Busca7,          \
                 Busca8,          \
                 Busca9,          \
                 Busca10,         \
                 Busca11,         \
                 Busca12,         \
                 Busca13,         \
                 Busca14,         \
                 Busca15,         \
                 Busca16,         \
                 Busca17,         \
                 Busca21, Busca22, Busca23, Busca24, Busca25, Busca26, \
                 ART.POR.FACTURA, \
                 rh.tmp%

Integer*4 Global PAG.ACTUAL%, \
                 COUNTER,     \
                 START.C,     \
                 rh.start,    \
                 rh.precioxq, \
                 Gr.DsctoNcPtg%, \
                 Gr.DsctoNc%, \
                 Gr.QtyAnl%

String Global tslinjec$(1),         \
              tslinjecd$(2),        \
              reference$,           \
              BCK.FLAG$,            \
              NC.SALDO$(1),         \
              X.Nc.Total$,          \
              Nc.Fecha.Trx$,        \
              Nc.Num.Trx$,          \
              Nc.Num.Term$,         \
              PAG.INDEX$,           \
              f.RH.NV.FACT$,        \
              rh.graba.total$,      \
              num.factura$,         \
              rh.data.cab$,         \
              rh.data.cli$,         \
              rh.data.art$(1),      \
              RH.CANASTAS.CoN.IVA$, \
              RH.CANASTAS.SIN.IVA$, \
              ERR.MSG.67$,          \
              rh.secuencia$,        \
              rh.cod.in$,           \
              Gr.Fiscal.ClvSri$,    \
              Gr.Fiscal.Serie$,     \
              Gr.Fiscal.DocOri$,    \
              Asc.Fis.NroSecun$,    \
              Asc.Fis.NroRucCl$,    \
              X.FECOR$

Integer*4 ACU.TRX.TOF.NVTA, \
          ACU.TRX.TOF.FAC
Integer*4 Global Gr.Fiscal.DscxMayor%																! Dscto aplicado por Mayor
Integer*4 Global Gr.Fiscal.MayorItm%																! Dscto aplicado por Mayor Item
Integer*4 Global Gr.PromOr.Total%																		! Dscto total Or 
Integer*1 Global Gr.Fiscal.NcDatos% 
String    Global RH.TMP.STACK.DI$(2)
Integer*4 Global RH.TOTALES(1), GR.TRX.QTY, acum.grupos%(1), Gr.PromOr.DsctItm%
Integer*2 global num.grupos%
REAL      GLOBAL Gr.Mkp.PrUnit%, Gr.Fiscal.Precio%
Integer*1 Global Gr.Recaudos.Intrx%
String    Global Pagostrx$(2), PagosSri$(2), PagoRes$(2), Gr.Tef2.Cuota$, Gr.Pvm.Cuota$, EP.CUOTAS.QTY$
Integer*1 Global Gr.RsmTkt.Ind%, Gr.Fiscal.Tktnf%
Integer*1 Global Gr.NoTkt.Suspend%
String    Global Gr.Loyalty.Trk2$                                           ! Track II Tarjeta
String    Global GR.TRACKII$, Gr.Loyalty.Card$                              ! Datos Track II banda magnetica
Integer*1 Global Gr.Loyalty.Capt%, Gr.Loyalty.Proc%, Gr.Fiscal.ConItm%

%INCLUDE EAMTSWKG.J86          ! working storage
%INCLUDE EAMTRANS.j86          ! Variables procesos de transaccion
%INCLUDE EAMTERMS.J86          ! Variables procesos terminal
%INCLUDE EAMTOPTS.J86          ! Variables Terminal Options
%INCLUDE EAMTSEVA.J86          ! Variables Cliente Frecuente
%INCLUDE ASCITEMR.J86          ! Variables ITEMR
%INCLUDE EAMITEMR.J86          ! Variables ITEMR
%INCLUDE EXIRTSVA.J86          ! Variables Extension Itemr
%INCLUDE IVASTSVA.011
%INCLUDE FISTSUVA.011          ! Variables desarrollo
%INCLUDE EAMTSXHC.J86
%INCLUDE EAMASMRT.J86

!Rhiscom Gestion de Correlativos.
%Include RHGETCOR.J86
%Include IMPRESIO.J86
%Include RHDIfACT.J86
%include rhimpser.j86
%include rhimpter.j86

Sub TSTCEC01 EXTERNAL          ! tender cashing
End Sub                        !

Sub TSIEEC01 EXTERNAL          ! item sales
End Sub                        !

Sub  IRRFEC.READ02 (SESS.NO, LOCK.IT) External
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  INTEGER*2 LOCK.IT    !* Lock the record ?                                  *!
End Sub  

Sub IRRFEC.READ01 (KEY$, SESS.NO, DATA$, LOCK.IT) External
  STRING    KEY$       !* KEY of the record to be read.                      *!
  INTEGER*2 SESS.NO    !* SESSION number to be used.                         *!
  STRING    DATA$      !* String to hold the data read from file.            *!
  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                               *!
End Sub 

Sub IRRFEC.SPLIT1 (RECORD$) External
  STRING    RECORD$    !* The record (including the key) read from file      *!
End Sub 

Sub  injecta.datos External
End Sub 

Sub  Version.DIfact External
End Sub  

Function U.EXPLUSADI External
End Function

Sub  TSPREC01 External                          ! Rutina de impresion SMA
End Sub 

Sub  TSPREC03 External                          ! Rutima de MSG SMA
End Sub 

Sub  TSCSEC03 External
End Sub 

Sub  TSHIECET External                          !  Tono de alerta
End Sub 

Sub  TSCSEC10 External                          ! Termination procedure
End Sub 

Sub  CUADRE.CAJERO External
End Sub 

Sub CONSULTA.PUNTOS.LOYAL	External
End Sub 

Sub  TRADUCE.ERROR(ERRValUE%, ERRValUE$) External! Traduccion Error Aplicacion
  Integer*4 ERRValUE%
  String ERRValUE$
End Sub 

Function Elimina.Blancos(rh.dato$) External
String rh.dato$, elimina.blancos
End Function 

Function RH.CANASTAS.Sub.LEER(RH.CANASTAS.Sub.LEER.ID$) External
Integer*1 RH.CANASTAS.Sub.LEER
End Function

Function PRT4610.FONT.AND.FORMAT$(ASTR$, ValUE) External
INTEGER*1 ValUE
STRING ASTR$, PRT4610.FONT.AND.FORMAT$
End Function

Function FORMAT.AMOUNT(AMT1) External ! Formateo de Valores
Integer*1 FORMAT.AMOUNT
Integer*4 AMT1
End Function

Function  VISORES4690(U.D.VISOR%, U.D.LINEA1$, U.D.LINEA2$, U.D.TIEMPO%, U.D.POSICION$) External ! Msg Display
String U.d.Linea1$, U.d.Linea2$, U.d.Posicion$! Variables de
String U.D.Linea1$, U.D.Linea2$
Integer*2 U.d.visor%, U.d.tiempo%   ! trabajo
End Function
!--- Fin manejo de visores

Function Asic.Datos$(Umsg1$, Umsg2$) External     													! Captura De Datos
String Asic.Datos$, Umsg1$, Umsg2$
End Function

Function ASIC.DATOS.BASE$(UMSG1$, UMSG2$) External													! Captura de datos
String UMSG1$, UMSG2$, A.DAT$, AB$, AQ$        															! Definicion de variables
String A1$, A2$, A3$, A4$, A5$, A6$, A7$, A8$, A9$
String ASIC.DATOS.BASE$, ADATA$
End Function

Function U.Imprime(UE.String$, UE.STATION) External                 				! Rutina de Impresion
String UE.String$                                                   				! Variables temporales
Integer*2 UE.STATION
End Function
!--- Rutina de Impresion

Function U.CORTACR External                                         				! Cortal papel impresora pos
End Function
!--- Fin U.CortarCr

Function Grabacion.Cadena.Usuario(X.clave$, X.datos$) External
String X.clave$, X.datos$
End Function
!--- Fin Grabacion String Usuario

Function Linea.Detalle(X.Ses%)   External                  									! Imprime Detalle De La Operacion
String Linea.Detalle, X.Oper$, X.A$
Integer*1 X.Ses%
End Function

Sub Resumen.Items.Fiscal External																						! Resumen de venta items
End Sub 																																		!

Function Imp.serial External
Integer*1 imp.serial
Integer*2 i
Integer*4 get.i4
String    bit$,     \
          buffer$,  \
          nombre.error$

End Function 

Function GR.GETUNPK(X.B$, X.J%)
Integer*2 X.J%, X.K%
String    X.B$, GR.GETUNPK

X.K% = MATCH(":", X.B$, X.J%) ! SEARCH FOR FIELD SEPERATOR
If (X.K%-X.J%) > 0 Then  \
  GR.GETUNPK = UNPACK$(MID$(X.B$, X.J%, X.K%-X.J%)) Else \ ! UNPACK FIELD
  GR.GETUNPK = ""  
Asc.Fis.Fisj% = X.K% + 1
End Function

Function GR.GETUNPK4(X.B$, X.J%)
Integer*2 X.J%, X.K%
String    X.B$, GR.GETUNPK4

X.K% = MATCH(":", X.B$, X.J%) ! SEARCH FOR FIELD SEPERATOR
If (X.K%-X.J%) > 0 Then  \
  GR.GETUNPK4 = (MID$(X.B$, X.J%, X.K%-X.J%)) Else \ ! UNPACK FIELD
  GR.GETUNPK4 = ""  
Asc.Fis.Fisj% = X.K% + 1
End Function


Function RH.GETUNPK(X.B$, X.J%)
Integer*2 X.J%, X.K%
String    X.B$, RH.GETUNPK

X.K% = MATCH(":", X.B$, X.J%) ! SEARCH FOR FIELD SEPERATOR
RH.GETUNPK = UNPACK$(MID$(X.B$, X.J%, X.K%-X.J%)) ! UNPACK FIELD
rh.tmp% = X.K% + 1
End Function

Function RH.Getunpk2(X.B$, X.J%)
Integer*2 X.J%, \
          X.K%
String X.B$,        \
       RH.Getunpk2

X.K%        = Match(";", X.B$, X.J%)                                   ! Busca Separador De Campo
RH.Getunpk2 = Unpack$(Mid$(X.B$, X.J%, X.K% - X.J%))                   ! Desempaqueta Campo
rh.tmp%     = X.K% + 1
End Function

Sub  VISOR.AND.BORRAR(X.MSG$) External
  String X.MSG$
End Sub 

Function Format.Fecha(X.Fecha$) External
String X.Fecha$, Format.Fecha
End Function
 
!-- Lectura Datos en los Hard Totals
Sub  FIS.LEC16
FIS.F16$     = "3C5 C6 C2 2C4 C29"
TS.ER.RETURN = -1
Read Form FIS.F16$; #FIS.HTO%, FIS.H16%; Asc.Fis.HTNtaVta$,  \ ! Nota de Venta
                                         Asc.Fis.HTFacVta$,  \ ! Factura de Venta
                                         Asc.Fis.HTNtaCred$, \ ! Nota de Credito
                                         Asc.Fis.FecTrx$,    \ ! Fecha de Transaccion
                                         Asc.Fis.Almacen2$,  \ ! Almacen de la numeracion
                                         ACU.TRX.TOF.NVTA$,  \ = 0
                                         ACU.TRX.TOF.FAC$,    \  = 0
                                         Asc.Fis.Relleno$
ACU.TRX.TOF.NVTA = Val(ACU.TRX.TOF.NVTA$)
ACU.TRX.TOF.FAC = Val(ACU.TRX.TOF.FAC$)
If TS.ER.RETURN <> -1 Then Begin                    ! Error de Lectura Hard Totals
   Asc.Fis.HTNtaVta$  = Pack$("00")                 ! Nota de Venta
   Asc.Fis.HTFacVta$  = Pack$("00")                 ! Factura de Venta
   Asc.Fis.HTNtaCred$ = Pack$("00")                 ! Nota de Credito
   Asc.Fis.Almacen2$  = Pack$("0000")               ! Almacen de la numeracion
EndIf
End Sub 

Sub  IMP.FIS.LEC16
TS.TEMP1$ = Left$("--- DATOS HARD TOTAL TERMINAL ---"+String$(38," "),38)
Call U.IMPRIME(TS.TEMP1$, 6100H)
TS.TEMP1$ = Left$("FACT VENTA :"+UnPack$(Asc.Fis.HTFacVta$)+String$(38," "),38)
Call U.IMPRIME(TS.TEMP1$, 6100H)
TS.TEMP1$ = Left$("NOTA CRED. :"+UnPack$(Asc.Fis.HTNtaCred$)+String$(38," "),38)
Call U.IMPRIME(TS.TEMP1$, 6100H)
TS.TEMP1$ = Left$("ALMACEN    :"+UnPack$(Asc.Fis.Almacen2$)+String$(38," "),38)
Call U.IMPRIME(TS.TEMP1$, 6100H)

End Sub 

!--- Procedimiento de grabacion en los Hard Totals
Sub  FIS.WRIF
String FECHA.TRX$

FECHA.TRX$ = "20" + DATE$ + TIME$                  ! Agrega Fecha y Hora
FECHA.TRX$ = Pack$(FECHA.TRX$)                     ! Empaqueta Fecha

Asc.Fis.HTNtaVta$  = Pack$(Right$("0000000000" + Str$(Asc.Fis.NotaNum%), 10))
Asc.Fis.HTFacVta$  = Pack$(Right$("0000000000" + str$(Asc.Fis.FacNum%), 10))
Asc.Fis.HTNtaCred$ = Pack$(Right$("0000000000" + Str$(Asc.Fis.NotaCrd%), 10))
Asc.Fis.Almacen2$  = Asc.Fis.Almacen1$
ACU.TRX.TOF.NVTA$  = Right$("0000" + Str$(ACU.TRX.TOF.NVTA), 4)
ACU.TRX.TOF.FAC$   = Right$("0000" + Str$(ACU.TRX.TOF.FAC), 4)
Asc.Fis.Relleno$   = String$(29, "0")

FIS.F16$ = "3C5 C6 C2 2C4 C29"
Write Form FIS.F16$; #FIS.HTO%, FIS.H16%; Asc.Fis.HTNtaVta$,  \ ! Nota de Venta
                                          Asc.Fis.HTFacVta$,  \ ! Factura de Venta
                                          Asc.Fis.HTNtaCred$, \ ! Nota de Credito
                                          Asc.Fis.FecTrx$,    \ ! Fecha de Transaccion
                                          Asc.Fis.Almacen2$,  \ ! Almacen de la numeracion
                                          ACU.TRX.TOF.NVTA$,   \ = 0
                                          ACU.TRX.TOF.FAC$,    \  = 0
                                          Asc.Fis.Relleno$
End Sub 

Sub  Reset.HARD.TOTALS
String FECHA.TRX$

FECHA.TRX$ = "20" + DATE$ + TIME$                  ! Agrega Fecha y Hora
FECHA.TRX$ = Pack$(FECHA.TRX$)                     ! Empaqueta Fecha

Asc.Fis.HTNtaVta$  = Pack$("0000000000")
Asc.Fis.HTFacVta$  = Pack$("0000000000")
Asc.Fis.HTNtaCred$ = Pack$("0000000000")
Asc.Fis.Almacen2$  = Asc.Fis.Almacen1$
ACU.TRX.TOF.NVTA$  = "0000"
ACU.TRX.TOF.FAC$   = "0000"
Asc.Fis.Relleno$   = String$(29, "0")

FIS.F16$ = "3C5 C6 C2 2C4 C29"
Write Form FIS.F16$; #FIS.HTO%, FIS.H16%; Asc.Fis.HTNtaVta$,  \ ! Nota de Venta
                                          Asc.Fis.HTFacVta$,  \ ! Factura de Venta
                                          Asc.Fis.HTNtaCred$, \ ! Nota de Credito
                                          Asc.Fis.FecTrx$,    \ ! Fecha de Transaccion
                                          Asc.Fis.Almacen2$,  \ ! Almacen de la numeracion
                                          ACU.TRX.TOF.NVTA$,  \ = 0
                                          ACU.TRX.TOF.FAC$,   \  = 0
                                          Asc.Fis.Relleno$
End Sub 

Function Valida.Ruc.Pagare(X.Nit$)
String X.Nit$,            \
       X.Lec$,            \
       Valida.Ruc.Pagare
String Gdata1$, Gdata2$

TS.ER.RETURN = -1
Open "R::$UC" KEYED RECL 97 AS Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   ValIDA.RUC.PAGARE = "CLIENTE NO DEFINIDO"
   Exit  Function
EndIf
X.Nit$ = RIGHT$("              " + X.Nit$, 14)
X.LEC$ = "C14 C30 C30 C10 C1 C2 C1 C10"
READ FORM X.LEC$; #Asc.Fis.FileClte% KEY X.Nit$; Gdata1$,    \ ! IdentIficacion del cliente
                                                 Gdata2$,    \ ! Nombre del cliente
                                                 Gdata1$,    \ ! Direccion del Cliente
                                                 Gdata1$,    \ ! Telefono del cliente
                                                 Gdata1$,    \ ! Si es empresa estatal
                                                 Gdata1$,    \ ! codigo del sri
                                                 Gdata1$       ! Uso futuro

Close Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   ValIDA.RUC.PAGARE = "CLIENTE NO DEFINIDO"
   Exit  Function
EndIf
ValIDA.RUC.PAGARE = Gdata2$
End Function
!--- Fin Validacion del RUC

Sub  BUSCA.PAGARE                 ! Impresion del pagare x vta institucional
String VENTA.TIQUETE, \
       X.LISTA$,      \
       X.TMP$,        \
       X.Vlr$

Integer*4 PRIC%, \
          TOT4%, \
          X.I%

For X.I% = 1 TO Asc.Fis.Slend%                           ! FOR ALL StringS
   H$ = READ.SL.STR$(X.I%)                               !' GET String
   If LEN(H$) > 5 Then \                                 !' ASSURE GOOD String
      If ASC(H$) = 5 Then Begin                          !' ITEM ENTRY String
         Asc.Fis.Fisj% = 3
         X.TMP$ = GR.GETUNPK(H$, Asc.Fis.Fisj%)        ! Tomo informacion del item vendido
         If X.TMP$ = Asc.Fis.Pagare$ Then Begin          ! Impresion del pagare
            X.Vlr$ = GR.GETUNPK(H$, Asc.Fis.Fisj%)     ! Valor del pago
            X.Tmp$ = GR.GETUNPK(H$, Asc.Fis.Fisj%)     ! data
            X.Tmp$ = GR.GETUNPK(H$, Asc.Fis.Fisj%)     ! Numero de cuenta
            Call U.IMPRIME(Asc.Fis.Linea1$, 4100H)
            Call U.IMPRIME(Asc.Fis.Linea2$, 4100H)
            Call U.IMPRIME(Asc.Fis.Linea3$, 4100H)
            Call U.IMPRIME(Asc.Fis.Linea4$, 4100H)
            Call U.IMPRIME(Asc.Fis.Linea5$, 4100H)
            Call U.IMPRIME(Asc.Fis.Linea6$, 4100H)
            TS.TEMP3$ = CHR$(1BH) + CHR$(21H) + CHR$(11H)
            Call U.Imprime("***********  P A G A R E  ***********", 4100H)
            Call U.IMPRIME(Asc.Fis.Documento$, 4100H)
            TS.TEMP3$ = "Cliente :" + X.Tmp$
            TS.TEMP3$ = Left$(TS.TEMP3$ + String$(40, " "), 40)
            Call U.IMPRIME(TS.TEMP3$, 4100H)
            TS.TEMP3$ = ValIDA.RUC.PAGARE(X.TMP$)
            TS.TEMP3$ = Left$(TS.TEMP3$ + String$(40, " "), 40)
            Call U.IMPRIME(TS.TEMP3$, 4300H)
            TS.TEMP3$ =  Asc.Fis.LineaB$
            TS.TEMP3$ = Left$(TS.TEMP3$ + String$(40, " "), 40)
            Call U.IMPRIME(TS.TEMP3$, 4100H)
            Call Format.AMOUNT(Val(X.VLR$))
            TS.TEMP3$ = "LA CANTIDAD DE USD " + TS.TEMP1$
            TS.TEMP3$ = Left$(TS.TEMP3$ + String$(40, " "), 40)
            Call U.IMPRIME(TS.TEMP3$, 4100H)
            TS.TEMP3$ = "POR CONCEPTO DE COMPRAS A CREDITO   "
            TS.TEMP3$ = Left$(TS.TEMP3$ + String$(40, " "), 40)
            Call U.IMPRIME(TS.TEMP3$, 4100H)
            TS.TEMP3$ = LINEA.DETALLE(38)
            Call U.IMPRIME(TS.TEMP3$, 4300H)
            Call U.IMPRIME("__________________________________", 4100H)
            Call U.IMPRIME("         FIRMA CLIENTE            ", 4100H)
            Call U.IMPRIME("", 4500H)
            Call U.CORTACR
            Call U.IMPRIME(TO.HEADERLINE1$, 4100H)
            Call U.IMPRIME(TO.HEADERLINE2$, 4100H)
         EndIf
      EndIf                                          !' ITEM ENTRY String
Next X.I%                                            !' NEXT String
End Sub 

Sub  Lectura.Consecutivo.Fiscal
String X.Key$,    \
       X.SERIAL$, \
       X.LEC$,    \
       X.I$,      \
       X.F$,      \
       X.Tipo$,   \
       X.RESOL$,  \
       X.REG$,    \
       fis.flag$

Integer*1 t.doc,  \
          rh.nok

Integer*4 X.INI%,  \
          X.FIN%,  \
          X.ACT%,  \
          X.Nota%, \
          X.NC%

   X.KEY$ = RIGHT$("0000" + TS.TERMINAL$, 4)
   X.KEY$ = PACK$(X.KEY$)
   X.LEC$ = "C2 2C15 C12 C3 C4 2I4 C6 C3 3I4"
   TS.ER.RETURN = -1
   Read FORM X.LEC$; #Asc.Fis.Sesion% KEY X.KEY$; X.KEY$,           \ !' Numero de la terminal
                                                  X.TIPO$,          \ !' Tipo de Maquina
                                                  X.SERIAL$,        \ !' Serial de la terminal
                                                  X.RESOL$,         \ !' Numero resolucion
                                                  X.FEC.EMI$,       \ !' Fecha Emision Autorizacion
                                                  X.PREF$,          \ !' Prefijo de facturacion
                                                  X.INI%,           \ !' Numero inicial de Facturacion
                                                  X.FIN%,           \ !' Rango final de Facturacion
                                                  X.AUT$,           \ !' Numero resolucion de la DIAN
                                                  X.FEC$,           \ !' Fecha Autorizacion
                                                  Asc.Fis.FacNum%,  \ !' Numero Actual de Facturacion
                                                  Asc.Fis.NotaNum%, \ !' Numero de Nota de Venta
                                                  Asc.Fis.NotaCrd%    !' Numero de Nota de Credito

If TS.ER.RETURN <> -1 Then Begin
  Asc.Fis.FacNum%  = 0  !' Numero Actual de Facturacion
  Asc.Fis.NotaNum% = 0  !' Numero de Nota de Venta
  Asc.Fis.NotaCrd% = 0  !' Numero de Nota de Credito
EndIf

End Sub 


Function Validacion.Fecha.Fiscal(X.Fec$)     ! Validacion fecha autorizacion fiscal
String X.Fec$

Integer*1 Validacion.Fecha.Fiscal

Integer*4 X.A%, \
          X.B%

Validacion.Fecha.Fiscal = -1							! Se elimina control Add 19Oct2016
Exit Function 

X.A% = Val(Left$(X.FEC$, 2))             ! Toma año autorizacion
X.B% = Val(Left$(DATE$, 2))              ! Toma año movimiento
If X.A% = X.B% Then Begin                ! Si el año coincide
   X.A% = Val(Mid$(X.FEC$, 3, 2))        ! Toma mes autorizacion
   X.B% = Val(Mid$(DATE$, 3, 2))         ! Toma mes movimiento
   If X.B% <= X.A% Then Begin            ! Si el mes es menor o igual al autorizado
      Validacion.Fecha.Fiscal = -1       ! Continua ejecucion programa
      Exit  Function                      ! Sale de la funcion
   EndIf \
   Else Begin                            ! Si mes mayor o igual
      Validacion.Fecha.Fiscal = 0        ! Para ejecucion programa
      Exit  Function                      ! Sale de la funcion
   EndIf
EndIf

If X.A% > X.B% Then Begin                ! Si el año fiscal mayor al registro
   If (X.A% - X.B%) > 1 Then Begin       ! DIferencia de mas de un año
      Validacion.Fecha.Fiscal = 0        ! Para ejecucion programa
      Exit  Function                      ! Sale de la funcion
   EndIf
   Validacion.Fecha.Fiscal = -1          ! Para ejecucion programa
   Exit  Function                         ! Sale de la funcion
EndIf \
Else Begin
   Validacion.Fecha.Fiscal = 0           ! Para ejecucion programa
   Exit  Function                         ! Sale de la funcion
EndIf
End Function
!--- Fin verIficacion fecha fiscal

Sub  Carga.Parametros.Terminal
String X.Key$,    \
       X.SERIAL$, \
       X.AUT$,    \
       X.FEC$,    \
       X.LEC$,    \
       X.MES$(1), \
       X.I$,      \
       X.F$,      \
       X.Tipo$,   \
       X.RESOL$,  \
       X.FEC2$,   \
       X.REG$

integer*1 X.Valida%

Integer*4 X.INI%,  \
          X.FIN%,  \
          X.ACT%,  \
          X.Nota%, \
          X.NC%

Dim X.MES$(12)
X.MES$(01) = "Ene"
X.MES$(02) = "Feb"
X.MES$(03) = "Mzo"
X.MES$(04) = "Abr"
X.MES$(05) = "May"
X.MES$(06) = "Jun"
X.MES$(07) = "Jul"
X.MES$(08) = "Ago"
X.MES$(09) = "Sep"
X.MES$(10) = "Oct"
X.MES$(11) = "Nov"
X.MES$(12) = "Dic"
TS.ER.RETURN = -1


If Asc.Ind.Open.Fiscal% = 0 Then Begin
   Open "R::$AMNMFIS" KEYED RECL 80 AS Asc.Fis.Sesion%    ! Abre control numeacion fiscal
   Asc.Ind.Open.Fiscal% = -1
EndIf

If TS.ER.RETURN = -1 Then Begin                   ! Si apertura OK
   X.KEY$ = RIGHT$("0000" + TS.TERMINAL$, 4)
   X.KEY$ = PACK$(X.KEY$)
   X.Lec$ = "C2 2C15 C12 C3 C4 2I4 C6 C3 3I4"
   Read FORM X.LEC$; #Asc.Fis.Sesion% KEY X.KEY$; X.KEY$,     \ !' Numero de la terminal
                                                  X.TIPO$,    \ !' Tipo de Maquina
                                                  X.SERIAL$,  \ !' Serial de la terminal
                                                  X.RESOL$,   \ !' Numero resolucion
                                                  X.FEC.EMI$, \ !' Fecha Emision Autorizacion
                                                  X.PREF$,    \ !' Prefijo de facturacion
                                                  X.INI%,     \ !' Numero inicial de Facturacion
                                                  X.FIN%,     \ !' Numero final de Facturacion
                                                  X.AUT$,     \ !' Numero resolucion
                                                  X.FEC$,     \ !' Fecha Autorizacion
                                                  X.ACT%,     \ !' Numero Actual de Facturacion
                                                  X.NOTA%,    \ !' Numero actual de Nota de Venta
                                                  X.NC%         !' Numero actual de notas credito
 
   If TS.ER.RETURN = -1 Then Begin
      RH.FEC$    = X.FEC$
      X.FEC$     = Unpack$(X.FEC$)
      X.FEC.emi$ = Unpack$(X.FEC.EMI$)
      X.FEC2$    = X.FEC$
      RH.AUT$    = X.AUT$
      X.AUT$     = Unpack$(X.AUT$)
      X.AUT$     = STR$(Val(X.AUT$))
      X.AUT$     = Right$("0000000000" + X.AUT$, 10)
      X.I$       = Right$("0000000000" + Str$(X.INI%), 10)
      X.F$       = Right$("0000000000" + Str$(X.FIN%), 10)
      X.FEC$     = MID$(X.Fec$, 5, 2) + "/" + MID$(X.Fec$, 3, 2) + "/" + "20" + MID$(X.Fec$, 1, 2)
      X.FEC.EMI$ = MID$(X.Fec.Emi$, 5, 2) + "/" + MID$(X.Fec.EMi$, 3, 2) + "/" + "20" + MID$(X.Fec.EMi$, 1, 2)

      Gr.Fiscal.Serie$ = Right$("000"+X.PREF$,3) + Right$("000"+TS.TERMINAL$,3)
      X.PREF$          = Right$("000"+X.PREF$,3)      
      Asc.FIS.TMPPREF$ = X.PREF$
      Asc.Fis.Resol$   = "RESOLUCION NRO : "     + X.RESOL$      !' Numero resolucion
      Asc.Fis.Autor$   = "AUTORIZACION S.R.I : " + X.AUT$        !' Numero Autorizacion
      Asc.Fis.NtaVta$  = "RECIBO DE CAJA NRO.: " + X.Pref$       !' Nota de venta
      Asc.Fis.NtaCrd$  = "   NOTA DE CREDITO : " + X.Pref$       !' Nota de credito
      Asc.Fis.Factura$ = "FACTURA NRO. : "       + X.Pref$       !' Factura de venta
      Asc.Fis.Fecemi$  = "VALIDO HASTA EL    : " + X.Fec.EMI$    !' Fecha de emision
      Asc.Fis.Fecven$  = "VALIDO HASTA EL    : " + X.Fec$        !' Fecha de vencimiento
      Asc.Fis.Serial$  = X.TIPO$ + " SERIE:"     + X.SERIAL$     !' Numero serial de la terminal
      Asc.Fis.Serial$  = Left$(Asc.Fis.Serial$ + String$(38, " "), 38)
      Asc.Fis.Autor$   = Left$(Asc.Fis.Autor$  + String$(38, " "), 38)
      Asc.Fis.FecVen$  = Left$(Asc.Fis.Fecven$ + String$(38, " "), 38)
      Asc.Fis.FacNum%  = X.ACT%
      Asc.Fis.NotaNum% = X.NOTA%
      Asc.Fis.NotaCrd% = X.NC%
      TS.TEMP1$ = Left$("****** DATOS NUMERACION FISCAL ******"+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 4100H)
      TS.TEMP1$ = Left$("NUMERO DE TERMINAL :" + TS.TERMINAL$ + String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 6100H)
      TS.TEMP1$ = Left$(Asc.Fis.Autor$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 4100H)
      TS.TEMP1$ = Left$(Asc.Fis.FecVen$+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 4100H)
      TS.TEMP1$ = Left$("FACTURA INICIAL :" +  Str$(Asc.Fis.FacNum%)+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 6100H)
      TS.TEMP1$ = Left$("NTA CRD INICIAL :" +  Str$(Asc.Fis.NotaCrd%)+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 6100H)
      TS.TEMP1$ = Left$("RECIBO CAJA INI :" +  Str$(Asc.Fis.NotaNum%)+String$(38," "),38)
      Call U.Imprime(TS.TEMP1$, 6100H)

      X.Valida% = Validacion.Fecha.Fiscal(X.FEC2$)                              ! Add 1-Dic-2005 OVS
      If X.Valida% <> -1 Then Begin                                             ! Si fecha erronea de autorizacion
         Call TSHIECET																													!
         Call TSHIECET                                                          ! Tono de Alerta
         Call VISOR.AND.BORRAR("FECHA FUERA DE RANGO DE AUTOR. FISCAL")
         Stop                                                                   ! Para la terminal
      EndIf
   EndIf Else Begin
      Call U.Imprime("TERMINAL NO DEFINIDA CON NUMERACION", 6100H)
      Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")
      Call VISORES4690(1, "TERMINAL BLOQUEADA", "", 0, "L")
      Stop
   EndIf
EndIf Else Begin
   Call U.Imprime("ERROR APERTURA ARCHIVO NUMERACION FISCAL", 6100H)
   Call VISOR.AND.BORRAR("TERMINAL SIN NUMERACION FISCAL")
   Call VISORES4690(1, "TERMINAL BLOQUEADA", "", 0, "L")
   Stop
EndIf

End Sub 

Sub CARGA.PARAMETROS.PAGOS																							! Definicion formas de pago SRI
String Xparm1$, Xparm2$, Xparm3$																        !
Integer*2 XI%																														!
  Dim PagosSri$(10,1)																										! Tipos pagos SRI
  XI% = 0
  TS.TS11WERR$ = ""																											! Control de errores
  Open "R::$F:SRIPAG" AS 38 NODEL                                       ! Apertura archivo parametrizacion
  If TS.TS11WERR$ <> "" Then BEGIN 																			! Error apertura archivo
  	 Call VISOR.AND.BORRAR("ERROR APERTURA TF:SRIPAG")									! Msg alerta
   	 Call U.IMPRIME("ERROR ARCHIVO PARAMETROS FISCAL",2100H)            ! 
   	 Asc.Proy.Fiscal% = 0 																							! Init variables
   	 Exit Sub 																													! Sale de la rutina
  EndIf																																	! fin Error apertura
  If End #38 Then UE.FIN.SRIPAG     															      ! Si es EOF
  While (1)														  														    ! Recorre archivo
       Read # 38 ; Xparm1$, Xparm2$																			! Lectura mensaje
       XI% = XI% + 1 																										! Posicion almacena
       If XI% > 10 Then GoTo UE.FIN.SRIPAG															! Sobre pasa 10 definciones
       PagosSri$(XI%,0) = Xparm1$																				! Asigna codigo del SRI
       PagosSri$(XI%,1) = Xparm2$																				! Asigna descriptor
  Wend 																																  ! Fin carga de parametros
  UE.FIN.SRIPAG:																										    ! 
    Close 38																													  ! Cierra archivo
    Wait ; 500																													! Espera de cierre archivo 
  Dim Pagostrx$(70,2)																										! Homologación pagos vs sri
  XI% = 0
  TS.TS11WERR$ = ""																											! Control de errores
  Open "R::$F:PAGOS" As 38 NODEL                                        ! Apertura archivo parametrizacion
  If TS.TS11WERR$ <> "" Then BEGIN 																			! Error apertura archivo
  	 Call VISOR.AND.BORRAR("ERROR APERTURA TF:PAGOS") 									! Msg alerta
   	 Call U.IMPRIME("ERROR ARCHIVO PARAMETROS FISCAL",2100H)            ! 
   	 Asc.Proy.Fiscal% = 0 																							! Init variables
   	 Exit Sub 																													! Sale de la rutina
  EndIf																																	! fin Error apertura
  If End #38 Then UE.FIN.POSPAG     															      ! Si es EOF
  While (1)														  														    ! Recorre archivo
       Read # 38 ; Xparm1$, Xparm2$, Xparm3$														! Lectura mensaje
       Xi% = Val(Xparm1$)																								! Tipo y variedad de pago
       If XI% <= 70 Then Begin                  										    ! Forma de pago correcta
          PagosTrx$(XI%,0) = Xparm2$																		! Codigo del SRI
          PagosTrx$(XI%,1) = Xparm3$																		! Lapso de tiempo
       EndIf   
  Wend 																																  ! Fin carga de parametros
  UE.FIN.POSPAG:																										    ! 
    Close 38																													  ! Cierra archivo

End Sub 																																! Fin definicion formas de pago SRI


Sub CALCULO.FORMAS.PAGO																											! 
Integer*4 PRIC%, TOT4%, X.I%, Xact%, Gri%													          !
String Xtv$, Xvlr$, Xtmp$, Xtf1$, Xtf2$, Xtf3$
String GRSRI$, GRDESC$, GRTIME$, GRVLR$, GRPLAZO$
Xtf1$ = "00" : Xtf2$ = "00" : Xtf3$ = "00"
For X.I% = 1 To SL.END                    																	! FOR ALL StringS
    H$ = READ.SL.STR$(X.I%)                 																! GET String
    If LEN(H$) > 5 Then Begin                                               ! ASSURE GOOD String
     Xact% = 0
     If ASC(H$)  = 5 Then Begin             																! String Forma de pago
     	  Asc.Fis.Fisj% = 3																										! Apuntador String
        Xtv$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  										! Tipo y variedad de pago
        Xvlr$ = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  										! Valor del pago
        Xact% = -1
     EndIf                                  																! 
     If ASC(H$)  = 6 Then Begin             																! String Forma de pago anulacion
     	  Asc.Fis.Fisj% = 3																										! Apuntador String
        Xtv$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  										! Tipo y variedad de pago
        Xvlr$ = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  										! Valor del pago
        Xvlr$ = "-" + Xvlr$
        Xact% = -1
     EndIf                                  																! 
     If ASC(H$)  = 153 Then Begin            																! User datas
     	  Asc.Fis.Fisj% = 3																										! Apuntador String
        Xtmp$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  										! ID user data
     	  If Xtmp$ = "20120917" Then Begin													          ! UD TEF I
     	  	 Xtmp$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  								! Nro tarjeta
           Xtmp$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  								! valor de la compra
           Xtmp$  = GR.GETUNPK4(H$,Asc.Fis.Fisj%)					  								! Nro cuotas
           Xtf1$  = Xtmp$																										! 
     	  EndIf
     	  If Xtmp$ = "20150506" Then Begin													          ! UD TEF II
           Xtmp$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  								! Tipo diferido
           Xtmp$  = GR.GETUNPK4(H$,Asc.Fis.Fisj%)					  								! Nro cuotas
           Xtf2$  = Xtmp$																										! 
     	  EndIf
     	  If Xtmp$ = "20080611" Then Begin													          ! UD Tarj. Manual
           Xtmp$  = GR.GETUNPK(H$,Asc.Fis.Fisj%)					  								! Nro tarjeta
           Xtmp$  = GR.GETUNPK4(H$,Asc.Fis.Fisj%)					  								! Nro cuotas
           Xtf3$  = Xtmp$																										! 
     	  EndIf
     EndIf
     If Xact% = -1 Then Begin																								!	Si actualiza dato
        TS.TEMP1$ = Xtv$																										! Tipo y variedad
        TS.TEMP2$ = PagosTrx$(Val(TS.TEMP1$),0)                             ! Codigo del SRI
        GRTIME$   = PagosTrx$(Val(TS.TEMP1$),1)                             ! PLAZO TIEMPO
        GRSRI$  = PagosSri$(1,0)																						! Codigo homologo del SRI default cash
        GRDESC$ = PagosSri$(1,1)																				  	! DESCRIPTOR PAGO default cash
        GRI% = 0 
        For GRI% = 1 To 10																									! Busca dato equivalente
          If TS.TEMP2$ = PagosSri$(GRI%,0) Then Begin												! Pago parametrizado
             GRSRI$  = PagosSri$(GRI%,0)																		! Codigo homologo del SRI
             GRDESC$ = PagosSri$(GRI%,1)																		! DESCRIPTOR PAGO
          EndIf																															!
        Next GRI%
   
 !       Write #34 ; "tipo variedad "+TS.TEMP1$ + CHR$(10)
 !       Write #34 ; "REPORTA AL SRI "+GRSRI$ + CHR$(10)
 !       Write #34 ; "MONTO DEL PAGO "+XVLR$ + CHR$(10)
 !       Write #34 ; "PAG:"+GRDESC$ + CHR$(10)
   
        PagoRes$(Val(GRSRI$),0) = Right$("00"+GRSRI$,2)                   	! Codigo del SRI
        PagoRes$(Val(GRSRI$),1) = GRDESC$                                   ! Descriptor Pago SRI
        PagoRes$(Val(GRSRI$),2) = Str$(Val(PagoRes$(Val(GRSRI$),2)) +      \! Acumula total del pago
                                       Val(Xvlr$))                          ! 
        PagoRes$(Val(GRSRI$),4) = GRTIME$                                   ! Tiempo

        If Val(Xtf1$) > Val(PagoRes$(Val(GRSRI$),3)) Then                  \!
           PagoRes$(Val(GRSRI$),3) = Xtf1$                                  ! Nro Cuotas Tef III

        If Val(Xtf2$) > Val(PagoRes$(Val(GRSRI$),3)) Then                  \!
           PagoRes$(Val(GRSRI$),3) = Xtf2$                                  ! Nro Coutas Trj Manual
      
        If Val(Xtf3$) > Val(PagoRes$(Val(GRSRI$),3)) Then                  \!
           PagoRes$(Val(GRSRI$),3) = Xtf3$                                  ! Nro Coutas Tef I
        Xtf1$ = "00" : Xtf2$ = "00" : Xtf3$ = "00"
        Xact% = 0
     EndIf																																	! Fin actualiza datos
    EndIf																																		! End Good String 
Next X.I%   																																! Fin recorrido strings
If SL.CH.AMTCHANGE Then Begin																								! Si Hay vueltas en la trx
   TS.TEMP1$ = "11"																													! Reporta al efectivo
   TS.TEMP2$ = PagosTrx$(Val(TS.TEMP1$),0)                                  ! Codigo del SRI
   GRTIME$   = PagosTrx$(Val(TS.TEMP1$),1)                                  ! PLAZO TIEMPO
   For GRI% = 1 To 10																												! Busca dato equivalente
     If TS.TEMP2$ = PagosSri$(GRI%,0) Then Begin														! Pago parametrizado
        GRSRI$  = PagosSri$(GRI%,0)																					! Codigo homologo del SRI
        GRDESC$ = PagosSri$(GRI%,1)																					! DESCRIPTOR PAGO
     EndIf																																	!
   Next GRI%
   PagoRes$(Val(GRSRI$),0) = Right$("00"+GRSRI$,2)                       		! Codigo del SRI
   PagoRes$(Val(GRSRI$),1) = GRDESC$                                     		! Descriptor Pago SRI
   PagoRes$(Val(GRSRI$),2) = Str$(Val(PagoRes$(Val(GRSRI$),2)) -           \! Acumula total del pago
                                      SL.CH.AMTCHANGE)                      ! 
   PagoRes$(Val(GRSRI$),4) = GRTIME$                                        ! Tiempo

!    Write #34 ; "tipo variedad vueltas "+TS.TEMP1$ + CHR$(10)
!    Write #34 ; "REPORTA AL SRI "+GRSRI$ + CHR$(10)
!    Write #34 ; "MONTO DEL PAGO "+Str$(SL.CH.AMTCHANGE) + CHR$(10)
!    Write #34 ; "PAG:"+GRDESC$ + CHR$(10)


EndIf

End Sub 

Sub ALMACENA.FORMAS.PAGO																								! Almacena Ud formas pago en compra
Integer*2 Gri%

  For GrI% = 1 To 40 																										! Recorre formas de pago 
   If Val(PagoRes$(Gri%,2)) > 0 Then Begin 															! Si pago reportado
   	  TS.TEMP2$ = "0"																										! Default plazo
   	  If Val(PagoRes$(Gri%,3)) > 0 Then                                \!
   	  	 TS.TEMP2$ = Str$(Val(PagoRes$(Gri%,3)) * 30)                   ! Tiempo en dias
   	  TS.TEMP1$ = PagoRes$(Gri%,0)                         +           \! Codigo del SRI
   	              ":" + Pack$(PagoRes$(Gri%,2))            +           \! Valor del pago
   	              ":" + PagoRes$(Gri%,1)                   +           \! Descripcion
   	              ":" + TS.TEMP2$                          +           \! Plazo
   	              ":" + PagoRes$(Gri%,4)                                ! Tiempo
      Call Grabacion.Cadena.Usuario("20161019",TS.TEMP1$)               ! Almacena Pagos realizados
   EndIf
  Next GrI%																															! Fin barrido formas de pago

End Sub 																																! Fin almacenamimento UD formas de pago

Sub  Calculo.ReteIva
Integer*4 X.I%

Real X.Base%,   \
     X.Total%,  \
     X.SinIva%

X.Base%   = 0
X.Total%  = 0
X.SinIva% = 0
For X.I% = 1 to 8
!   X.SinIva% = ROUND(X.SinIva% + ROUND(FLOAT(UE.TOTALS(X.I%))/(1. + FLOAT(UE.TABLAIVA(X.I%,0))/100.), 0, 0) , 2, 0)
!   X.Total%  = X.Total% + UE.TOTALS(X.I%)

    X.base% = ROUND(X.base% + ROUND(FLOAT(UE.IVAXTAR(X.I%)),0,0),2,0)  ! TOTAL DE IVAS
    
Next X.I%

!X.Base% =  X.Total% - X.SinIva%

Asc.Fis.VlrIva% = Round((X.Base% / 100) * Asc.Fis.PtgIva% , 2, 0)
Call Format.Amount(X.Base%)
TS.USER5$ = TS.TEMP1$
End Sub 

Sub  Calculo.ReteIva.Exportador
Integer*4 X.I%
Real X.Base%, X.Total%, X.SinIva%

X.Base%   = 0
X.Total%  = 0
X.SinIva% = 0
For X.I% = 1 to 8
!   X.SinIva% = ROUND(X.SinIva% + ROUND(FLOAT(UE.TOTALS(X.I%))/(1. + FLOAT(UE.TABLAIVA(X.I%,0))/100.), 0, 0) , 2, 0)
!   X.Total%  = X.Total% + UE.TOTALS(X.I%)
    
    X.base% = ROUND(X.base% + ROUND(FLOAT(UE.IVAXTAR(X.I%)),0,0),2,0)  ! TOTAL DE IVAS
    
Next X.I%

!X.Base% =  X.Total% - X.SinIva%

Asc.Fis.VlrIva% = Round((X.Base% / 100) * Asc.Fis.PtgIva2% , 2, 0)
Call Format.Amount(X.Base%)
TS.USER5$ = TS.TEMP1$
End Sub 

Sub  Calculo.Retefuente
String AUX.CANASTAS.SIN.IVA$, \
       AUX.CANASTAS.CON.IVA$
Integer*4 X.I%
Real X.Base%

If enable.4d% Then Begin
   AUX.CANASTAS.CON.IVA$ = str$(round(Val(RH.CANASTAS.CON.IVA$)/100,0,0))
   AUX.CANASTAS.SIN.IVA$ = str$(round(Val(RH.CANASTAS.SIN.IVA$)/100,0,0))
EndIf \
Else Begin
   AUX.CANASTAS.CON.IVA$ = RH.CANASTAS.CON.IVA$
   AUX.CANASTAS.SIN.IVA$ = RH.CANASTAS.SIN.IVA$
EndIf

X.Base% = 0

For X.I% = 1 to 8

!   If X.i% = 1 Then \
!      X.Base% = ROUND(X.Base% + ROUND(FLOAT(UE.TOTALS(X.I%))/(1. + FLOAT(UE.TABLAIVA(X.I%,0))/100.), 0, 0) + Val(AUX.CANASTAS.CON.IVA$) , 2, 0) \
!   Else If x.i% = 2 Then \
!      X.Base% = ROUND(X.Base% + ROUND(FLOAT(UE.TOTALS(X.I%))/(1. + FLOAT(UE.TABLAIVA(X.I%,0))/100.), 0, 0) + Val(AUX.CANASTAS.SIN.IVA$) , 2, 0) \
!   Else \
!      X.Base% = ROUND(X.Base% + ROUND(FLOAT(UE.TOTALS(X.I%))/(1. + FLOAT(UE.TABLAIVA(X.I%,0))/100.), 0, 0) , 2, 0)

    X.Base% = ROUND(X.base% + ROUND(FLOAT(UE.BASEIVA(X.I%)),0,0),2,0)  ! TOTAL Base COMPRAS

Next X.I%

Asc.Fis.VlrFte% = Round((X.Base% / 100) * Asc.Fis.PtgFte% , 2, 0)
Call Format.Amount(X.Base%)
TS.USER5$ = TS.TEMP1$
End Sub 

Function GRABA.CONSECUTIVO.FISCAL
String X.Key$,     \
       X.SERIAL$,  \
       X.AUT$,     \
       X.FEC$,     \
       X.LEC$,     \
       X.TIPO$,    \
       X.FEC.EMi$, \
       X.RESOL$

Integer*4 X.INI%,  \
          X.FIN%,  \
          X.ACT%,  \
          X.Nota%, \
          X.NC%


   X.KEY$ = RIGHT$("0000" + TS.TERMINAL$, 4)
   X.KEY$ = PACK$(X.KEY$)
   X.LEC$ = "C2 2C15 C12 C3 C4 2I4 C6 C3 3I4"                   ! Formato Grabacion Registro

   Read FORM X.LEC$; #Asc.Fis.Sesion% KEY X.KEY$; X.KEY$,     \ ! Numero de la terminal
                                                  X.TIPO$,    \ ! Tipo de Maquina
                                                  X.SERIAL$,  \ ! Serial de la terminal
                                                  X.RESOL$,   \ ! Numero resolucion
                                                  X.FEC.EMI$, \ ! Fecha Emision Autorizacion
                                                  X.PREF$,    \ ! Prefijo de facturacion
                                                  X.INI%,     \ ! Numero inicial de Facturacion
                                                  X.FIN%,     \ ! Rango final de Facturacion
                                                  X.AUT$,     \ ! Numero resolucion de la DIAN
                                                  X.FEC$,     \ ! Fecha Autorizacion
                                                  X.ACT%,     \ ! Numero Actual de Facturacion
                                                  X.NOTA%,    \ ! Numero de Nota de Venta
                                                  X.NC%         ! Numero de Nota Credito

   Write Form X.LEC$; #Asc.Fis.Sesion%; X.KEY$,           \ ! Numero de la terminal
                                        X.TIPO$,          \ ! Tipo de Maquina
                                        X.SERIAL$,        \ ! Serial de la terminal
                                        X.RESOL$,         \ ! Numero resolucion
                                        X.FEC.EMI$,       \ ! Fecha Emision Autorizacion
                                        X.PREF$,          \ ! Prefijo de facturacion
                                        X.INI%,           \ ! Numero inicial de Facturacion
                                        X.FIN%,           \ ! Rango final de Facturacion
                                        X.AUT$,           \ ! Numero resolucion de la DIAN
                                        X.FEC$,           \ ! Fecha Autorizacion
                                        Asc.Fis.FacNum%,  \ ! Numero Actual de Facturacion
                                        Asc.Fis.NotaNum%, \ ! Numero de Nota de Venta
                                        Asc.Fis.NotaCrd%    ! Numero de Nota de Credito

Call FIS.WRIF                          											! Grabacion datos en los Hard Totals
End Function

!---- Grabacion del movimiento diario
Sub  GRABAR.MOVIMIENTO.DIARIO(X.TIPO%, X.DOC$, X.VLR1%)
String X.DOC$,   \
       X.LLAVE$, \
       X.FECHA$, \
       X.TRX$,   \
       X.DATA$,  \
       X.ORIGEN$

Integer*1 X.TIPO%

Integer*4 X.VLR1%, \
          X.VLR2%

If Not(ts.standalone) Then Begin
   TS.ER.RETURN = -1
   Open "R::$AMMVFIS" KEYED RECL 44 AS Asc.Fis.FileClte%
   If TS.ER.RETURN = -1 Then Begin                                ! Apertura correcta
      X.LLAVE$ = Right$("0000" + TS.TERMINAL$, 4) + \             ! Numero de la terminal
                 Right$("0" + Str$(X.TIPO%), 1) + \               ! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
                 Right$("000000000" + X.DOC$, 9)                  ! Numero del documento
      X.VLR2%  = 0
      Gr.Bon.DatFis$ = X.Llave$                                   ! Dato Fiscal para Bonos Regalo
      X.LLAVE$ = Pack$(X.LLAVE$)
      X.FECHA$ = Pack$(DATE$)
      X.DATA$  = ""
      X.TRX$   = Pack$(Right$("0000" + Str$(SL.HD.TRANSNUM), 4))
      X.ORIGEN$ = Asc.Fis.Documento$

      Write Form "C7 C3 C2 2I4 C3 C17 C4"; #Asc.Fis.FileClte%; X.LLAVE$, \! Llave
                                                           X.FECHA$,     \! Fecha movimiento
                                                           X.TRX$,       \! Trx del movimiento
                                                           X.VLR1%,      \! Valor de la compra
                                                           X.VLR2%,      \! Valor usuado
                                                           X.FECHA$,     \! Fecha movimiento
                                                           X.ORIGEN$,    \! Documento origen
                                                           X.DATA$        ! Uso futuro

      Gr.Bon.ActFis% = -1                                                 ! Reporta bonos regalo act fiscal
   EndIf Else Begin
      X.DATA$ = Pack$(Right$("0000" + TS.TERMINAL$, 4))      + \ ! Numero de la terminal
                ":" + Pack$(Right$("0" + Str$(X.TIPO%), 1))  + \ ! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
                ":" + Pack$(Right$("000000000" + X.DOC$, 9)) + \ ! Numero del documento
                ":" + Pack$(Date$)                           + \ ! Fecha del movimiento
                ":" + Pack$(Str$(SL.HD.TRANSNUM))            + \ ! Numero de transaccion
                ":" + Pack$(Str$(X.Vlr1%))                   + \ ! Valor del movimiento
                ":" + Asc.Fis.Documento$                     + \ ! Documento origen
                ":" + Pack$("00")                                ! Filler
      Call Grabacion.Cadena.Usuario("20080811", X.Data$)
      X.LLAVE$ = Right$("0000" + TS.TERMINAL$, 4) + \ ! Numero de la terminal
                 Right$("0" + Str$(X.TIPO%), 1)   + \ ! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
                 Right$("000000000" + X.DOC$, 9)        ! Numero del documento
      Gr.Bon.DatFis$ = X.Llave$    ! Dato Fiscal para Bonos Regalo
      Gr.Bon.ActFis% = -1          ! Reporta bonos regalo act fiscal
   EndIf   
   Close Asc.Fis.FileClte%
EndIf Else Begin                                                       ! Error apertura archivo
   X.DATA$ = Pack$(Right$("0000" + TS.TERMINAL$, 4))      + \ ! Numero de la terminal
             ":" + Pack$(Right$("0" + Str$(X.TIPO%), 1))  + \ ! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
             ":" + Pack$(Right$("000000000" + X.DOC$, 9)) + \ ! Numero del documento
             ":" + Pack$(Date$)                           + \ ! Fecha del movimiento
             ":" + Pack$(Str$(SL.HD.TRANSNUM))            + \ ! Numero de transaccion
             ":" + Pack$(Str$(X.Vlr1%))                   + \ ! Valor del movimiento
             ":" + Asc.Fis.Documento$                     + \ ! Documento origen
             ":" + Pack$("00")                                ! Filler
   Call Grabacion.Cadena.Usuario("20080811", X.Data$)
   X.LLAVE$ = Right$("0000" + TS.TERMINAL$, 4) + \ ! Numero de la terminal
              Right$("0" + Str$(X.TIPO%), 1)   + \ ! Tipo de documento 1.Factura 2.Nta Crd 3. Nota Vta
              Right$("000000000" + X.DOC$, 9)        ! Numero del documento
   Gr.Bon.DatFis$ = X.Llave$    ! Dato Fiscal para Bonos Regalo
   Gr.Bon.ActFis% = -1          ! Reporta bonos regalo act fiscal
   Exit  Sub 
EndIf

End Sub 
!---

!--- Actualiza los saldos de movimientos para las notas credito
!---
Sub  ACTUALIZA.SALDO.MOVIMIENTO
String X.LLAVE$, \
       X.FECHA$, \
       X.TRX$,   \
       X.Origen$, \
       X.DATA$

Integer*4 X.VLR1%, \
          X.VLR2%

TS.ER.RETURN = -1
Open "R::$AMMVFIS" KEYED RECL 44 AS Asc.Fis.FileClte%
If TS.ER.RETURN = -1 Then Begin      											! Apertura correcta
   X.LLAVE$ = Right$("0000" + Asc.Fis.TermNc$, 4)    + \ ! Numero de la terminal
              Right$("0" + Asc.Fis.TipoNc$, 1)       + \ ! Tipo de documento 1.Factura 3. Nota Vta
              Right$("000000000" + Asc.Fis.ConsNc$, 9)     ! Numero del documento

   X.LLAVE$ = Pack$(X.LLAVE$)
   Read Form "C7 C3 C2 2I4 C3 C17 C4"; #Asc.Fis.FileClte% KEY X.LLAVE$; X.LLAVE$, \ ! Llave
                                                                    X.FECHA$,  \! Fecha movimiento
                                                                    X.TRX$,    \! Trx del movimiento
                                                                    X.VLR1%,   \! Valor de la compra
                                                                    X.VLR2%,   \! Valor usuado
                                                                    X.FECOR$,  \! Fecha movimiento
                                                                    X.ORIGEN$, \! Documento origen 
                                                                    X.DATA$     ! Uso futuro

   If TS.ER.RETURN = -1 Then Begin ! Lectura Correcta
      X.VLR2% = X.VLR2% + (TS.TOTALS(0, 0, 0) * -1)
      Write Form "C7 C3 C2 2I4 C3 C17 C4"; #Asc.Fis.FileClte%; X.LLAVE$,  \! Llave
                                                           X.FECHA$,  \! Fecha movimiento
                                                           X.TRX$,    \! Trx del movimiento
                                                           X.VLR1%,   \! Valor de la compra
                                                           X.VLR2%,   \! Valor usuado
                                                           X.FECOR$,  \! Fecha movimiento
                                                           X.ORIGEN$, \! Documento origen 
                                                           X.DATA$     ! Uso futuro
   EndIf
EndIf
Close Asc.Fis.FileClte%
End Sub 


!--- Valida si el numero de factura ya esta registrado en el sistema
!--- este bien puede ser del dia corriente o de dias anteriores
Function VALIDAR.MOVIMIENTO(Xterm$, Xtipo$, Xdoc$)
String X.LLAVE$, \
       X.FECHA$, \
       X.TRX$,   \
       X.DATA$,  \
       Xterm$,   \
       Xtipo$,   \
       X.origen$, \
       Xdoc$

Integer*1 VALIDAR.MOVIMIENTO

Integer*4 X.VLR1%, \
          X.VLR2%

Asc.Fis.VlrNta% = 0                          ! Valor de la nota
Asc.Fis.SalNta% = 0                          ! Saldo de la nota
TS.ER.RETURN    = -1
Open "R::$AMMVFIS" KEYED RECL 44 AS Asc.Fis.FileClte%
If TS.ER.RETURN = -1 Then Begin                                 ! Apertura correcta

   X.LLAVE$ = Right$("0000" + Xterm$, 4) + \                    ! Numero de la terminal
              Right$("0" + Xtipo$, 1) + \                       ! Tipo de documento 1.Factura 3. Nota Vta
              Right$("000000000" + Xdoc$, 9)                    ! Numero del documento

   X.LLAVE$ = Pack$(X.LLAVE$)
   X.FECHA$ = Pack$(DATE$)
   Read Form "C7 C3 C2 2I4 C3 C17 C4"; #Asc.Fis.FileClte% KEY X.LLAVE$; X.LLAVE$,  \! Llave
                                                                    X.FECHA$,  \! Fecha movimiento
                                                                    X.TRX$,    \! Trx del movimiento
                                                                    X.VLR1%,   \! Valor de la compra
                                                                    X.VLR2%,   \! Valor usuado
                                                                    X.FECOR$,  \! Fecha movimiento original
                                                                    X.ORIGEN$, \! Documento origen
                                                                    X.DATA$     ! Uso futuro

   If X.FECOR$ = "   " Then \
      X.FECOR$ = X.FECHA$ 
   
   Nc.Fecha.Trx$ = Unpack$(X.FECOR$)
   
   Nc.Num.Trx$   = str$(Val(Unpack$(X.TRX$)) + 1)
   If TS.ER.RETURN = -1 Then Begin                                        ! Lectura Correcta
      Asc.Fis.VlrNta% = X.Vlr1%                                           ! Valor de la nota
      Asc.Fis.SalNta% = X.VLR1% - X.VLR2%                                 ! Saldo de la nota
      Gr.Fiscal.DocOri$ = X.Origen$																				! Nro Documento Origen
      If Asc.Fis.SalNta% <= 0 Then Begin
         Call VISOR.AND.BORRAR("DOCUMENTO REVERSADO EN SU TOTALIDAD")
         Asc.Fis.VlrNta%    = 0                                           ! Valor de la nota
         Asc.Fis.SalNta%    = 0                                           ! Saldo de la nota
         VALIDAR.MOVIMIENTO = 0
      EndIf Else \
         VALIDAR.MOVIMIENTO = -1
   EndIf Else Begin                                                            ! No existe movimiento
      VALIDAR.MOVIMIENTO = 0
   EndIf
EndIf Else Begin                                                               ! Error apertura archivo
   VALIDAR.MOVIMIENTO = 0
   Exit  Function
EndIf
Close Asc.Fis.FileClte%
End Function
!---

Sub  Valida.Ruc(X.Nit$)
String X.Nit$, \
       X.Lec$, X.Fut$, X.Fiel$

Integer*1 X.ValIDA%

TS.ER.RETURN = -1
OPEN "R::$UC" KEYED RECL 97 AS Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   Call TSHIECET
   Call Visores4690(1, "Error Apertura Archivo", "Cliente No Validado", 1800, "L")
   Asc.Fis.IndFac% = 0
   Exit  Sub 
EndIf
X.Fiel$ = X.nit$
X.Nit$ = RIGHT$("              " + X.Nit$, 14)
X.LEC$ = "C14 C30 C30 C10 C1 C2 C1 C1 C8"
READ FORM X.LEC$; #Asc.Fis.FileClte% KEY X.Nit$; Asc.Fis.Cliente$,   \ ! IdentIficacion del cliente
                                                 Asc.Fis.Nombre$,    \ ! Nombre del cliente
                                                 Asc.Fis.Direccion$, \ ! Direccion del Cliente
                                                 Asc.Fis.Telefono$,  \ ! Telefono del cliente
                                                 Asc.Fis.Estatal$,   \ ! Si es empresa estatal
                                                 Asc.Fis.CodSri$,    \ ! Codigo del SRI
                                                 Asc.Fis.Fidelizado$,\ ! Si cliente fidelizado
                                                 Asc.Fis.Empleado$,  \ ! Si cliente empleado
                                                 X.Fut$
Close Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   Call TSHIECET
   Call VISOR.AND.BORRAR("CLIENTE NO DEFINIDO     Borrar=Continuar")
   Dim TS.IO.DATA$(10)
   Dim TS.IO.KEYS(10)                                       ! Init vectores de aplicacion
   TS.IO.MOTORKEY  = 73                                     ! Tecla borrar
   Asc.Fis.IndFac% = 0
   Exit  Sub 
EndIf
If TS.STANDALONE Then Begin
   Call TSHIECET
   Call Visores4690(1, "FACTURA CONS. FINAL ", "EN MODO AUTONOMO", 1800, "C")
   Dim TS.IO.DATA$(10)
   Dim TS.IO.KEYS(10)                                       ! Init vectores de aplicacion
   TS.IO.MOTORKEY  = 73                                     ! Tecla borrar
   Asc.Fis.IndFac% = -1
EndIf
!--- Add 7 Mayo 2008 OVS
If Asc.Fis.Estatal$ = "S" and TS.INTRX Then Begin           ! Si entidad estatal y en transaccion
   Call VISOR.AND.BORRAR("NO PERMITE CAPTURA  CLIENTE ESTATAL     ")
   Dim TS.IO.DATA$(10)
   Dim TS.IO.KEYS(10)                                       ! Init vectores de aplicacion
   TS.IO.MOTORKEY   = 73                                   ! Tecla borrar
   Asc.Fis.IndFac%  = 0
   Asc.Fis.Estatal$ = "N"
   Exit  Sub 
EndIf
!-- Add 20 Jul 2017 
If Asc.Fis.Fidelizado$ = "S" Then Begin																			! Si cliente fidelizado
	 If Gr.Loyalty.Capt% = -1 Then Exit Sub 																	! Cliente ya capturado
	 TS.TEMP5$ = X.fiel$																											! Id a consultar
   Gr.Loyalty.Card$ = ""         																		      	! Init numero de cliente
   Gr.Loyalty.Capt% = 0  																				          	! Tarjeta No Capturada
	 Gr.Loyalty.Proc% = 0
   Gr.Loyalty.Card$ = TS.TEMP5$																				  		! Almacena numero de cliente
   GR.TRACKII$ = String$(37," ")
   Gr.Loyalty.Trk2$ = TS.TEMP5$
   TS.ER.RETURN = -1                                                       	! Control del proceso
   Call CONSULTA.PUNTOS.LOYAL																								! Consulta del cliente fidelizado
   If TS.ER.RETURN = -1 Then Begin 																			  	! Proceso satisfactorio
      Gr.Loyalty.Card$ = TS.TEMP5$																				  ! Almacena numero de cliente
      Gr.Loyalty.Capt% = -1																					        ! Tarjeta Capturada
      Gr.Loyalty.Proc% = -1																									! Loyalty en proceso
    EndIf Else Begin
      Call Visor.and.Borrar("CLIENTE NO FIDELIZADO")
      Gr.Loyalty.Card$ = ""           																		  ! Almacena numero de cliente
      Gr.Loyalty.Capt% = 00																					        ! Tarjeta Capturada
      Gr.Loyalty.Proc% = 00																									! Loyalty en proceso
      Gr.Loyalty.Trk2$ = ""
    Endif 
   
EndIf

Call Visores4690(1, "ESTIMADO(A) CLIENTE", Asc.Fis.Nombre$, 0, "L")
Call Visores4690(2, "ESTIMADO(A) CLIENTE", Asc.Fis.Nombre$, 1800, "L")

Asc.Fis.Cliente$ = X.Fiel$

Asc.Fis.IndFac%  = -1
Dim TS.IO.DATA$(10)
Dim TS.IO.KEYS(10)                                                   ! Init vectores de aplicacion
TS.IO.MOTORKEY = 73                                                  ! Tecla borrar
End Sub 
!--- Fin Validacion del RUC

Sub  Valida.Ruc.Cheque.Si(X.Nit$)
String X.Nit$,  \
       X.Lec$,  \
       X.Name$

Integer*1 X.ValIDA%

TS.ER.RETURN = -1
OPEN "R::$UC" KEYED RECL 97 AS Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   Asc.Fis.ClienteSi$ = STRING$(30, "_")
   Exit  Sub 
EndIf
X.Nit$ = RIGHT$("              " + X.Nit$, 14)
X.LEC$="C14 C30 C30 C10 C1 C2 C10"   !
READ FORM X.LEC$; #Asc.Fis.FileClte% KEY X.Nit$; TS.TEMP1$, \ ! IdentIficacion del cliente
                                                 X.Name$,   \ ! Nombre del cliente
                                                 TS.TEMP1$, \ ! Direccion del Cliente
                                                 TS.TEMP1$, \ ! Telefono del cliente
                                                 TS.TEMP1$, \ ! Si es empresa estatal
                                                 TS.TEMP1$,  \! Codigo del SRI
                                                 TS.TEMP1$    ! datos nuevos

Close Asc.Fis.FileClte%
If TS.ER.RETURN NE -1 Then Begin
   X.Name$ = STRING$(30, "_")
EndIf
Asc.Fis.ClienteSi$ = X.Name$
End Sub 
!--- Fin Validacion del RUC

! Conversi¢n de datos a hexadecimal
Function ERRN.TO.HEX$(cod.err)
integer*4 cod.err
string errn.to.hex$, errn$
integer*2 i, retrn

errn$ = ""
for i = 0 to 7
   retrn = shIft(cod.err, i*4) and 000FH
   If (retrn < 10) Then \
      retrn = retrn + 48 \
   Else \
      retrn = retrn + 55
   errn$ = chr$(retrn) + errn$
next i
errn.to.hex$ = errn$
end Function

Function Actualiza.Saldo
Integer*1 Actualiza.Saldo, Existe
Integer*2 Saldo.Sess, ue.for
String Archivo$, Pos$

On Error GoTo Err.Actualiza

Pos$ = Asc.Fis.TermNc$

Archivo$ = "r::VT:" + mid$(Nc.Fecha.Trx$, 5, 2) + "\V" + \
           mid$(Nc.Fecha.Trx$, 3, 4) +                    \ ! Fecha
           Right$("000" + Nc.Num.Trx$, 3) +               \ ! Transaccion
           "." +                                          \ ! Punto
           Right$("000" + Pos$, 3)                           ! Numero de Pos

Actualiza.Saldo = 0
Saldo.Sess = 95
Existe = 1
Create Archivo$ Direct Recl 102 As Saldo.Sess

If Not(Existe) Then Begin
   Actualiza.Saldo = 0
   Exit  Function
EndIf

For ue.for = 1 to Val(NC.SALDO$(0))
   Write Form "C100 C2"; #Saldo.Sess, ue.for; NC.SALDO$(ue.for), chr$(13) + chr$(10)
Next ue.for
Close Saldo.Sess
Actualiza.Saldo = -1
Exit  Function

Err.Actualiza:
If Err = "ME" And Errf% = Saldo.Sess Then Begin
   Existe = 0
   Resume
EndIf
Write #34;"error actualiza saldo: ", err, errf%, errn.to.hex$(errn), chr$(10)
Actualiza.Saldo = 0
Fend

!Rhiscom Manejos de Saldos por Articulos
Function VerIfica.Saldo(Pos$)
Integer*1 VerIfica.Saldo, \
          Existe,         \
          EOF,            \
          cantidad.ok
Integer*2 Saldo.Sess, \
          Contador,   \
          ue.for,     \
          Puntero,    \
          i
Integer*4 Cantidad,          \
          cantidad.original
String Pos$,     \
       Trx$,     \
       reg$,     \
       Archivo$, \
       paso$,    \
       codigo$

Archivo$ = "r::VT:" + mid$(Nc.Fecha.Trx$, 5, 2) + "\V" + \
           mid$(Nc.Fecha.Trx$, 3, 4) +                     \ ! Fecha
           Right$("000" + Nc.Num.Trx$, 3) +                \ ! Transaccion
           "." +                                           \ ! Punto
           Right$("000" + Pos$, 3)                           ! Numero de Pos

VerIfica.Saldo = 0
Saldo.Sess     = 95
Existe         = 1

TS.ER.RETURN = -1
Open Archivo$ Direct Recl 102 As Saldo.Sess
If TS.ER.RETURN <> -1 Then Begin
   VerIfica.Saldo = 0
   Exit  Function
EndIf

Contador = 0
EOF = 0
Dim NC.SALDO$(1000)
If END # Saldo.Sess Then  VERIfICA
While (1)
   Read form "C102"; #Saldo.Sess; Reg$
   Contador = Contador + 1
   NC.SALDO$(Contador) = Left$(Reg$, 100)
Wend

VerIfica:
NC.SALDO$(0) = str$(Contador)
Close Saldo.Sess
cantidad.ok = 0

For ue.for = 1 To Contador
   If (left$(NC.SALDO$(ue.for), 2) = "01") Then Begin
      GoSub  Parsea.Datos
      cantidad = Val(mid$(NC.SALDO$(ue.for), Busca8 + 1, Busca9 - Busca8 - 1))
      If cantidad > 0 Then Begin
         cantidad.ok = 1
      EndIf
   EndIf
Next ue.for
If cantidad.ok Then \ !a lo menos hay un codigo por devolver.
   VerIfica.Saldo = -1
Exit  Function 

Parsea.Datos:
Busca1 = match(" ", NC.SALDO$(ue.for), 1)
Busca2 = match(" ", NC.SALDO$(ue.for), Busca1 + 1)
Busca3 = match(" ", NC.SALDO$(ue.for), Busca2 + 1)
Busca4 = match(" ", NC.SALDO$(ue.for), Busca3 + 1)
Busca5 = match(" ", NC.SALDO$(ue.for), Busca4 + 1)
Busca6 = match(" ", NC.SALDO$(ue.for), Busca5 + 1)
Busca7 = match(" ", NC.SALDO$(ue.for), Busca6 + 1)
Busca8 = match(" ", NC.SALDO$(ue.for), Busca7 + 1)
Busca9 = match(" ", NC.SALDO$(ue.for), Busca8 + 1)
Return

End Function 

!'*************************************************
!'*************************************************
Function transforma.formato.trx$(aString$) Public
String transforma.formato.trx$, \
       aString$,                \
       larcad$,                 \
       tmp.string$,             \
       cod.item$,               \
       precio$,                 \
       prec.Pes$,               \
       cant.peso$,              \
       anula$,                  \
       filler$,                 \
       pago$,                   \
       tipo$,                   \
       variedad$,               \
       monto$,                  \
       INDICAT2$,               \
       INDICAT3$,               \
       precio.uni$

integer*2 Start.Pos, \
          N.Char,    \
          tmp.pos,   \
          asd.pos

REAL AMOUNT, \
     WEIGHT, \
     PRICE

On Error GoTo err.change.trx

GoSub  inicializa
GoSub  get.next !TYPE
GoSub  get.next !ITEMCODE
cod.item$ = Right$(string$(12, Chr$(48)) + mid$(aString$, Start.Pos, tmp.pos), 12)

GoSub  get.next !XPRICE
precio$= Right$(string$(8, Chr$(48)) + mid$(aString$, Start.Pos, tmp.pos), 8)

GoSub  get.next !DEPARTME
GoSub  get.next !FAMILYNU
GoSub  get.next !INDICAT1
prec.Pes$ = mid$(aString$, Start.Pos, tmp.pos)

!If Val(mid$(aString$, Start.Pos, tmp.pos) = "16452"

If Val(mid$(aString$, Start.Pos, tmp.pos)) and 4000H Then \
   prec.Pes$ = "P" \
Else \
   prec.Pes$ = "C"

GoSub  get.next                                                         !INDICAT2
INDICAT2$ = mid$(aString$, Start.Pos, tmp.pos)

GoSub  get.next                                                         !INDICAT3
INDICAT3$ = mid$(aString$, Start.Pos, tmp.pos)

If Left$(INDICAT3$,1) = "8" Or Left$(INDICAT3$,2) = "23" Then \
   anula$ = "A" \
Else \
   anula$ = " "

GoSub  get.next !XXX
cant.peso$ = Right$(string$(6, Chr$(48)) + mid$(aString$, Start.Pos, tmp.pos), 6)

GoSub  get.next !XXX
precio.uni$ = Right$(string$(6, Chr$(48)) + mid$(aString$, Start.Pos, tmp.pos), 6)

If INDICAT2$ = "2050" Then Begin
   If prec.Pes$ = "P" Then Begin
      WEIGHT  = Val(cant.peso$)
      AMOUNT  = Val(precio$) *10
      PRICE   = (AMOUNT / WEIGHT) *100
      precio.uni$ = STR$(Val(PRECIO$))
      precio$ = Right$(string$(8, Chr$(48)) + precio.uni$, 8)
   EndIf \
   Else If prec.Pes$ = "C" Then Begin
      WEIGHT  = Val(cant.peso$)
      AMOUNT  = Val(precio$)
      PRICE   = (AMOUNT / WEIGHT)
      PRICE   = Val(PRECIO$)
      precio$ = Right$(string$(8, Chr$(48)) + STR$(round(PRICE, 0, 0)), 8)
   EndIf
EndIf \
Else If prec.Pes$ = "C" Then Begin
   WEIGHT  = Val(cant.peso$)
   AMOUNT  = Val(precio$)
   PRICE   = (AMOUNT / WEIGHT)
   PRICE   = Val(PRECIO$)
   precio$ = Right$(string$(8, Chr$(48)) + STR$(round(PRICE, 0, 0)), 8)
EndIf


!anula$ = "A"

tmp.string$ = cod.item$ + precio$ + prec.Pes$ + cant.peso$ + anula$
transforma.formato.trx$ = tmp.string$
Exit  Function

inicializa:
tmp.string$ = ""
Start.Pos   = 1
N.Char      = 0
tmp.pos     = 0
asd.pos     = 0
WEIGHT      = 0
AMOUNT      = 0
PRICE       = 0
Return

get.next:
If asd.pos > 0 Then \
   Start.Pos = asd.pos
N.Char  = match(" ", aString$, Start.Pos)
tmp.pos = N.Char - Start.Pos
If (tmp.pos <= 0) Then \
   tmp.pos = 1
asd.pos = N.Char + 1
Return

err.change.trx:
write #34; "Error change.trx: " + Err + " " + Str$(Errf%) + " " + Errn.To.Hex$(Errn) + " " + Str$(Errl)
End function

Function Simula.Venta.item(cod.in$, precio.i4, cantidad, pesable) 
Integer*1 Simula.Venta.Item, pesable
String cod.in$
Integer*4 precio.i4, cantidad

DIM TS.IO.KEYS(10), TS.IO.DATA$(10)

Ts.Temp1I2 = 2
If pesable Then Begin
   TS.IO.KEYS(6)  = 72
   TS.IO.DATA$(6) = str$(cantidad)
EndIf \
Else Begin
   If cantidad > 1 Then Begin
      TS.IO.KEYS(6)  = 75
      TS.IO.DATA$(6) = str$(cantidad)
      Ts.Temp1I2     = 2
   EndIf
EndIf

TS.IO.KEYS(2)  = 80
TS.IO.DATA$(2) = cod.in$
TS.IO.KEYS(5)  = 74
TS.IO.DATA$(5) = str$(precio.i4)
TS.IO.DEVICE   = 1
TS.IO.MOTORKEY = 80
TS.IO.KEYS(8)  = 67

cod.in$ = str$(Val(cod.in$))
If left$(cod.in$, 2) = "97" and len(cod.in$) = 10 Then \
   Call RH.CANASTAS.Sub.LEER(RIGHT$(cod.in$, 8))

Simula.Venta.Item = 1
Fend


Sub NC.CON.DATOS Public 
Integer*4 xx%
String Xitem$
  If X.NC.TOTAL$ = "4" Then Begin 
     Dim TS.IO.DATA$(10)
     Dim TS.IO.KEYS(10)
     TS.IO.DATA$(2) = mid$(tslinjec$(1), 2, 12)
     TS.IO.MOTORKEY = 80
     Asc.ITEMCODE$  = PACK$(mid$(tslinjec$(1), 2, 12))
     %INCLUDE EAMIRRD4.J86
     If ASC.INDICAT0 AND 40H Then Begin
        TS.IO.KEYS(6)  = 72
        TS.IO.DATA$(6) = str$(val(mid$(tslinjec$(1), 23, 6)))
     EndIf Else Begin
        TS.IO.KEYS(6)  = 75
        TS.IO.DATA$(6) = str$(val(mid$(tslinjec$(1), 23, 6)))
     EndIf
     TS.IO.DEVICE   = 1
     Gr.Pes.Barra%  = -1
     TS.IO.DEVICE   = 1
     TS.IO.COUNT    = 1
     UE.TRX.ANUL%   = 0
     RH.NC.REGISTRO% = 0
     RH.nc.EXISTE    = -1
     Gr.Fiscal.NcDatos% = -1
     Gr.Fiscal.TipDoc% = 1             ! Generación factura
     TS.GROSSPOS = 0
     TS.GROSSNEG = 0
     TE.TR.GROSSPOS = 0
     TE.TR.GROSSNEG = 0
     SL.CH.AMTCHANGE = 0
     Dim acum.grupos%(Num.Grupos%)
     Call DI.BUFFER(0)
     Call TSIEEC01
  EndIf
End Sub 

!- Mod13Ago2008  OVS Grupo Retail
Function RETORNA.DOCUMENTO
String RETORNA.DOCUMENTO, \
       X.Nro.Term$,       \
       X.Nro.Trx$,        \
       X.Nro.Tipo$,       \
       X.Nro.Concepto$,   \
       X.Key$,            \
       X.SERIAL$,         \
       X.AUT$,            \
       X.FEC$,            \
       X.FEC.EMI$,        \
       X.LEC$,            \
       X.TIPO$,           \
       X.RESOL$,          \
       X.REG$,            \
       Reg$,              \
       tipoart$,          \
       porcen$,           \
       descuento$,        \
       Porcentaje$,       \
       signo$,            \
       codigo$

Integer*1 X.ValIDA%,  \
          hay.feria%, \
          Gr.Proc%

Integer*2 Sesion, \
          idx4,   \
          ue.for

Integer*4 X.INI%,   \
          X.FIN%,   \
          X.ACT%,   \
          X.Nota%,  \
          X.NC%,    \
          pago,     \
          precio,   \
          cantidad

Integer*4 Gk%, Gj%, Gs%
String    GA$, gtd$, Gb$, Gts$, Gtp$, Gitm$, Gplu$, Grpvn$

ENTRADA.TIPO.ORIGEN:
!' - IDB RHISCOM - 03/12/2009 14:40:01

f.RH.NV.FACT$ = " "
!X.Nro.Tipo$   = ASIC.DATOS.BASE$("DOCUMENTO ORIGEN", "1.NOTA VTA 9.FACTURA")
!f.RH.NV.FACT$ = X.Nro.Tipo$

!If X.Nro.Tipo$ = "1" Then \
!   X.Nro.Tipo$ = "3" \
!Else If X.Nro.Tipo$ = "9" Then \
!   X.Nro.Tipo$ = "1"

!If (X.Nro.Tipo$ = "E") Then Begin
!   Call VISOR.AND.BORRAR("PROCEDIMIENTO  CANCELADO....")
!   X.NC.total$       = ""
!   RETORNA.DOCUMENTO = ""
!   Asc.Fis.Anulmov%  = 0
!   Exit  Function
!EndIf
!If (X.Nro.Tipo$ = "") Then Begin          ! No se capturo dato
!   Call VISOR.AND.BORRAR("TIPO DOCUMENTO NO   VALIDO..")
!   GoTo ENTRADA.TIPO.ORIGEN
!EndIf
!If MATCH(X.Nro.Tipo$, "13", 1) <= 0 Then Begin
!   Call VISOR.AND.BORRAR("TIPO DOCUMENTO NO   VALIDO..")
!   GoTo ENTRADA.TIPO.ORIGEN
!EndIf

X.Nro.Tipo$ = "9"
f.RH.NV.FACT$ = X.Nro.Tipo$
X.Nro.Tipo$ = "1"

If TS.STANDALONE Then Begin
   Call VISOR.AND.BORRAR("NO SE PERMITE NOTA  CREDITO EN AUTONOMO")
   X.NC.total$       = ""
   RETORNA.DOCUMENTO = ""
   Asc.Fis.Anulmov%  = 0
   Exit  Function
EndIf
NC.IN% = 1

ENTRADA.TERMINAL:
Call VISOR.AND.BORRAR("PROCESO NOTA CREDITO    Borrar=Continuar")

X.Nro.Term$ = ASIC.DATOS.BASE$("NUMERO TERMINAL", "ORIGEN")
If (X.Nro.Term$ = "E") Then Begin
   Call VISOR.AND.BORRAR("PROCEDIMIENTO  CANCELADO....")
   X.NC.total$       = ""
   RETORNA.DOCUMENTO = ""
   Asc.Fis.Anulmov%  = 0
   Exit  Function
EndIf
If (X.Nro.Term$ = "") Then Begin          ! No se capturo dato
   Call VISOR.AND.BORRAR("NUMERO DE TERMINAL  NO VALIDO..")
   GoTo ENTRADA.TERMINAL
EndIf

ENTRADA.FACTURA:
X.Nro.Trx$ = ASIC.DATOS.BASE$("NUMERO DOCUMENTO", "ORIGINAL")
If (X.Nro.Trx$ = "E") Then Begin
   Call VISOR.AND.BORRAR("PROCEDIMIENTO       CANCELADO....")
   RETORNA.DOCUMENTO = ""
   X.NC.total$       = ""
   Asc.Fis.Anulmov%  = 0
   Exit  Function
EndIf
If (X.Nro.Trx$ = "") Then Begin          ! No se capturo dato
   Call VISOR.AND.BORRAR("NUMERO DOCUMENTO    NO VALIDO..")
   GoTo ENTRADA.FACTURA
EndIf  !
If Val(X.Nro.Trx$) <= 0 Then Begin
   Call VISOR.AND.BORRAR("NUMERO DOCUMENTO    NO VALIDO..")
   GoTo ENTRADA.FACTURA
EndIf

X.Nro.Trx$       = Right$("000000000" + X.Nro.Trx$, 9)
Asc.Fis.TermNc$  = X.Nro.Term$                                 ! NUMERO DE TERMINAL
Asc.Fis.TipoNc$  = X.Nro.Tipo$                                 ! TIPO DOCUMENTO
Asc.Fis.ConsNc$  = X.Nro.Trx$                                  ! Numero de trx

X.ValIDA% = VALIDAR.MOVIMIENTO(X.Nro.Term$, X.Nro.Tipo$, X.Nro.Trx$)        ! Valida si documento existe
If X.ValIDA% = 0 Then Begin
   Call VISOR.AND.BORRAR("MOVIMIENTO NO EXISTE EN EL SISTEMA")
   GoTo ENTRADA.TIPO.ORIGEN
EndIf

If (enable.nc%) Then Begin

ENTRADA.DEVOLUCION:
   !'*****************************************
   !'FVG Manejos de Saldos de Notas de Credito

   !X.Nc.Total$ = ASIC.DATOS.BASE$("MOTIVO: 1.DINERO    ", "2.CAMB.PAGO 3.PARCIA")

   X.Nc.Total$ = ASIC.DATOS.BASE$("1.DINERO 3.PARCIAL  ", "4.DATOS/PAGOS       ")
   If (X.Nc.Total$ = "E") Then Begin
      Call VISOR.AND.BORRAR("PROCEDIMIENTO       CANCELADO....")
      X.NC.total$       = ""
      RETORNA.DOCUMENTO = ""
      Asc.Fis.Anulmov%  = 0
      Exit  Function
   EndIf
   If Val(X.Nc.Total$) > 4 or Val(X.Nc.Total$) < 1 Then Begin
      Call VISOR.AND.BORRAR("TIPO DEVOLUCION     NO VALIDO           ")
      Goto ENTRADA.DEVOLUCION
   EndIf
   If Val(X.Nc.Total$) = 2 Then Begin
      Call VISOR.AND.BORRAR("TIPO DEVOLUCION     NO VALIDO           ")
      Goto ENTRADA.DEVOLUCION
   EndIf

   If Not(VerIfica.Saldo(X.Nro.Term$)) Then Begin
      Call VISOR.AND.BORRAR("TRX SIN SALDO       POR DEVOLVER")
      RETORNA.DOCUMENTO = ""
      X.NC.total$       = ""
      Call VISOR.AND.BORRAR("PROCEDIMIENTO       CANCELADO....")
      Asc.Fis.Anulmov%  = 0
      Exit  Function
   EndIf
   
!  If X.NC.TOTAL$ = "2" Then BEGIN 
!  	  TS.IO.MOTORKEY = 73
!  	  Dim TS.IO.DATA$(10) : Dim TS.IO.KEYS(10)
!  	  Asc.Fis.Anulmov%  = 0
!  	  Exit Function 
!  EndIf
   
   If ( X.Nc.Total$ = "1" Or X.Nc.Total$ = "4") Then Begin   ! Si es al Total de la Nota Credito.
      Dim tslinjec$(500)
      Dim tslinjecd$(500,2)
      idx4 = 0
      pago = 0
      hay.feria% = 0
     
      for ue.for = 1 to Val(NC.SALDO$(0))
         If (left$(NC.SALDO$(ue.for), 2) = "01") Then Begin
            GoSub  Obtiene.Cantidad

            If (cantidad <> 0) and \
               (tipoart$ = "00" or tipoart$ = "01" or tipoart$ = "03" or Left$(tipoart$,1) = "8" or tipoart$ = "23") Then Begin


               !formato devuelto por funcion transforma.
               !      (12)        (8)       (1)         (6)          (1)
               !"I" + cod.item$ + precio$ + prec.Pes$ + cant.peso$ + anula$


               Reg$ = transforma.formato.trx$(NC.SALDO$(ue.for))
               reg$ = "I" + reg$ 
               hay.feria% = 0
               idx4 = idx4 + 1
               tslinjec$(idx4)   = Reg$
               NC.SALDO$(ue.for) = left$(Left$(NC.SALDO$(ue.for), Busca8) + "0" + String$(100, " "), 100)
               tslinjec$(0)      = RIGHT$("000000" + str$(idx4), 6) + "000000"
            EndIf
         EndIf
         If (left$(NC.SALDO$(ue.for), 2) = "99") Then Begin      						! Valida descuentos
           GoSub TIPO.STR.TRX2
         	 If Gts$ = "20110818" Then Begin																	! Si es String descuento
         	    GoSub Obtiene.Descuento																				! Busca dscto
         	    tslinjecd$(idx4+1,0) = Descuento$  	                          ! Almacena Valor
         	    tslinjecd$(idx4+1,1) = Porcentaje$ 	                          ! Almacena Ptg Aplicado
           EndIf																														!
           If Gts$ = "97" Then Begin						      											! Si es String descuento
         	    GoSub Obtiene.Descuento.Or																		! Busca dscto
         	    If Gtp$ = "01" Then Begin 																		! Promocion Simple
         	       tslinjecd$(idx4+1,0) = Descuento$  	                      ! Almacena Valor
         	       tslinjecd$(idx4+1,1) = Porcentaje$ 	                      ! Almacena Ptg Aplicado
         	    EndIf																													! Fin promo simple
         	    If Gtp$ = "02" Then Begin 																		! Promocion Compleja
         	    	 For Gs% = 1 To Val(NC.SALDO$(0))														! Recorre busqueda items
         	    	   Gitm$ = Mid$(tslinjec$(Gs%), 2, 12)                      ! 
         	    	   If Val(Gplu$) = Val(Gitm$) Then Begin										! Encontro Item
         	            tslinjecd$(Gs%,0) = Str$( Val(tslinjecd$(Gs%,0))  +  \! Almacena Valor Dscto
         	                                      Val(Descuento$) )
         	            tslinjecd$(Gs%,1) = "OR2"                             ! Almacena Ptg Aplicado
         	            tslinjecd$(Gs%,2) = Descuento$
         	    	   EndIf
         	    	 Next Gs%
         	    EndIf																													! Fin promo compleja
           EndIf
         EndIf																															! Fin String de clientes


      Next ue.for

      idx4 = idx4 + 1
      tslinjec$(idx4) = "T"

      tslinjec$(0) = RIGHT$("000000" + str$(idx4), 6) + "000000"
      peso = 0
      If mid$(tslinjec$(1), 22, 1) = "P" Then \
         peso = 1
      codigo$     = mid$(tslinjec$(1), 2, 12)
      precio      = Val(mid$(tslinjec$(1), 14, 8))
      cantidad    = Val(mid$(tslinjec$(1), 23, 6))
      rh.precioxq = 0
      rh.nc.existe    = -1
      RH.NC.REGISTRO% = 0
      RH.NC.FINAL%    = idx4 - 1
   EndIf
EndIf
!************************************************************************************

X.Nro.Term$ = Right$("000" + X.Nro.Term$, 3)                            ! Ajusta numero de terminal

!' -IDB RHISCOM 03/12/2009 16:10:29-
If Val(f.RH.NV.FACT$) = 9 Then \
   TS.TEMP1$ = "FACTURA NRO: " \
Else If Val(f.RH.NV.FACT$) = 1 Then \
   TS.TEMP1$ = "NOTA DE VENTA NRO: "

Asc.Fis.Anulmov%  = -1
Nc.Num.Term$      = X.Nro.Term$
RETORNA.DOCUMENTO = TS.TEMP1$ + X.PREF$ + "-" + X.Nro.Term$ + "-" + X.Nro.Trx$
Exit  function

Obtiene.Cantidad:
Busca1   = match(" ", NC.SALDO$(ue.for), 1)
Busca2   = match(" ", NC.SALDO$(ue.for), Busca1 + 1)
Busca3   = match(" ", NC.SALDO$(ue.for), Busca2 + 1)
Busca4   = match(" ", NC.SALDO$(ue.for), Busca3 + 1)
Busca5   = match(" ", NC.SALDO$(ue.for), Busca4 + 1)
Busca6   = match(" ", NC.SALDO$(ue.for), Busca5 + 1)
Busca7   = match(" ", NC.SALDO$(ue.for), Busca6 + 1)
Busca8   = match(" ", NC.SALDO$(ue.for), Busca7 + 1)
Busca9   = match(" ", NC.SALDO$(ue.for), Busca8 + 1)
precio   = Val(mid$(NC.SALDO$(ue.for), Busca2 + 1, Busca3 - Busca2 - 1))
tipoart$ = mid$(NC.SALDO$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1)
cantidad = Val(mid$(NC.SALDO$(ue.for), Busca8 + 1, Busca9 - Busca8 - 1))

If Left$(tipoart$,1) = "8" Then \
	 precio = (precio * -1) : \
	 cantidad = (cantidad * -1)

If tipoart$ = "23" Then \
	 precio = (precio * -1) : \
	 cantidad = (cantidad * -1)

Return

Obtiene.datos:
Busca11    = match(" ", NC.SALDO$(ue.for), 1)
Busca12    = match(" ", NC.SALDO$(ue.for), Busca11 + 1)
Busca13    = match(" ", NC.SALDO$(ue.for), Busca12 + 1)
porcen$    = mid$(NC.SALDO$(ue.for), Busca11 + 1, Busca12 - Busca11 - 1)
descuento$ = mid$(NC.SALDO$(ue.for), Busca12 + 1, Busca13 - Busca12 - 1)
Return

Obtiene.Descuento:
Busca21    = match(" ", NC.SALDO$(ue.for), 1)																! Tipo String
Busca22    = match(" ", NC.SALDO$(ue.for), Busca21 + 1)											! Numero del proyecto
Busca23    = match(" ", NC.SALDO$(ue.for), Busca22 + 1)											! Ptg dscto aplicado
Busca24    = match(" ", NC.SALDO$(ue.for), Busca23 + 1)											! Valor del descuento
Busca25    = match(" ", NC.SALDO$(ue.for), Busca24 + 1)											! Ean Producto
Busca26    = match(" ", NC.SALDO$(ue.for), Busca25 + 1)											! Signo operacion
Porcentaje$ = mid$(NC.SALDO$(ue.for), Busca22 + 1, (Busca23 - Busca22 - 1)) ! porcentaje aplicado
descuento$  = mid$(NC.SALDO$(ue.for), Busca23 + 1, (Busca24 - Busca23 - 1))  ! Retorna descuento
signo$      = mid$(NC.SALDO$(ue.for), Busca25 + 1, (Busca26 - Busca25 - 1))  ! Retorna signo de la operacion
If Signo$ = "01" Then descuento$ = Str$( Val(descuento$) * -1)

Return 

Obtiene.Descuento.Or:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA97											! Tipo de registro
GoSub GRDATA97											! Numero proyecto
GoSub GRDATA97											! Tipo de promocion
Gtp$ = Ga$
GoSub GRDATA97											! ID Promo                     
GoSub GRDATA97											! Sub id promo                 
GoSub GRDATA97											! Nro id promo                 
GoSub GRDATA97											! SKU producto
Gplu$ = Ga$
GoSub GRDATA97											! Codigo interno santa maria
GoSub GRDATA97											! precio maestra
GoSub GRDATA97											! precio venta
Grpvn$ = Ga$
GoSub GRDATA97											! Dscto total otorgado
descuento$  = Ga$
Porcentaje$ = "0"
GoSub GRDATA97											! signo de la operacion
Signo$ = Ga$
If Signo$ = "01" Then descuento$ = Str$( Val(descuento$) * -1)

Return 

TIPO.STR.TRX2:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA97
GoSub GRDATA97
Gts$ = Ga$
GoSub GRDATA97
Gtp$ = Ga$
Return 

GRDATA97:
GK%  = MATCH(" ", GB$, GJ%)                            	! SEARCH FOR FIELD SEPERATOR
If (GK% - GJ%) > 0 Then                                \!
GA$ = (Mid$(GB$, GJ%, GK% - GJ%))      Else            \! FIELD
GA$ = ""
GJ%  = GK% + 1                                      		! POINT TO BeginNING OF NEXT FIELD
Return

End Function
!---

Function Modulo11(XDATA$) External
String Xdata$, Modulo11
End Function 

Sub  CLAVE.ACCESO.SRI(Rdoc$) Public
String Ydata$, BARCODE$, ENDBCODE$, DgCh$, Rdoc$, Rtdoc$
   If Gr.Fiscal.TipDoc% = 1 Then Rtdoc$ = "01"															! Factura de venta
 	 If Gr.Fiscal.TipDoc% = 2 Then Rtdoc$ = "04"															! Nota Credito
 	 If Gr.Fiscal.TipDoc% = 3 Then Rtdoc$ = "05"															! Nota debito
 	 If Gr.Fiscal.TipDoc% = 4 Then Rtdoc$ = "06"															! Guida de remision
 	 If Gr.Fiscal.TipDoc% = 5 Then Rtdoc$ = "07"															! Comprobante retencion
 		
   Ydata$ = Mid$(DATE$,5,2) + Mid$(DATE$,3,2) + "20"+Mid$(DATE$,1,2) +     \! ddmmaaaa                C 8
            Rtdoc$                                                   +     \! Tipo de comprobante     C 2
            Right$("0000000000000"+Asc.Fis.NroRucCl$,13)             +     \! Ruc del cliente         C 13
            Asc.Fis.TipoAmbt$                                        +     \! Tipo de Ambiente        C 1
            Gr.Fiscal.Serie$                                         +     \! Prefijo y terminal      C 6
            Right$("000000000"+Rdoc$,9)                              +     \! Secuencial              C 9
            Right$("00000000" + Asc.Fis.NroSecun$, 8)                +     \! Codigo Secuencia SRI    C 8  
            Asc.Fis.TipoEmis$                                               ! Tipo de emision         C 1
   DgCh$ = Modulo11(Ydata$)																									! Calculoa digito chequeo Mod 11
   Ydata$ = Ydata$ + DgCh$																									! Arma String
   Gr.Fiscal.ClvSri$ = Ydata$																								! Clave asignada para SRI
   
End Sub  

Sub CAMBIO.PAGO.NC 																													! Cambio formas de pago NC
      X.Nc.Total$ = ""
      TS.IO.DATA$(5) = "2"																									! Secuencia cambio a efectivo          
      TS.IO.KEYS(5)  = 61																										!                                        
      TS.IO.MOTORKEY = 61																										!                                        
      Call TSTCEC01                                                         !                        	     
End Sub 

Sub Grabacion.Fact.Datos Public																							! Grabacion datos nc para factura datos

End Sub 																																		! Fin Grabacion.Fact.Datos


!--- Bloque Principal Modulo Fiscal
!--- DEFINICION DE USER Exit
!---

Sub  FISMTS02.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU02.ECU                                         **
!   **    Inclu¡r en :  EAMTSU02.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

rh.flag.usuario% = 0
If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo
   %Include FISTSU02.ECU
   Asc.Fis.ChequeSi$    = ""
   Asc.Fis.ImpChequeSi% = 0
   Asc.Fis.ClienteSi$   = ""
   X.Nc.Total$          = ""
   Nc.Fecha.Trx$        = ""
   Nc.Num.Trx$          = ""
   Nc.Num.Term$         = ""
   Gr.Recaudos.Intrx%   = 0
EndIf
f.Factura = 0
NC.IN%    = 0
rh.flag.nc.total% = 0
Gr.Fiscal.ImpDoc% = 0
Gr.DsctoNc% = 0
Gr.DsctoNcptg% = 0
rh.mkp.dsct% = 0
rh.MKP.DSCOTORGADO% = 0
Gr.PromOr.DsctItm% = 0
Gr.DsctoNc% = 0 
Dim PagoRes$(40,4)
SL.CH.AMTCHANGE  = 0
Gr.NoTkt.Suspend% = 0
End Sub 

Sub  FISMTS07.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU07.ECU                                         **
!   **    Inclu¡r en :  EAMTSU07.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

%Include FISTSU07.ECU     ! Carga parametros Numeracion Fiscal

End Sub 

Function Tiene.Saldo(cod.in$)
String codigo$, cod.in$, tipoart$, precio$, metodo$
Integer*1 tiene.saldo, Xfnd%, Xcompleja%
Integer*2 ue.for
Integer*4 Cantidad, Lpos%
String    Descuento$, Porcentaje$, signo$
Integer*4 GdscTot%, Gqty%, Gprice%
Integer*4 Gk%, Gj%
String    GA$, gtd$, Gb$, GrTipo$,Gtp$, Gitm$, Gts$

tiene.saldo  = 0
rh.ex.feria% = 0
cantidad     = 0 
GdscTot%     = 0 
Gqty%        = 0 
Xfnd%        = 0 
Gr.DsctoNc%  = 0 
Gprice% = 0 
descuento$ = "0"
Xcompleja% = 0
For Ue.For = 1 To Val(NC.SALDO$(0))
   If (left$(NC.SALDO$(ue.for), 2) = "99") Then Begin      						! Valida descuentos

   	 GoSub TIPO.STR.TRX																								! Tipo de String
   	 If Gts$ = "20110818"  Then Begin			  													! Si es String descuento
   	    GoSub Obtiene.Descuento1																			! Busca dscto
     EndIf																														!

   	 If Gts$ = "97"  Then Begin																			  ! Si es String descuento Oracle
   	    GoSub Obtiene.Descuento2																			! Toma dsctos aplicados en la trx
   	    If Gtp$ = "01" Then Begin																			! Promocion Simple
   	    	GoTo Siguiente.Str.Trx
   	    EndIf
   	    If Gtp$ = "02" Then Begin																			! Promocion compleja
   	    	 If Val(Gitm$) = Val(codigo$) Then Begin					  			  ! Si Item para anular
   	    	 	  GdscTot% = GdscTot% + Val(descuento$)										! Total descuentos
   	    	 	  descuento$ = "0"
   	    	 	  Xcompleja% = -1
   	    	 EndIf	Else descuento$ = "0"																! Fin si es Item 
   	    Endif																													! Si promo compleja
     EndIf																														! Si promociones Oracle 


   EndIf

   If (Left$(NC.SALDO$(ue.for), 2) = "01") Then Begin									! Si String Item
   	  GoSub  Obtiene.Datos																						! Analiza String 
   	  GrTipo$ = " "+tipoart$
	   If match(GrTipo$," 00 01 03 23 81 80",1) > 0 Then Begin          ! Trx de venta 
   	   If Val(cod.in$) = Val(codigo$) Then Begin                      ! Es Item validado
   	   	If Left$(tipoart$,1) <> "8" Then Begin 
          Gqty% = Gqty% + Cantidad																		! Total cantidad vendida   	 
          Gprice% = Gprice% + Val(precio$)														! Total de venta	
        EndIf Else Begin
          Gqty%   = Gqty% - Cantidad																	! Total cantidad vendida   	 
          Gprice% = Gprice% - Val(precio$)														! Total de venta	
        EndIf  
        GdscTot% = GdscTot% + Val(descuento$)												! Total descuentos
        descuento$ = "0"
        Xfnd%      = -1																						  ! Encontrado
   	   EndIf Else Begin              																  ! Fin Item validado
   	   	 descuento$ = "0"
   	   EndIf
   	  EndIf																														! Fin si es venta
   EndIf																															! Fin validacion String Item
   Siguiente.Str.Trx:
Next Ue.For

If TS.IO.DATA$(6) = "" Then TS.IO.DATA$(6) = "1"											! Ajusta Cantidad

If Xfnd% = -1 Then Begin																							! Producto encontrado
!	 Write #34 ; "precio "+Str$(Gprice%) + Chr$(10)
!	 Write #34 ; "Qty    "+Str$(Gqty%)   + Chr$(10)
 
	 If Val(TS.IO.DATA$(6)) = Gqty% Then Begin													! Si devuelve todo
	 	  TS.TEMP6$ = Str$(Gprice%)																				! Total de la venta
	 	  Gr.DsctoNc% = GdscTot%																					! Total del dscto 
	 	  Tiene.saldo = -1   																							! Reporta exitosa anulacion
 	    Exit Function 																									! Sale del proceso
	 EndIf																															! 
	
	 If Gqty% <= 0 Then Begin																						! Si Qty es menor
	 	  Call VISOR.AND.BORRAR("CANTIDAD NO SUFICIENTE PARA NOTA CRD ")  ! Msg Alerta
	 	  Tiene.saldo = 0   																							! Reporta exitosa anulacion
	 	  Gr.DsctoNc% = 0
	 	  Exit Function 																									! Sale del proceso
	 EndIf
	 If Val(TS.IO.DATA$(6)) > Gqty% Then Begin													! Si qty mayor existencia
	 	  Call VISOR.AND.BORRAR("CANTIDAD INGRESADA  SUPERA EXISTENCIA")  ! Msg Alerta
	 	  Tiene.saldo = 0   																							! Reporta exitosa anulacion
	 	  Gr.DsctoNc% = 0
	 	  Exit Function 																									! Sale del proceso
	 EndIf																															! 
	 
	 
   If Val(TS.IO.DATA$(6)) <= Gqty% Then Begin                  				! Si le queda saldo

      	 If Xcompleja% = -1 Then Begin																! si promo compleja
      	 	  Gprice% = Gprice% - GdscTot%															! Retiro dscto del precio
      	 EndIf

      	 If IR.INDICAT0 AND 40H Then                                 \! Si es pesable
      	    TS.TEMP5$ = Str$((float(Gprice%) / float(Gqty%)) * 1000)  Else \! Calculo Precio unitario
      	    TS.TEMP5$ = Str$(Gprice% / Gqty%)                         ! 
            IR.PRICE1 = Val(TS.TEMP5$)                                ! Precio unitario
            IR.PRICE2 = IR.PRICE1 
       	 TS.TEMP6$ = "0"																							! Queda cantidad pendiente

      If GdscTot% > 0 Then \
         Gr.DsctoNc% = (GdscTot% / Gqty%) *  Val(TS.IO.DATA$(6)) Else \  ! Valor proporcional descuento
         Gr.DsctoNc% = 0
      
      Gqty% = Gqty% - Val(TS.IO.DATA$(6))															! Retira la cantidad a devolver

!      Write #34 ; "retorna "+TS.TEMP5$ + Chr$(10)

      Tiene.saldo = -1																							  ! Reporta exitosa anulacion      
   EndIf 
  
EndIf 																																! Fin producto encontrado

Exit  Function																												! Sale de la funcion

Obtiene.Descuento1:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA98											! Tipo de registro
GoSub GRDATA98											! Numero proyecto
GoSub GRDATA98											! Ptg Dscto
GoSub GRDATA98											! Valor del dscto
descuento$  = Ga$
GoSub GRDATA98											! codigo del producto
GoSub GRDATA98											! signo de la operacion
Signo$ = Ga$
If Signo$ = "01" Then descuento$ = Str$( Val(descuento$) * -1)

Return 

Obtiene.Descuento2:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA98											! Tipo de registro
GoSub GRDATA98											! Numero proyecto
GoSub GRDATA98											! Tipo de promocion
Gtp$ = Ga$
GoSub GRDATA98											! ID Promo                     
GoSub GRDATA98											! Sub id promo                 
GoSub GRDATA98											! Nro id promo                 
GoSub GRDATA98											! SKU producto
Gitm$ = Ga$
GoSub GRDATA98											! Codigo interno santa maria   
GoSub GRDATA98											! precio maestra               
GoSub GRDATA98											! precio venta
GoSub GRDATA98											! Dscto total otorgado
descuento$  = Ga$
GoSub GRDATA98											! signo de la operacion
Signo$ = Ga$
If Signo$ = "01" Then descuento$ = Str$( Val(descuento$) * -1)

Return 

Obtiene.Datos:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA98
GoSub GRDATA98
codigo$  = ga$
GoSub GRDATA98
precio$  = ga$
GoSub GRDATA98
GoSub GRDATA98
GoSub GRDATA98
GoSub GRDATA98
GoSub GRDATA98
tipoart$ = ga$
GoSub GRDATA98
Cantidad = Val(Ga$)
Gr.QtyAnl% = Cantidad
TS.TEMP6$ = PRECIO$
Return 

TIPO.STR.TRX:
GJ% = 3
GB$ = NC.SALDO$(ue.for)
GoSub GRDATA98
GoSub GRDATA98
Gts$ = Ga$

Return 


GRDATA98:
GK%  = MATCH(" ", GB$, GJ%)                            ! SEARCH FOR FIELD SEPERATOR
If (GK% - GJ%) > 0 Then 		  												\! SI ENCONTRO DATO
   GA$ = (Mid$(GB$, GJ%, GK% - GJ%))    ELSE          \! FIELD
   GA$ = ""
GJ%  = GK% + 1                                      	 ! POINT TO BeginNING OF NEXT FIELD
Return


End Function 

Sub  FISMTS08.011 Public
String a$
Integer*2 ue.for
Integer*4 Xpos%

If X.Nc.Total$ = "3" Then Begin 																						! Nc Parcial
	
!   a$ = Unpack$(IR.ITEMCODE$)
!   rh.cod.in$ = a$
!   If not(tiene.saldo(a$)) Then Begin
!      !Call VISOR.AND.BORRAR("NO TIENE SALDO      POR DEVOLVER....")
!      ERR.MSG.67$ = "NO TIENE SALDO      POR DEVOLVER...."
!      TS.LINEDATA = 67
!      Call TSCSEC03
!      IR.DEPARTME$ = Pack$("0000")
!      Exit Sub 
!   EndIf

   TS.IO.KEYS(8) = 67

EndIf

If (X.NC.Total$ = "1" Or X.NC.TOTAL$ = "4") Then Begin											! Anulación total
   If RH.nc.EXISTE Then Begin																								!
      CHAIN.COUNT = 1                                             					! Contador de item linkeados
      RH.NC.REGISTRO% = RH.NC.REGISTRO% + 1																	! Barre lista de items
      Xpos% = RH.NC.REGISTRO%
       
      If Gr.Fiscal.NcDatos% <> -1 Then begin                                    \! Generando NC
       If Right$(tslinjec$(Xpos%),1) = "A" Then Begin												! Si es una anulaci
      	 TS.IO.KEYS(8) = 0
       EndIf Else Begin 
      	 TS.IO.KEYS(8) = 67
       EndIf
      endif
      If Gr.Fiscal.NcDatos% = -1 Then                                      \! Generando Nueva factura
       If Right$(tslinjec$(Xpos%),1) = "A" Then Begin												! Si es una anulaci
      	 TS.IO.KEYS(8) = 67
       EndIf Else Begin 
      	 TS.IO.KEYS(8) = 0
       EndIf

      If RH.NC.REGISTRO% < RH.nc.FINAL% Then Begin													! No ha recorrido toda la lista
      	 If RH.NC.REGISTRO% = 1 And Gr.Fiscal.NcDatos% = -1 Then BEGIN      !
      	    TS.TEMP1$ = mid$(tslinjec$(RH.NC.REGISTRO%), 2, 12)             ! Primer Item de la lista
            TS.TEMP1$ = Pack$(Right$("000000000000"+TS.TEMP1$,12))          !
            Call IRRFEC.READ01 (TS.TEMP1$, 4, TS.TEMP2$, 0)									!
            Call IRRFEC.SPLIT1(TS.TEMP2$)																		!
            SL.IT.ITEMCODE$ = Str$(Val(Unpack$(IR.ITEMCODE$)))							!
            TS.IO.DATA$(2)  = SL.IT.ITEMCODE$																!
      	 EndIf 																															!
         rh.precioxq = 0																										!
         If mid$(tslinjec$(RH.NC.REGISTRO%), 1, 1) = "D" Then              \!
            rh.precioxq = Val(mid$(tslinjec$(RH.NC.REGISTRO%), 30, 8)) 			!
         IR.LINKEDTO$ = pack$(mid$(tslinjec$(RH.NC.REGISTRO% + 1), 2, 12))	!
         
         If IR.INDICAT0 AND 40H Then Begin        													! Si el Item es pesable 
            TS.IO.KEYS(6)  = 72																							! Simula secuencia registro
            TS.IO.DATA$(6) = str$(Val(mid$(tslinjec$(RH.NC.REGISTRO%), 23, 6)))
         EndIf Else Begin																										! Item por cantidad
            TS.IO.KEYS(6)  = 75																							! Simula cantidad
            TS.IO.DATA$(6) = str$(Val(MID$(tslinjec$(RH.NC.REGISTRO%), 23, 6)))
         EndIf																															!
      EndIf																																	
      If RH.NC.REGISTRO% >= RH.NC.FINAL% Then Begin													! Es el ultimo producto
         rh.precioxq = 0																										!
         If mid$(tslinjec$(RH.NC.REGISTRO%), 1, 1) = "D" Then              \!
            rh.precioxq = Val(mid$(tslinjec$(RH.NC.REGISTRO%), 30, 8))      !
         IR.LINKEDTO$   = PACK$("0000")																			! Termina liga de productos
         If IR.INDICAT0 AND 40H Then Begin																	! Si es pesble
            TS.IO.KEYS(6)  = 72																							! Simula secuencia pesable
            TS.IO.DATA$(6) = str$(Val(MID$(tslinjec$(RH.NC.REGISTRO%), 23, 6))) !
         EndIf Else Begin																										! Si es unitario
            TS.IO.KEYS(6)  = 75																							! Simula secuencia venta por unidad
            TS.IO.DATA$(6) = str$(Val(MID$(tslinjec$(RH.NC.REGISTRO%), 23, 6)))
         EndIf																															! 
         RH.Nc.EXISTE    = 0																								!
         RH.NC.REGISTRO% = RH.NC.REGISTRO% + 1															!
      EndIf
   EndIf
EndIf


If TS.IO.DATA$(6) = "" Then Begin
   If IR.INDICAT0 AND 40H Then Begin
   EndIf Else Begin                           ! ES POR UNIDAD
 !     TS.IO.KEYS(6)  = 75
 !    TS.IO.DATA$(6) = "1"
   EndIf
EndIf

End Sub 

Sub  FISMTS43.011 Public
String a$
Integer*2 ue.for
Integer*4 Xpos%

If X.Nc.Total$ = "3" Then Begin 																						! Nc Parcial
   a$ = Unpack$(IR.ITEMCODE$)																								! Toma item
   rh.cod.in$ = a$																													!
   If Not(tiene.saldo(a$)) Then Begin																				! Valida si tiene saldo
      ERR.MSG.67$ = "NO TIENE SALDO      POR DEVOLVER...."									! Msg alerta
      TS.LINEDATA = 67																											!
      Call TSCSEC03
      IR.DEPARTME$ = Pack$("0000")
      Exit  Sub 
   EndIf Else Begin																													! Tiene saldo
   If VAL(TS.TEMP6$) > 0 Then                                              \! Retorna todo el Item      
    If IR.INDICAT0 AND 40H Then Begin                                       ! Si Item pesable
    	 SL.IT.XPRICE = Val(ts.temp6$)                       									! Precio total de la venta del Item
    	 SL.IE.SALEPRIC = (float(SL.IT.XPRICE) / FLOAT(SL.IE.QTYORWGT)) * 1000							! Retorna valores a las 
    	 IR.PRICE1 = SL.IE.SALEPRIC																						! variables nativas 
    	 IR.PRICE2 = IR.PRICE1																								! 
       TS.XPRICE = IR.PRICE1																								!
       TS.XXMOD = 0 																												!
       TS.YYMOD = 0																													!
       SL.IE.SALEPRIC2 = SL.IE.SALEPRIC																			! 
    EndIf Else Begin                                       									! Si Item por unidad
!       If Gr.QtyAnl% = SL.IE.QTYORWGT Then Begin														! 

       	  SL.IT.XPRICE = Val(ts.temp6$)                    									! Precio total de la venta del Item

!       EndIf																																!
       TS.IO.DATA$(6) = STR$(SL.IE.QTYORWGT)																!
       If SL.IE.QTYORWGT > 1 Then 																				 \! Calcula precio unitario
          SL.IE.SALEPRIC = (SL.IT.XPRICE / SL.IE.QTYORWGT) Else    				 \! Retorna valores a las 
          SL.IE.SALEPRIC = SL.IT.XPRICE																			! 
    	 IR.PRICE1 = SL.IE.SALEPRIC																						!
    	 IR.PRICE2 = IR.PRICE1																								!
       TS.XPRICE = IR.PRICE1																								!
       TS.XXMOD = 0 																												!
       TS.YYMOD = 0																													!
       SL.IE.SALEPRIC2 = SL.IE.SALEPRIC																			!
    EndIf																																		!
    
    If VAL(TS.TEMP6$) = 0 Then                                             \! Retorno parcial
    If IR.INDICAT0 AND 40H Then Begin                                       ! Si Item pesable
    	 SL.IE.SALEPRIC = Val(ts.temp5$)                       								! Precio unitario Item
    	 SL.IT.XPRICE = (FLOAT(SL.IE.SALEPRIC) * FLOAT(SL.IE.QTYORWGT)) / 1000	  					! total nc
    	 IR.PRICE1 = SL.IE.SALEPRIC																						! variables nativas 
    	 IR.PRICE2 = IR.PRICE1																								! 
       TS.XPRICE = IR.PRICE1																								!
       TS.XXMOD = 0 																												!
       TS.YYMOD = 0																													!
       SL.IE.SALEPRIC2 = SL.IE.SALEPRIC																			! 
    EndIf Else Begin                                       									! Si Item por unidad
       SL.IE.SALEPRIC = Val(ts.temp5$)                       								! Precio unitario Item
    	 SL.IT.XPRICE = (SL.IE.SALEPRIC * SL.IE.QTYORWGT)       	 						! total nc
    	 IR.PRICE1 = SL.IE.SALEPRIC																						! variables nativas 
    	 IR.PRICE2 = IR.PRICE1																								! 
       TS.XPRICE = IR.PRICE1																								!
       TS.XXMOD = 0 																												!
       TS.YYMOD = 0																													!
       SL.IE.SALEPRIC2 = SL.IE.SALEPRIC																			! 
    EndIf																																		!
   EndIf 																																	  
EndIf

If (X.Nc.Total$ = "1" Or X.NC.TOTAL$ = "4") And rh.precioxq <> 0 Then Begin ! Nc Total
   SL.IT.XPRICE = rh.precioxq																								!
EndIf																																				!

If (X.Nc.Total$ = "1" Or X.NC.TOTAL$ = "4") Then Begin                      ! Nc Total
    If RH.NC.REGISTRO% > RH.NC.FINAL% Then Begin														!
  	   Xpos% = RH.NC.REGISTRO% - 1																					!
    EndIf Else Xpos% = RH.NC.REGISTRO%																			!

    If IR.INDICAT0 AND 40H Then Begin    																		! Si Item pesable
       Gr.DsctoNc% = Val(tslinjecd$(Xpos%,0))																! Dscto appl
       If tslinjecd$(Xpos%,1) <> "OR2" Then                                \! Dscto compleja 
          Gr.DsctoNcPtg% = Val(tslinjecd$(Xpos%,1)) Else                   \! Ptg dscto aplicado
          Gr.DsctoNcPtg% = 0																								! Dscto 0 para complejas
       If tslinjecd$(Xpos%,1) = "OR2" Then Begin    												! Es promo compleja Or
       	  SL.IT.XPRICE = SL.IT.XPRICE - Val(tslinjecd$(Xpos%,2))            ! Retira dscto total
       EndIf Else Begin 
       	  SL.IT.XPRICE = Val(mid$(tslinjec$(Xpos%), 14, 8))                 ! Venta total
       EndIf
    	 SL.IE.SALEPRIC = (FLOAT(SL.IT.XPRICE) / FLOAT(SL.IE.QTYORWGT)) * 1000 !
    	 IR.PRICE1 = SL.IE.SALEPRIC																						!
    	 IR.PRICE2 = IR.PRICE1																								!
       TS.XPRICE = IR.PRICE1																								!
       TS.XXMOD = 0 																												!
       TS.YYMOD = 0																													!
       SL.IE.SALEPRIC2 = SL.IE.SALEPRIC																			!
    EndIf Else Begin                     																		! Si Item por unidad
       Gr.DsctoNc% = Val(tslinjecd$(Xpos%,0))                               ! Valor del dscto
       If tslinjecd$(Xpos%,1) <> "OR2" Then                                \! Dscto complejas Oracle
          Gr.DsctoNcPtg% = Val(tslinjecd$(Xpos%,1)) Else                   \! 
          Gr.DsctoNcPtg% = 0																								!
       If tslinjecd$(Xpos%,1) = "OR2" Then Begin    												! Es promo compleja Or
       	  SL.IT.XPRICE = SL.IT.XPRICE - Val(tslinjecd$(Xpos%,2))            ! Retira dscto total
       EndIf Else Begin 
       	  SL.IT.XPRICE = Val(mid$(tslinjec$(Xpos%), 14, 8))                 ! Venta total
       EndIf
       SL.IE.SALEPRIC = (SL.IT.XPRICE / SL.IE.QTYORWGT)                     ! Precio unitario
    	 IR.PRICE1 = SL.IE.SALEPRIC																						!
    	 IR.PRICE2 = IR.PRICE1																								!
       TS.XPRICE = IR.PRICE1																								!
    EndIf																																		!
    
EndIf																																				! Fin control NC

If (X.Nc.Total$ = "1" Or X.NC.TOTAL$ = "4") < 0 Then Begin
   Gr.Mkp.PrUnit%   = (FLOAT(SL.IE.SALEPRIC)/(1.+FLOAT(UE.TABLAIVA(RH.DSC.TARIfA%,0))/100.)) ! Precio unitario para almacenar
EndIf

Gr.Graba.Dato% = -1

End Sub 

Sub  FISMTS14.012 Public
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit  :  FISTSU14.ECU                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
String Xtmp1$, Xtmp2$, Xtmp3$

If Asc.Proy.Fiscal% = -1  Then Begin                               ! Proyecto activo
	
 TS.TEMP5$ = " "+Str$(TS.IO.MOTORKEY) + " "
 If (TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97) Or                       \! 
    (MATCH(TS.TEMP5$," 176 177 180 175 ",1) > 0) Then                    \! 
   If TS.TOTALS(0,0,0) >= 20000 And Asc.Fis.Cliente$ = "" Then Begin 			! Si vta mayor Us 200 y Sin dato cliente 
	    Call VISOR.AND.BORRAR("DEBE CAPTURAR DATO  DEL CLIENTE  /Borrar")	  ! Mensaje de alerta
	    Dim TS.IO.KEYS(10) : Dim TS.IO.DATA$(10)														!
	    TS.IO.MOTORKEY = 73																									!
	    Exit Sub 
   EndIf

   If Not TS.INTRX Then \                                          ! No esta dentro de una trx
      If TS.IO.KEYS(4) = 100 AND Val(TS.IO.DATA$(2)) = 80 Then \   ! Reporte de cuadre de caja
         Exit  Sub 

   If TS.IO.KEYS(4) = 100 AND Val(TS.IO.DATA$(2)) = 80  AND \
      TS.IO.KEYS(8) = 79 Then \                                    ! Intercambio de Gaveta
      Exit  Sub 

   If TS.IO.KEYS(4) = 100 AND TS.IO.DATA$(2) <> "" Then Begin      ! Consulta de Precio
      If TS.IO.DEVICE = 3 Then \                                   ! Si escanea producto
         TS.IO.DATA$(2) = Left$(TS.IO.DATA$(2), Len(TS.IO.DATA$(2)) - 1)
      Asc.ITEMCODE$ = Pack$(Right$("000000000000" + TS.IO.DATA$(2), 12))
      TS.ER.RETURN  = -1
      If TS.STANDALONE Then Begin                                  ! Si terminal OFFLINE
         %INCLUDE EAMIRRD5.J86                                     ! Lectura de registro EAMIMAGE
      EndIf \
      Else Begin                                                   ! Si esta en linea
         %INCLUDE EAMIRRD4.J86                                     ! Lectura EAMITEMR
      EndIf

      If TS.ER.RETURN = -1 Then Begin                              ! Si lo encontre
         If Unpack$(Asc.Indicat2$) = "03" Then Begin               ! Con precio reducido
            XTMP1$ = Unpack$(Asc.SALEPRIC$)                        ! Desempaqueta precio
            Xtmp3$ = Left$(Xtmp1$, 5)                               ! Toma primeros 5
            Call Format.Amount(Val(Xtmp3$))                        ! Formateo el Valor
            XTmp1$ = Right$("        " + TS.TEMP1$, 08)               ! Ajusto Precio
            XTMP1$ = "*VP*" + XTMP1$
            XTMP2$ = Unpack$(Asc.SALEPRIC$)                        ! Desempaqueta precio
            Xtmp3$ = Right$(Xtmp2$, 5)                              ! Toma ultimos 5
            Call Format.Amount(Val(Xtmp3$))                        ! Formateo el Valor
            XTmp2$ = Right$("        " + TS.TEMP1$, 8)                ! Ajusto Precio
         EndIf \
         Else Begin                                                ! Precio Unitario
            XTMP1$ = Unpack$(Asc.SALEPRIC$)                        ! Desempaqueta precio
            Call Format.Amount(Val(Xtmp1$))                        ! Formateo el Valor
            XTmp2$ = Right$("          " + TS.TEMP1$, 10)
            XTMP1$ = "   *VP*   "
         EndIf
         Call Visores4690(1, ASC.ITEMNAME$, Xtmp1$ + Xtmp2$, 0, "L")
      EndIf \
      Else Begin                                                   ! No encontrado
         Call TSHIECET
         Call Visores4690(1, "ARTICULO NO EXISTE", "EN MAESTRA PRODUCTOS", 0, "L")
      EndIf
      TS.IO.MOTORKEY = 0
      Dim TS.IO.DATA$(10)
      Dim TS.IO.KEYS(10)
      Exit  Sub 
   EndIf

   If NOT(TS.INTRX) Then \
      If (TS.IO.KEYS(5) = 61) AND (TS.IO.STATE = 10) Then Begin
         If Val(TS.IO.DATA$(5)) = 6 Then Begin   ! Validacion de precios
            Exit  Sub 
         EndIf
         If Val(TS.IO.DATA$(5)) > 0 AND Val(TS.IO.DATA$(5)) < 12 Then Begin
            ASIC.LLAVE% = 0
            Call ASIC.DATOS.BASE$("GIRE LA LLAVE DEL", "SUPERVISOR...")
            If ASIC.LLAVE% = 0 Then Begin
               TS.IO.MOTORKEY = 0     ! Inicializa secuencia
               DIM TS.IO.KEYS(10)     ! de tecleo y reporta
               DIM TS.IO.DATA$(10)    ! Error
               TS.GUIDANCE = 1004     ! Msg Procedimiento no Valido
               Exit  Sub 
            EndIf
         EndIf
      EndIf

   If TS.INTRX Then                                               \ ! En una venta
      If TS.IO.MOTORKEY > 91 AND TS.IO.MOTORKEY < 97 Then Begin     ! Si forma de pago
         If TS.IO.DATA$(7) = "" Then Begin                          ! No ingreso Valor pago
            TS.IO.MOTORKEY = 0                                      ! Inicializa secuencia
            DIM TS.IO.KEYS(10)                                      ! de tecleo y reporta
            DIM TS.IO.DATA$(10)                                     ! Error
            TS.GUIDANCE = 1003                                      ! Msg Procedimiento no Valido
            Exit  Sub 
         EndIf
      EndIf

   If NOT(TS.INTRX) Then \
      If TS.IO.KEYS(4) = 100 AND TS.IO.KEYS(7) = 91 Then Begin        ! Secuencia contenido gaveta
          Exit  Sub 
      EndIf

   If TS.IO.KEYS(4) = 100 AND TS.IO.KEYS(2) = 80 Then Begin        ! Secuencia Apertura Gaveta
      If TS.IO.DATA$(2) <> ""  Then Begin                          ! Validacion de precios
         Exit  Sub 
      EndIf
      TS.IO.MOTORKEY = 0     ! Inicializa secuencia
      DIM TS.IO.KEYS(10)                                          ! de tecleo y reporta
      DIM TS.IO.DATA$(10)                                         ! Error
      TS.GUIDANCE = 1094                                          ! Msg Procedimiento no permitido
      Exit  Sub 
   EndIf
EndIf
End Sub 

Sub  FISMTS14.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU14.ECU                                         **
!   **    Inclu¡r en :  EAMTSU14.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo

   %Include FISTSU14.ECU

EndIf
End Sub 

Sub  FISMTS20.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU20.ECU                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo

   %Include FISTSU20.ECU
EndIf
End Sub 

Sub  FISMTS20.012 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU20.EC2                                         **
!   **    Inclu¡r en :  EAMTSU20.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo

   %Include FISTSU20.EC2
EndIf
End Sub 

Sub  FISMTS32.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU32.ECU                                         **
!   **    Inclu¡r en :  EAMTSU32.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                   ! Proyecto activo
   If TS.IO.KEYS(7) - 90 = 2 Then Begin                                ! Si pago con
      If Val(TS.IO.DATA$(3)) <= 1 Then Begin                           ! cheque si
         Asc.Fis.ChequeSi$ = TS.IO.DATA$(9)                            ! Toma dato del cliente
         Asc.Fis.ImpChequeSi% = -1
      EndIf
   EndIf
EndIf

 If TS.IO.KEYS(7) > 90 AND TS.IO.KEYS(7) < 97 Then Begin                  	! Si forma de pago 
   TS.TEMP3$ = Str$(TS.IO.KEYS(7) - 90)                                   	! Toma forma de pago
   If TS.IO.DATA$(3) = "" Then TS.TEMP3$ = TS.TEMP3$ + "1" Else            \! Toma variedad de pago 
      TS.TEMP3$ = TS.TEMP3$ + TS.IO.DATA$(3)                              	!  
   If TS.TEMP3$ = "51" Then Begin																						! Si forma de pago NC
   	  If Len(TS.IO.DATA$(9)) <> 15 Then Begin
       TS.TEMP5$ = ASIC.DATOS$("NUMERO DE NOTA DE  ","CREDITO Intro/Borrar")! Captura dato de retención
       If (TS.TEMP5$ = "") OR (TS.TEMP5$ = "E") Then Begin      						! No se capturo dato
           TS.USER.RETURN = 99
           TS.LINEDATA = 51                 																! "TARJETA INVALIDA"            
           TS.STACKERR(0) = 0                                               ! manager's override required   
           TS.STACKERR(3) = 0                                               ! item descriptor               
           TS.STACKERR(6) = 0                                               ! put "M" on last display       
           TS.STACKERR(7) = -1                                              ! indicate no printing required 
           TS.MO.REASON = 0                                                 ! invalid key with department   
           Call TSCSEC03       																							!
           TS.IO.MOTORKEY = 0 							
          Exit Sub                                             							! Sale de la rutina
       EndIf																																!
       If Len(TS.TEMP5$) <> 15 Then Begin																		! Valida longitud documento origen
       	  Call VISOR.AND.BORRAR("NUMERO DOCUMENTO NO VALIDO       /Borrar")	!
           TS.USER.RETURN = 99
           TS.LINEDATA = 51                 																! "TARJETA INVALIDA"            
           TS.STACKERR(0) = 0                                               ! manager's override required   
           TS.STACKERR(3) = 0                                               ! item descriptor               
           TS.STACKERR(6) = 0                                               ! put "M" on last display       
           TS.STACKERR(7) = -1                                              ! indicate no printing required 
           TS.MO.REASON = 0                                                 ! invalid key with department   
           Call TSCSEC03       																							!
           TS.IO.MOTORKEY = 0 							
          Exit Sub                                             							! Sale de la rutina       	  
       EndIf   																															! 
      EndIf Else TS.TEMP5$ = TS.IO.DATA$(9)																	! 
      TS.IO.DATA$(9) = TS.TEMP5$																						!
      TS.ACNUM$      = TS.TEMP5$																						! 
      Exit Sub 
   EndIf																																		!
 EndIf

End Sub 

Sub  FISMTS56.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU56.ECU                                         **
!   **    Inclu¡r en :  EAMTSU56.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------

If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo

   %Include FISTSU56.ECU
   
EndIf
End Sub 

Sub  FISMTS60.011 PUBLIC
!------------------------------------------------------------------------------
!   ***************************************************************************
!   **    Fecha      :  Marzo de 2.004                                       **
!   **    Proyecto   :  NUMERACION FISCAL                                    **
!   **    User Exit   :  FISTSU60.ECU                                         **
!   **    Inclu¡r en :  EAMTSU60.J86                                         **
!   **    Programa   :  EAMTSUPC.BAS                                         **
!   **    Executable :  EAMTS10L.286                                         **
!   **                                                                       **
!   ***************************************************************************
!
!------------------------------------------------------------------------------
!Integer*2 GRI%
!String    GRSRI$, GRDESC$, GRTIME$, GRVLR$, GRPLAZO$

If Asc.Proy.Fiscal% = -1  Then Begin                                         ! Proyecto activo

   If Asc.Fis.ImpChequeSi% = -1 Then Begin                             ! Si impresion dato de cliente
      TO.USERExit (20) = 0
      TO.USERExit (60) = 0
      TS.TEMP1$ = Left$(("IDENTIF. :" + Asc.Fis.ChequeSi$) + STRING$(37, " "), 37)
      Call U.IMPRIME(TS.TEMP1$, 6100H)
      Call Valida.Ruc.Cheque.Si(Asc.Fis.ChequeSi$)                    ! Busca dato del cliente
      TS.TEMP1$ = Left$(("USUARIO  :" + Asc.Fis.ClienteSi$) + STRING$(37, " "), 37)
      Call U.IMPRIME(TS.TEMP1$, 6100H)
      Asc.Fis.ChequeSi$    = ""
      Asc.Fis.ImpChequeSi% = 0
      Asc.Fis.ClienteSi$   = ""
      TO.USERExit (20) = -1
      TO.USERExit (60) = -1
  EndIf

!  %INCLUDE FISTSU60.ECU

EndIf
End Sub 

!'------------------------------------------------------------------------------
!'   ***************************************************************************
!'* RHISCOM 06/11/2009 15:35:48
!'   **    User Exit   :  EAMTSU20.J86                                         **
!'   ***************************************************************************
!'
!'------------------------------------------------------------------------------

Sub FISMTS02.RH Public
String aString$

rh.user%      = 0
rh.standalone = 0
rh.secuencia$ = ""

If Gr.Fiscal.ImpDoc% = -1 Then Begin 	
   Call GROUP.SORT.DI
   PAG.INDEX$ = "1"
   f.rprint = 0
   aString$ = "1"														! Genera documento
   PAG.ACTUAL% = 1
   COUNTER     = 0
   Call WRITE.HEADER.DI(1)
   If (enable.Imptot%) Then \
      Call DI.WRITE(8) \
   Else \   
      Call DI.WRITE(12) 
   Call WRITE.TRAILER.DI(f.rprint, 2)

EndIf
rh.ya.imprimi% = 0
Call DI.BUFFER(0) 													! Inicializa String datos
End Sub 

Sub  FISMTS20.RH Public
Integer*1 fReimpresion, \
          index
String aString$

If Not(enable.fact%) Then Exit  Sub 

If (TS.LINETYPE = 6  and TS.LINEDATA = 1) and TS.STANDALONE Then \
   Asc.Fis.Cliente$ = ""

!If (TS.LINETYPE = 6  And TS.LINEDATA = 2) Then fRef% = 1
!If (TS.LINETYPE = 29 And TS.LINEDATA = 1) And (fRef%) Then Begin
!   reference$ = TS.PRTBUF$
!   fRef% = 0
!EndIf

End Sub 

!'------------------------------------------------------------------------------
!'   ***************************************************************************
!'* RHISCOM 06/11/2009 15:35:48
!'   **    User Exit   :  EAMTSU01.J86                                         **
!'   ***************************************************************************
!'
!'------------------------------------------------------------------------------
Sub  FISMTS01.RH Public
string astring$
If Not(enable.fact%) Then Exit  Sub 

Call DI.BUFFER(0)
f.Factura = 0
End Sub 

Sub  FISMTS53.RH Public

If Not(enable.fact%) Then Exit  Sub 

If flag.paso = 0 Then Begin
   Call DI.BUFFER(0)
   f.Factura = 0
EndIf
End Sub 

Sub  FISMTS53.011 Public
string rh.user$, rh.user1$

If rh.sus = 0 Then Begin
   If Unpack$(Left$(SL.STR.ENTRY$, 1)) = "00" Then Begin            ! Si User Data
      rh.secuencia$ = ""
      rh.user% = 1
   EndIf   
   
   If Unpack$(Left$(SL.STR.ENTRY$, 1)) = "01" Then Begin            ! Si User Data
      rh.secuencia$ = ""
      rh.user% = 1
      Gr.Graba.Dato% = -1
   EndIf   

   If Unpack$(Left$(SL.STR.ENTRY$, 1)) = "05" Then Begin            ! Si User Data
      rh.user% = 1
      rh.secuencia$ = "000"
   EndIf
   If Unpack$(Left$(SL.STR.ENTRY$, 1)) = "06" Then Begin            ! Si User Data
      rh.user% = 1
      rh.secuencia$ = "000"
   EndIf
   If Unpack$(Left$(SL.STR.ENTRY$, 1)) = "99" Then Begin            ! Si User Data
      RH.Tmp% = 3                                          					! Apuntador String
      rh.user1$ = RH.GETUNPK(SL.STR.ENTRY$, RH.Tmp%) 								! CODIGO ARTICULO
      If rh.user1$ = "20040903" Then Begin
         rh.secuencia$ = RH.GETUNPK2(SL.STR.ENTRY$, RH.Tmp%) 				! CODIGO ARTICULO
         rh.user% = 1
      EndIf
   EndIf
EndIf
End Sub 

Sub  FISMTS56.RH Public

If Not(enable.fact%) Then Exit  Sub 
flag.paso = 0

End Sub 

Sub  FISMTS57.RH Public

If Not(enable.fact%) Then Exit  Sub 

Call DI.BUFFER(0)
f.Factura = 0

End Sub 

!'------------------------------------------------------------------------------
!'   ***************************************************************************
!'* RHISCOM 17-11-2009 11:35:04
!'   **    User Exit   :  EAMTSU07.J86                                         **
!'   ***************************************************************************
!'
!'------------------------------------------------------------------------------
Sub  FISMTS07.RH Public
String aString$, \
       paso1$,   \
       paso2$


flag.paso = 0
Open "R::$SCNTRL" AS 38 NOWRITE NODEL
If End #38 Then RH.UE.FINAL
While (1)
   Read #38; TS.TEMP1$
   If TS.TEMP1$ = "[RHCONF]" Then Begin
      Read #38; TS.TEMP1$
      enable.Corr% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      enable.Fact% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      enable.nc% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      enable.rf% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      enable.4d% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      enable.is% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      Num.puerto% = Val(Mid$(TS.TEMP1$, 30, 1))
      Read #38; TS.TEMP1$
      RH.POS.Hab$ = Mid$(TS.TEMP1$, 30, len(TS.TEMP1$) - 29)
      Read #38; TS.TEMP1$
      RH.Monto.Factura% = Val(Mid$(TS.TEMP1$, 30, len(TS.TEMP1$) - 29))

      GoTo RH.UE.FINAL
   EndIf
Wend
rh.ya.imprimi% = 1
rh.standalone  = 0

RH.UE.FINAL:
Close 38

enable.pos% = 1

Call Version.DIfact							! Version del RHDIfACT

End Sub 

Sub  PLUTSU20.RH Public
If Not(enable.fact%) Then Exit  Sub 

If (TS.LINETYPE = 1) AND (TS.PROCEDURE < 1) Then Begin
   Call DI.BUFFER(1)
EndIf

End Sub 


Sub  FISMTS63.011 PUBLIC                                                                    ! Para Rebajar saldos
String codigo$, cod.in$, tipoart$, precio$, metodo$, precio.un$
Integer*2 ue.for
Integer*4 Cantidad, Monto
Real GrUnit%

If Not(enable.nc%) Then Exit  Sub 

If (Ts.Linetype = 1 and Ts.Linedata = 0) and X.Nc.Total$ = "3" Then Begin                 ! Nc Parcial
   for ue.for = 1 to Val(NC.SALDO$(0))
      If (left$(NC.SALDO$(ue.for), 2) = "01") Then Begin
         Cantidad = 0
         GoSub  Obtiene.Datos
         If (tipoart$ = "00" or tipoart$ = "03" or tipoart$ = "01" Or Left$(tipoart$,1) = "8") Then Begin            ! si es venta
            If Val(rh.cod.in$) = Val(codigo$) Then Begin                                  ! mismo codigo
               GoSub  Obtiene.Datos1
               If SL.IE.QTYORWGT <= Cantidad Then Begin                                   ! si le queda saldo
                Gr.Mkp.PrUnit%   = (FLOAT(SL.IE.SALEPRIC)/(1.+FLOAT(UE.TABLAIVA(RH.DSC.TARIfA%,0))/100.)) ! Precio unitario para almacenar
                If Cantidad = 0 Then Begin
                 	SL.IT.XPRICE = monto
                EndIf
                cantidad = cantidad - SL.IE.QTYORWGT
                monto    = monto - SL.IT.XPRICE
                
                NC.SALDO$(ue.for) = Left$(mid$(NC.SALDO$(ue.for), 1, busca2) +              \
                                    Str$(monto) +                                           \
                                    Mid$(NC.SALDO$(ue.for), busca3, busca8 - busca3 + 1) +  \
                                    Str$(cantidad) + " " + precio.un$ + String$(100, " "), 100)
               ue.for = Val(NC.SALDO$(0))
               
               EndIf
            EndIf
         EndIf
      EndIf
   Next ue.for
EndIf
Exit  Sub 

Obtiene.Datos:
Busca1     = match(" ", NC.SALDO$(ue.for), 1)
Busca2     = match(" ", NC.SALDO$(ue.for), Busca1 + 1)
Busca3     = match(" ", NC.SALDO$(ue.for), Busca2 + 1)
Busca4     = match(" ", NC.SALDO$(ue.for), Busca3 + 1)
Busca5     = match(" ", NC.SALDO$(ue.for), Busca4 + 1)
Busca6     = match(" ", NC.SALDO$(ue.for), Busca5 + 1)
Busca7     = match(" ", NC.SALDO$(ue.for), Busca6 + 1)
Busca8     = match(" ", NC.SALDO$(ue.for), Busca7 + 1)
Busca9     = match(" ", NC.SALDO$(ue.for), Busca8 + 1)
Busca10    = match(" ", NC.SALDO$(ue.for), Busca9 + 1)
tipoart$   = mid$(NC.SALDO$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1)
codigo$    = mid$(NC.SALDO$(ue.for), Busca1 + 1, Busca2 - Busca1 - 1)
cantidad   = Val(mid$(NC.SALDO$(ue.for), Busca8 + 1, Busca9 - Busca8 - 1))
precio.un$ = mid$(NC.SALDO$(ue.for), Busca9 + 1, Busca10 - Busca9 - 1)
monto      = Val(mid$(NC.SALDO$(ue.for), Busca2  + 1, Busca3 - Busca2 - 1))
cod.in$    = unpack$(IR.ITEMCODE$)

Return

Obtiene.Datos1:

If left$(NC.SALDO$(ue.for + 1), 2) = "02" Then Begin
   Busca11 = match(" ", NC.SALDO$(ue.for + 1), 1)
   Busca12 = match(" ", NC.SALDO$(ue.for + 1), Busca11 + 1)
   Busca13 = match(" ", NC.SALDO$(ue.for + 1), Busca12 + 1)
   Busca14 = match(" ", NC.SALDO$(ue.for + 1), Busca13 + 1)
   Busca15 = match(" ", NC.SALDO$(ue.for + 1), Busca14 + 1)
   Busca16 = match(" ", NC.SALDO$(ue.for + 1), Busca15 + 1)
   Busca17 = match(" ", NC.SALDO$(ue.for + 1), Busca16 + 1)
   metodo$ = mid$(NC.SALDO$(ue.for + 1), Busca13 + 1, Busca14 - Busca13 - 1)
   precio$ = mid$(NC.SALDO$(ue.for + 1), Busca15 + 1, Busca16 - Busca15 - 1)
   If metodo$ = "3" Then Begin
      precio$   = right$("0000000000" + precio$, 10)
   EndIf
EndIf 

Return

End Sub 

Sub Fistsu02.Gr Public
If Gr.Fiscal.Tktnf% = -1 Then Begin																					! Si eliminacion tiquete fiscal
   Gr.RsmTkt.Ind% = -1																											! Imprime tiquete fiscal
   EP.ETO.PRINT.POST.TRANS = 0																							! 
EndIf
End Sub 

Sub Fistsu02B.Gr Public
If Gr.Fiscal.Tktnf% = -1 Then Begin																					! Si eliminacion tiquete fiscal
   Gr.RsmTkt.Ind% = 0 																											! Imprime tiquete fiscal
   EP.ETO.PRINT.POST.TRANS = -1																						  ! 
EndIf
End Sub 

Sub Fistsu20.Gr Public
If Gr.Fiscal.Tktnf% = -1 Then Begin																					! Si eliminacion tiquete fiscal
 If match("NUMERO DE RECUPERACION",TS.PRTBUF$,1) > 0 Then Begin							! Trx Suspendida
 	  Gr.NoTkt.Suspend% = -1
 EndIf
 If match("TRANSACCION ANULAD",TS.PRTBUF$,1) > 0 Then Begin									! Trx Suspendida
 	  Gr.NoTkt.Suspend% = -2
 EndIf
 
EndIf

End Sub 

