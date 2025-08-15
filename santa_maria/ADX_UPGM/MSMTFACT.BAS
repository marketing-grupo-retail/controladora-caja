!-------------------------------------------------------------------------------
!Modulo de impresion de factura MSM
!Fecha: 13 de marzo de 2023
!Autor: Alejandro Farre P.
!-------------------------------------------------------------------------------
! Mod 20/Mzo/2024
! Se adiciona impresion parámetros codigo QR para mensaje fidelizacion clientes
! desarrollado por Grupo Retail - OVS
!-------------------------------------------------------------------------------

%ENVIRON T

String \
   lineasCabecera$(1), \         !Datos de impresion de la cabecera de factura
   lineasFinales$(1), \          !Datos de impresion del final de factura
   lineasPagare$(1),  \          !Datos de impresion de pagare de factura
   pagosSRI$(2), \               !Datos de formas de Pago SRI
   mapeoPagosSRI$(2), \          !Mapeo de formas de Pago SRI
   detallePagosSRI$(2), \        !Detalle de formas de Pago SRI
   deducibles$(1), \             !Nombres de los grupos de deducibles
   impuestos$(2), \              !Contiene porcentajes de impuestos
   teclasPago$, \                !Contiene los codigos de teclas de pago
   lineasDescuento$, \           !Contiene lineas con descuentos para SJ
   MSMlogo1$,        \           !Comando para imprimir el logo numero 1
   MSMlogo2$                     !Comando para imprimir el logo numero 2

Integer*1 \
   noMasVenta, \                 !Indica que no se deben vender mas articulo
   inicioFactura, \              !Indica si se ha iniciado la transaccion
   lineasCabecera.estado, \      !Indica si esta habilitado la impresion de cabecera 0: Desactivado
   lineasFinales.estado, \       !Indica si esta habilitado la impresion de lineas finales 0: Desactivado
   lineasPagare.estado, \        !Indica si esta habilitado la impresion de lineas pagare 0: Desactivado
   establecimiento.estado, \     !Indica si esta habilitado la impresion de parametros fiscales 0: Desactivado
   impuestos.estado, \           !Indica si esta habilitado la lectura de parametros de impuestos 0: Desactivado
   pagosSRI.estado, \            !Indica si esta habilitado la impresion de detalle de pagos SRI  0: Desactivado
   deducibles.estado, \          !Indica si esta habilitado la impresion de deducibles  0: Desactivado
   deducibles.size, \            !Indica la cantidad de grupos de deducibles
   lineasCabecera.size, \        !Indica la cantidad de lineas de cabecera de la factura
   lineasFinales.size, \         !Indica la cantidad de lineas finales de la factura
   lineasPagare.size, \          !Indica la cantidad de lineas de pagare de la factura
   impuestos.size, \             !Indica la cantidad de impuestos
   pagosSRI.size, \              !Indica la cantidad de pagos SRI
   mapeoPagosSRI.size, \         !Indica la cantidad de mapeo de pagos SRI
   impuestos.iva, \              !Porcentaje de IVA
   artCanasta, \                 !Indicador de articulo canasta
   venCanasta, \                 !Indicador de articulo canasta vencida
   tipoDocumento, \              !Tipo de documento  ! 01  Factura de Venta, 04  Nota Credito, 05  Nota Debito, 06  Guia de Remision, 07  Comprobante Retencion
   tipoResumen, \                !Indicador de resumen a imprimir: 1 trx Anulada, 2 trx suspendida
   trxRecuperada                 !Indicador de transaccion recuperada

String \
   terminal.clave$, \            !Numero de la terminal
   terminal.tipo$, \             !Tipo de Maquina
   terminal.serie$, \            !Serial de la terminal
   terminal.resolucion$, \       !Numero resolucion
   terminal.fechaEmision$, \     !Fecha Emision Autorizacion
   terminal.prefijo$, \          !Prefijo de facturacion
   terminal.autorizacion$, \     !Numero Autorizacion
   terminal.fechaAutorizacion$,\ !Fecha Autorizacion
   establecimiento.codigo$, \    !Establecimiento
   establecimiento.ambiente$, \  !Tipo de ambiente
   establecimiento.emision$, \   !Tipo de Emision
   establecimiento.secuencia$, \ !Secuencia
   establecimiento.ruc$,        \!Ruc
   llaveAccesoSRI$               !Clave de acceso SRI

Integer*4 \
   terminal.secFactura, \        !Numero Actual de Facturacion
   terminal.otrosParametros, \   !otros parametros de la terminal
   numeroNotaCredito, \          !Numero de nota de credito
   expCanasta, \                 !Fecha expiracion canasta
   afeCanasta                    !Monto afecto canasta

Integer*1 \
   anula400, \                   !Indicador de anulacion de venta sobre USD 400
   tieneBalanza, \               !Indicador se tiene una balanza conectada
   barraBalanza                  !Indicador de lectura de codigo de barra de balanza

Integer*4 \
   limiteAnulacion, \            !Monto limite de anulacion para pedir doble autorizacion
   limiteConsFinal               !Monto limite de venta a consumidor final

!-- Variables Delivery
Integer*1 Global Gr.Domi.Graba%,               \! Grabacion UD
                 Gr.Domi.Delivery%              ! ID Delivery
Integer*2 Global Gr.Domi.Ptg%                   ! Ptg domiciliario
String    Global Gr.Domi.Pedido$,              \!
                 Gr.DeUna.Ptg$
Integer*4 Global GR.IVA.DEVOL%,                \! Devolucion IVA
                 Gr.DeUna.Dscto%
String    Global Gr.DeUna.DcItm$(2)             ! Dscto items deuna
                 
!-------------------------------------------------------------------------------
! arreglos para almacenar el resumen de productos ingresados en la transaccion
!-------------------------------------------------------------------------------
%INCLUDE RESPROD.J86

Integer*4 \
   resTotImp%(1)    !Arreglo que contiene los totales por tarifa
                    ! 0 Base imponible no afecto a impuestos (monto total tarifa 0%)
                    ! 1 Base imponible afecto a impuestos (monto total sin impuesto tarifa 12%)
                    ! 2 Valor del impuesto (impuesto total tarifa 12%)

Integer*4 \
   resTotDes%(1)    !Arreglo que contiene los totales de descuento por tarifa
                    ! 0 Base imponible no afecta a impuestos (descuento total tarifa 0%)
                    ! 1 Base imponible afecta a impuestos (descuento total sin impuesto tarifa 12%)
                    ! 2 Valor del impuesto (impuesto del descuento total tarifa 12%)

Integer*4 \
   resTotDed%(1)    !Arreglo que contiene los totales deducibles
                    ! elemento 1 Vestimenta
                    ! elemento 2 Alimentacion
                    ! elemento 3 Medicinas
                    ! elemento 4 Educacion

Integer*2 \
   resProdDed%(1),  \ tipo de deducible del producto: 0 no deducible, 1 Vestimenta, 2 Alimentacion, 3 Medicina, 4 Educación
   resProdTsl%(1),  \ indice a la cadena 99 01 en el Tsl
   indiceActual     ! indice actual

Integer*1 noCadena1,       \ indicador para no agregar cadena 1
          noCadena2,       \ indicador para no agregar cadena 2
          noCadena10       ! indicador para no agregar cadena 10

String    resProd99$,      \ cadena 9901
          resDescuentos$   ! Resumen de descuentos

Integer*4 totCanasta       ! numero total de canastas vendidas


!-------------------------------------------------------------------------------
!Otros variables que contienen datos de la transaccion
!-------------------------------------------------------------------------------
String    lineaDatosTrx$
Integer*2 terminal.indice  ! Indice de registro con informacion de terminal

!--- Varaiables impresion QR fidelizacion
String    Global Gr.Clqr.Term$,                                            \! Lista terminales QR
                 Gr.Clqr.Url$,                                             \! URL a generar 
                 Gr.Clqr.Msg1$,                                            \! Mensaje 1
                 Gr.Clqr.Msg2$,                                            \! Mensaje 2
                 Gr.Clqr.Msg3$                                              ! Mensaje 3

!-------------------------------------------------------------------------------
! variables globales de otros modulos
!-------------------------------------------------------------------------------
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMITEMR.J86
%INCLUDE EAMTRANS.J86
%INCLUDE EAMTERMS.J86
%INCLUDE NUOTVARI.J86
%INCLUDE JAVAGUIV.J86
%INCLUDE EAMTOPTS.J86

! Contiene la informacion de descuentos ya aplicados
! se define global para simplificar su uso al distribuir los descuentos
String global desAplic(1)

Integer*1 global TS.RETV.IN.PROGRESS, TS.RECOVERY

Integer*2 global \
   GC.SL.WARN,   \ warning threshold for buffer nearly full
   SL.END,       \ last index to be used in summary log
   SL.END.RCOVRD ! # OF StringS RECOVERED

String global    \
   IR.USERDATA$  ! extension maestro de articulos

!String GLOBAL                 \
!    TMCFR.FORM$,           \ TMCFR Header Form
!    TMCFRR.FORM$           ! TMCFR Maint Rec Form
!Integer*1 GLOBAL              \
!    GA.TMCFR.IGNORE        ! Ignore Item Rec TOF Flag (Use All)
!Integer*2 GLOBAL              \
!    GA.TMCFR.RECL          ! TMCFR Rec Length

!-------------------------------------------------------------------------------
! Rutinas publicas de otros modulos
!-------------------------------------------------------------------------------
%INCLUDE NUOTRUTI.J86
%INCLUDE NUOTDIVA.J86
%INCLUDE NUOTELEC.J86
%INCLUDE NUOTPROM.J86
%INCLUDE JAVAGUIC.J86
%INCLUDE EAMASMCT.J86
%INCLUDE NUOTJAVA.J86
%INCLUDE MSMTCLIE.J86

!para grabar en el journal electronico
FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
   String QXL.TSUPEC21, PRTLINE
FEND

!-- Impresion logo imagen para modelos viejos impresoras 4610
Sub Impresion.Logo(X.I%) External                                						! Rutina impresion logos
INTEGER*1 X.I%, X.J%                                              					! Variable temporal
End Sub 

!------------------------------------------------------------------------
! PAD INPUT DESCRIPTOR TO SPECIFIED NO OF CHARS
! CONVERT AMOUNT FIELD TO DOLLAR String
! MERGE AMOUNT FIELD AND DESCRIPTOR
! retorna la linea formateada en TS.TEMP1$
! ejemplo FORMAT.LINE( "TOTAL", 1234, 20 )
! TS.TEMP1$ = "TOTAL          1.234"
!------------------------------------------------------------------------
FUNCTION FORMAT.LINE( DESC$, AMT, LGTH ) EXTERNAL
   Integer*1 FORMAT.LINE
   String    DESC$
   Integer*4 AMT
   Integer*2 LGTH
END FUNCTION

!------------------------------------------------------------------------------
! To read weight from scale
! TS.TEMP1I4 gives weight on return
!------------------------------------------------------------------------------
SUB TSHIEC05 EXTERNAL       ! read weight from scale
END SUB

!------------------------------------------------------------------------------
! Retorna linea con datos de factura
! se debe llamar despues de la impresion de la factura
!------------------------------------------------------------------------------
function lineaDatosFactura$ public
   String lineaDatosFactura$
   lineaDatosFactura$ = establecimiento.codigo$ + "-" + TS.TERMINAL$ + "-" + \
      right$( String$( 9, "0" ) + str$( terminal.secFactura ), 9 )
end function

!------------------------------------------------------------------------
!Disponibiliza la linea con fecha hora tienda caja transaccion operador
!------------------------------------------------------------------------
sub getLinetrailer( m$ ) public
    String m$
    m$ = lineaDatosTrx$
end sub

!Disponibiliza si se trata de una promocion Aleatoria
sub promocionAleatoria( PrAl ) external
    Integer*1 PrAl
end sub

sub seteaCupones( f, opc ) external
    Integer*1 f
    Integer*2 opc
end sub

sub pideLineaCabecera( numL , lin$ ) public
   Integer*1 numL
   String lin$

   on error goto errLoc

   lin$ = lineasCabecera$( numL )

   sale:
   exit sub

   errLoc:
   if errf% = 39 then resume
   lin$ = "ERROR EN MSMTFAC.BAS"
   resume sale
end sub

!-----------------------------------------------------------------------
! Carga valores de los arreglos en las respectivas variables
!-----------------------------------------------------------------------
sub leeParametrosFiscales
   Integer *1 f, i, k, w
   Integer*4 inicioBloque
   String n$, v$

!!!!! almacenar como mensajes predefinidos en la impresora el
!!!!! ---encabezado y trailer
!!!!! antes de grabar verificar con el checksum que no este grabado ya
!!!!! ejemplo grabar mensaje 1
!!!!! grabar mensaje 1
!!!!! X'1D3A01'
!!!!! 'Thank You For Shopping' X'0D'
!!!!! ' At RSD STORE' X'0D'
!!!!! 'Store #1234567' X'0D'
!!!!! X'1D3A'
!!!!! ---checksum del mensaje 1
!!!!! X'1B7C0201
!!!!! ---imprimir mensaje 1
!!!!! X'1D5E01'

   v$ = "[ESTABLECIMIENTO]"
   inicioBloque = abreParametros( "R::$ARESTAB", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   v$ = leeParametro$( "ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin
      establecimiento.estado = val( v$ )
      establecimiento.ambiente$ = leeParametro$( "TIPO_DE_AMBIENTE", 80, inicioBloque )
      establecimiento.emision$ = leeParametro$( "TIPO_DE_EMISION", 80, inicioBloque )
      establecimiento.secuencia$ = leeParametro$( "NRO_SECUENCIA_SRI", 80, inicioBloque )
      establecimiento.ruc$ = leeParametro$( "NRO_RUC_COMERCIO", 80, inicioBloque )
      v$ = leeParametro$( "ESTABLECIMIENTO_SRI", 80, inicioBloque )
      if v$ <> "" then begin
         establecimiento.codigo$ = v$
         terminal.serie$ = v$ + TS.TERMINAL$
      EndIf
   EndIf

   v$ = "[LINEAS INICIO]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   lineasCabecera.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         lineasCabecera.estado = val( v$ )
         lineasCabecera.size = w - 1
         dim lineasCabecera$( lineasCabecera.size )
         k = 1
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            lineasCabecera$( k ) = mid$( v$, 2, 40 )
            k = k + 1
         next i
      EndIf
   EndIf

   v$ = "[LINEAS PAGARE]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   lineasPagare.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         lineasPagare.estado = val( v$ )
         lineasPagare.size = w - 1
         dim lineasPagare$( lineasPagare.size )
         k = 1
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            lineasPagare$( k ) = mid$( v$, 2, 40 )
            k = k + 1
         next i
      EndIf
   EndIf

   v$ = "[LINEAS FINAL]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   lineasFinales.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         lineasFinales.estado = val( v$ )
         lineasFinales.size = w - 1
         dim lineasFinales$( lineasFinales.size )
         k = 1
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            lineasFinales$( k ) = mid$( v$, 2, 40 )
            k = k + 1
         next i
      EndIf
   EndIf

   v$ = "[DEDUCIBLES]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   deducibles.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         deducibles.estado = val( v$ )
         deducibles.size = w - 1
         dim deducibles$( deducibles.size )
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            if esNumero( v$ ) then begin
               k = val( v$ )
               deducibles$( k ) = n$
            EndIf
         next i
      EndIf
   EndIf

   v$ = "[IMPUESTOS]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARESTAB" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   impuestos.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         impuestos.estado = val( v$ )
         impuestos.size = w - 1
         dim impuestos$( impuestos.size, 1 )
         k = 1
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            impuestos$( k, 0 ) = n$
            impuestos$( k, 1 ) = v$
            k = k + 1
         next i
         impuestos.iva = val( impuestos$( 1, 1 ) )
      EndIf
   EndIf

   call cierraParametros( 80 )

   v$ = "[PAGOS SRI]"
   inicioBloque = abreParametros( "R::$ARGENER", v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   pagosSRI.estado = 0
   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      call rescataParametro( 1, n$, v$ )
      if n$ = "ACTIVO" then begin
         pagosSRI.estado = val( v$ )
         pagosSRI.size = w - 1
         dim pagosSRI$( pagosSRI.size, 1 )
         k = 1
         for i = 2 to w
            call rescataParametro( i, n$, v$ )
            pagosSRI$( k, 0 ) = n$
            pagosSRI$( k, 1 ) = v$
            k = k + 1
         next i
      EndIf
   EndIf

   v$ = "[MAPEO PAGOS SRI]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   w = cargaParametros( 80, inicioBloque )
   if w > 0 then begin
      mapeoPagosSRI.size = w
      dim mapeoPagosSRI$( mapeoPagosSRI.size, 1 )
      for i = 1 to w
         call rescataParametro( i, n$, v$ )
         mapeoPagosSRI$( i, 0 ) = n$
         mapeoPagosSRI$( i, 1 ) = v$
      next i
   EndIf

   v$ = "[OTROS DATOS]"
   inicioBloque = buscaParametros( v$, 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      call imprimeCJ( v$ + " NO CONFIGURADO" )
      call cierraParametros( 80 )
      exit sub
   EndIf

   !Teclas de pagos
   v$ = leeParametro$( "TECLAS_PAGO", 80, inicioBloque )
   if v$ <> "" then begin
      teclasPago$ = buscaValores$( v$ )
   EndIf

   !Monto limite anulacion
   limiteAnulacion = 40000
   v$ = leeParametro$( "LIMITE_ANULACION", 80, inicioBloque )
   if v$ <> "" then begin
      if esNumero(v$) then limiteAnulacion = val( v$ )
   EndIf

   !Monto limite consumidor final
   v$ = leeParametro$( "LIMITE_CONSUMIDOR_FINAL", 80, inicioBloque )
   if v$ <> "" then begin
      if esNumero(v$) then limiteConsFinal = val( v$ )
   EndIf

   call cierraParametros( 80 )

end sub

!-------------------------------------------------------------------------------
!Carga arreglo detallePagosSRI$ con los parametros de pagos SRI
!--------------------------------------------------------------------------------
sub cargaDetallePagosSRI
   String pago$, codigo$, tiempo$, descripcion$, valor$
   Integer *1 i, j, k

   DIM detallePagosSRI$(mapeoPagosSRI.size, 5)
   for i= 1 to mapeoPagosSRI.size
      pago$  = mapeoPagosSRI$(i,0)
      valor$ = mapeoPagosSRI$(i,1)
      if len(valor$) > 0 then begin
         k = match(";", valor$, 1)
         if k > 0 then begin
            descripcion$ = ""
            codigo$ = mid$(valor$, 1, k-1)
            tiempo$ = mid$(valor$, k + 1, len(valor$)-k )
            for j= 1 to pagosSRI.size
               if pagosSRI$(j,0) = codigo$ then begin
                  descripcion$ = pagosSRI$(j,1)
                  j = pagosSRI.size
               EndIf
            next j
         EndIf
         detallePagosSRI$(i,0) = pago$         ! codigo del medio de pago
         detallePagosSRI$(i,1) = codigo$       ! codigo del medio de pago SRI
         detallePagosSRI$(i,2) = descripcion$  ! descripcion del pago SRI
         detallePagosSRI$(i,3) = tiempo$       ! unidad de tiempo del plazo
         detallePagosSRI$(i,4) = "0"           ! numero de plazo se saca de la transaccion
         detallePagosSRI$(i,5) = "0"           ! monto del pago se saca de la transaccion
      EndIf
   next i
end sub

!------------------------------------------------------------------------------
! Lee numero de documento del archivo R::ADX_UDT1:PARTERMI.DAT
! y lo compara con el valor almacenado en TE.TR.USERHT$
! para utilizar el mayor
!------------------------------------------------------------------------------
sub leeNumFactura
   Integer*1 f
   Integer*2 i, n, t
   Integer*4 numFactura
   String arc$, frm$, c$, s$, o$, p$

   on error goto orroroso

!call traceNUO( "TE.TR.USERHT$ " + lineahex$( TE.TR.USERHT$ ) )

   arc$ = "R::$ARTERMI"

   f = 0
   open arc$ direct recl 23 as 83 nowrite nodel
   if f = 1 then begin
      wait; 2000
      close 83
      f = 0
      open arc$ direct recl 23 as 83 nowrite nodel
   EndIf
   if f then begin
      call muestraVisor( 1, "NO EXISTE ARCHIVO", "PARTERMI" )
      call QXL.TSUPEC21( "NO EXISTE ARCHIVO PARTERMI" )
      stop
   EndIf

   call QXL.TSUPEC21( "TE.TR.USERHT$=[" + TE.TR.USERHT$ + "]" )

   s$ = trim$( TE.TR.USERHT$ )
   if not esNumero( s$ ) then TE.TR.USERHT$ = String$( 10, "0" )
   numFactura = val( TE.TR.USERHT$ )

   n = size( arc$ ) / 23
   t = val( TS.TERMINAL$ )
   frm$ = "C3 C9 C9 C2"
   for i = 1 to n
      read form frm$; #83, i; c$, s$, o$, p$
      if f then goto finArchivo
      if t = val( c$ ) then begin
         terminal.secFactura = val( s$ )
         if numFactura > terminal.secFactura then begin
            terminal.secFactura = numFactura
         EndIf else begin
            numFactura = terminal.secFactura
            TE.TR.USERHT$ = right$( String$( 10, "0" ) + str$( numFactura ), 10 )
         EndIf
         terminal.otrosParametros = val( o$ )
         if ( terminal.otrosParametros and 1 ) = 1 then tieneBalanza = -1
         terminal.indice = i
         goto finArchivo
      EndIf
   next i

   finArchivo:
   if terminal.indice = 0 then begin
      call muestraVisor( 1, "NO EXISTE TERMINAL", arc$ )
      call QXL.TSUPEC21( "NO EXISTE TERMINAL " + str$(t) + " EN PARTERMI" )
      stop
   EndIf
   close 83

!call traceNUO( "terminal = " + str$(terminal.indice) + " sec=" + str$(terminal.secFactura) )

   salir:
   exit sub

   orroroso:
   !sesion ocupada
   if err = "DF" and errn = 14h and errf% = 83 then begin
      f = 1
      resume
   EndIf

   !no puede abrir el archivo
   if err = "OE" and errf% = 83 then begin
      f = -1
      resume
   EndIf

   !fin de archivo
   if err = "EF" and errn = 1Ch then begin
      f = -1
      resume
   EndIf

   call traceNUO( "leeNumFactura sesion " + str$( errf% ) + " ERROR " + err + hexanum$(errn) )

   if errf% = 39 then resume
   resume salir
end sub

!---------------------------------------------------------------------
!Actualiza el numero de secuencia de factura
!Incrementa el numero de secuencia de factura y
!actualiza la variable TE.TR.USERHT$, terminal.secFactura y el
!registro del archivo R::ADX_UDT1:PARTERMI.DAT correspondiente
!Si incremento es cero no se incrementa el numero de factura
!---------------------------------------------------------------------
sub actualizaFactura( incremento )
   Integer*1 incremento, f
   String arc$, frm$, t$, s$, o$, p$

   on error goto orroroso

   if incremento then begin
      terminal.secFactura = terminal.secFactura + 1
      TE.TR.USERHT$ = right$( String$( 10, "0" ) + str$( terminal.secFactura ), 10 )
   EndIf

   arc$ = "R::PARTERMI"
   frm$ = "C3 C9 C9 C2"

   f = 0
   open arc$ direct recl 23 as 83
   if f = 1 then begin
      wait; 2000
      f = 0
      close 83
      open arc$ direct recl 23 as 83
   EndIf

   if f then begin
      call QXL.TSUPEC21( "No pudo abrir archivo PARTERMI" )
      goto salir
   EndIf

   read form frm$; #83, terminal.indice; t$, s$, o$, p$

   if f then begin
      call QXL.TSUPEC21( "No pudo leer archivo PARTERMI" )
      goto finArchivo
   EndIf

   if val( TS.TERMINAL$ ) = val( t$ ) then begin
      s$ = right$( String$( 9, "0" ) + str$( terminal.secFactura ), 9 )
      write form frm$; #83, terminal.indice; t$, s$, o$, crlf$
      if f then begin
         call QXL.TSUPEC21( "No pudo grabar archivo PARTERMI" )
         goto finArchivo
      EndIf
   EndIf

   finArchivo:
   close 83

   salir:
   exit sub

   orroroso:
   if errf% = 39 then resume

   !sesion ocupada
   if err = "DF" and errn = 14h and errf% = 83 then begin
      f = 1
      resume
   EndIf

   !no puede abrir el archivo
   if err = "OE" and errf% = 83 then begin
      f = -1
      resume
   EndIf

   !fin de archivo
   if err = "EF" and errn = 1Ch then begin
      f = -1
      resume
   EndIf

   call traceNUO( "leeNumFactura sesion " + str$( errf% ) + " ERROR " + err + hexanum$(errn) )
   resume salir

end sub

!------------------------------------------------------------------------
! Imprime detalle de parametros fiscales por terminal
!-------------------------------------------------------------------------
sub cargaParametrosTerminal
   call leeNumFactura
   NUOcmdImpresion$ = NUO.LetraJunta$
   TS.TEMP1$ = "NUMERO DE TERMINAL: " + TS.TERMINAL$
   call imprimeCJ( TS.TEMP1$ )
   TS.TEMP1$ = "FACTURA INICIAL:    " + str$( terminal.secFactura + 1 )
   call imprimeCJ( TS.TEMP1$ )
   if (terminal.otrosParametros and 1) = 0 then begin
      TS.TEMP1$ = "NO"
   EndIf else begin
      TS.TEMP1$ = "SI"
   EndIf
   TS.TEMP1$ = "BALANZA CONECTADA:  " + TS.TEMP1$
   call imprimeCJ( TS.TEMP1$ )

end sub

!-------------------------------------------------------------------------------
! limpia variables usadas en la factura
!-------------------------------------------------------------------------------
sub limpiaFactura public
   Integer*2 k

   !Inicia arreglos de productos
   k = resProdTot
   dim resProdCod$(k)    ! codigo de producto PD 6
   dim resProdSku$(k)    ! codigo interno del producto PD de largo variable
   dim resProdNom$(k)    ! nombre del producto ASC 18
   dim resProdTsl%(k)    ! indice a la cadena 99 01 en el Tsl
   dim resProdVen%(k)    ! monto total sin impuesto ni descuento (4 decimales)
   dim resProdAfe%(k)    ! monto de la parte que tiene impuesto
   dim resProdDes%(k)    ! monto total descuento sin impuesto (4 decimales)
   dim resProdImp%(k)    ! monto total impuesto (4 decimales)
   dim resProdCan%(k)    ! cantidad de productos (si se vende por peso 3 decimales)
   dim resProdTip%(k)    ! tipo de producto, 0 normal, 1 se vende por peso, 2 canasta
   dim resProdDed%(k)    ! tipo de deducible: 0 no deducible, 1 Vestimenta, 2 Alimentacion, 3 Medicina, 4 Educación
   dim resProdCat$(k)    ! Categoria del producto
   dim resTotImp%(2)     ! totales de venta por tarifa
   dim resTotDes%(2)     ! totales de descuentos por tarifa
   dim resTotDed%(deducibles.size)     ! totales deducibles
   resProdNum = 0
   indiceActual = 0
   noCadena1 = 0
   resProd99$ = ""
   totCanasta = 0
   noMasVenta = 0
   inicioFactura = 0

   resDescuentos$ = ""
   TS.TENDERED(0) = 0
end sub

!-------------------------------------------------------------------------
! Limpia totales de descuento
! 0 Base imponible total no afecto a impuestos
! 1 Base imponible total afecto a impuestos
! 2 Impuesto total
!-------------------------------------------------------------------------
sub limpiaDescuentos Public 
   dim resProdDes%(resProdTot)
   dim resTotDes%(2)
   lineasDescuento$ = ""
end sub

!-----------------------------------------------------------------------------
! Aplica el descuento a cada linea de venta con el descuento indice r
!-----------------------------------------------------------------------------
sub aplicaDescuentos( r ) public
   Integer*2 r

   Integer*2      \
      i, k, p,    \
      canDesc,    \ cantidad de descuentos en una instancia
      canLineas,  \ cantidad de lineas de venta en el descuento
      linea       ! Identificador de línea de venta

   Integer*4      \
      v4, v1, v2, \
      monto,      \ Monto a descontar a la línea de venta
      montot      ! Monto total de una promocion

   String         \
      desAplic$,  \
      desPromo    ! codigo y descripcion de la promocion

   real f

   on error goto salidaError:

   if r < 0 then begin
      desAplic$ = NUOTPROMresPromociones
   EndIf else begin
      desAplic$ = desAplic(r)
   EndIf

   canDesc = val( mid$( desAplic$, 8, 3 ) )
   
   !Write #34 ; "Qty Desc:"+Str$(canDesc) + chr$(10)
   
   p = 11

   for i = 1 to canDesc

      montot = 0
               
      !descripción de la promocion
      desPromo = mid$( desAplic$, p, 20 )
      p = p + 20

      !IdPromo
      desPromo = mid$( desAplic$, p, 6 ) + " " + desPromo
      p = p + 6

      !cantidad de lineas de venta
      canLineas = val( mid$( desAplic$, p, 3 ) )
      p = p + 3
      
      !Write #34 ; "Qty Lineas:"+Str$(canLineas) + chr$(10)
      
      for k = 1 to canLineas
         monto = val( mid$( desAplic$, p, 7 ) )  !! + Gr.DeUna.Dscto%
         p = p + 7

         montot = montot + monto

         linea = val( mid$( desAplic$, p, 4 ) )

         v4 = 100 * monto
         resProdDes%(linea) = resProdDes%(linea) + v4
         if resProdImp%(linea) = 0 then begin
            resTotDes%(0) = resTotDes%(0) + v4
         EndIf else begin
            if resProdTip%(linea) = 2 then begin
!call traceNUO("tot=" + str$(resProdVen%(linea)) + " afe=" + str$(resProdAfe%(linea)) + " imp=" + str$(resProdImp%(linea)))
               f = 1.0 * ( resProdAfe%(linea) - resProdImp%(linea) ) / resProdVen%(linea)
               v1 = round( f * v4, 0, 0 )
               resTotDes%(1) = resTotDes%(1) + v1
               f = 1.0 * resProdImp%(linea) / resProdVen%(linea)
               v2 = round( f * v4, 0, 0 )
               resTotDes%(2) = resTotDes%(2) + v2
               resTotDes%(0) = resTotDes%(0) + v4 - v1 - v2
!call traceNUO("d0=" + str$(resTotDes%(0)) + " d1=" + str$(resTotDes%(1)) + " d2=" + str$(resTotDes%(2)))
            EndIf else begin
               v1 = round( 1.0 * v4 * impuestos.iva / ( 100 + impuestos.iva ) , 0, 0 )
               resTotDes%(1) = resTotDes%(1) + v4 - v1
               resTotDes%(2) = resTotDes%(2) + v1
            EndIf
         EndIf

         p = p + 10
      Next

      lineasDescuento$ = lineasDescuento$ + desPromo + " " + \
         right$( String$( 7, " " ) + "-" + formatea2Dec( montot ), 7 )

   next

   salida:
   exit sub

   salidaError:
   if errf% = 39 then resume
   !Indice de arreglo fuera de limite
   if(err = "SU") then resume

   if(err = "FU") then resume

   call traceNUO( "aplicaDescuentos ERROR " + err + hexanum$( errn ) )
   call muestraBorrar( "aplicaDescuentos", "ERROR " + err + hexanum$( errn ) )

   resume salida

end sub

!-------------------------------------------------------------------------
! Base imponible total por tarifa, incluido el descuento (2 decimales)
! 0 Base imponible total no afecta a impuestos
! 1 Base imponible total afecta a impuestos
! 2 Valor total del impuesto
!-------------------------------------------------------------------------
function baseTarifa( i ) public
   Integer*4 baseTarifa, v
   Integer *1 i
   v = resTotImp%( i ) - resTotDes%( i )
   baseTarifa = round( 1.0 * v / 100, 0, 0 )
!call traceNUO( "baseTarifa(" + str$(i) + ")=" + str$( round( 1.0 * v / 100, 0, 0 ) ) )
end function

!------------------------------------------------------------------------------
! Calcula tarifas con el monto de descuento al total
!------------------------------------------------------------------------------
sub tarifasConDescuento( t0, t1, t2 ) public
   Integer*4 t0, t1, t2

   call limpiaDescuentos

   call aplicaDescuentos(0)
   t0 = baseTarifa(0)
   t1 = baseTarifa(1)
   t2 = baseTarifa(2)

   call limpiaDescuentos

end sub

!------------------------------------------------------------------------------
! Calcula tarifas con el monto de descuento al total
! y al pago
!------------------------------------------------------------------------------
sub tarifasAjustadas( t0, t1, t2 ) public
   Integer*4 t0, t1, t2

   call limpiaDescuentos

   call aplicaDescuentos(0)
   call aplicaDescuentos(-1)
   t0 = baseTarifa(0)
   t1 = baseTarifa(1)
   t2 = baseTarifa(2)

   !Write #34 ; "T0: "+Str$(t0) + chr$(10)
   !Write #34 ; "T1: "+Str$(t1) + chr$(10)
   !Write #34 ; "T2: "+Str$(t2) + chr$(10)

   call limpiaDescuentos

end sub

!------------------------------------------------------------------------------
! calcula el monto total de venta antes de aplicar descuentos( 2 decimales )
! Este valor es equivalente a TS.TOTALS(0,0,0)
!------------------------------------------------------------------------------
function totalAcumulado
   Integer*4 mon, totalAcumulado
   mon = resTotImp%(0) + resTotImp%(1) + resTotImp%(2)
   totalAcumulado = round( 0.01 * mon, 0, 0 )
end function

!------------------------------------------------------------------------
! Consulta a partir del indice i el producto cod$
! cod$ codigo del producto (PD6)
! i    indice inicial
! retorna el indice al arreglo de resumen de productos o -1
!------------------------------------------------------------------------
function consultaProducto( cod$, i )
   Integer*2 consultaProducto
   String    cod$
   Integer*2 i

   consultaProducto = -1
   While i < resProdNum
      if cod$ = resProdCod$(i) then begin
         consultaProducto = i
         exit function
      EndIf
      i = i + 1
   Wend

end function

!-------------------------------------------------------------------------------
! Acumula productos en arreglos
! Cod$ codigo de producto PD 6
! Sku$ codigo interno PD
! Nom$ nombre del producto ASC 18
! Ven% monto total vendido (multiplicado por 10000)
! Imp% monto total impuesto (multiplicado por 10000)
! Can% cantidad de productos (si se vende por peso; multiplicado por 1000)
! Tip% tipo de producto, 0 normal, 1 se vende por peso, 2 canasta
! Ded% tipo de deducible 0 no deducible,
!      1 Vestimenta, 2 Alimentacion, 3 Medicina, 4 Educación
! Cat$ Datos adicionales para descuento
!-------------------------------------------------------------------------------
! dado que los descuentos se aplican despues de la venta de todos los articulos
! no es necesario mantener la variable reProdDes%(1)
sub acumulaProductos( Cod$, Sku$, Nom$, Ven%, Imp%, Can%, Tip%, Ded%, Cat$ )
   String Cod$, Sku$, Nom$, Cat$
   Integer*4 Ven%, Imp%, Can%
   Integer*1 Tip%, Ded%

   Integer*1 r
   Integer*2 i, k, p, q
   Integer*4 v, ven4
   String    s$

   ! Activa el calculo de descuentos
   call calcularDescuentos( -1 )

   !Para almacenar con 4 decimales
   ven4 = 100 * Ven%

   !busca producto en arreglo
   k = -1
   i = 0
   if tip% = 1 then begin
      !si se vende por peso
      if Can% < 0 then begin
         While i < resProdNum
            k = consultaProducto( Cod$, i )
            if k < 0 then begin
               i = resProdNum
            EndIf else begin
               if ( resProdCan%(k) + Can% ) = 0 then begin
                  i = resProdNum
               EndIf else begin
                  i = k + 1
               EndIf
            EndIf
         Wend
      EndIf
   EndIf else begin
      !Producto unitario
      k = consultaProducto( Cod$, i )
   EndIf

!call traceNUO( "acumulaProductos TRX=" + str$( SL.HD.TRANSNUM + 1 ) + " k=" + str$( k ) + " Cod=" + unpack$( Cod$ ) + " Can=" + str$( Can% ) + " tsl=" + str$( SL.END ) )
!call traceNUO( "acumulaProductos TS.RECOVERY=" + str$( TS.RECOVERY ) + " RCOVRD=" + str$(SL.END.RCOVRD) + " END=" + str$(SL.END) )

   ! No existe
   if k = -1 then begin
      k = resProdNum
      resProdCod$(k) = Cod$   ! codigo de producto PD 6
      resProdSku$(k) = Sku$   ! codigo interno del producto PD de largo variable
      resProdNom$(k) = Nom$   ! nombre del producto ASC 18
      resProdVen%(k) = Ven4   ! monto total vendido (4 decimales)
      resProdAfe%(k) = afeCanasta
      resProdImp%(k) = Imp%   ! monto total impuesto (4 decimales)
      resProdCan%(k) = Can%   ! cantidad de productos (si se vende por peso 3 decimales)

      ! indice a la cadena 99 01 en el Tsl, el valor es tentativo se corrige en UE68
      if TS.RETV.IN.PROGRESS OR TS.RECOVERY then begin
         resProdTsl%(k) = SL.END.RCOVRD - 1
      EndIf else begin
         if SL.END = 0 then begin
            resProdTsl%(k) = 2
         EndIf else begin
            resProdTsl%(k) = SL.END + 1
         EndIf
      EndIf
      resProdTip%(k) = Tip%   ! tipo de producto, 0 normal, 1 se vende por peso, 2 canasta
      resProdDed%(k) = Ded%   ! tipo de deducible: 0 no deducible, 1 Vestimenta, 2 Alimentacion, 3 Medicina, 4 Educación
      resProdCat$(k) = Cat$   ! categoria del producto
      resProdNum = resProdNum + 1
      indiceActual = k
      noCadena1 = 0

!call traceNUO( "acumulaProductos indiceActual=" + str$( indiceActual ) + " resProdNum=" + str$(resProdNum) + " tsl=" + str$( resProdTsl%(indiceActual) ) )

   EndIf else begin
      resProdVen%(k) = resProdVen%(k) + Ven4   ! monto total vendido (4 decimales)
      resProdAfe%(k) = resProdAfe%(k) + afeCanasta
      resProdImp%(k) = resProdImp%(k) + Imp%   ! monto total impuesto (4 decimales)
      resProdCan%(k) = resProdCan%(k) + Can%   ! cantidad de productos (peso 3 decimales)
      indiceActual = k
      noCadena1 = 0
      p = resProdTsl%(k)

!call traceNUO( "tsl=" + str$( p ) + " Acumulado=" + str$( resProdCan%(k) ) )

      !Si la cantidad es cero se elimina la linea
      if resProdCan%(k) = 0 then begin
         noCadena1 = -1
         resProdNum = resProdNum -1
         While k < resProdNum
            resProdCod$(k) = resProdCod$(k+1)
            resProdSku$(k) = resProdSku$(k+1)
            resProdNom$(k) = resProdNom$(k+1)
            resProdVen%(k) = resProdVen%(k+1)
            resProdAfe%(k) = resProdAfe%(k+1)
            resProdImp%(k) = resProdImp%(k+1)
            resProdCan%(k) = resProdCan%(k+1)
            resProdTsl%(k) = resProdTsl%(k+1)
            resProdTip%(k) = resProdTip%(k+1)
            resProdDed%(k) = resProdDed%(k+1)
            resProdCat$(k) = resProdCat$(k+1)
            k = k + 1
         Wend
         resProdCod$(k) = ""
         resProdSku$(k) = ""
         resProdNom$(k) = ""
         resProdVen%(k) = 0
         resProdAfe%(k) = 0
         resProdImp%(k) = 0
         resProdCan%(k) = 0
         resProdTsl%(k) = 0
         resProdTip%(k) = 0
         resProdDed%(k) = 0
         resProdCat$(k) = ""
      EndIf

      ! Determina cuantas cadenas eliminar del TSL
      q = 2
      if (p+q) < (SL.END+1) then begin
         if left$( SL.STR$(p+q), 1 ) = chr$(02H) then q = q + 1
         if left$( SL.STR$(p+q), 1 ) = chr$(10H) then begin
            SL.STR$(p+q) = ""
         EndIf
      EndIf

!call traceNUO( "Elimina tsl=[" + str$( p ) + "] cant=[" + str$( q ) + "] SL.END=" + str$( SL.END) )

      !Actualiza indices a TSL
      i = 0
      While i < resProdNum

         if resProdTsl%(i) > p then begin
            resProdTsl%(i) = resProdTsl%(i) - q
         EndIf

         i = i + 1
      Wend

      call eliminaCadenaTSL( p, q )

   EndIf

   ! Acumula totales de impuesto
   if imp% = 0 then begin
      resTotImp%(0) = resTotImp%(0) + Ven4
   EndIf else begin
      if Tip% = 2 then begin
         resTotImp%(0) = resTotImp%(0) + Ven4 - afeCanasta
         resTotImp%(1) = resTotImp%(1) + afeCanasta - Imp%
         resTotImp%(2) = resTotImp%(2) + Imp%
      EndIf else begin
         resTotImp%(1) = resTotImp%(1) + Ven4 - Imp%
         resTotImp%(2) = resTotImp%(2) + Imp%
      EndIf
   EndIf

   ! Acumula totales de deducibles
   if Ded% > 0 then begin
      resTotDed%(Ded%) = resTotDed%(Ded%) + Ven4 - Imp%
   EndIf

   if TS.RETV.IN.PROGRESS OR TS.RECOVERY then exit sub

   resProd99$ = ""

   if noCadena1 then exit sub

   ! Crea cadena 99 01
   !k = indiceActual
   v = round( 0.01 * resProdImp%(k), 0, 0 )

!call traceNUO( "acumulaProductos k=[" + str$( k ) + "-->" + str$(indiceActual) + "] v=[" + str$( v ) + "] Ven4=[" + str$( Ven4 ) )

   if abs(v) > 0 then begin
      s$ = pack$( str$( impuestos.iva ) ) + ":" + pack$( str$( abs(v) ) )
   EndIf else begin
      s$ = ":"
   EndIf
   v = abs(Ven4) - abs(Imp%)
   if imp% = 0 then r = -2 else r = 0
   if Tip% = 1 then begin
      v = round( 1000.0 * v / abs(Can%), r, 0 )
   EndIf else begin
      v = round( 1.0 * v / abs(Can%), r, 0 )
   EndIf

   !esta cadena esta incompleta
   !Al imprimir la factura, se agrega precio unitario sin impuesto,
   !precio unitario descuento aplicado y monto de descuento
   resProd99$ = chr$(99H) + ":" + chr$(1) + ":" + \
      Sku$ + ":" + \
      s$

end sub


!------------------------------------------------------------------------
! Impresion detalle productos en documento fiscal
! comprobante = 0 factura normal
! comprobante = 1 comprobante resumen anulacion
! comprobante = 2 comprobante resumen suspension
!------------------------------------------------------------------------
sub imprimeDetalleProductos( comprobante )
   Integer*1 comprobante
   Integer*1 r
   Integer*2 i, k, x
   Integer*4 mv, md, puv, pud, mi, u, v, w, de, ca
   Integer*4 td, t0, t1, ti, ni%
   real      f, tdu%
   String    t$, v$, c$

   td = 0
   t0 = 0
   t1 = 0
   ti = 0
   i  = 0
   tdu% = 0
   ni% = 0
   Dim Gr.DeUna.DcItm$(700,1)
!   Write #34 ; "NUMERO PRODUCTOS :"+STR$(resProdNum) + CHR$(10)
!   Write #34 ; "DSCTO  DISTRIB   :"+STR$(TDU%) + CHR$(10)
   
   While i < resProdNum

!Evaluar si es mejor regenerar el TSL completo
      !valida cadena 9901, 01 y 02 en TSL
!      x = resProdTsl%( i )
!      c$ = leeCadenaTSL$( x )
!agregar el codigo de producto SKU
!      v$ = chr$(99H) + ":" + chr$(1)
!      if left$( c$, 3 ) <> v$ then begin
!         call QXL.TSUPEC21( "ERROR TSL " + str$( x ) + " item " + unpack$( resProdCod$(i) ) )
!         !buscar o agregar cadena 9901 en TSL
!         !x = corrigeCadena9901( i )
!         !resProdTsl%( i ) = x
!      EndIf
!      x = x + 1
!      c$ = leeCadenaTSL$( x )
!comparar tambien el codigo de barra
!      if left$( c$, 1 ) <> chr$( 1 ) then begin
!         c$ = chr$(1) + ":" + resProdCod$(i) + ":" + ...
!         insertaCadena( c$ )
!      EndIf
!si tiene cantidad o peso validar cadena 02
      
      if resProdTip%(i) = 9 then goto siguiente

      If Gr.DeUna.Dscto% > 0 Then Begin                                        ! si hay dscto deuna
         tdu% = ((float(resProdVen%(i)) - float(resProdDes%(i)) ) /10) * (Val(Gr.DeUna.Ptg$) / 1000)
         ni% = ni% + 1 
         Gr.DeUna.DcItm$(ni%,0) = unpack$( resProdCod$(i) )                 ! Item beneficio deuna
         Gr.DeUna.DcItm$(ni%,1) = Str$(tdu%)                                ! Dscto Deuna
      EndIf

!      Write #34 ; "ProdCod:"+Str$(i)+" "+resProdCod$(i) + Chr$(10)
!      Write #34 ; "ProdTip:"+Str$(i)+" "+Str$(resProdTip%(i)) + Chr$(10)
!      Write #34 ; "ProdVen:"+Str$(i)+" "+Str$(resProdVen%(i)) + Chr$(10)
!      Write #34 ; "ProdDes:"+Str$(i)+" "+Str$(resProdDes%(i)) + Chr$(10)
!      Write #34 ; "ProdImp:"+Str$(i)+" "+Str$(resProdImp%(i)) + Chr$(10)
!      Write #34 ; "ProdCan:"+Str$(i)+" "+Str$(resProdCan%(i)) + Chr$(10)
!      Write #34 ; "DsctoUna:"+Str$(tdu%) + chr$(10)

      resProdDes%(i) = resProdDes%(i)  + tdu%  ! Add 09Jun2025 GR      
      mv = resProdVen%(i)
      de = resProdDes%(i)
      mi = resProdImp%(i)
      ca = resProdCan%(i)
      c$ = ""
      
      if resProdTip%(i) = 1 then begin
         t$ = resProdCod$(i)
         k = i + 1
         While k < resProdNum
            if resProdCod$(k) = t$ then begin
               mv = mv + resProdVen%(k)
               de = de + resProdDes%(k)
               mi = mi + resProdImp%(k)
               ca = ca + resProdCan%(k)
               resProdTip%(k) = 9
            EndIf
            k = k + 1
         Wend
      EndIf
      
      !Genera lineas para resumen descuentos
      if de > 0 then begin
         v = round( 1.0 * de / 100, 0, 0 )
         resDescuentos$ = resDescuentos$ + \
                          left$( resProdNom$(i), 12 ) + \
                          right$( String$( 9, " " ) + \
                          formatea2dec( v ), 9 )
      EndIf

      !Impuesto
      if mi > 0 then begin
         ti = ti + mi
         if de > 0 then begin
            v = round( 1.0 * de * mi / mv, 0, 0 )
            de = de - v
            ti = ti - v
         EndIf
      EndIf

      !Actualiza deducibles con el descuento
      if de > 0 then begin
         td = td + de
         k = resProdDed%(i)
         resTotDed%(k) = resTotDed%(k) - de
      EndIf

      mv = mv - mi
      md = mv - de

      !Calcula precio unitario sin impuesto y precio unitario con descuento (4 decimales)
      if mi = 0 then r = -2 else r = 0
      if resProdTip%(i) = 1 then begin
         v$ = formatea3Dec( ca )
         puv = round( 1000.0 * mv / ca, r, 0 )
         pud = round( 1000.0 * md / ca, 0, 0 )
      EndIf else begin
         v$ = str$( ca )
         puv = round( 1.0 * mv / ca, r, 0 )
         pud = round( 1.0 * md / ca, 0, 0 )
      EndIf
      if de = 0 then pud = puv
      if pud > puv then pud = puv

      !Imprime
      t$ = right$( String$( 7, " " ) + v$, 7 ) + " x " + \
         right$( String$( 9, " " ) + formatea4Dec( puv ), 9 ) + "   " + \
         right$( String$( 9, " " ) + formatea4Dec( pud ), 9 )
      call imprimeCJ( t$ )

      ! codigo y descriptor
      v$ = unpack$( resProdCod$(i) )
      t$ = right$( String$( 12, " " ) + v$, 12 ) + " " + \               ! Codigo del producto
         left$( resProdNom$(i), 12 ) + " "                               ! Nombre producto

      !Obtiene total afecto y acumula tarifas
      if resProdTip%(i) = 2 then begin
         f = 1.0 - ( 1.0 * resProdDes%(i) / resProdVen%(i) )
         u = resProdAfe%(i) - resProdImp%(i)
         v = round( 1.0 * u * f, 0, 0 )
         t1 = t1 + v
         t0 = t0 + md - v
!call traceNUO( "imprimeDetalleProductos precio afe=" + str$(resProdAfe%(i)) + " ven=" + str$(resProdVen%(i)) + " des=" + str$(resProdDes%(i)) + " iva=" + str$(resProdImp%(i)) + " u=" + str$(u) + " v=" + str$(v) + " f=" + str$(f) )
!call traceNUO( "mv=" + str$(mv) + " de=" + str$(de) + " md=" + str$(md) + " mi=" + str$(mi) + " t0=" + str$(t0) + " t1=" + str$(t1) )
      EndIf else begin
         v = md
         if mi > 0 then \
            t1 = t1 + md \
         else \
            t0 = t0 + md
      EndIf

      ! Precio total Venta sin impuesto
      v = round( 0.01 * v, 0, 0 )
      v$ = right$( String$( 9, " " ) + formatea2Dec( v ), 9 )

      ! Imprime una "I" en los productos con Impuesto
      if mi > 0 then \
         v$ = v$ + " I"

      call imprimeCJ( t$ + v$ )

      !es canasta
      if resProdTip%(i) = 2 then begin

         c$ = chr$( 99H ) + ":" + chr$( 88H ) + ":" + \ ! cadena de canasta
            resProdCod$(i) + ":" + \                    ! Codigo del Item
            pack$( str$( ca ) ) + ":" + \               ! Cantidad unidades vendidas
            pack$( str$( v ) )                          ! Monto afecto a IVA

         !Monto no afecto a IVA
         v = round( 0.01 * md, 0, 0 ) - v

         !Imprime linea de canasta no afecta a IVA
         v$ = right$( String$( 9, " " ) + formatea2Dec( v ), 9 )
         call imprimeCJ( t$ + v$ )

         c$ = c$ + ":" + pack$( str$( v ) )         ! Monto no afecto a IVA
         v = round( 0.01 * mi * md / mv, 0, 0 )
         c$ = c$ + ":" + pack$( str$( v ) )         ! Monto de iva
         v = round( 0.01 * de, 0, 0 )
         c$ = c$ + ":" + pack$( str$( v ) )         ! Monto de descuento
         call agregaCadenaTSL( c$ )
         totCanasta = totCanasta + ca

      EndIf

      !imprime linea descuento
      v = 0
      if de > 0 then begin
         v = round( 0.01 * de, 0, 0 )
         t$ = "DESCUENTO APLICADO :" + right$( String$( 9, " " ) + \
            formatea2dec( v ), 9 )
         call imprimeCJ( t$ )
      EndIf

      if comprobante = 0 then begin
         x = resProdTsl%( i )
         c$ = leeCadenaTSL$( x )
         v$ = chr$(99H) + ":" + chr$(1)
         if left$( c$, 3 ) = v$ then begin

            !Incorpora a cadena 99:01 precio unitario sin impuesto y precio unitario con descuento
            c$ = c$ + ":" + pack$( str$( puv ) ) + ":"  + pack$( str$( pud ) )

            !Calcula monto de descuento para cadena 99 01
            if resProdCan%( i ) = ca then begin
               !Aplica todo el descuento a la cadena
               u = v
               w = 0
            EndIf else begin
               !Aplica la proporcion de descuento a la cadena
               u = round( 1.0 * v * resProdCan%( i ) / ca, 0, 0 )
               if u < v then begin
                  w = v - u
               EndIf else begin
                  u = v
                  w = 0
               EndIf
            EndIf

            call escribeCadenaTSL( x, c$ + ":" + pack$( str$( u ) ) )

            !Si es vendido por peso
            !busca otras entradas para agregar el precio unitario con descuento
            !puede ser un loop hasta w = 0
            if resProdTip%(i) = 1 then begin
               t$ = resProdCod$( i )
               k = i + 1
               While k < resProdNum
                  if resProdCod$(k) = t$ then begin
                     x = resProdTsl%( k )
                     c$ = leeCadenaTSL$( x )
                     if left$( c$, 3 ) = v$ then begin

                        !Incorpora a cadena 99:01 precio unitario sin impuesto y precio unitario con descuento
                        c$ = c$ + ":" + pack$( str$( puv ) ) + ":"  + pack$( str$( pud ) )

                        !calcula el descuento a aplicar a esa entrada
                        u = round( 1.0 * v * resProdCan%( i ) / ca, 0, 0 )
                        if u < w then begin
                           w = w - u
                        EndIf else begin
                           u = w
                           w = 0
                        EndIf

                        call escribeCadenaTSL( x, c$ + ":" + pack$( str$( u ) ) )

                     EndIf
                  EndIf
                  k = k + 1
               Wend

               !Agrega el resto de descuento a la ultima cadena 99 01 actualizada
               if w <> 0 then begin
                  call escribeCadenaTSL( x, c$ + ":" + pack$( str$( w + u ) ) )
               EndIf

            EndIf

         EndIf
      EndIf

      siguiente:
      i = i + 1
   Wend

!call traceNUO( "FACTURA total=" + str$(totalAcumulado) + " descuento=" + str$(NUOTPROMdescuento) )
!call traceNUO( " tarifa0=" + str$(baseTarifa(0)) + " tarifa1=" + str$(baseTarifa(1)) + " tarifa2=" + str$(baseTarifa(2)) )
!call traceNUO( " td=" + str$(td) + " t0=" + str$(t0) + " t1=" + str$(t1) + " ti=" + str$(ti) )

   !Redondea totales a dos decimales
   
   td = round( 0.01 * td, 0, 0 )
   t0 = round( 0.01 * t0, 0, 0 )
   t1 = round( 0.01 * t1, 0, 0 )
   ti = round( 0.01 * ti, 0, 0 )

   !t0 = baseTarifa(0)
   !t1 = baseTarifa(1)
   !ti = baseTarifa(2)
   !ti = TS.TOTALS(0,0,0) - NUOTPROMdescuento - t0 - t1
   !v = round( 0.01 * impuestos.iva * t1, 0, 0 )
   !v = TS.TOTALS(0,0,0) - NUOTPROMdescuento

   if comprobante > 0 then begin
      v = t0 + t1 + ti
      if comprobante = 1 then t$ = "   TOTAL ANULACION USD:     "
      if comprobante = 2 then t$ = "  TOTAL SUSPENSION USD:     "
      t$ = t$ + right$( String$( 9, " " ) + formatea2Dec( v ), 9 )
      t$ = chr$(10) + t$
      call imprimeCJ( t$ )
      call seteaCupones( 1, -1 )
      exit sub
   EndIf
!-- ca,mbio formatea2Dec a formateamonto
   t$ = "==>>     DESCUENTO USD:     " + \
      right$( String$( 9, " " ) + formateaMonto( td ), 9 )
   call imprimeCJ( t$ )

   t$ = "==>>     SUBTOTAL  USD:     " + \
      right$( String$( 9, " " ) + formateaMonto( t0 + t1 ), 9 )
   call imprimeCJ( t$ )

   t$ = String$( 11, " " ) + "TARIFA  =00% USD:" + \
      right$( String$( 9, " " ) + formateaMonto( t0 ), 9 )
   call imprimeCJ( t$ )

   t$ = String$( 11, " " ) + "TARIFA I="+str$(impuestos.iva)+"% USD:" + \
      right$( String$( 9, " " ) + formateaMonto( t1 ), 9 )
   call imprimeCJ( t$ )

   t$ = String$( 11, " " ) + str$(impuestos.iva)+"% I.V.A.   USD:" + \
      right$( String$( 9, " " ) + formatea2Dec( ti ), 9 )
   call imprimeCJ( t$ )
   
   If Ni% NE 0 Then Begin 
    I = 0
    for i = 1 to ni%
      t$ = chr$(99H) + ":" + chr$(53H)        + ":" +                      \! String descuento
           pack$( "999999" )                  + ":" +                      \! Dscto Deuna
           pack$( Gr.DeUna.DcItm$(i,1) )      + ":" +                      \! Valor del descuento
           pack$( Gr.DeUna.DcItm$(i,0) )      + ":" +                      \! Item beneficiado
           pack$( "01" )                      + ":"                         ! Filler
      Call  agregaCadenaTSL( t$ )                                           ! Almacena UD
    next i
   EndIf  
end sub

!------------------------------------------------------------------------
! Imprime la cabecera de un comprobante
!------------------------------------------------------------------------
sub imprimeCabeceraFactura
   Integer*1 i

   ! Imprime logo posicion 1 4610
   NUOcmdImpresion$ = NUO.Centrado$
   call imprime34( MSMlogo1$ )
   if lineasCabecera.estado then begin
      NUOcmdImpresion$ = NUO.LetraJunta$ + NUO.NegritaA$
      call imprimeCJ( lineasCabecera$(1) )
      NUOcmdImpresion$ = NUO.NegritaD$
      for i = 2 to lineasCabecera.size
         call imprimeCJ( lineasCabecera$(i) )
      next i
   EndIf
   call imprimeCJ( "--------------------------------------" )
   call imprimeCJ( "CANT.   PVU        PNETO              " )
   call imprimeCJ( "CODIGO       DESCRIPCION       VALOR  " )
   call imprimeCJ( "--------------------------------------" )

end sub

!----------------------------------------------------------------------------
! Acumula los valores de los pagos realizados leyendo el TSL y llenando el
! arreglo 'detallePagosSRI$'
!----------------------------------------------------------------------------
sub acumulaValoresTSL
   Integer*2 i
   Integer*1 j
   String dato$, monto$, pago$, plazo$, paso$, vuelto$     ! 27-04-2022 CCY

!call traceNUO( "acumulaValoresTSL" )

   !Inicializa valores Pagos SRI
   for i=1 to mapeoPagosSRI.size
      detallePagosSRI$(i,4) = "0" !Plazo
      detallePagosSRI$(i,5) = "0" !Monto
   next i

   vuelto$ = "0"                                           ! 27-04-2022 CCY

   for i= 1 to SL.END
      dato$ = leeCadenaTSL$(i)
      if len(dato$) > 0 then begin

         if getCampoTSL$(dato$,1) = "99" and \
            getCampoTSL$(dato$,2) = "54" then begin
            plazo$ = str$( val(getCampoTSL$( dato$, 11) ) * 30 )
         EndIf

         if left$(dato$, 1) = chr$(5) or left$(dato$, 1) = chr$(6) then begin  !5:Pagos 6:Anulaciones
            pago$  = getCampoTSL$(dato$,2)
            monto$ = getCampoTSL$(dato$,3)
            if i < SL.END then begin                           ! 27-04-2022 CCY
               paso$ = leeCadenaTSL$(i+1)
               if left$(paso$, 1) = chr$(9) then begin         !9:Vuelto
                  if getCampoTSL$(paso$,2) = pago$ then vuelto$ = getCampoTSL$(paso$,3)
                  monto$ = str$( val( monto$) - val(vuelto$) )
               EndIf
            EndIf
            if left$(dato$, 1) = chr$(6) then monto$ = "-" + monto$
            for j=1 to mapeoPagosSRI.size
               if detallePagosSRI$(j,0) = pago$ then begin
                  if val(plazo$) > val(detallePagosSRI$(j,4)) then detallePagosSRI$(j,4) = plazo$
                  detallePagosSRI$(j,5) = str$(val(detallePagosSRI$(j,5)) + val(monto$))
                  plazo$ = ""
               EndIf
            next j
         EndIf
      EndIf
   next i

end sub

!------------------------------------------------------------------------
! Imprime datos de factura
!------------------------------------------------------------------------
sub imprimeDatosFactura
   String linea$, s$
   Integer *1 i, k
   Integer *4 totalDeducibles
   real totalFactura

   totalDeducibles = 0

   totalFactura = totalAcumulado - NUOTPROMdescuento - Gr.DeUna.Dscto%  ! Add 09Jun2025 GR

   !Devolucion de IVA
   linea$ = NUOTDIVA.devolver
   if linea$ <> "" then begin
      TS.TEMP1$ = String$( 28, " " ) + String$( 10, "-" )
      call imprimeCJ( TS.TEMP1$ )
      TS.TEMP1$ = String$( 11, " " ) + "TOTAL VENTA  USD:" + \
         right$( String$( 10, " " ) + formateaMonto( totalFactura ), 10 )
      call imprimeCJ( TS.TEMP1$ )
      call imprimeCJ( linea$ )
      totalFactura = totalFactura - NUOTDIVA.monto
   EndIf

   linea$ = String$( 28, " " ) + String$( 10, "-" )
   call imprimeCJ( linea$ )

   !imprime total a pagar
   TS.TEMP1$ = formatea2Dec( totalFactura )
   linea$ = String$( 8, " " ) + "TOTAL A PAGAR  USD:" + \
      right$( String$( 10, " " ) + TS.TEMP1$, 10 )
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ( linea$ )
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprime34( "" )
   call acumulaValoresTSL

   !PAGOS SRI
   if pagosSRI.estado then begin

      ! Suma por codigo de pago SRI
      i = 1
      While i <= mapeoPagosSRI.size
         if val( detallePagosSRI$(i,5) ) > 0 then begin
            k = i + 1
            While k <= mapeoPagosSRI.size
               if val( detallePagosSRI$(k,5) ) > 0 then begin
                  if detallePagosSRI$(i,1) = detallePagosSRI$(k,1) then begin
                     detallePagosSRI$(i,4) = detallePagosSRI$(k,4)
                     detallePagosSRI$(i,5) = str$( val(detallePagosSRI$(i,5)) + val(detallePagosSRI$(k,5)) )
                     detallePagosSRI$(k,5) = "0"
                  EndIf
               EndIf
               k = k + 1
            Wend
         EndIf
         i = i + 1
      Wend

      for i=1 to mapeoPagosSRI.size
         if val(detallePagosSRI$(i,5)) > 0 then begin
            linea$ = "PAGO: "+ detallePagosSRI$(i,2)
            call imprimeCJ( linea$ )
            linea$ = "VALOR: $" + formateaMonto(val(detallePagosSRI$(i,5))) + " PLAZO:" + detallePagosSRI$(i,4) +" TIEMPO:" + detallePagosSRI$(i,3)
            call imprimeCJ( linea$ )
         EndIf
      next i
   EndIf

   !imprime la linea con fecha hora tienda caja transaccion operador
   call imprimeCJ( lineaDatosTrx$ )

   !ARTICULOS VENDIDOS
   linea$ = "NUMERO DE ARTICULOS ENTREGADOS " + str$( ITEM.COUNT )
   call imprimeCJ( linea$ )

   !Gastos deducibles
   linea$ = "      *** GASTOS DEDUCIBLES ***"
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ( linea$ )
   NUOcmdImpresion$ = NUO.NegritaD$

   for i = 1 to deducibles.size
      if resTotDed%(i) > 0 then begin
         linea$ = formatea2Dec( round( 1.0 * resTotDed%(i) / 100, 0, 0 )  )
         linea$ = deducibles$(i) + " : " + linea$
         totalDeducibles = totalDeducibles + resTotDed%(i)
         call imprimeCJ( linea$ )
      EndIf
   next i
   NUOcmdImpresion$ = NUO.NegritaA$
   linea$ = formatea2Dec( round( 1.0 * totalDeducibles / 100, 0, 0 ))
   linea$ = "TOTAL DEDUCIBLES : " + linea$
   call imprimeCJ( linea$ )
   NUOcmdImpresion$ = NUO.NegritaD$

   !total de canastas
   NUOcmdImpresion$ = NUO.NegritaA$
   linea$ = "TOTAL CANASTAS : " + str$( totCanasta )
   call imprimeCJ( linea$ )

   !DATOS DE FACTURA
   call actualizaFactura(1)

!call traceNUO( "MSM20 sec=" + str$( terminal.secFactura ) + " HT$=" + lineahex$( TE.TR.USERHT$ ) )

   linea$ = "FACTURA NRO.: " + lineaDatosFactura$
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ( linea$ )
   NUOcmdImpresion$ = NUO.NegritaD$
   call imprime34( "" )

   s$ = datosCliente$(9)
   linea$ = "CLIENTE    :" + s$
   call imprimeCJ( linea$ )

   s$ = datosCliente$(8)
   linea$ = "CED./RUC   :" + s$
   call imprimeCJ( linea$ )

   !DATOS SRI
   if establecimiento.estado then begin
      linea$ = "ERROR DEFINICION"
      if establecimiento.ambiente$ = "1" then linea$ = "PRUEBAS   "
      if establecimiento.ambiente$ = "2" then linea$ = "PRODUCCION"
      linea$ = "AMBIENTE   :" + linea$
      call imprimeCJ( linea$ )
      TS.TEMP2$ = "ERROR DEFINICION"
      if establecimiento.emision$ = "1" then TS.TEMP2$ = "NORMAL"
      if establecimiento.emision$ = "2" then TS.TEMP2$ = "INDISPONIBILIDAD SISTEMA"
      linea$ = "EMISION    :" + TS.TEMP2$
      call imprimeCJ( linea$ )
      linea$ = "CLAVE   DE   ACCESO"
      NUOcmdImpresion$ = NUO.NegritaA$
      call imprimeCJ( linea$ )
      linea$ = "NUMERO AUTORIZACION"
      call imprimeCJ( linea$ )
      NUOcmdImpresion$ = NUO.NegritaD$ + chr$(0AH)
      call imprime34( "" )

      !Codigo de Barra
      linea$ = claveAccesoSRI$(tipoDocumento, terminal.secFactura, terminal.serie$, establecimiento.ruc$, establecimiento.ambiente$, establecimiento.secuencia$, establecimiento.emision$)
      llaveAccesoSRI$ = linea$

      call QXL.TSUPEC21( linea$ )
      NUOcmdImpresion$ = Chr$(1DH)+Chr$(77H)+Chr$(01H)+ \ !Horizontal barcode
         Chr$(1DH)+Chr$(68H)+Chr$(40H)+ \                 !Vertical barcode
         Chr$(1DH)+Chr$(48H)+Chr$(02H)+ \                 !Human Read characters
         Chr$(1DH)+Chr$(66H)+Chr$(00H) + \                !activado
         chr$(1DH) + chr$(6BH) + chr$(07H) + \            !
         linea$ + chr$(00H)
      call imprime34( "" )
   EndIf

   call imprimeCJ( " " )
   !Leyendas adicionales
   NUOcmdImpresion$ = NUO.LetraJunta$
   if lineasFinales.estado then begin
      for i = 1 to lineasFinales.size
         call imprimeCJ( lineasFinales$( i ) )
      next i
   EndIf

   !Resumen de descuentos
   call NUOTPROMresumen(resDescuentos$)

   !INFORMACION ELECTORAL
   if datosCliente$(5) <> "" then call NUOTELEC.imprime

   if java.init then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg)
   EndIf

end sub

!------------------------------------------------------------------------------
!Imprime detalle de productos en un QR
!------------------------------------------------------------------------------
sub QRproductos public
   Integer*2 i
   String s$

   i = 0
   s$ = lineaDatosTrx$ + String$(10, " ")
   While i < resProdNum
      s$ = s$ + resProdCod$(i) + " " + resProdNom$(i) + \
         right$( "        " + str$(resprodCan%(i)), 8) + \
         right$( "        " + str$(resprodVen%(i)), 8)
      i = i + 1
   Wend
   call imprime34("contiene " + str$(len(s$)) + " bytes")
   call imprimeQR(s$)
   s$ = ""
end sub

!------------------------------------------------------------------------------
! Imprime resumen para anulacion o suspension
! comprobante = 1 trx anulada
! comprobante = 2 trx suspendida
!------------------------------------------------------------------------------
sub imprimeResumen( comprobante ) public
   Integer*1 comprobante
   String    t$

   NUOcmdImpresion$ = NUO.Centrado$
   call imprime34( "" )
   NUOcmdImpresion$ = NUO.LetraJunta$ + NUO.NegritaA$
   call imprimeCJ( lineasCabecera$(1) )
   NUOcmdImpresion$ = NUO.NegritaD$

   call imprimeCJ( "--------------------------------------" )
   call imprimeCJ( "CANT.   PVU                           " )
   call imprimeCJ( "CODIGO       DESCRIPCION       VALOR  " )
   call imprimeCJ( "--------------------------------------" )

   call limpiaDescuentos
   call aplicaDescuentos(0)
   call imprimeDetalleProductos( comprobante )

   if comprobante = 1 then begin
      NUOcmdImpresion$ = chr$(10)
      t$ = "    *** TRANSACCION   ANULADA  ***"
   EndIf

   if comprobante = 2 then begin
      NUOcmdImpresion$ = chr$(10) + NUO.NegritaA$
      call imprimeCJ( lineaDatosTrx$ )
      NUOcmdImpresion$ = NUO.NegritaD$ + chr$(10)
      t$ = "    *** TRANSACCION SUSPENDIDA ***"
   EndIf

   call imprimeCJ( t$ )

   t$ = "    " + TS.PRTBUF$
   call imprimeCJ( t$ )
   write #34; NUO.SaltaLineas$ + chr$( 5 )

end sub

!------------------------------------------------------------------------
!Actualiza String Datos de Documento Fiscal y
!Agrega cadena de cliente nuevo si existe
!------------------------------------------------------------------------
!Tipo                1 PD  99H Cadena de Usuario
!Subtipo             1 PD  11H Datos de Documento Fiscal
!SecuenciaDocumento  5 PD  Secuencia del número de documento fiscal
!TipoDocumento       1 PD  Código del Tipo de Documento Interno: 02 Factura 03 Nota de Crédito
!TipoDocumentoSRI    1 PD  Código del Tipo de Documento SRI: 01 Factura de venta
!                                                            04 Nota Crédito
!                                                            05 Nota débito
!                                                            06 Guía de remisión
!                                                            07 Comprobante retención
!TipoAmbienteSRI     1 PD  Tipo de Ambiente SRI: 1 Pruebas 2 Producción
!TipoEmisionSRI      1 PD  Tipo de Emisión SRI:  1 Normal
!                                                2 Indisponibilidad del Sistema
!ClaveAccesoSRI      V PD  Clave de Acceso SRI
!monDeducible1       V PD  Monto del deducible 1 Vestimenta
!monDeducible2       V PD  Monto del deducible 2 Alimentacion
!monDeducible3       V PD  Monto del deducible 3 Medicina
!monDeducible4       V PD  Monto del deducible 4 Educación
!tipoCliente         1 ASC Indicador de tipo de cliente
!                          1 cedula, 2 RUC, 3 pasaporte
!idCliente           V ASC Número de Identificación del Cliente
!nomCliente          V ASC Nombre del Cliente
Sub cadenaFactura
   Integer*1 i
   String s$

   s$ = chr$(99H) + ":" + \
      chr$(11H) + ":" + \
      pack$( right$( String$( 9, "0" ) + str$( terminal.secFactura ), 9 ) ) + ":" + \
      chr$(2) + ":" + \
      chr$(1) + ":" + \
      pack$(establecimiento.ambiente$) + ":" + \
      pack$(establecimiento.emision$) + ":" + \
      pack$(llaveAccesoSRI$)
   for i = 1 to deducibles.size
      s$ = s$ + ":" + pack$( str$( resTotDed%(i) ) )
   next i

   !agrega base 0%, base 12% y monto de IVA
   s$ = s$ + ":" + pack$( str$( baseTarifa( 0 ) ) ) + ":" + \
      pack$( str$( baseTarifa( 1 ) ) ) + ":" + \
      pack$( str$( baseTarifa( 2 ) ) )

   !agrega datos de cliente
   s$ = s$ + ":" + datosCliente$(6) + ":" + datosCliente$(8) + ":" + datosCliente$(9)

   call escribeCadenaTSL( 1, s$ )

   !agrega cadena de datos de cliente nuevo
   s$ = datosCliente$(7)
   if len( s$ ) > 0 then begin
      call agregaCadenaTSL( s$ )
      call QXL.TSUPEC21( "******* Se crea nuevo cliente *******" )
      call QXL.TSUPEC21( datosCliente$(6) + " " + datosCliente$(5) )
      call QXL.TSUPEC21( datosCliente$(2) )
      call QXL.TSUPEC21( "*************************************" )
   EndIf
   s$ = ""
   
	If Gr.Domi.Graba% = -1 Then Begin																				  ! Venta delivery
     s$  = pack$("99") + ":" + Pack$("1003") +                             \!
           ":"+(Str$(Gr.Domi.Delivery%))                          +        \! ID domiciliario
           ":"+(Str$(Gr.Domi.Ptg%))                               +        \! Porcentaje domicilio
           ":"+ Gr.Domi.Pedido$                                   +        \! Numero del pedido
           ":"+Pack$("00")                                                  ! Filler 
     call agregaCadenaTSL( s$ )                                             ! Almacena String 
     Gr.Domi.Graba% = 0                                                     !
   EndIf																																		! Fin UD remates
   s$ = ""
   
   !registra cliente nuevo
   call registraCliente

End Sub

Sub ImprimeQRPublicidad
String Xtmp$
Xtmp$ = ";"+TS.TERMINAL$+";"
If Match(Xtmp$,Gr.Clqr.Term$,1) > 0 Then Begin															! Terminal aut imprime qr
  Call imprimeQR( Gr.Clqr.Url$ )																						! Genera QR
  Call imprime34( Gr.Clqr.Msg1$ )																							! Mesajes adicionales
  Call imprime34( Gr.Clqr.Msg2$ )
  Call imprime34( Gr.Clqr.Msg3$ )
  
EndIf Else Begin																														! Terminal de imagen
  !Call Impresion.Logo(2)																										! 
EndIf

End Sub 


!------------------------------------------------------------------------
! Al inicio de una transaccion de venta
! Despues de vender el primer articulo en una transaccion
!------------------------------------------------------------------------
sub MSMTFACT01 public

   call traceNUO( "MSMTFACT01 TRX=" + str$( SL.HD.TRANSNUM + 1 ) + " " + time$ )

   anula400 = 0                !Indicador de anulacion de venta sobre USD 400
   tipoResumen = 0             ! CCY

end sub

!------------------------------------------------------------------------
! Al finalizar una transaccion de venta
!------------------------------------------------------------------------
sub MSMTFACT02 public

   call traceNUO( "MSMTFACT02 TRX=" + str$( SL.HD.TRANSNUM ) + " HORA: " + time$ )

   anula400 = 0                !Indicador de anulacion de venta sobre USD 400
   artCanasta = 0
   GR.IVA.DEVOL% = 0           ! Reporte iva devolucion
   
end sub

!------------------------------------------------------------------------
! Carga parametros del modulo
!------------------------------------------------------------------------
sub MSMTFACT07 public

   call imprimeCJ( "MODULO FACTURACION MSM     20-Dic-2022" )

   ! Comando para imprimir el logo numero 1
   MSMlogo1$ = chr$(1DH) + chr$(2FH) + chr$(00H) + chr$(01H)
   
   resProdTot = 500
   lineasCabecera.size = 0
   lineasFinales.size = 0
   lineasPagare.size = 0
   deducibles.size = 0
   tipoDocumento = 1
   terminal.indice = 0
   tieneBalanza = 0
   barraBalanza = 0
   limiteConsFinal = 20000

   !TO.HEADERLINE1$ = ""
   !TO.HEADERLINE2$ = ""
   !TO.TRAILERLINE1$ = ""
   !TO.TRAILERLINE2$ = ""

   call leeParametrosFiscales
   call cargaDetallePagosSRI
   call cargaParametrosTerminal

   tipoResumen = 0
   call limpiaFactura
   artCanasta = 0
   trxRecuperada = 0

end sub

!------------------------------------------------------------------------
! Control productos vendidos por peso y canastas
!------------------------------------------------------------------------
sub MSMTFACT08 public
   String s$
   Integer*4  c
   Integer*2  i, k, f
   Integer*1  a

!    call traceNUO( "MSMTFACT08 ITEM = [" + unpack$( IR.ITEMCODE$ ) +"] TOTALS=[" + str$(TS.TOTALS(0,0,0)) + "] BALDUE=[" + str$(TS.BALDUE(0)) + "]" )

   !No se permite vender mas de limiteConsFinal USD en modo autonomo
   if TS.STANDALONE then begin
      if ( TS.IO.KEYS(1) = 0 ) then begin
         c = val( unpack$( IR.SALEPRIC$ ) )
         if TS.IO.KEYS(6) = 75 then c = c * val( TS.IO.DATA$(6) )   ! unitarios con cantidad
!         if TS.IO.KEYS(6) = 72 then c = c * val( TS.IO.DATA$(6) )   ! pesables
         if ( TS.TOTALS(0,0,0) + c ) > limiteConsFinal then begin
            s$ = str$(limiteConsFinal)
            k = len(s$)
            if k > 2 then begin
               s$ = left$(s$, k - 2)
            EndIf else begin
               s$ = "." + right$("00" + s$, 2)
            EndIf
            s$ = "DEBE SUPERAR " + s$ + " USD"
            call muestraBorrar("EN AUTONOMO VTA. NO", s$)
            IR.INDICAT0 = IR.INDICAT0 OR 04H
            exit sub
         EndIf
      EndIf
   EndIf

   !Impide vender mas articulos cuando el arreglo de transaccion
   !supera el limite de alerta
   if noMasVenta then begin
      call muestraBorrar( "NO MAS PRODUCTOS", "FINALICE TRANSACCION" )
      IR.INDICAT0 = IR.INDICAT0 OR 04H
      exit sub
   EndIf

   ! Si articulo canasta
   if left$( unpack$( IR.ITEMCODE$ ), 4 ) = "0097" then begin

      venCanasta = 0

      !valida fecha de expiracion
      expCanasta = GETN4( IR.USERDATA$, 24 )      !AMMDDhhmm
      s$ = right$( date$, 5 ) + left$( time$, 4 )
      if val( s$ ) > expCanasta then begin
         venCanasta = -1
         IR.INDICAT0 = IR.INDICAT0 OR 04H
      EndIf
      artCanasta = -1
      exit sub

   EndIf

   ! Si articulo se vende por peso
   c = 0
   a = 0
   if IR.INDICAT0 and 40H then begin

      f = -1
      if barraBalanza then begin
         f = 0
         barraBalanza = 0
      EndIf else if tieneBalanza then begin
         TS.TEMP1I4 = 0
         call TSHIEC05
         if TS.TEMP1I4 > 0 and TS.TEMP1I4 < 15000 then begin
            TS.IO.DATA$(6) = str$(TS.TEMP1I4)
            TS.IO.KEYS(6) = 72
            f = 0
         EndIf else begin
            TS.TEMP1I4 = 0
         EndIf
      EndIf

      if (TS.IO.KEYS(6) <> 72) or ((TS.IO.KEYS(6) = 72) and (TS.IO.DATA$(6) = "")) then begin
         f = 0
      EndIf

      if TS.IO.KEYS(1) = 70 then begin
         a = -1
         c = val(TS.IO.DATA$(6))
      EndIf

      if f then begin

         ! autorizacion de supervisor y llave
         TO.USEREXIT(14) = 0
         call respaldaVariablesIO

         jGuiSubState = 80501
         call javaEvent(terminalSubStateMsg)

         if not entradaAutorizacion then begin
            IR.INDICAT0 = IR.INDICAT0 OR 04H
            f = 0
         EndIf
         call restauraVariablesIO
         TO.USEREXIT(14) = -1
      EndIf

      if f = 0 then exit sub

   EndIf

   !Si esta anulando un producto
   if a then begin

      !busca producto en arreglo
      k = -1
      i = 0

      While i < resProdNum
         k = consultaProducto( IR.ITEMCODE$, i )
         if k < 0 then begin
            i = resProdNum
         EndIf else begin
            if resProdTip%(k) = 1 then begin
               ! Se vende por peso
               if c = resProdCan%(k) then begin
                  k = i
                  i = resProdNum
               EndIf else begin
                  i = k + 1
                  k = -1
               EndIf
            EndIf else begin
               if c <= resProdCan%(k) then begin
                  k = i
               EndIf else begin
                  k = -1
               EndIf
               i = resProdNum
            EndIf

         EndIf
      Wend

      if k = -1 then begin
         IR.INDICAT0 = IR.INDICAT0 OR 04H
      EndIf

   EndIf

end sub

!------------------------------------------------------------------------
! Muestra el numero de articulos vendidos en la transaccion
!------------------------------------------------------------------------
sub cantidadArticulos
   Integer*1 f, tip%
   Integer*2 i
   Integer*4 can%, mon%
   String dato$, m1$, m2$

   f = 3
   ! Se solicita el ingreso de articulo a consultar
   otro.item:
   jGuiSubState = 80501
   call javaEvent(terminalSubStateMsg)
   dato$ = ingresoDatos$( "INGRESE PLU/EAN A","CONSULTAR" )
   if left$( dato$, 1 ) = "A" then goto saliringreso
   if left$( dato$, 1 ) <> "P" then goto saliringreso
   dato$ = right$( dato$, len( dato$ ) - 2 )
   if not esNumero( dato$ ) then begin
      f = f - 1
      if f = 0 then goto saliringreso
      call muestraBorrar( "DATO INVALIDO", "REINTENTE" )
      goto otro.item
   EndIf

   can% = 0
   mon% = 0
   m1$  = ""
   dato$ = pack$( right$( String$( 12, "0" ) + dato$, 12 ) )
   f = 0
   i = 0
   While i < resProdNum

      if dato$ = resProdCod$(i) then begin
         if not f then begin
            m1$  = resProdNom$(i)
            tip% = resProdTip%(i)
            f = -1
         EndIf
         can% = can% + resProdCan%(i)
         mon% = mon% + resProdVen%(i)
      EndIf
      i = i + 1
   Wend

   if not f then begin
      call muestraBorrar( "ARTICULO NO VENDIDO", "EN LA TRANSACCION" )
      goto saliringreso
   EndIf

   if tip% = 1 then \
      m2$ = formatea3Dec( can% ) \
   else \
      m2$ = str$( can% )
   m2$ = right$( String$( 20, " " ) + m2$ + "  $" + \
      formatea2Dec( round( 0.01 * mon%, 0, 0 ) ) + " ", 20 )

   if (JAVA.INIT = -1) then begin
      call muestraOperador( m1$, m2$ )
      wait; 4000
   EndIf else begin
      call muestraBorrar( m1$, m2$ )
   EndIf

   !call muestraBorrar( m1$, m2$ )

   saliringreso:
   call ignoraTeclas

end sub

!------------------------------------------------------------------------
! Despues de leer IOPROCE
!------------------------------------------------------------------------
sub MSMTFACT14 public
   Integer*1 f
   Integer*4 FAbase0, FAbase1, FAbase2

!call traceNUO( "MSM14 trxRecuperada=" + str$(trxRecuperada) + " TS.INTRX=" + str$(TS.INTRX) )

   !Si es una transaccion recuperada
   if trxRecuperada then begin
      if ( TS.IO.STATE = 5 ) and ( TS.IO.MOTORKEY = 61 ) and ( TS.IO.KEYS(5) = 61 ) then begin
         call QXL.TSUPEC21( "TRANSACCION RECUPERADA CONEX" )
         exit sub
      EndIf
      if TS.IO.MOTORKEY <> 81 then begin
         call QXL.TSUPEC21( "TRANSACCION RECUPERADA TOTAL" )
         TS.GUIDANCE = 1020
         TS.BAL.TAKEN = 0
         TS.IO.MOTORKEY = 0
      EndIf else begin
         call QXL.TSUPEC21( "TRANSACCION RECUPERADA DESCUENTOS" )
         call calcularDescuentos(-1)
         trxRecuperada = 0
      EndIf
      exit sub
   EndIf

   ! Consultar la cantidad de productos que existen en la TRX (CTRL + RUC)
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 174 ) and (TS.INTRX) then begin
       call cantidadArticulos
       goto salir
   EndIf

   !impide vender despues de ingresado un medio de pago
   !if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) and (TS.INTRX) and ( TS.TENDERED(0) <> 0 ) then begin
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) and (TS.IO.DATA$(2) <> "") and (TS.INTRX) and ( TS.TENDERED(0) <> 0 ) then begin
      call muestraBorrar( "NO PERMITIDO DESPUES", "DE UN PAGO" )
      goto salir
   EndIf

   ! Cuando se ingresa un articulo
   ! TS.IO.DEVICE
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) then begin

      ! Cuando se digita o ingresa un codigo de balanza
      if ( len( TS.IO.DATA$(2) ) > 11 ) and ( left$( TS.IO.DATA$(2), 2 ) = "22" ) then begin
         if TS.IO.DEVICE = 3 then begin
            TS.IO.DEVICE = 1
            barraBalanza = -1
         EndIf
         TS.IO.KEYS(6) = 72
         TS.IO.DATA$(6) = mid$( TS.IO.DATA$(2), 8, 5 )
         TS.IO.DATA$(2) = mid$( TS.IO.DATA$(2), 3, 5 )
      EndIf

      ! Cuando se digita o ingresa un codigo de cupon 05
      if (( len( TS.IO.DATA$(2) ) = 11 ) and ( left$( TS.IO.DATA$(2), 1 ) = "5" )) or \
         (( len( TS.IO.DATA$(2) ) = 12 ) and ( left$( TS.IO.DATA$(2), 2 ) = "05" )) then begin
         TS.GUIDANCE = 1039
         TS.IO.MOTORKEY = 0
         exit sub
      EndIf

      !Activa indicador para que se totalice
      TS.BAL.TAKEN = 0
      exit sub

   EndIf

   !No permite pagar cuando el monto es cero
   if ( TS.IO.MOTORKEY > 90 and TS.IO.MOTORKEY < 97 ) and TS.IO.STATE = 10 then begin
      if ( totalAcumulado - NUOTPROMdescuento ) = 0 then begin
         call muestraBorrar( "NO SE PERMITE VENTA", "EN CERO (0)  /BORRAR")
         goto salir
      EndIf
   EndIf

   !Si se trata de una venta premiada
   f = 0
   call promocionAleatoria( f )
   if f then exit sub

   ! Si esta pagando una venta mayor que limiteConsFinal y no se ha identificado cliente
   if matchb( chr$(TS.IO.MOTORKEY), teclasPago$, 1 ) > 0 then begin
      call tarifasConDescuento(FAbase0, FAbase1, FAbase2)
      if (FAbase0 + FAbase1 + FAbase2) >= limiteConsFinal and datosCliente$(5) = "" then begin
         call muestraBorrar( "DEBE CAPTURAR DATO", "DEL CLIENTE  /Borrar" )
         goto salir
      EndIf
   EndIf

   !Cuando esta anulando una venta de mas de USD 400
   if (TS.IO.MOTORKEY = 81) and (TS.IO.KEYS(1) = 70) and \
      (TS.TOTALS(0,0,0) > limiteAnulacion) then begin
      anula400 = -1
      exit sub
   EndIf

   exit sub

   salir:

   if java.init = -1 then begin
      jGuiSubState = 80671
      call javaEvent(terminalSubStateMsg)
   EndIf
   call ignoraTeclas

end sub

!------------------------------------------------------------------------
! Antes de grabar en archivo de transacciones
!------------------------------------------------------------------------
!sub MSMTFACT18 public
!   call traceNUO( "MSM18 " + str$( inicioFactura ) + " TS.RETV.IN.PROGRESS=" + str$(TS.RETV.IN.PROGRESS) + " TS.RECOVERY=" + str$(TS.RECOVERY) )
!end sub

!------------------------------------------------------------------------
! Al imprimir una linea en el recibo del cliente
!------------------------------------------------------------------------
function MSMTFACT20 public
   Integer*1 MSMTFACT20

   Integer*4 Ven%,  \ monto total sin impuesto ni descuento (2 decimales)
             Imp%,  \ monto total impuesto (2 decimales)
             Can%,  \ cantidad de productos (si se vende por peso 3 decimales)
             k

   Integer*1 Tip%,  \ tipo de producto: 0 normal, 1 se vende por peso, 2 canasta
             Ded%   ! tipo de deducible

   Integer*2 Cla%,  \ clase
             Sub%,  \ subclase
             i, n

   String    Sku$,  \ codigo interno
             Imp$,  \
             s$, m$

   MSMTFACT20 = -1

!call traceNUO( "MSM20 TS.RETV.IN.PROGRESS=" + str$(TS.RETV.IN.PROGRESS) + " TS.RECOVERY=" + str$(TS.RECOVERY)  )
!call traceNUO( "MSM20 inicioFac " + str$(inicioFactura) + " resProdNum=" + str$(resProdNum) )

   !Elimina mensaje de transaccion recuperada
   if (TS.LINETYPE = 31) and (TS.LINEDATA = 3) then begin
      call QXL.TSUPEC21( "TRANSACCION RECUPERADA" )
      inicioFactura = -1
      exit function
   EndIf

   !acumula item
   if (TS.LINETYPE = 1) and (TS.LINEDATA = 0) then begin
      Ven% = TS.XPRICE
      afeCanasta = 0
      Imp% = 0
      Can% = SL.IE.QTYORWGT
      Tip% = 0
      Imp$ = "  "

      !es una canasta
      if artCanasta then begin

         artCanasta = 0
         Tip% = 2

         !calcula el impuesto
         afeCanasta = GETN4( IR.USERDATA$, 0 )
         if afeCanasta > 0 then begin
            afeCanasta = 100 * afeCanasta * Can%
            Imp% = round( 1.0 * impuestos.iva * afeCanasta / ( 100 + impuestos.iva ), 0, 0 )
            if Ven% < 0 then begin
               afeCanasta = - afeCanasta
               Imp% = - Imp%
            EndIf
            Imp$ = " I"
         EndIf

      EndIf else begin

         !se vende por peso
         if IR.INDICAT0 and 40H then Tip% = 1

         !Calcula el monto de impuesto
         if IR.INDICAT1 and 80H then begin
            Imp% = round( 100.0 * impuestos.iva * Ven% / ( 100 + impuestos.iva ), 0, 0 )
            Imp$ = " I"
         EndIf

      EndIf

      !si es una resta se cambia a negativo la cantidad (TS.XPRICE ya esta en negativo)
      !if (TS.XXMOD = 8) then begin
      !   Can% = - Can%
      !EndIf

      !controla la cumulacion de articulos para inventario ya que la variable TS.XPRICE viene en cero cuando se trata de inventario
      if ( TS.IO.KEYS(1) = 70 ) and ( TS.IO.KEYS(6) = 75 )  and ( Ven% = 0 ) then Can% = - Can%

      !si el monto es negativo se hace negativa la cantidad tambien
      if Ven% < 0 then begin
         Can% = - Can%
      EndIf

      !Deducible
      k = GETN4( IR.USERDATA$, 20 )
      if k > 0 and k < 5 then \
         Ded% = k \
      else \
         Ded% = 0

      !Codigo interno
      k = GETN4( IR.USERDATA$, 16 )
      Sku$ = pack$( str$( k ) )

      Cla% = IR.USEREXIT1
      Sub% = IR.USEREXIT2

      !NDEPTO (grupo y departamento)
      k = GETN4( IR.USERDATA$, 12 )
      s$ = right$( String$( 8, "0" ) + str$( k ), 8 )

      m$ = \
         left$( s$, 4 ) + \                                ! departamento
         right$( s$, 4 ) + \                               ! grupo
         right$( String$( 4, "0" ) + str$( Cla% ), 4 ) + \ ! clase (subcategoria)
         right$( String$( 4, "0" ) + str$( Sub% ), 4 ) + \ ! sub clase (clase)
         "0"

      call acumulaProductos( IR.ITEMCODE$, Sku$, IR.ITEMNAME$, Ven%, Imp%, Can%, Tip%, Ded% , m$)

      !Muestra el articulo vendido en la Interfaz grafica
      s$ = TS.PRTBUF$
      if Tip% = 1 then begin
         TS.PRTBUF$ = formatea3Dec( Can% ) + " Kg x " + formatea2Dec( SL.IE.SALEPRIC ) + "/Kg"
         call javaEvent(updateReceiptMsg)
      EndIf else if Can% <> 1 then begin
         TS.PRTBUF$ = str$( Can% ) + " x " + formatea2Dec( SL.IE.SALEPRIC )
         call javaEvent(updateReceiptMsg)
      EndIf
      TS.PRTBUF$ = unpack$( IR.ITEMCODE$ ) + " " + left$( IR.ITEMNAME$, 16 ) + \
         right$( String$( 7, " " ) + formatea2Dec( Ven% ), 7 ) + Imp$
      call javaEvent(updateReceiptMsg)
      inicioFactura = -1
      TS.PRTBUF$ = s$
      exit function
   EndIf

   !Elimina linea de cantidad o peso
   if TS.LINETYPE = 1 and TS.LINEDATA = 1 then begin
      exit function
   EndIf

   !Elimina linea de total en la conexion del operador
   if (TS.LINETYPE = 8) and (TS.LINEDATA = 0) then begin
      exit function
   EndIf

   !Elimina linea de encabezado del supermarket
   if TS.LINETYPE = 18 and TS.LINEDATA = 99 then begin
      exit function
   EndIf

   !Elimina otras lineas de impresion
   if ( (TS.LINETYPE = 5) and (TS.LINEDATA = 3) ) or \
      ( (TS.LINETYPE = 31) and (TS.LINEDATA = 4) ) or \
      ( TS.LINETYPE = 29 ) then begin
      exit function
   EndIf

   !Activa indicador de comprobante de trx anulada
   if (TS.LINETYPE = 22) and (TS.LINEDATA = 0) and tipoResumen = 0 then begin
      tipoResumen = 1
      exit function
   EndIf

   !Activa indicador de comprobante de trx suspendida
   if (TS.LINETYPE = 31) and (TS.LINEDATA = 1) then begin
      if ( left$( TS.PRTBUF$, 19 ) = "NUMERO RECUPERACION" ) and ( tipoResumen = 0 ) then begin
         tipoResumen = 2
         lineaDatosTrx$ = TS.PRTBUF$
      EndIf
      exit function
   EndIf

   !actualiza GUI
   if TS.LINETYPE = 7 and TS.LINEDATA = 1 then begin
      call javaEvent(updateReceiptMsg)
      exit function
   EndIf

   !actualiza GUI
   if ( TS.LINETYPE = 2 )  or \
      ( TS.LINETYPE = 4 and TS.LINEDATA = 1 ) then begin
      call javaEvent(updateReceiptMsg)
      exit function
   EndIf

   if (TS.LINETYPE = 6) AND (TS.LINEDATA = 1) then begin

      !Imprime descripcion de descuento en SJ
      if TS.PROCEDURE = -1 then begin
         n = len( lineasDescuento$ )
         i = 1
         While( i < n )
            s$ = mid$( lineasDescuento$, i, 35 )
            call imprimeSJ( s$ )
            i = i + 35
         Wend
      EndIf

      !Imprime resumen de transaccion anulada o suspendida
      if tipoResumen <> 0 then begin
         call imprimeResumen( tipoResumen )
         call javaEvent(updateReceiptMsg)
         tipoResumen = 0
         call limpiaFactura
         exit function
      EndIf

      !Imprime factura
      if TS.INTRX then begin
         lineaDatosTrx$ = TS.PRTBUF$

         TS.PRTBUF$ = "INICIO FACTURA"
         call javaEvent(updateReceiptMsg)

         call imprimeCabeceraFactura
         call imprimeDetalleProductos( 0 )
         call imprimeDatosFactura
         call cadenaFactura
         call limpiaFactura
         Call imprimeQRpublicidad
         
         TS.PRTBUF$ = "FINAL FACTURA"
         call javaEvent(updateReceiptMsg)

         exit function
      EndIf

   EndIf
   MSMTFACT20 = 0

end function

!------------------------------------------------------------------------
! Antes de mostrar un mensaje en el visor
!------------------------------------------------------------------------
sub MSMTFACT23 public

   !canasta vencida
   if venCanasta then begin
      if TS.LINETYPE = 12 then begin
         TS.DISP1$ = "CANASTA YA EXPIRO"
         TS.DISP2$ = "FECHA : 202" + left$( str$( expCanasta ), 5 )
         venCanasta = 0
      EndIf
   EndIf

   !al vender un articulo muestra monto total acumulado de la venta
   if ( TS.TENDERED(0) = 0 ) and ( TS.IO.STATE = 10 ) and ( TS.LINETYPE = 1 ) then begin
      TS.DISP2$ = left$( "(" + formatea2Dec( totalAcumulado ) + ")" + String$( 10, " " ), 10 ) + \
         right$( TS.DISP2$, 10 )
      TS2.DISP2$ = TS.DISP2$
   EndIf

end sub

!------------------------------------------------------------------------
! Retorna indicador de balanza conectada
!------------------------------------------------------------------------
function MSMTFACT27 public
   Integer*1 MSMTFACT27
   MSMTFACT27 = tieneBalanza
end function

!------------------------------------------------------------------------------
! En el calculo del saldo a pagar
!------------------------------------------------------------------------------
sub MSMTFACT30 public
   Integer*4 t0, t1, t2

   t0 = baseTarifa(0)
   t1 = baseTarifa(1)
   t2 = baseTarifa(2)

!call traceNUO( "MSM30 t0=" + str$(t0) + " t1=" + str$(t1) + " t2=" + str$(t2) )

end sub

!------------------------------------------------------------------------------
! En la salida del modo autonomo
!------------------------------------------------------------------------------
sub MSMTFACT35 public
   Integer*4 numFactura

   call actualizaFactura(0)

!call traceNUO( "MS35 sec=" + str$( terminal.secFactura ) + " HT$=" + lineahex$( TE.TR.USERHT$ ) )

   numFactura = val( TE.TR.USERHT$ )
   if numFactura > terminal.secFactura then begin
      terminal.secFactura = numFactura
   EndIf else begin
      numFactura = terminal.secFactura
      TE.TR.USERHT$ = right$( String$( 10, "0" ) + str$( numFactura ), 10 )
   EndIf

!call traceNUO( "MS35 sec=" + str$(terminal.secFactura) + " HT$=" + lineahex$(TE.TR.USERHT$) )

end sub

!------------------------------------------------------------------------------
! En la intervencion del jefe de departamento
!------------------------------------------------------------------------------
!sub MSMTFACT36 public
!end sub

!------------------------------------------------------------------------------
! En el control de errores
!------------------------------------------------------------------------------
!sub MSMTFACT37 public
!end sub

!------------------------------------------------------------------------
! En la comprobación de los limites de entrada, edición y
! cancelación de artículos
!------------------------------------------------------------------------
!sub MSMTFACT39 public
!
!call traceNUO( "MSM39 LD=" + str$( TS.LINEDATA ) + " cod=" + unpack$(IR.ITEMCODE$) )
!
!end sub

!------------------------------------------------------------------------
! En la recuperacion de transaccion
!------------------------------------------------------------------------
sub MSMTFACT53 public
   String m$, f$

!call traceNUO( "MSM53 SL.END.RCOVRD=" + str$(SL.END.RCOVRD) + " TS.RETV.IN.PROGRESS=" + str$(TS.RETV.IN.PROGRESS) + " TS.RECOVERY=" + str$(TS.RECOVERY) )
!call traceNUO( cadenahex$( SL.STR.ENTRY$ ) )

   !Si es una recuperacion de transaccion suspendida o al cargar la aplicacion
   if TS.RETV.IN.PROGRESS or TS.RECOVERY then trxRecuperada = -1

   !Activa indicador de canasta
   m$ = SL.STR.ENTRY$
   f$ = getCampoTSL$(m$,1)
   if f$ = "01" then begin
      f$ = getCampoTSL$(m$,2)
      if len(f$) = 10 and left$(f$,2) = "97" then artCanasta = -1
   EndIf

end sub

!------------------------------------------------------------------------
! Despues de escribir en el TSL
!------------------------------------------------------------------------
!sub MSMTFACT59 public
!   call traceNUO( "MSM59 " + str$(inicioFactura) )
!end sub

!------------------------------------------------------------------------
! Despues de la intervencion del jefe de departamento
!------------------------------------------------------------------------
sub MSMTFACT66(callparm) public
   Integer*1 callparm   ! -1 at good mgr ovr, 0 at bad mgr ovr
   Integer*1 f, k
   String dato$

!call traceNUO("MSM66 callparm=" + str$(callparm))

   if anula400 then begin

      anula400 = 0
      if callparm then begin
         call respaldaVariablesIO
         dato$ = str$( limiteAnulacion )
         if len(dato$) < 3 then dato$ = right$("000" + dato$, 3)
         dato$ = left$( dato$, len( dato$ ) - 2 ) + "." + right$( dato$, 2 )
         call muestraBorrar( "INGRESAR AUTORIZADOR", "ANULA SOBRE US" + dato$ )
         call muestraVisor(1, "INGRESAR OTRO", "SUPERVISOR")

         k = 3
         entrada:

!call traceNUO( "MSM66 TS.TEMP1I4=" + str$(TS.TEMP1I4) )

         jGuiSubState = 80501
         call javaEvent(terminalSubStateMsg)
         TO.USEREXIT(14) = 0
         f = supervisor
         TO.USEREXIT(14) = -1
         if f then begin
            if TS.TEMP1I4 = val( TS.IO.DATA$(10) ) then begin
               call muestraBorrar( "DEBE INGRESAR UN", "AUTORIZADOR DISTINTO" )
            EndIf else begin
               goto salir
            EndIf
         EndIf

         k = k - 1
         if k > 1 then begin
            call muestraBorrar( "B059 COMPRUEBE", "NUM ALTERACION" )
            goto entrada
         EndIf
         TS.GUIDANCE = 1059

         salir:
         call restauraVariablesIO
      EndIf
   EndIf

!call traceNUO("MSM66 fin rutina")
end sub

!------------------------------------------------------------------------
! Antes de actualizar la matriz de transacciones
!------------------------------------------------------------------------
sub MSMTFACT68(ENTRY$, ELEMENT) public
   String ENTRY$, s$
   Integer*2 ELEMENT
   Integer*4 v
   Integer*2 k

!call traceNUO( "MSM68 inicioFactura=" + str$( inicioFactura ) + " TS.RECOVERY=" + str$(TS.RECOVERY) )
!call traceNUO( "MSM68 SL.END=" + str$(SL.END) + " ELEMENT=" + str$(ELEMENT) + " " + lineahex$(ENTRY$) )

   if not inicioFactura then begin
      !Inutiliza cadenas de pago
      if TS.RECOVERY then begin
         s$ = left$( ENTRY$, 1 )
         if s$ = chr$(5) or s$ = chr$(6) then ENTRY$ = pack$("99") + ":" + pack$("99") + ENTRY$
      EndIf
      exit sub
   EndIf

   if ELEMENT = 0 then exit sub

   ! Agrega cadena con datos de documento fiscal
   ! aqui se agregan los valores en cero
   ! despues de imprimir (MSMTFACT20) se debe actualizar SL.STR$(1)
   if ELEMENT = 1 then begin
      s$ = chr$(99H) + ":" + chr$(11H) + ":" + \
         chr$(2) + ":" + \
         chr$(1) + ":" + chr$(1) + ":" + chr$(1) + ":" + \
         chr$(0) + ":" + String$( 4, chr$(0) + ":" ) + ":::::"
      call agregaCadenaTSL68( s$ )
      ELEMENT = ELEMENT + 1
   EndIf

   !esta agregando una cadena 01
   if left$( ENTRY$, 1 ) = chr$(1) then begin

!call traceNUO( "MSM68 nCad1=" + str$(noCadena1) + " indice=" + str$(indiceActual) + " cod=" + unpack$(resProdCod$(indiceActual)) + " can=" + str$(resProdCan%(indiceActual)) + " tsl=" + str$(resProdTsl%(indiceActual)))

      if noCadena1 then begin
         noCadena1 = 0
         noCadena2 = -1
         noCadena10 = -1
         goto noAgregaCadena
      EndIf

      ! Agrega cadena con datos adicionales del producto
      resProdTsl%(indiceActual) = ELEMENT

!call traceNUO( "MSM68 agrega cadena ELEMENT=" + str$(ELEMENT) + " " + lineahex$( resProd99$ ) )

      call agregaCadenaTSL68( resProd99$ )
      ELEMENT = ELEMENT + 1
      noCadena2 = 0
      noCadena10 = 0

      ! modifica el campo extended price
      v = round( 0.01 * resProdVen%( indiceActual ), 0, 0 )
      s$ = pack$( str$( v ) )
      call modificaCadenaTSL$( ENTRY$, s$, 3 )

      ! cambia item cancel a normal item
      v = val( getCampoTSL$( ENTRY$, 8 ) )
      v = v and 0FH
      s$ = pack$( str$( v ) )
      call modificaCadenaTSL$( ENTRY$, s$, 8 )

      ! cambia indicador cancel key pressed a 0
      v = val( getCampoTSL$( ENTRY$, 7 ) )
      v = v and 0FF7FH
      s$ = pack$( str$( v ) )
      call modificaCadenaTSL$( ENTRY$, s$, 7 )

      !si esta activo el indicador extension follow this item
      if ( v and 2 ) then begin
         if resProdCan%( indiceActual ) = 1 then begin
            v = v and 0FFFDH   !quita el indicador extension follow
            s$ = pack$( str$( v ) )
            call modificaCadenaTSL$( ENTRY$, s$, 7 )
            noCadena2 = -1
         EndIf
      EndIf else begin
         if resProdCan%( indiceActual ) <> 1 then begin
            v = v or 02
            s$ = pack$( str$( v ) )
            call modificaCadenaTSL$( ENTRY$, s$, 7 )

!call traceNUO( "MSM68 agrega cadena ELEMENT=" + str$(ELEMENT) + " " + lineahex$( ENTRY$ ) )

            call agregaCadenaTSL68( ENTRY$ )
            ELEMENT = ELEMENT + 1

            !Cantidad tecleada
            k = 20H

            ! es un producto vendido por peso
            if resProdTip%( indiceActual ) = 1 then k = 40H

            !INDICAT1
            s$ = pack$( str$( k ) )

            !prepara cadena 02
            ENTRY$ = chr$(2) + ":" + \
               ":" + \
               ":" + \
               ":" + \
               pack$( str$( SL.IE.SALEQUAN ) ) + ":" + \
               pack$( str$( SL.IE.SALEPRIC ) ) + ":" + \
               pack$( str$( resProdCan%( indiceActual ) ) ) + ":" + \
               s$
         EndIf
      EndIf
      goto salir

   EndIf

   !esta agregando una cadena 02
   if left$( ENTRY$, 1 ) = chr$(2) then begin

!call traceNUO( "MSM68 nCad2=" + str$(noCadena2) + " indice=" + str$(indiceActual) + " cod=" + unpack$(resProdCod$(indiceActual)) + " can=" + str$(resProdCan%(indiceActual)) + " tsl=" + str$(resProdTsl%(indiceActual)) + " ELEMENT=" + str$(ELEMENT) + " ENTRY$=" + lineahex$( ENTRY$ ) )

      if noCadena2 then begin
         noCadena2 = 0
         goto noAgregaCadena
      EndIf

      !modifica el campo quantity or weight de la cadena 02
      s$ = pack$( str$( resProdCan%( indiceActual ) ) )
      call modificaCadenaTSL$( ENTRY$, s$, 7 )

      !modifica el campo INDICAT1 de la cadena 02
      s$ = getCampoTSL$( ENTRY$, 8 )
      k = 0
      if esNumero( s$ ) then k = val( s$ )

      !cantidad tecleada
      !k = k or 20H

      ! es un producto vendido por peso
      if resProdTip%( indiceActual ) = 1 then k = k or 40H else k = k or 20H
      s$ = pack$( str$( k ) )

      call modificaCadenaTSL$( ENTRY$, s$, 8 )
      goto salir
   EndIf

   !esta agregando una cadena 10
   if left$( ENTRY$, 1 ) = chr$(10H) then begin

!call traceNUO( "MSM68 nCad10=" + str$(noCadena10) + " indice=" + str$(indiceActual) + " cod=" + unpack$(resProdCod$(indiceActual)) + " can=" + str$(resProdCan%(indiceActual))+ " ELEMENT=" + str$(ELEMENT) + " tsl=" + str$(resProdTsl%(indiceActual)))

      if noCadena10 then begin
         noCadena10 = 0
         goto noAgregaCadena
      EndIf
      goto salir
   EndIf

   !esta agregando una cadena 11
   if left$( ENTRY$, 1 ) = chr$(11H) then begin

!call traceNUO( "MSM68 no agrega cadena 11 indice=" + str$(indiceActual) + " cod=" + unpack$(resProdCod$(indiceActual)) + " can=" + str$(resProdCan%(indiceActual))+ " ELEMENT=" + str$(ELEMENT) + " tsl=" + str$(resProdTsl%(indiceActual)))

      goto noAgregaCadena
   EndIf

   salir:
   !validar que pasa si en vez de noMasVenta se usa TS.INHIBIT
   noMasVenta = 0
   if SL.END >= GC.SL.WARN then noMasVenta = -1
   exit sub

   noAgregaCadena:
   call evitaCadenaTSL( ELEMENT, ENTRY$ )

end sub

!-------------------------------------------------------------------------
!Setea el valor de la variable local tipoDocumento, utilizar para N/C
!-------------------------------------------------------------------------
sub setTipoDocumento(tipo) public
   Integer *1 tipo
   tipoDocumento = tipo
end sub

!-------------------------------------------------------------------------
! Retorna arreglo con codigos de impuestos
!-------------------------------------------------------------------------
function MSMTFACTimpuestos$(i) public
   String MSMTFACTimpuestos$
   Integer*1 i
   if i > impuestos.size then begin
      MSMTFACTimpuestos$ = ""
      exit function
   EndIf
   MSMTFACTimpuestos$ = impuestos$( i, 1 )
end function

!-------------------------------------------------------------------------
! Retorna RUC del establecimiento
!-------------------------------------------------------------------------
function MSMTFACTrucEstablecimiento$ public
   String MSMTFACTrucEstablecimiento$
   MSMTFACTrucEstablecimiento$ = establecimiento.ruc$
end function

!-------------------------------------------------------------------------
! Retorna No. Serie de Terminal
!-------------------------------------------------------------------------
function MSMTFACTserieTerminal$ public
   String MSMTFACTserieTerminal$
   MSMTFACTserieTerminal$ = terminal.serie$
end function

!-------------------------------------------------------------------------
! Retorna secuencia del No de Factura de la transaccion
!-------------------------------------------------------------------------
function MSMTFACTnumeroFactura public
   Integer*4 MSMTFACTnumeroFactura
   MSMTFACTnumeroFactura = terminal.secFactura
end function

!-------------------------------------------------------------------------
! Retorna tipo de ambiente SRI
!-------------------------------------------------------------------------
function MSMTFACTtipoAmbienteSRI$ public
   String MSMTFACTtipoAmbienteSRI$
   MSMTFACTtipoAmbienteSRI$ = establecimiento.ambiente$
end function

!-------------------------------------------------------------------------
! Retorna tipo de emision SRI
!-------------------------------------------------------------------------
function MSMTFACTtipoEmisionSRI$ public
   String MSMTFACTtipoEmisionSRI$
   MSMTFACTtipoEmisionSRI$ = establecimiento.emision$
end function

!-------------------------------------------------------------------------
! Retorna secuencia SRI
!-------------------------------------------------------------------------
function MSMTFACTsecuenciaSRI$ public
   String MSMTFACTsecuenciaSRI$
   MSMTFACTsecuenciaSRI$ = establecimiento.secuencia$
end function


!-------------------------------------------------------------------------
!codigo de producto segun indice (PD 6)
!-------------------------------------------------------------------------
function MSMTFACTresProdCod$( i ) public
   String MSMTFACTresProdCod$
   Integer *2 i
   MSMTFACTresProdCod$ = resProdCod$( i )
end function

!-------------------------------------------------------------------------
! numero de productos ingresados
!-------------------------------------------------------------------------
function MSMTFACTresProdNum public
   Integer*2 MSMTFACTresProdNum
   MSMTFACTresProdNum = resProdNum
end function

!-------------------------------------------------------------------------
! Genera linea de detalle para descuento
! i indice del producto
!-------------------------------------------------------------------------
function MSMTFACTlineaDescuento$( i ) public
   String MSMTFACTlineaDescuento$
   Integer*2 i
   Integer*4 v
   String cant, peso, s$

   MSMTFACTlineaDescuento$ = ""
   if resProdCod$( i ) = "" then exit function

   !cantidad precio unitario y precio unitario con descuento (2 decimales)
   cant = ""
   peso = ""
   if resProdTip%( i ) = 1 then begin
      v = round( 10.0 * resProdVen%( i ) / resProdCan%( i ), 0, 0 )
      peso = str$( resProdCan%( i ) )
   EndIf else begin
      v = round( 0.01 * resProdVen%( i ) / resProdCan%( i ), 0, 0 )
      cant = str$( resProdCan%( i ) )
   EndIf

   s$ = String$( 7, "0" )
   MSMTFACTlineaDescuento$ = \
      unpack$( resProdCod$( i ) ) + \  !Codigo producto
      right$( s$ + str$( i ), 4 ) + \  !Linea
      right$( s$ + cant, 6 ) + \       !Cantidad
      right$( s$ + peso, 7 ) + \       !Peso
      right$( s$ + str$( v ), 7 ) + \  !Precio
      "1" + \                          !Descontable
      resProdCat$( i )                 !Categoria del producto

end function

!-------------------------------------------------------------------------
! Logo de encabezado de factura
!-------------------------------------------------------------------------
function MSMTFACTlogo$ public
   String MSMTFACTlogo$
   MSMTFACTlogo$ = MSMlogo1$
end function

!-------------------------------------------------------------------------
! Retorna codigo impuesto IVA
!-------------------------------------------------------------------------
function MSMTFACTimpuestoIVA% public
   Integer*1 MSMTFACTimpuestoIVA%
   MSMTFACTimpuestoIVA% = impuestos.iva
end function

!-------------------------------------------------------------------------
! Retorna el total de venta sin desglose de impuesto ni descuentos
!-------------------------------------------------------------------------
function MSMTFACTtotal public
   Integer*4 MSMTFACTtotal
   MSMTFACTtotal = totalAcumulado
end function
