!-----------------------------------------------------------------------
!Modulo de Promociones MSM
!Fecha: 06 de Julio de 2021
!Autor: Alejandro Farre P
!-----------------------------------------------------------------------
%ENVIRON T

! Contiene la informacion de descuentos ya aplicados
! se define global para simplificar su uso al distribuir
! los descuentos en el modulo MSMTFACT

!-----------------------------------------------------------------------
! Observaciones:
! Mod 05Jun2025
! Se modifica modulo promociones para aplicación descuento adicional generado
! por forma de pago DeUna.
! Desarollado por Grupo Retail - OVS
!-----------------------------------------------------------------------

String global desAplic(1)

Integer*1                \
   activo,               \
   depurar,              \
   pagoEfectivo,         \
   pipeAbierto,          \ indicador de PIPE de escucha abierto
   calcularDesc,         \
   calcDescuento         ! Indicador para calcular descuentos

!Variables para las tramas de respuesta de comunicacion (Response)
String \
   resLargo,             \ Largo del mensaje de respuesta
   resEncabezado,        \ Encabezado mantiene el mismo formato que la trama inicial de la solicitud 50 caracteres
   resPromociones,       \ Contiene la informacion de respuesta de promociones
   resCupones,           \ Contiene la informacion correspondiente a una promociones de cupones
   resCupones$(1)        ! Contiene la informacion correspondiente a todas las promociones de cupones

Integer*1 promoRDM       ! indicador de promocion "RDM" --> Compra gratis

Integer*4                \
   descEfectivo,         \
   desTotal              ! Monto total de descuento incluido impuesto

!Variables de informacion de cupones
String                   \
   cupNomPromocion,      \ Descripción de la promoción: 100 caracteres. (VER)
   cupCodigoPromocion,   \ Codigo de promocion 4 caracteres
   cupIdPromocion,       \ Id de promocion 6 caracteres
   cupTipo,              \ Tipo de cupón: 2 caracteres
   cupFechaInicio,       \ 6 caracteres (ddmmaa). Fecha de expedición
   cupFechaFin,          \ 6 caracteres (ddmmaa). Fecha de vencimiento
   cupMensaje            ! 200 caracteres. Este mensaje es el que se imprimirá en el TICKET

!Variables de informacion de promociones de medios de pago
String                   \
   pagCodigo$(2),        \ Codigos de medio de pago del motor de promociones
   idPago$,              \ Contiene la informacion del pago que se esta ingresando
   Pagos$(1)             ! Contiene la informacion de pagos ingresados

Integer*2 desNum         ! numero de descuentos aplicados

Integer*1                \
   pagCantCodigo%,       \ Cantidad de mapeo de codigos de medio de pago
   pagDescuento,         \ Indica si es un descuento al pago
   desInd                ! Indice al arreglo de descuentos aplicados

! Variables de comunicacion pi: hacia JAVA
String                   \
   pipeInName,           \ Nombre de pipe de escritura JAVA
   pipeOutName           ! Nombre de pipe para lectura JAVA

Integer*4                \
   bufferSize%,          \ Numero de bytes a escribir
   pipeTimeout%,         \ Tiempo maximo de espera para lectura de pipe creado por JAVA con la trama de respuesta
   cuponTimeout          ! Tiempo maximo de espera para consulta de cupones externos

Integer*2                \
   pipeInSession%,       \ Sesion para apertura de pipe de lectura
   pipeOutSession%       \ Sesion para apertura de pipe de escritura


Integer*2 Global GrtraceOn%            ! Grabacion trace de rastreo aplicacion

!variables para descuento empleado
Integer*1                \
   empleadoCon           ! Indicador para consultar descuento empleado

Integer*4                \
   empleadoCup           ! monto disponible de descuento empleado

String                   \
   empleadoCod,          \ id de empleado
   empleadoDes,          \ Codigo de descuento empleado
   cupoNext$             ! Cupones externos ingresados

Integer*1                \
   empleadoInd           ! Indicadores de descuento empleado

Integer*2                \
   cupoNextInd,          \ indice de cupon externo
   cupoNextCod(1),       \ codigo de promocion de cupon externo
   cupoNextCan(1)        ! cantidad de cupon externo

String version$
String Gr.Prm.LstTerm$, Gr.Demo$
Integer*1 global TS.RECOVERY
Integer*4 Global GR.TOTAL.DSC%,																						 \!
                 Gr.DeUna.Dscto%                                            ! Dscto pago deuna 
!-----------------------------------------------------------------------
! variables globales de otros modulos
!-----------------------------------------------------------------------
%INCLUDE EAMTOPTS.J86
%INCLUDE EAMTRANS.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMITEMR.J86
%INCLUDE JAVAGUIV.J86

!-----------------------------------------------------------------------
! Rutinas publicas de otros modulos
!-----------------------------------------------------------------------
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86
%INCLUDE EAMASMCT.J86
%INCLUDE JAVAGUIC.J86

%INCLUDE RECATSSU.011          					      															! RUTINAS GENERICAS APLICACION     

!-----------------------------------------------------------------------
! RUTINAS DEL MODULO EAMTSTPC.BAS
!-----------------------------------------------------------------------
\ REM  ****************  Function  BINPACK$ *****************************
\ REM      THIS Function  IS USED TO PRODUCE A PACKED DECIMAL
\ REM      NUMBER FROM AN Integer VALUE.
\ REM      IT TAKES AS INPUT THE Integer AND RETURNS THE String.
\ REM      P2 SPECIFIES THE LENGTH OF THE String BUT If P2 IS
\ REM      ZERO Then THE String WILL BE SET TO THE SHORTEST
\ REM      POSSIBLE THAT CAN CONTAIN THE Integer.
\ REM      THIS ROUTINE WILL ALSO ADD A COLON TO THE End OF THE String.
\ REM  ****************************************************************
\ REM   Function  BINPACK$(P1, P2)
\ REM   CONVERT P1 TO PACKED DECIMAL AND RETURN THE VALUE
\ REM   End Function 
\ REM  ****************************************************************
  Function  BINPACK$(BP1,BP2) EXTERNAL
  String BINPACK$
  Integer*2 BP1,BP2
  End Function 

\ REM  ****************  Function  ABINPACK$ ****************************
\ REM      THIS Function  IS USED TO PRODUCE A PACKED DECIMAL
\ REM      NUMBER FROM AN Integer VALUE.
\ REM  ****************************************************************
  Function  ABINPACK$(BP1) EXTERNAL
  String ABINPACK$
  Integer*2 BP1
  End Function                                     !

\ REM  ****************  Function  BINPACK4$ ****************************
\ REM      THIS ROUTINE IS IDENTICAL TO BINPACK$ ABOVE EXCEPT
\ REM      THAT IT TAKES A 4 BYTE Integer.
\ REM
\ REM      THIS ROUTINE HAS BEEN DEFINED TO AVOID THE LARGE AMOUNT
\ REM      OF STORAGE REQUIRED TO CONVERT PARAMETERS TO 4 BYTES.
\ REM  ****************************************************************
\ REM
\ REM   Function  BINPACK4$(P1, P2)
\ REM   CONVERT P1 TO PACKED DECIMAL AND RETURN THE VALUE
\ REM   End Function 
\ REM
\ REM  ****************************************************************
  Function  BINPACK4$(BP1,BP2) EXTERNAL
  String BINPACK4$
  Integer*4 BP1
  Integer*2 BP2
  End Function                                     !

\ REM  ****************  Function  ABINPACK4$ ****************************
\ REM      THIS ROUTINE IS IDENTICAL TO BINPACK4$ ABOVE EXCEPT
\ REM      THAT IT TAKES NO SECOND PARAMETER.
\ REM  ****************************************************************
  Function  ABINPACK4$(BP1) EXTERNAL
  String ABINPACK4$
  Integer*4 BP1
  End Function                                     !

Function BIGLEN(A$) EXTERNAL																								! Longitud de dato
  Integer*4 BIGLEN																													!
  String A$																																	!
End Function																																!

!------------------------------------------------------------------------------
! Retorna datos de cliente o "_________" si no existe
! k = 1 codigo de cliente
! k = 2 nombre del cliente
! k = 3 direccion del cliente
! k = 4 telefono del cliente
! n     numero de caracteres para formatear
!------------------------------------------------------------------------------
Function  datosCliente$( k, n ) external 
   String datosCliente$
   Integer*2 k, n
End Function  

!-----------------------------------------------------------------------------
!Escribe log de auditoria
!t$ titulo
!m$ mensaje
!-----------------------------------------------------------------------------
Sub grabaLogPromocion(t$, m$)
   String t$, m$
   If not depurar Then Exit Sub
   If t$ <> "" Then Call  traceNuo(t$)
   Call  traceNuo(m$)
End Sub

!Disponibiliza si se trata de una promocion Aleatoria
Sub promocionAleatoria( f ) public
    Integer*1 f
    If promoRDM Then f = -1 Else f = 0
End Sub 

!Disponibiliza RUC de MSM
Sub getRUCMSM( RUCMSM ) external
    String RUCMSM
End Sub 

!------------------------------------------------------------------------------
!limpio variables con datos de descuento empleado (tipo, disponible y flag)
!------------------------------------------------------------------------------
Sub limpiaDesEmpleado
   empleadoCon = 0
   empleadoDes = "0"
   empleadoInd = 1
   empleadoCup = 0
   empleadoCod = ""
End Sub

!-----------------------------------------------------------------------------
!limpia las variables de promociones
! se llama antes de consultar al motor de promociones los descuentos al total
!-----------------------------------------------------------------------------
Sub limpiaPromociones

   dim desAplic(20)
   dim Pagos$(20)
   dim resCupones$(20)
   desNum = 0
   desTotal = 0
   desInd = 0

   pagDescuento = 0
   idPago$ = ""

End Sub

!-----------------------------------------------------------------------------
! Genera encabezado de mensajeria
!-----------------------------------------------------------------------------
Function  generaEncabezado$
   String generaEncabezado$, s$, p$, T$

   p$ = "0"
   If pagDescuento Then p$ = "1"
   s$ = String$( 6, "0" )
   T$ = p$ + TS.TERMINAL$ + \
      right$( s$ + str$( SL.HD.TRANSNUM + 1 ), 4 ) + \
      date$ + time$ + \
      right$( s$ + TS.STORE$, 6 ) + \
      right$( s$ + str$( val( unpack$( TS.OPER$ ) ) ), 6 ) + \
      "USD" + \
      String$( 19, "9" ) + \
      empleadoDes + \
      right$( s$ + str$( empleadoCup ), 7 ) + \
      str$( empleadoInd )
   Gr.Demo$ = t$
   generaEncabezado$ = t$
End Function 

Function GrRtaProm(Gmsg$,ghead$)
String    GrRtaProm, GM$, Xrtrama$, Xtemp4$, Gmsg$, ghead$
Integer*4 XI%, XJ%, Gk%
   Call muestraOperador("VALIDA PROMOCIONES","ESPERE POR FAVOR")
   Call tracenuo("TORI:"+Gmsg$)
   GM$ = Armar.Trama.Msg("20","01",Gmsg$,"00","0001","123456") 		 						! Armar trama MSG
   Call tracenuo("Trama GR:"+gm$)
   XRtrama$  = Rutina.Java("com.grpretail.mega.ecuador.sigui.actions.Core","execute", gm$)  		! Ejecuta Requerimiento
   Call tracenuo("Rta GR:"+xrtrama$)
   XTEMP4$ = Valida.Rta(Xrtrama$)																			      ! Valida rta entregada
   Call tracenuo("Rta valida rta:"+xtemp4$)
   If Xtemp4$ <> "00" Then Begin 																						! Si falla proceso
  	 Call muestraBorrar("FALLA COMUNICACION","MOTOR PROMOCIONES")
     Gm$ = ghead$ + "425Tiempo de espera agotado para PIPE " 
     Gm$ = "99"
   EndIf Else Begin
     XJ% = Val(Mid$(Xrtrama$,14,5))
     Call tracenuo("len trama:"+str$(xj%))
     Gm$ = Mid$(Xrtrama$,19,xj%)
     Xtemp4$ = mid$( Gm$, 64, 3 )
     Gk% = 0
     If esNumero(Xtemp4$) Then \
     	  Gk% = val( Xtemp4$ ) Else \
     		Gk% = 999
     If Gk% <> 200 Then Begin
        Gm$ = ghead$ + (Str$(Gk%) + mid$(Gm$,67,37))   ! Msg Err de la aplicacion 
     EndIf
     Call tracenuo("rta al motor:"+gm$)
   EndIf
   GrRtaProm = Gm$

End Function 

!-----------------------------------------------------------------------------
! Envia mensajeria a motor de promociones
!-----------------------------------------------------------------------------
Function  enviaMensajeria$( encabezado$, detalle$ )
 String enviaMensajeria$, encabezado$, detalle$, \
        m$, s$, Gm$, m2$, Gcaj$
 Integer*1 f
 Integer*4 t, xg%

 On Error GoTo ErrEnvMsg
 enviaMensajeria$ = ""

 Gcaj$ = ";" + Right$("000"+TS.TERMINAL$,3) + ";"														! Terminal a validacion
 Call  muestraOperador( "Consultando", "Promociones..." )										!
 s$ = str$( len( encabezado$ ) + len( detalle$ ) )													!
 s$ = right$( "000000" + s$, 5 )																						!
 m$ = s$ + encabezado$ + detalle$																						!
 m2$ = m$																																		!
   
!-- Defincion por terminal de fronteo de promociones   
   xg% = 0																																	!
   xg% = match( Gcaj$ , Gr.Prm.LstTerm$ , 1)																!
   If xg% > 0 Then Begin																										! Para motor en controladora
   	  m$ = ""
   	  Call  traceNUO("Validando en Controlador")														!
      M$ = GrRtaProm(M2$,encabezado$)           														! CALCULA PROMO EXTERNA

!-- Aqui adicionar mensaje de prueba descuentos
   !m$ = "00450043250715071211000002000001USD99999999999999999990000000012000000118001LLEVE2 PAGUE1       00374400100001180001000002000"

!---
      
   	  If M$ <> "99" Then GoTo SALIDA.MOTOR.NUO															!
!--- Procesa nuevamente localmente por falla en controlador   	  		
      s$ = str$( len( encabezado$ ) + len( detalle$ ) )											!
      s$ = right$( "000000" + s$, 5 )																				!
      m$ = s$ + encabezado$ + detalle$																			!
      m2$ = m$																															!
   EndIf																																		!
!-- Fin promociones   

   If ( len( m$ ) > bufferSize% ) Then Begin																! Trama muy larga
      Call  restauraDisplay																									!
      Call  traceNUO( "Error: Largo de mensaje mayor a buffer.." )					!
      Call  muestraBorrar( "Largo de mensaje", "mayor a buffer.." )					!
      Exit Function 																												!
   EndIf																																		!

   !Si aun no se ha creado el PIPE pi:PROUTxxx
   If Not pipeAbierto Then Begin																						! Falla pipe
      If creaPIPE( pipeOutSession%, pipeOutName, bufferSize% ) Then Begin		!
         pipeAbierto = -1																										!
      EndIf Else Begin																											!
         M$ = GrRtaProm(M$,encabezado$)           													! CALCULA PROMO EXTERNA
         GoTo SALIDA.MOTOR.NUO																							!
      EndIf																																	!
   EndIf																																		!
   t = 2
   f = -1
   While f
      If abreComunicacionPI( pipeInSession%, pipeInName , -1, bufferSize% ) Then Begin
         f = 0
         Call  traceNUO("Pipe requerimiento OK "+pipeInName)
      EndIf Else Begin
         wait; 1000
         t = t - 1
         Call  muestraOperador( "Consultando", "Promociones... ")
         If t < 1 Then Begin
            M$ = GrRtaProm(M$,encabezado$)           ! CALCULA PROMO EXTERNA
            GoTo SALIDA.MOTOR.NUO
         EndIf
      EndIf
   Wend

   If Not enviaMensajePI( pipeInSession%, m$ ) Then Begin
   	  Call  traceNUO("Falla envio Trama:"+m$)
      M$ = GrRtaProm(M$,encabezado$)           ! CALCULA PROMO EXTERNA
      GoTo SALIDA.MOTOR.NUO
   EndIf Else Begin
   	  Call  traceNUO("Trama:"+m$)
   EndIf

   leeRespuesta:
   Call  traceNUO("Esperando Respuesta Pipe...")
   m$ = leePIPE( pipeOutSession%, pipeTimeout% )
   If left$( m$, 63 ) <> encabezado$ Then goto leeRespuesta

   SALIDA.MOTOR.NUO:
    Call  restauraDisplay
    Call  traceNUO("Rta Motor:"+m$)
    enviaMensajeria$ = m$
    Exit Function 

ErrEnvMsg:
  Call traceNUO(" ERROR " + err +" "+ hexanum$(errn) + " SES:" + str$( errf% ))
  Resume
  	
End Function 


!-----------------------------------------------------------------------------
! Agrega cadenas 9953 para el descuento indice r y genera arreglo de cupones
!-----------------------------------------------------------------------------
Sub agregaCadenas9953( r )
   Integer*4 \
      monto,      \ Monto a descontar a la línea de venta
      deuna%,     \ Monto a descontar descuento DeUna Add 8Jul2025 GR
      numero      ! numero de veces que se aplico el descuento a esta linea

   Integer*2  i, k, p, r, \
      canDesc,    \ cantidad de descuentos en una instancia
      canLineas,  \ cantidad de lineas de venta en el descuento
      linea       ! Identificador de línea de venta

   String s$, p$

   On Error GoTo salidaError:

   canDesc = val( mid$( desAplic(r), 8, 3 ) )
   p = 31
   If (canDesc > 0) Then Begin

      For i = 1 to canDesc

         !arma cadena con codigo del descuento
         p$ = chr$(99H) + ":" + chr$(53H) + ":" + \
            pack$( mid$( desAplic(r), p, 6 ) ) + ":"

         !Write #34 ; "P$="+mid$( desAplic(r), p, 6 ) + chr$(10)

         !cantidad de lineas de venta
         p = p + 6
         canLineas = val( mid$( desAplic(r), p, 3 ) )
         
         !Write #34 ; "Lineas="+Str$(canLineas) + chr$(10)
         
         p = p + 3
         For k = 1 to canLineas


            monto = val( mid$( desAplic(r), p, 7 ) )
            p = p + 7
            linea = val( mid$( desAplic(r), p, 4 ) )
            p = p + 4
            numero = val( mid$( desAplic(r), p, 6 ) )
            p = p + 6

!            Write #34 ; "Monto="+Str$(monto) + chr$(10)
!            Write #34 ; "Linea="+Str$(linea) + chr$(10)
!            Write #34 ; "numero="+Str$(numero) + chr$(10)
!            Write #34 ; "data="+unpack$(MSMTFACTresProdCod$( linea )) + chr$(10)

            s$ = p$ + pack$( str$( monto ) ) + ":" + \
               MSMTFACTresProdCod$( linea ) + ":" + \
               pack$( str$( numero ) ) + ":"
            Call  agregaCadenaTSL( s$ )

         Next 
         p = p + 20

      Next

   EndIf
   p = p - 20

   !genera arreglo de cupones
   resCupones$(r) = "000"
   If len( desAplic(r) ) < ( p + 3 ) Then goto salida
   If val( mid$( desAplic(r), p, 3 ) ) > 0 Then Begin
      resCupones$(r) = right$( desAplic(r), len( desAplic(r) ) - p + 1 )
   EndIf

   salida:
   s$ = ""
   p$ = ""
   Exit Sub

   salidaError:
   !Indice de arreglo fuera de limite
   if(err = "SU") Then resume

   if(err = "FU") Then resume

   Call  traceNUO( "generaCadenas9953 ERROR" + err + hexanum$( errn ) )
   Call  muestraBorrar( "generaCadenas9953", "ERROR " + err + hexanum$( errn ) )

   resume salida

End Sub 

!----------------------------------------------------------------------------------------------------------------
! Lee lista de productos en la respuesta de cupones
!----------------------------------------------------------------------------------------------------------------
Sub leeListaProductosCupon( cantListaProductos%, k% )
   Integer*2 \
      cantListaProductos%, \
      k%, \
      l%, \
      i%
   String \
      idArticulo

   For i% = 1 to cantListaProductos%
     l%= 12
     idArticulo = mid$( resCupones , k%, l% )
     k%= k% + l%
!Call  traceNUO( "leeListaProductosCupon i%=["+ str$( i% ) +"]" + \
   "idArticulo=["+ idArticulo +"]")
   Next i%

End Sub 

!---------------------------------------------------------------------
! Genera lista de descuentos a partir de las respuestas recibidas
! el detalle de los descuentos aplicados se retorna en el parametro rDes
!---------------------------------------------------------------------
Sub generaListaDescuentos( rDes )
   String rDes, s$

   Integer*2 i, k, p, r

   Integer*4 monto       ! Monto que descontar a la línea de venta

   Integer*2 \
      canDesc,           \ cantidad de descuentos en una instancia
      totDesc,           \ cantidad total de descuentos
      canLineas,         \ cantidad de lineas de venta en el descuento
      linea              ! Identificador de línea de venta

   On Error GoTo salidaError:

   rDes = "000"
   s$ = ""
   totDesc = 0
!Call  traceNUO( "generaListaDescuentos desNum=" + str$( desNum ) )
   r = 0
   while r <= desNum

      If len( desAplic(r) ) > 13 Then Begin
         canDesc = val( mid$( desAplic(r), 8, 3 ) )
         totDesc = totDesc + canDesc

         p = 31
         For i = 1 to canDesc

            !Id del descuento y cantidad de lineas de venta
            s$ = s$ + mid$( desAplic(r), p, 9 )

            !cantidad de lineas de venta
            p = p + 6
            canLineas = val( mid$( desAplic(r), p, 3 ) )

            p = p + 3
            For k = 1 to canLineas
               s$ = s$ + mid$( desAplic(r), p, 11 )
               p = p + 17
            Next

            p = p + 20
         Next

      EndIf

      r = r + 1

   wend
   rDes = right$( String$( 3, "0" ) + str$( totDesc ), 3 ) + s$

   salida:
   s$ = ""
   Exit Sub

   salidaError:
   !Indice de arreglo fuera de limite
   If (err = "SU") Then resume

   If (err = "FU") Then resume

   Call  traceNUO( "generaListaDescuentos Error " + err + hexanum$( errn ) )
   Call  muestraBorrar( "generaListaDescuentos", "ERROR " + err + hexanum$( errn ) )

   Resume salida

End Sub

!----------------------------------------------------------------------------------------------------------------
! Lee lista de clave valor en la respuesta de descuentos
!----------------------------------------------------------------------------------------------------------------
!Sub leeListaClaveValorDescuento(cantListaClaveValor%, k%)
!   Integer*2 \
!      cantListaClaveValor%, \
!      cantClave%, \
!      cantValor%, \
!      largoTotal%, \
!      k%, \
!      l%, \
!      i%
!
!   String \
!      desClave, \
!      desValor
!
!   For i%=1 to cantListaClaveValor%
!
!      l%= 3
!      cantClave% = val( mid$( resDescuentos , k%, l% ) )
!      k%= k% + l%
!      l%= cantClave%
!      desClave = mid$( resDescuentos , k%, l% )
!      k%= k% + l%
!      l%= 3
!      cantValor% = val( mid$( resDescuentos , k%, l% ) )
!      k%= k% + l%
!      l%= cantValor%
!      desValor = mid$( resDescuentos , k%, l% )
!      k%= k% + l%
!
!      Call  grabaLogPromocion("leeListaClaveValorDescuento", "i%=["+ str$( i% ) +"]" + \
!                                                            "cantClave%=["+ str$( cantClave% ) +"]" + \
!                                                            "clave$=["+ desClave +"]" + \
!                                                            "cantValor%=["+ str$(  cantValor% ) +"]" + \
!                                                            "valor$ =["+ desValor  +"]")
!
!   Next i%
!
!End Sub

!-----------------------------------------------------------------------------
! Actualiza totales y graba cadenas de descuento al TSL
!-----------------------------------------------------------------------------
Sub actualizaTSL
   Integer*1 i
   Integer*2 k
   Integer*4 v
   String s$
   
   
   If (desTotal > 0) Then Begin
      destotal = desTotal + Gr.DeUna.Dscto%
      !actualiza variables de la aplicacion y de total de la transaccion
      SL.DI.AMTDISCO = desTotal 
      v = TS.TOTALS(0,0,0)
      If v > 0 Then Begin
         !TS.DISC.RT.SV = round( 100.0 * desTotal / v, 0, 0 )
         SL.DI.DISRATE = round( 1000.0 * (desTotal) / v, 0, 0 )
      EndIf
      
      TS.BALDUE(0) = TS.BALDUE(0) - (desTotal)
      SL.HD.GROSSNEG  = SL.HD.GROSSNEG + (desTotal)
      TS.GROSSNEG = TS.GROSSNEG + (desTotal)
      TS.TOTALS(0,0,1) = TS.TOTALS(0,0,1) + (desTotal)
      TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) + (desTotal)
      TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - (desTotal)

      !agrega cadena de descuento
      s$ = chr$(3) + ":" + \
         BINPACK$( SL.DI.DISGROUP, 1 ) + \
         ABINPACK$( SL.DI.DISRATE ) + \
         ABINPACK4$( (desTotal) ) + \
         ABINPACK4$( 0 )                    ! abs%( SL.DI.AMTTAXEX )
      Call  agregaCadenaTSL( s$ )
      s$ = ""
   EndIf

End Sub 

!---------------------------------------------------------------------
! Lee respuesta de descuentos de motor de promociones
!---------------------------------------------------------------------
Sub descuentoTotal
   Integer*2 n

   If len( resPromociones ) < 13 Then resPromociones = "0000000000000"

!Call  traceNUO( "descuentoTotal Encabezado=[" + resEncabezado + "]" )
!Call  traceNUO( "descuentoTotal Promociones=[" + resPromociones + "]" )

   desTotal = val( left$( resPromociones, 7 ) ) 
   desAplic(0) = resPromociones
   GR.TOTAL.DSC% = desTotal
   !Cupon RDM
   n = val( mid$( resPromociones, 8, 3 ) )
   If n = 0 Then Begin
      n = val( mid$( resPromociones, 11, 3 ) )
      If n = 1 Then Begin
         If mid$( resPromociones, 17, 3 ) = "RDM" Then promoRDM = -1
      EndIf
   EndIf

   resPromociones = "0000000000000"

End Sub 

Sub seteaCupones( f, opc ) public
   Integer*1 f
   Integer*2 opc, no.imprime.cupon

   If f = 1 Then Begin
      no.imprime.cupon = opc
      opc = 0
   EndIf

   If f = 0 Then Begin
      opc = no.imprime.cupon
      no.imprime.cupon = 0
   EndIf

End Sub 

!---------------------------------------------------------------------
! Lee mensaje de cupones
!---------------------------------------------------------------------
Sub leeMensajeCupon( mensaje, cantidad% )
   String \
      mensaje, \ Mensaje a imprimir en el ticket de venta
      linea,     \ Linea con el texto correspondiente a 'mensaje'
      saltoLinea,\ Salto de linea del 'mensaje'
      m$

   Integer*2 \
      cantidad%, \ Cantidad de cupones a imprimir
      nroLineas%,\ Número de líneas correspondiente a 'mensaje'
      i%,        \ Indice para recorrer cantidad%
      k%,        \ Indice para recorrer nroLineas%
      l%

   If len( mensaje ) < 3 Then Exit Sub

   saltoLinea = "\n"

   nroLineas% = cuentaCadenas( mensaje, saltoLinea )

   !Centrado
   write #34; chr$(1BH) + chr$(61H) + chr$(1)

   For i%=1 to cantidad%

      Call  imprime34(MSMTFACTlogo$)

      m$ = mensaje
      For k%=1 to nroLineas%

         l% = match( saltoLinea , m$ , 1)
         linea = mid$(m$, 1, l% - len ( saltoLinea ) )
         If linea <> " " Then linea = trim$(linea)
         m$ = mid$( m$, l% + 1, len( m$ ) )
         write #34;linea + chr$(13)

      Next k%

      l% = 24

      If cupTipo = "00" Then goto trailer
      If cupTipo = "01" Then Begin
         m$ = String$( 24, "_" )
         write #34; NUO.SaltaLineas$ + chr$(1) + "Nombre:     " + m$ + chr$(13)
         write #34; "Id Cliente: " + m$ + chr$(13)
         write #34; "Direccion:  " + m$ + chr$(13)
         write #34; "Telefono:   " + m$ + chr$(13)
         goto numFactura
      EndIf
      If cupTipo = "02" Then Begin

         m$ = datosCliente$( 2, l% )
         write #34; NUO.SaltaLineas$ + chr$(1) + "Nombre:     " + m$ + chr$(13)

         m$ = datosCliente$( 1, l% )
         write #34; "Id Cliente: " + m$ + chr$(13)

         m$ = datosCliente$( 3, l% )
         write #34; "Direccion:  " + m$ + chr$(13)

         m$ = datosCliente$( 4, l% )
         write #34; "Telefono:   " + m$ + chr$(13)

         numFactura:
         m$ = left$( lineaDatosFactura$ + String$( l%, " " ), l% )
         write #34; "No Factura: " + m$ + chr$(13)

      EndIf

      trailer:
      Call  getLinetrailer( m$ )
      write #34; NUO.SaltaLineas$ + chr$(1) + m$ + chr$(13)

      !codigo de barra
      If cupTipo = "03" Then Begin
         write #34; chr$(1DH)+chr$(77H)+chr$(01H)+ \         !Horizontal barcode
            chr$(1DH)+chr$(68H)+chr$(40H)+ \                 !Vertical barcode
            chr$(1DH)+chr$(48H)+chr$(02H)+ \                 !Human Read characters
            chr$(1DH)+chr$(66H)+chr$(00H) + \                !activado
            chr$(1DH) + chr$(6BH) + chr$(07H) + \            !
            "27" + right$(cupCodigoPromocion, 4) + "000000" + chr$(00H)
      EndIf

      write #34; NUO.SaltaLineas$ + chr$(2) + NUO.CortaPapel$

   Next i%

   !Alineado a la Izquierda
   write #34; chr$(1BH) + chr$(61H) + chr$(0)

End Sub 

!---------------------------------------------------------------------
! Lee respuesta de cupones de motor de promociones
!---------------------------------------------------------------------
Sub leeRespuestaCupones
   Integer*1 imprimir, f
   Integer*2 \
      cantListaProductos%, \
      i%, \
      k%, \
      l%, \
      opc

   Integer*2               \
      cupCantPromociones%, \ Cantidad de promociones de cupones
      cupCantidad%         ! Cantidad de cupones a imprimir por promocion

   On Error GoTo salidaError:

   !Para no imprimir cupones ante una ANULA TOTAL
   opc = 0
   Call  seteaCupones( 0 , opc )

   If opc Then Exit Sub

   cupNomPromocion = ""
   cupCodigoPromocion = ""
   cupIdPromocion = ""
   cupTipo= ""
   cupFechaInicio = ""
   cupFechaFin = ""
   cupMensaje = ""
   cupCantPromociones% = 0
   cupCantidad% = 0
   cantListaProductos% = 0

!Call  traceNUO("leeRespuestaCupones resCupones=["+resCupones+"]")

   !No hay cupones
   If left$( resCupones, 3 ) = "000" Then Exit Sub

   cupCantPromociones% = val( left$( resCupones, 3 ) )
   k% = 4

   For i% = 1 to cupCantPromociones%
      l%= 3
      cupCantidad% = val(mid$( resCupones, k%, l%))
      k%= k% + l%
      l%= 20
      cupNomPromocion = mid$( resCupones , k%, l%)
      cupNomPromocion = eliizq$( cupNomPromocion )
      k% = k% + l%
      l% = 6
      cupCodigoPromocion = mid$( resCupones , k%, l%)
      k%= k% + l%
      l%= 6
      cupIdPromocion = mid$( resCupones , k%, l%)
      k%= k% + l%
      l%= 2
      cupTipo = mid$( resCupones , k%, l%)
      k%= k% + l%
      l% = 6
      cupFechaInicio = mid$( resCupones , k%, l% )
      k% = k% + l%
      l% = 6
      cupFechaFin = mid$( resCupones , k%, l% )
      k% = k% + l%
      l% = 3
      l% = val( mid$( resCupones, k%, l%) )
      k% = k% + 3
      cupMensaje = mid$( resCupones , k%, l% )
      cupMensaje = eliIzq$( cupMensaje )
      k%= k% + l%
      l%= 3
      cantListaProductos% = val( mid$( resCupones , k%, l% ) )
      k%= k% + l%

      If cantListaProductos% > 0 Then Call  leeListaProductosCupon( cantListaProductos%, k% )

      Call  leeMensajeCupon( cupMensaje, cupCantidad% )

   Next i%

   salida:
   Exit Sub

   salidaError:

   !Indice de arreglo fuera de limite
   if(err = "SU") Then resume

   if(err = "FU") Then resume

   Call  traceNUO( "leeRespuestaCupones ERROR " + err + hexanum$(errn) )
   Call  muestraBorrar( "leeRespuestaCupones", "ERROR " + err + hexanum$(errn) )
   resume salida

End Sub

!---------------------------------------------------------------------
!Consulta cupon externo
! retorna:
!  0 -> cupon no valido
! -1 -> cupon valido
!---------------------------------------------------------------------
Function  consultaCupon( cupon$ )
   Integer*1 consultaCupon
   String cupon$, m$, t$, r$

   consultaCupon = 0

   t$ = "'|'"
   m$ = "P" + t$ + \                ! canal de comunicacion S=socket P=Pipe
      "66" + t$ + \                 ! requerimiento 66 consulta de cupon seriado
      "0000" + t$ + \               ! canal de respuesta
      TS.TERMINAL$ + t$ + \         ! terminal de origen
      "0" + t$ + \                  ! tipo de conexion 0=temporal 1=permanente
      "20" + date$ + time$ + t$ + \ ! fecha y hora actual
      cupon$                        ! codigo de cupon

!Call  traceNuo("ENV=["+ m$ + "]" )

   !para evitar que existan mensajes antiguos
   Call  limpiaPipe

   !Envia el mensaje
   If not enviaMensaje( m$ ) Then Begin
      Call  muestraBorrar( "Cupon " + cupon$, "NO VALIDADO" )
      wait; 5000
      Exit Function 
   EndIf

   !Espera que lleguen datos
   If esperaPipe( cuponTimeout, "Cupon Externo" ) Then Begin
      r$ = recibeMensaje( "Srv Cupones Externos" )
      If buscaCampo( r$, t$, 7 ) = "1" Then Begin
         consultaCupon = -1
         m$ = "VALIDO"
      EndIf Else Begin
         m$ = "NO VALIDO"
      EndIf
      Call  muestraVisor( 1, "Cupon " + cupon$, m$ )
      wait; 2000
      Call  restauraVisor(1)
!Call  traceNuo("RSP=["+ r$ + "]" )
   EndIf

End Function 

!---------------------------------------------------------------------
!Consulta Descuento empleado
!---------------------------------------------------------------------
Sub consultaEmpleado
   String m$, t$, r$

   t$ = "'|'"
   m$ = "P" + t$ + \                ! canal de comunicacion S=socket P=Pipe
      "67" + t$ + \                 ! requerimiento 67 consulta descuento empleado
      "0000" + t$ + \               ! canal de respuesta
      TS.TERMINAL$ + t$ + \         ! terminal de origen
      "0" + t$ + \                  ! tipo de conexion 0=temporal 1=permanente
      "20" + date$ + time$ + t$ + \ ! fecha y hora actual
      empleadoCod                   ! numero identificacion empleado

!Call  traceNUO("consultaEmpleado ENV=["+ m$ + "]" )

   !para evitar que existan mensajes antiguos
   Call  limpiaPipe

   !Envia el mensaje
   If not enviaMensaje( m$ ) Then Begin
      Call  muestraBorrar( "Empleado " + empleadoCod, "NO CONSULTADO" )
      wait; 5000
      Exit Sub
   EndIf

   !Espera que lleguen datos
   If esperaPipe( cuponTimeout, "Cons Empleado" ) Then Begin
      r$ = recibeMensaje( "Srv Consulta Empleado" )
      If buscaCampo( r$, t$, 7 ) = "0" Then Begin
         empleadoDes = buscaCampo( r$, t$, 8 )
         empleadoInd = 0
         If buscaCampo( r$, t$, 9 ) = "S" Then empleadoInd = 1
         empleadoCup = 0
         m$ = buscaCampo( r$, t$, 10 )
         If esNumero( m$ ) Then empleadoCup = val( m$ )
         If buscaCampo( r$, t$, 11 ) = "S" Then empleadoInd = empleadoInd + 2
         m$ = "DESC " + empleadoDes + " " + formatea2Dec( empleadoCup )
      EndIf Else Begin
         m$ = "NO TIENE DESCUENTO"
      EndIf
      Call  muestraVisor( 1, "Empleado " + empleadoCod, m$ )
      wait; 3000
      Call  restauraVisor(1)
   EndIf

End Sub

!---------------------------------------------------------------------
! Muestra el total de la trasaccion luego del descuento
!---------------------------------------------------------------------
Sub muestraNuevoTotal
   String \
      total, \
      nuevoTotal
   Integer*4 v

   total = ""
   nuevoTotal = ""

   If desTotal = 0 Then Exit Sub
   v = TS.TOTALS(0,0,0)
   total = left$( "(" + formatea2dec( TS.TOTALS(0,0,0) ) + ")"+ "          ", 10 )
   total = total + right$("         " + "-" + formatea2dec( desTotal ) , 10 )
   nuevoTotal = "NUEVO TOTAL" + right$("        "+ formatea2dec( TS.TOTALS(0,0,0) - desTotal ) , 9 )
   Call  muestraBorrar( total , nuevoTotal )
   TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - desTotal
   Call  javaEvent(updateTotalsDataMsg)
   TS.TOTALS(0,0,0) = v

End Sub

!----------------------------------------------------------------------------------------------------------------
! Interpreta respuesta de motor de promociones
!----------------------------------------------------------------------------------------------------------------
Sub leeRespuestaPromociones( respuesta )
   String  respuesta, s$
   Integer*2 k
   Integer*4 n, p

   resEncabezado = ""
   resPromociones = ""

   n = len( respuesta )
   If n < 66 Then goto salir

   !longitud del encabezado
   p = 63

   s$ = mid$( respuesta, p+1, 3 )
   k = 0
   If esNumero(s$) Then k = val( s$ )
   If k <> 200 Then Begin
      s$ = "Desconocido"
      If k = 400 Then s$ = "Encabezado erroneo "
      If k = 401 Then s$ = "Archivo promociones"
      If k = 402 Then s$ = "Calculo de motor   "
      If k = 403 Then s$ = "Creacion de trama  "
      If k = 424 Then s$ = "Error al abrir PIPE"
      If k = 425 Then s$ = "Error tiempo espera"
      If k = 426 Then s$ = "Error lectura PIPE "
	    If K = 999 Then s$ = "Err Defin. Cupones "          
	    	
      Call  traceNUO( "leeRespuestaPromociones respuesta=[" + respuesta + "]" )
      Call  muestraBorrar( "Error " + str$( k ) , s$ )
      calcularDesc = -1
      Exit Sub
   EndIf

   resEncabezado = left$( respuesta, p )
   resPromociones = mid$( respuesta, p+4, n )

   Exit Sub

   salir:
   Call  grabaLogPromocion( "leeRespuestaPromociones: respuesta no valida", "[" + respuesta + "]" )

End Sub

!-------------------------------------------------------------------------------------------
! Envia informacion de productos, cupones y promociones ya aplicadas a motor de promociones
!-------------------------------------------------------------------------------------------
Function  consultaPromociones$
   String consultaPromociones$
   Integer*2 i, k, n, p
   String s$, \
      rEnc,   \ Encabezado del mensaje de consulta de promociones
      rDet,   \ Detalle del ticket
      rDes    ! Detalle de los descuentos ya aplicados

   If not activo Then Exit Function 

   rEnc = generaEncabezado$

   !Detalle de productos
   rDet = ""
   For i = 0 to MSMTFACTresProdNum
      rDet = rDet + MSMTFACTlineaDescuento$( i )
   Next i

   s$ = String$( 17, "0" )

   If pagDescuento Then Begin

      !Detalle de todas las promociones ya aplicadas
      Call  generaListaDescuentos( rDes )

      rDet = right$( s$ + str$( MSMTFACTresProdNum ) , 3 ) + rDet + \
         rDes + idPago$

   EndIf Else Begin

      ! Agrega detalle de cupones externos ingresado en la transacción
      i = 0
      while i < cupoNextInd
         rDet = rDet + \
            right$( s$ + str$( cupoNextCod(i) ), 12 ) + \  !Codigo cupon
            right$( s$, 4 ) + \                            !Linea
            right$( s$ + str$( cupoNextCan(i) ), 6 ) + \   !Cantidad
            right$( s$, 7 ) + \                            !Peso
            right$( s$, 7 ) + \                            !Precio
            "0" + \                                        !Descontable
            s$                                             !Categoria
         i = i + 1
      wend

   EndIf

   s$ = ""
   rDes = ""
   consultaPromociones$ = enviaMensajeria$( rEnc, rDet )
   rEnc = ""
   rDet = ""

End Function 

!------------------------------------------------------------------------------
! Obtiene el codigo de pago valido para el motor
!------------------------------------------------------------------------------
Function  tipoPagoMotor$( variedadPago )
  String variedadPago, \
         tipoPagoMotor$
  Integer*1 \
         index%

  tipoPagoMotor$ =""

   For index% = 1 to pagCantCodigo%
       If pagCodigo$( index%, 0 ) = variedadPago Then Begin
          tipoPagoMotor$ = pagCodigo$( index%, 1 )
          index% =  pagCantCodigo%
       EndIf
   Next index%

End Function 

!------------------------------------------------------------------------------
! Consulta un descuento al medio de pago
! retorna el monto de descuento
!------------------------------------------------------------------------------
Function  descuentoPago( monto , tarjeta , codPago ) public
   Integer*4 descuentoPago, v
   String    monto,          \ Monto del medio de pago
             tarjeta,        \ Tarjeta
             codPago,        \ Codigo de medio de pago
             s$, p$, t$
   Integer*2 n               ! Numero de descuentos

   If not calcDescuento Then Exit Function 

   v = 0
   pagDescuento = -1

   t$ = tipoPagoMotor$( codPago )
   s$ = String$( 7, "0" )
   idPago$ = right$( s$ + trim$( monto ), 7 ) + \
      left$( trim$( tarjeta ) + s$, 7 ) + left$( t$ + s$, 1 )
   resPromociones = "0000000000000"

!Call  traceNUO( "descuentoPago idPago$=[" + idPago$ + "]" )

   If t$ = "" Then goto salir

   s$ = consultaPromociones$
   If s$ = "" Then goto salir

   Call  leeRespuestaPromociones(s$)

!Call  traceNUO( "descuentoPago Encabezado=[" + resEncabezado + "]" )
!Call  traceNUO( "descuentoPago Promociones=[" + resPromociones + "]" )

   If len( resPromociones ) < 13 Then goto salir

   !hay descuentos
   n = val( mid$( resPromociones, 8, 3 ) )
   If n > 0 Then Begin
      v = val( left$( resPromociones, 7 ) )
      TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0) - v
   EndIf

   salir:
   descuentoPago = v

End Function 

!------------------------------------------------------------------------------
! Agrega un descuento al medio de pago
! retorna el monto de descuento
!------------------------------------------------------------------------------
Function  agregaPago
   Integer*4 agregaPago, v
   String    s$
   Integer*2 n               ! Numero de descuentos

   s$ = String$( 13, "0" )
   v = 0
   If len( resPromociones ) < 13 Then resPromociones = s$
   If resPromociones = s$ Then goto salir

!Call  traceNUO( "agregaPago idPago$=[" + idPago$ + "] promo=[" + resPromociones + "]" )

   !hay descuentos
   v = val( left$( resPromociones, 7 ) )
   If v > 0 Then Begin
      desTotal = desTotal + v
   EndIf

   desNum = desNum + 1
   Pagos$(desNum) = idPago$
   desAplic(desNum) = resPromociones
   resPromociones = s$

   salir:
   s$ = ""
   agregaPago = v

End Function 

!-------------------------------------------------------------------------
! consulta el descuento asociado a un pago
! retorna el monto del descuento que fue aplicado
! desInd contiene el indice al arreglo de descuentos
!-------------------------------------------------------------------------
Function  consultaPago( monto , tarjeta , codPago ) public
   Integer*4 consultaPago
   String      \
      monto,   \ Monto del medio de pago
      tarjeta, \ Tarjeta
      codPago, \ Codigo de medio de pago
      s$
   Integer*1 i, k
   Integer*4 v

   consultaPago = 0

   !buscar el medio de pago
   idPago$ = right$( String$( 7, "0" ) + trim$( monto ), 7 ) + \
      left$( trim$( tarjeta ) + String$( 7, "0" ), 7 ) + \
      left$( tipoPagoMotor$( codPago ) + "0", 1 )

!Call  traceNUO( "consultaPago idPago$=[" + idPago$ + "]" )

   k = 0
   i = 1
   while i <= desNum
      If idPago$ = Pagos$(i) Then Begin
         k = i
         i = desNum
      EndIf
      i = i + 1
   wend
   If k = 0 Then Exit Function 

   !recupera el monto de descuento
   v = val( left$( desAplic(k), 7 ) )
   desInd = k
   consultaPago = v

End Function 

!-------------------------------------------------------------------------
! elimina el descuento correspondiente al indice desInd
!-------------------------------------------------------------------------
Sub eliminaPago
   String    s$
   Integer*1 i, k
   Integer*4 v, p

!Call  traceNUO( "eliminaPago desInd=" + str$(desInd) )

   If desInd > 0 Then Begin
      k = desInd
      v = val( left$( desAplic(k), 7 ) )
      p = val( left$( Pagos$(k), 7 ) ) - v

!Call  traceNUO( "eli 1 TOTALS=[" + str$(TS.TOTALS(0,0,0)) + "] BALDUE=[" + str$(TS.BALDUE(0)) + "] TENDER=[" + str$(TS.TENDERED(0)) + "]" )
!Call  traceNUO( "eli 2 desTotal=[" + str$(desTotal) + "] desPago=[" + str$(v) + "] pago=[" + str$(p) + "]" )

      !restaura totales
      desTotal = desTotal - v
      TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)
      !TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - v
      !TS.TENDERED(0) = TS.TENDERED(0) - p
      !TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) + v

!Call  traceNUO( "eli 3 TOTALS=[" + str$(TS.TOTALS(0,0,0)) + "] BALDUE=[" + str$(TS.BALDUE(0)) + "] TENDER=[" + str$(TS.TENDERED(0)) + "]" )
!Call  traceNUO( "eli 4 desTotal=[" + str$(desTotal) + "] desPago=[" + str$(v) + "]" )

      !eliminar el registro de pago y descuento
      i = k
      while i < desNum
         Pagos$(i) = pagos$(i+1)
         desAplic(i) = desAplic(i+1)
         i = i + 1
      wend
      Pagos$(i) = ""
      desAplic(i) = ""

      desNum = desNum - 1
      desInd = 0

   EndIf

End Sub

!-------------------------------------------------------------------------
! cancela el descuento al pago
!-------------------------------------------------------------------------
Sub cancelaPago public
   String    s$
   Integer*1 i, k
   Integer*4 v, p

!Call  traceNUO( "cancelaPago desNum=" + str$(desNum) + " desTotal=" + str$(desTotal) + " idPago$=[" + idPago$ + " resPromociones=[" + resPromociones + "]" )

   s$ = left$( resPromociones, 7 )
   If not esNumero( s$ ) Then Exit Sub
   v = val( s$ )
   If v = 0 Then Exit Sub

   !restaura totales
   TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)

!Call  traceNUO( "cancelaPago TS.BALDUE=" + str$( TS.BALDUE(0) ) )

   resPromociones = "0000000000000"
   idPago$ = ""

End Sub

!------------------------------------------------------------------------------
! Al inicio de una transaccion de ventas
!------------------------------------------------------------------------------
Sub NUOTPROM01 public
   calcDescuento = -1
   calcularDesc = -1
   descEfectivo = 0
   pagoEfectivo = 0
End Sub

!------------------------------------------------------------------------------
! Al finalizar una transaccion de venta
!------------------------------------------------------------------------------
Sub NUOTPROM02 public
   Integer*1 i

   cupoNext$ = ""
   cupoNextInd = 0
   dim cupoNextCod(32)
   dim cupoNextCan(32)

   !Impresion de cupones
   i = 0
   while i <= desNum
      resCupones = resCupones$(i)
      Call  leeRespuestaCupones
      i = i + 1
   wend
   promoRDM = 0

   !limpia las variables de promociones
   Call  limpiaPromociones

   !limpio variables con datos de descuento empleado
   Call  limpiaDesEmpleado
   calcDescuento = -1
   calcularDesc = -1
   descEfectivo = 0
   pagoEfectivo = 0

End Sub

!----------------------------------------------------------------------------------------------------------------
! Despues de apertura de todos los archivos
!----------------------------------------------------------------------------------------------------------------
Sub NUOTPROM07 public
   String    r
   Integer*1 i, k, w
   String    n$, v$
   Integer*4 inicioBloque

   !version$ = "11 May 2022"
   version$ = "27 SEP 2023"
   activo = 0
   depurar = 0
   pipeAbierto = 0
   
   pipeInName = "PI:INPIPE"
   pipeOutName = "PI:PROUT" + right$( "000" + str$( SL.HD.TERMINAL ), 3 )
   bufferSize% = 8192  !!! Definir
   pipeTimeout% = 5000
   cuponTimeout = 15000
   pipeInSession%= 57
   pipeOutSession%= 58

   !limpio variables con datos de descuento empleado (tipo, disponible y flag)
   Call  limpiaDesEmpleado

   cupoNext$ = ""
   cupoNextInd = 0
   dim cupoNextCod(32)
   dim cupoNextCan(32)

   Call  imprimeCJ("******* MODULO DE PROMOCIONES *********")

   !Lectura de parametros del archivo c:/ADX_UDT1/PARGENER.DAT
   inicioBloque = abreParametros( "R::$ARGENER", "[PROMOCIONES]", 80 )
   If inicioBloque = -1 Then Begin
      !Call  muestraBorrar( "NO EXISTE PROMO", "EN ARCHIVO PARGENER" )
      Call  imprimeCJ( "NO CONFIGURADO " + version$ )
      Call  cierraParametros( 80 )
      Exit Sub
   EndIf

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   If v$ <> "" Then Begin

      If esNumero( v$ ) Then k = val(v$) Else k = 0
      If (k and 1) Then activo = -1
      If (k and 2) Then depurar = -1

      If activo Then Begin

         !Tiempo de espera respuesta motor
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         If esNumero( v$ ) Then pipeTimeout% = val(v$)

         !Pipe de escritura JAVA
         v$ = leeParametro$( "PIPEIN", 80, inicioBloque )
         v$ = trim$( v$ )
         k = len( v$ )
         If k > 3 Then Begin
            pipeInName = v$
         EndIf

         !Pipe de lectura JAVA
         v$ = leeParametro$( "PIPEOUT", 80, inicioBloque )
         v$ = trim$( v$ )
         k = len( v$ )
         If k > 3 Then Begin
            !esto es para que funcione con el motor anterior
            If v$ = "pi:OUTPIPE" Then Begin
               pipeOutName = v$
            EndIf Else Begin
               If k < 9 Then pipeOutName = v$ + right$( "000" + str$( SL.HD.TERMINAL ), 3 )
            EndIf
         EndIf

         !Tamaño del pipe
         v$ = leeParametro$( "BUFFER_SIZE", 80, inicioBloque )
         If esNumero( v$ ) Then bufferSize% = val(v$)

         !Sesion para uso de pipe de escritura
         v$ = leeParametro$( "PIPEIN_SESION", 80, inicioBloque )
         If esNumero( v$ ) Then pipeInSession% = val(v$)

         !Sesion para uso de pipe de lectura
         v$ = leeParametro$( "PIPEOUT_SESION", 80, inicioBloque )
         If esNumero( v$ ) Then pipeOutSession% = val(v$)

         !Sesion para uso de pipe de lectura
         v$ = leeParametro$( "PIPEOUT_SESION", 80, inicioBloque )
         If esNumero( v$ ) Then pipeOutSession% = val(v$)

         !Sesion para uso de pipe de lectura
         v$ = leeParametro$( "ACTIVAR_LOG", 80, inicioBloque )
         If esNumero( v$ ) Then GrtraceOn% = val(v$)

         v$ = leeParametro$( "TERMINALES_POS", 80, inicioBloque )
         Gr.Prm.LstTerm$ = trim$( v$ )

         !Tiempo de espera maximo de respuesta srv cupones externos
         v$ = leeParametro$( "TIEMPO_ESPERA_CUPON", 80, inicioBloque )
         If esNumero( v$ ) Then cuponTimeout = val(v$)
         	
      EndIf

   EndIf

   v$ = "[PROMOCIONES PAGOS]"
   inicioBloque = buscaParametros( v$, 80 )
   If inicioBloque = -1 Then Begin
      !Call  muestraBorrar( "NO " + v$, "EN ARCHIVO PARGENER" )
      Call  imprimeCJ( v$ + " NO CONFIGURADO" )
      Call  cierraParametros( 80 )
      Exit Sub
   EndIf

   w = cargaParametros( 80, inicioBloque )
   If w > 0 Then Begin
      If w > 66 Then w = 66
      pagCantCodigo% = w
      dim pagCodigo$( pagCantCodigo% , 1 )
      For i = 1 to w
          Call  rescataParametro( i, n$, v$ )
          pagCodigo$( i, 0 ) = n$
          pagCodigo$( i, 1 ) = v$
      Next i
   EndIf

   Call  cierraParametros( 80 )

   If activo Then r = "ON " Else r = "OFF"
   Call  imprimeCJ( "ESTADO " + r + "  " + version$ )
   If depurar Then Begin
      Call  imprimeCJ( "TIMEOUT " + right$( String$( 11, " " ) + str$( pipeTimeout% ), 11 ) )
      Call  imprimeCJ( "PIPEIN  " + right$( String$( 11, " " ) + pipeInName , 11 ) )
      Call  imprimeCJ( "PIPEOUT " + right$( String$( 11, " " ) + pipeOutName , 11 ) )
      Call  imprimeCJ( "BUFFER  " + right$( String$( 11, " " ) + str$( bufferSize% ), 11 ) )
   EndIf

   !limpia las variables de promociones
   Call  limpiaPromociones
   calcDescuento = -1
   calcularDesc = -1
   descEfectivo = 0
   pagoEfectivo = 0

   If creaPIPE( pipeOutSession%, pipeOutName, bufferSize% ) Then Begin
     pipeAbierto = -1
   EndIf
   
!-- Limpieza archivo trace
!   TS.TEMP1$ = "R::C:/TRACE/TRACE" + TS.TERMINAL$ + ".TXT"
!   TS.ER.RETURN = -1
!   Create TS.TEMP1$ As 103
!   If TS.ER.RETURN = -1 Then Close 103
   
End Sub

!----------------------------------------------------------------------------------------------------------------
! Despues de la lectura del Teclado
!----------------------------------------------------------------------------------------------------------------
Sub NUOTPROM14 public
   Integer*1 f, vx
   Integer*2 n, k
   Integer*4 v
   String    c$, m$

   ! Cuando se digita o lee un codigo de cupon externo
   If ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 80 ) and ( TS.IO.KEYS(2) = 80 ) and \
      ( len( TS.IO.DATA$(2) ) > 11 ) and ( left$( TS.IO.DATA$(2), 2 ) = "27" ) Then Begin

      c$ = left$( TS.IO.DATA$(2), 12 )
      m$ = "CU" + right$( c$, 10 )

      !busca cupon
      n = match( m$, cupoNext$, 1 )
      If n < 0 Then Begin
         Call  traceNUO( "error en busqueda de cupon " + c$ )
         goto salir
      EndIf

      !para evitar las consultas inútiles se puede generar
      !en el controlador un archivo con cupones no secuenciados
      !que estén activos de modo que la caja para los cupones
      !000000 solo los verifique en el archivo y no consulte al agente
      !archivo keyed CUPONext.DAT largo de la clave PD 2
      !la clave es el codigo de la promo ( 0001 a 1000 )
      !el contenido del registro puede ser la fecha de vencimiento
      !AAMMDD PD 3, largo registro 5

      !consulta validez del cupon
      f = -1
      If right$( c$, 6 ) = "000000" Then Begin
         Call  muestraVisor( 1, "Cupon " + c$, "VALIDO" )
         wait; 2000
      EndIf Else Begin
         If n > 0 Then Begin
            Call  muestraBorrar( "Cupon " + c$, "YA INGRESADO" )
            goto salirExterno
         EndIf
         f = consultaCupon( c$ )
      EndIf

      !agrega cupon externo
      If f Then Begin

         If n > 0 Then Begin
            k = ( n - 1 ) / 12
         EndIf Else Begin
            cupoNext$ = cupoNext$ + m$
            k = cupoNextInd
            cupoNextCod(k) = val( mid$( c$, 3, 4) )
            cupoNextInd = k + 1
         EndIf
         cupoNextCan(k) = cupoNextCan(k) + 1

         !forzar un total
         TS.GUIDANCE = 1020
         TS.BAL.TAKEN = 0
         TS.IO.MOTORKEY = 0
         goto salir
      EndIf

      salirExterno:
      Call  ignoraTeclas
      goto salir
   EndIf

   !Consulta WS descuento empleado
   If empleadoCon Then Begin
      Call  consultaEmpleado
      empleadoCon = 0
   EndIf

   !tecla efectivo
   If ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 91 ) and ( TS.IO.KEYS(7) = 91 ) Then Begin

!Call  traceNUO( "PRM14 1 TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDER=" + str$(TS.TENDERED(0)) )

      If TS.IO.KEYS(1) = 70 Then Begin

         !Es anulacion
         c$ = TS.IO.DATA$(7)

!Call  traceNUO( "PRM14 2 c$=[" + c$ + "]" )

         If c$ <> "" Then Begin
            v = consultaPago( c$, "", "11" )
            If v > 0 Then Begin
               TS.IO.DATA$(7) = str$( val( TS.IO.DATA$(7) ) - v )
               !TS.DISC.SAVE(0,0) = TS.DISC.SAVE(0,0) - v
            EndIf
            If val(TS.IO.DATA$(7)) > 0 Then Begin
               If JAVA.INIT = -1 Then Begin
                  jGuiSubState = 80539
                  Call  javaEvent(terminalSubStateMsg)
               EndIf
            EndIf
         EndIf

      EndIf Else Begin

         !es pago
         If ( TS.IO.DATA$(7) = "" ) Then Begin
            If ( TS.IO.PREV.KEYS(7) = 91 ) and ( TS.IO.PREV.DATA$(7) = "" ) Then Begin
               c$ = str$( TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0) )
               pagoEfectivo = -1
            EndIf Else Begin
               goto salir
            EndIf
         EndIf Else Begin
            c$ = TS.IO.DATA$(7)
         EndIf

         descEfectivo = descuentoPago( c$, "", "11" )

!Call  traceNUO( "PRM14 3 c$=[" + c$ + "] pagoEfectivo=" + str$(pagoEfectivo) + " descEfectivo=" + str$(descEfectivo) )

         If ( TS.IO.DATA$(7) <> "" ) Then Begin
            TS.IO.DATA$(7) = str$( val( TS.IO.DATA$(7) ) - descEfectivo )
         EndIf

!Call  traceNUO( "PRM14 4 TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDER=" + str$(TS.TENDERED(0)) + " DATA$(7)=[" + TS.IO.DATA$(7) + "]" )

         !v = TS.TOTALS(0,0,0)
         !TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - desTotal
         !Call  javaEvent(updateTotalsDataMsg)
         !TS.TOTALS(0,0,0) = v
      EndIf

!Call  traceNUO( "PRM14 5 TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDER=" + str$(TS.TENDERED(0)) )

   EndIf

   salir:
   c$ = ""
   m$ = ""

End Sub

!----------------------------------------------------------------------------------------------------------------
! Antes de escribir una linea al visor
!----------------------------------------------------------------------------------------------------------------
Sub NUOTPROM23 public
   Integer*4 v, mt

!Call  traceNUO( "PRM23 LT=" + str$( TS.LINETYPE ) + " LD=" + str$(TS.LINEDATA) )

   !Muestra el total con el descuento aplicado
   If TS.LINETYPE = 3 Then Begin

!Call  traceNUO( "PRM23 LT 3 TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDER=" + str$(TS.TENDERED(0)) + " AMTTENDE=" + str$(SL.TE.AMTTENDE) + " desTotal=" + str$(desTotal) )
!Call  traceNUO( "TS.DISP1$=[" + TS.DISP1$ + "] TS.DISP2$=[" + TS.DISP2$ + "]" )

      v = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)
      TS.DISP2$ = left$( TS.DISP2$, 10 ) + \
         right$( String$( 10, " " ) + formatea2Dec( v ) + " ", 10 )
      TS2.DISP2$ = TS.DISP2$

!Call  traceNUO( "TS.DISP1$=[" + TS.DISP1$ + "] TS.DISP2$=[" + TS.DISP2$ + "]" )

      If java.init = -1 Then Begin  
         mt = TS.TOTALS(0,0,0)
         If TS.BALDUE(0) < 0 Then Begin
            TS.TOTALS(0,0,0) = (-1) * TS.BALDUE(0) 
            Call  javaEvent(updateTotalsDataMsg)
            wait; 2000
         EndIf Else Begin
            TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - desTotal
            Call  javaEvent(updateTotalsDataMsg)
         EndIf
         TS.TOTALS(0,0,0) = mt
      EndIf
   EndIf

   !Muestra el pago EFECTIVO y el vuelto con el descuento aplicado
   If TS.LINETYPE = 2 Then Begin

!Call  traceNUO( "PRM23 LT 2 TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDER=" + str$(TS.TENDERED(0)) + " AMTTENDE=" + str$(SL.TE.AMTTENDE) + " desTotal=" + str$(desTotal) )
!Call  traceNUO( "TS.DISP1$=[" + TS.DISP1$ + "] TS.DISP2$=[" + TS.DISP2$ + "]" )

      If (descEfectivo > 0) and (TS.BALDUE(0) <> 0) Then Begin
         !v = 100.0 * val(right$( TS.DISP1$, 10 )) + descEfectivo
         v = val(TS.IO.DATA$(7)) + descEfectivo
         TS.DISP1$ = left$( TS.DISP1$, 10 ) + \
            right$( String$( 10, " " ) + formatea2Dec( v ) + " ", 10 )
         TS2.DISP1$ = TS.DISP1$
         !v = 100.0 * val(right$( TS.DISP2$, 10 )) + descEfectivo
         If TS.BALDUE(0) < 0 Then Begin
            v = descEfectivo - TS.BALDUE(0)
            TS.DISP2$ = left$( TS.DISP2$, 10 ) + \
               right$( String$( 10, " " ) + formatea2Dec( v ) + " ", 10 )
            TS2.DISP2$ = TS.DISP2$
         EndIf
      EndIf

!Call  traceNUO( "TS.DISP1$=[" + TS.DISP1$ + "] TS.DISP2$=[" + TS.DISP2$ + "]" )

      If java.init = -1 Then Begin  
         mt = TS.TOTALS(0,0,0)
         If TS.BALDUE(0) < 0 Then Begin
            TS.TOTALS(0,0,0) = (-1) * TS.BALDUE(0) 
            Call  javaEvent(updateTotalsDataMsg)
            wait; 2000
         EndIf Else Begin
            TS.TOTALS(0,0,0) = TS.TOTALS(0,0,0) - desTotal
            Call  javaEvent(updateTotalsDataMsg)
         EndIf
         TS.TOTALS(0,0,0) = mt 
      EndIf

   EndIf

End Sub

!------------------------------------------------------------------------------
! En el calculo del saldo a pagar
!------------------------------------------------------------------------------
Sub NUOTPROM30 public
   String s$
   Integer*4 v, vx
   Integer*2 i

   !Si no hay una transaccion en curso
   If NOT TS.INTRX Then goto salir

   !Descuento al total
   If TS.IO.MOTORKEY = 81 Then Begin

      !Se presionó tecla anular
      If TS.IO.KEYS(1) = 70 Then goto salir

      !Si ya existen pagos
      If TS.TENDERED(0) <> 0 Then Begin
         TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)
         goto salir
      EndIf

      !si no ha cambiado nada en la transaccion
      If not calcularDesc Then Begin
         TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal
         goto salir
      EndIf

      If calcDescuento Then Begin

         !limpia las variables de promociones
         Call  limpiaPromociones

         s$ = consultaPromociones$
         If s$ <> "" Then Begin
            calcularDesc = 0
            Call  leeRespuestaPromociones(s$)
            Call  descuentoTotal
            Call  muestraNuevoTotal
         EndIf

         !Ajusta totales
         TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal
         !TS.DISC.SAVE(0,0) = desTotal

         If promoRDM Then Begin
            TO.USEREXIT(14) = 0
            While(1)
               Call  muestraBorrar("PROMOCION RDM","COMPRA GRATIS")
               ! autorizacion de supervisor y llave
               If entradaAutorizacion Then Begin
                  TO.USEREXIT(14) = -1
                  goto salir
               EndIf
            Wend
         EndIf
      EndIf

   EndIf Else Begin																													! En la aplicacion de un pago

         !limpia las variables de promociones
         Call  limpiaPromociones

         s$ = consultaPromociones$
         If s$ <> "" Then Begin
            calcularDesc = 0
            Call  leeRespuestaPromociones(s$)
            Call  descuentoTotal
            Call  muestraNuevoTotal
         EndIf
   	
      TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal			  										! Actualiza total
      If TS.IO.KEYS(1) = 70 Then Begin																			! Es una anulacion de pago
         Call  eliminaPago
         TS.BALDUE(0) = TS.TOTALS(0,0,0) - (desTotal) - TS.TENDERED(0)        ! Actualiza total pago
         If JAVA.INIT = -1 Then Begin
            If TS.BALDUE(0) > 0 Then Begin
               jGuiSubState = 80539
               Call  javaEvent(terminalSubStateMsg)
            EndIf
         EndIf 
      EndIf Else Begin																											! Es la aplicación de un pago
         Call  agregaPago																										!
         TS.BALDUE(0) = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)
         If JAVA.INIT = -1 Then Begin
            If TS.BALDUE(0) > 0 and NOT (TS.IO.KEYS(1) = 82 and TS.IO.MOTORKEY = 80) and ( TS.RECOVERY = 0 ) Then Begin
               TS.IO.MOTORKEY = 81 
               TS.IO.STATE = 10
               TS.IO.KEYS(6) = 81
               TS.IO.INPUT.READY = -1
            EndIf
         EndIf
      EndIf
      !Actualiza totales en la interfaz grafica
      If java.init = -1 Then Begin  
         If TS.BALDUE(0) < 0 Then Begin
            vx = TS.TOTALS(0,0,0)
            TS.TOTALS(0,0,0) = (-1) * TS.BALDUE(0) 
            Call  javaEvent(updateTotalsDataMsg)
            wait; 2000
            TS.TOTALS(0,0,0) = vx
         EndIf
      EndIf

      !Si ya se realizadon todos los pagos, se distribuyen los descuentos
      !graba cadenas 99 53 y genera cupones de descuento
      If TS.BALDUE(0) <= 0 Then Begin
         i = 0
         while i <= desNum
            Call  aplicaDescuentos( i )
            Call  agregaCadenas9953( i )
            i = i + 1
         wend
      EndIf
   EndIf

   salir:
   s$ = ""
End Sub

!------------------------------------------------------------------------------
! En la comprobacion de limite de pagos
!------------------------------------------------------------------------------
Sub NUOTPROM40 public

!Call  traceNUO("PRM40    AMTTENDE=" + str$(SL.TE.AMTTENDE) + " TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDERED=" + str$(TS.TENDERED(0)) + " desTotal=" + str$(desTotal) + " descEfectivo=" + str$(descEfectivo) + " DATA$(7)=[" + TS.IO.DATA$(7) + "]" )

   If pagoEfectivo and ( descEfectivo = 0 ) Then Begin
      TS.TENDERED(0) = TS.TENDERED(0) - SL.TE.AMTTENDE
      SL.TE.AMTTENDE = TS.TOTALS(0,0,0) - desTotal - TS.TENDERED(0)
      TS.TENDERED(0) = TS.TENDERED(0) + SL.TE.AMTTENDE
      pagoEfectivo = 0
!Call  traceNUO("PRM40 pE AMTTENDE=" + str$(SL.TE.AMTTENDE) + " TOTALS=" + str$(TS.TOTALS(0,0,0)) + " BALDUE=" + str$(TS.BALDUE(0)) + " TENDERED=" + str$(TS.TENDERED(0)) + " desTotal=" + str$(desTotal) + " descEfectivo=" + str$(descEfectivo) + " DATA$(7)=[" + TS.IO.DATA$(7) + "]" )
   EndIf

End Sub

!-----------------------------------------------------------------------------
! Imprime resumen de descuentos en factura
!-----------------------------------------------------------------------------
Sub NUOTPROMresumen(m$) public
   String m$, d$
   Integer*2 i

!Call  traceNUO( "resumen descuento total=" + str$(desTotal) + " detalles=" + str$(len(m$)) )

   If (desTotal = 0) or (m$ = "") Then Exit Sub

   !Graba cadenas de cupones externos
   i = 3
   while i < len( cupoNext$ )
      d$ = mid$( cupoNext$, i, 10 )
      If right$( d$, 6 ) <> "000000" Then Begin
         Call  agregaCadenaTSL( chr$( 99H ) + ":" + chr$( 3 ) + ":" + pack$( "27" + d$ ) )
      EndIf
      i = i + 12
   wend

   Call  actualizaTSL
   NUOcmdImpresion$ = NUO.LetraJunta$ + NUO.NegritaA$
   Call  imprimeCJ("**************************************")
   Call  imprimeCJ("*****  TU DINERO !SI QUE RINDE!  *****")
   Call  imprimeCJ("AHORRO TOTAL: $" + right$(String$( 9, " ") + formatea2dec( desTotal ) , 9 ))
   Call  imprimeCJ("INCLUYE IVA")
   Call  imprimeCJ("**************************************")
   NUOcmdImpresion$ = NUO.LetraJunta$ + NUO.NegritaD$

   Call  imprimeCJ("DESCRIPCION            AHORRO RENDIDOR")
   Call  imprimeCJ("--------------------------------------")
   For i = 1 to len(m$) step 21
      d$ = mid$( m$, i, 21 )
      Call  imprimeCJ( d$  )
   Next i
   Call  imprimeCJ("--------------------------------------")
   Call  imprimeCJ("SU AHORRO INCLUIDO IVA EN ")
   Call  imprimeCJ("ESTA COMPRA  ES DE : $" + right$(String$( 9, " ") + formatea2dec( desTotal ) , 9 ) )
   Call  imprimeCJ( " " )

End Sub

!------------------------------------------------------------------------------
!Datos para descuento empleado
!------------------------------------------------------------------------------
Sub NUOTPROMempleado( c$ ) public
   String c$

   !limpio variables con datos de descuento empleado (tipo, disponible y flag)
   Call  limpiaDesEmpleado

!Call  traceNUO( "NUOTPROMempleado cod=["+ c$ +"]" )

   If len( c$ ) = 0 Then Exit Sub

   empleadoCon = -1
   empleadoCod = c$

End Sub

!------------------------------------------------------------------------------
! retorna el monto descuento
!------------------------------------------------------------------------------
Function  NUOTPROMdescuento public
   Integer*4 NUOTPROMdescuento
   NUOTPROMdescuento = desTotal
End Function 

!------------------------------------------------------------------------------
! retorna la cadena de descuento
!------------------------------------------------------------------------------
Function  NUOTPROMresPromociones public
   String NUOTPROMresPromociones
   NUOTPROMresPromociones = resPromociones
End Function 

!------------------------------------------------------------------------------
! Para no aplicar descuentos en recargas
!------------------------------------------------------------------------------
Sub noAplicaDescuento public
   calcDescuento = 0
End Sub

!------------------------------------------------------------------------------
! Activa/Desactiva temporalmente el calculo de descuentos
! f = 0  Para no calcular descuentos si no ha cambiado nada en la transaccion
! f = -1 Para calcular los descuentos 
!------------------------------------------------------------------------------
Sub calcularDescuentos( f ) public
   Integer*1 f
   calcularDesc = f
End Sub

