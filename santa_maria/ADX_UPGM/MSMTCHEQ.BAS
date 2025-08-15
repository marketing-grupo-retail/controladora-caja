!-----------------------------------------------------------------------
!Modulo de impresion de cheque
!Fecha: 20 de diciembre de 2021
!Autor: Alejandro Farre
!
! Modificacion: 25 de Enero de 2022 Edison Leon
! Modificacion: 27 de Enero de 2022 Alejandro Farre
! Modificacion: 16 de Mayo  de 2022 Carlos Calderon
!               Se parametriza la funcionalidad
!-----------------------------------------------------------------------

%ENVIRON T

integer*1 estado, \
          depurar, \
          dummy, \
          procesandoCheque, \
          intentos, \
          estadocheque, \
          autorizacion.cheque, \
          procesa.pago, \
          anula, \
          procesa.pago.sn.autori, \
          aut400, \
          autCheq

string    beneficiario$, \
          ciudad$, \
          fechaModificacion$

integer*4 PagoLimite
INTEGER*1 GLOBAL PRT4610.SS.ENABLE       ! single station printer

%INCLUDE NUOTVARI.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE JAVAGUIV.J86

%INCLUDE NUOTRUTI.J86
%INCLUDE MSMTFACT.J86
%INCLUDE MSMTCLIE.J86
%INCLUDE NUOTPROM.J86
%INCLUDE JAVAGUIC.J86

!-------------------------------------------------------------------------------
! Retorna en letras un numero inferior a mil
! 1 es la cadena que contiene el numero (siempre positivo)
! s una cadena conteniendo el numero en palabras
!-------------------------------------------------------------------------------
function bajomil$( ba1$ )
   string    bajomil$, ba1$, ba2$, ba3$
   integer*2 bn, bi

   on error goto err.loc

   ba2$ = ""
   ba1$ = right$( "   " + ba1$, 3 )
   bn = val( ba1$ )
   if bn < 1 then goto fin.bajomil

   ! Unidades y Decenas
   bi = val( right$( ba1$, 2 ) )
   if bi = 0 then goto cen.bajomil
   ba3$  =  "UN     DOS    TRES   CUATRO CINCO  SEIS   SIETE  OCHO   " + \
            "NUEVE  DIEZ   ONCE   DOCE   TRECE  CATORCEQUINCE "
   if bi < 16 then begin
      ba3$ = mid$( ba3$, 7 * bi - 6, 7 )
      ba2$ = elider$( ba3$ )
      endif \
   else begin
      bi = val( right$( ba1$, 1 ) )
      if bi <> 0 then begin
         ba3$ = mid$( ba3$, 7 * bi - 6, 7 )
         ba2$ = " Y " + elider$( ba3$ )
      endif
      bi = val( mid$( ba1$, 2, 1 ) )
      ba3$  =  "DIEZ     VEINTE   TREINTA  CUARENTA CINCUENTA" + \
               "SESENTA  SETENTA  OCHENTA  NOVENTA  "
      ba3$ = mid$( ba3$, 9 * bi - 8, 9 )
      ba2$ = elider$( ba3$ ) + ba2$
   endif

   ! Centenas
   cen.bajomil:
   if ba1$ = "100" then ba2$ = "CIEN": goto fin.bajomil
   bi = val( left$( ba1$, 1 ) )
   ba3$  =  "CIENTO       DOSCIENTOS   TRESCIENTOS  CUATROCIENTOS" + \
            "QUINIENTOS   SEISCIENTOS  SETECIENTOS  OCHOCIENTOS  NOVECIENTOS  "
   if bi <> 0 then begin
      ba3$ = mid$( ba3$, 13 * bi - 12, 13 )
      ba2$ = elider$( ba3$ ) + " " + ba2$
   endif

   fin.bajomil:
   bajomil$ = ba2$

   sale:
   exit function

   err.loc:
   if errf% = 39 then resume
   call traceNUO( "bajmil ERROR " + err + hexanum$( errn ) )
   resume sale
fend

!-------------------------------------------------------------------------------
!Funcion para cortar una cadena en palabras completas
!1 es la cadena a cortar
!2 es la cantidad de caracteres m ximos a cortar
!s es la cadena cortada en palabras completas
!-------------------------------------------------------------------------------
function cortalin( corta$, corta1, corta2 )

   string       cortalin, corta$, cortat$
   integer*2    corta1, corta2, corta3

   on error goto err.loc

   cortat$ = corta$ + " "
   corta3 = corta2 + corta1
   if corta3 <= len( cortat$ ) then begin
      while corta3 > 1 and mid$( cortat$, corta3, 1 ) <> " "
         corta3 = corta3 - 1
      wend
      if ( corta3 - corta1 ) < 1 then begin
         corta3 = corta2 + corta1
      endif
      endif \
   else begin
      corta3 = len ( cortat$ )
   endif
   if corta1 < len( cortat$ ) then \
      cortalin = mid$( cortat$, corta1, corta3 - corta1 )

   sale:
   exit function

   err.loc:
   if errf% = 39 then resume
   call traceNUO( "cortalin ERROR " + err + hexanum$( errn ) )
   resume sale
fend

SUB TSHIEC04 EXTERNAL
END SUB

!------------------------------------------------------------------------------
! Retorna en letras un numero
! 1 es un numero entero de cuatro bytes (se ignora el signo)
! s una cadena conteniendo el numero en palabras
!------------------------------------------------------------------------------
function crletras$(num.letr)
   string    crletras$, letr$, letr1$, letr2$, letp$
   integer*2 i.letr, j.letr
   integer*4 num.letr

   on error goto err.loc

   letp$ = ""
   num.letr = abs(num.letr)
   letr$ = right$("            " + str$(num.letr), 12)
   letr2$ = " MIL      MILLONES MIL              "
   j.letr = 1
   i.letr = 1
   while i.letr < 12
      letr1$ = mid$( letr$, i.letr, 3 )
      if val( letr1$ ) > 0 then begin
         letp$ = letp$ + bajomil$( letr1$ ) + mid$( letr2$, j.letr, 9 )
         if i.letr = 4 then \
            if val( mid$( letr$, 1, 6 ) ) = 1 then letp$ = left$( letp$, len( letp$ ) - 2 )
         letp$ = elider$( letp$ ) + " "
      endif
      i.letr = i.letr + 3
      j.letr = j.letr + 9
   wend
   crletras$ = elider$( letp$ )

   sale:
   exit function

   err.loc:
   if errf% = 39 then resume
   call traceNUO( "crletras$ ERROR " + err + hexanum$( errn ) )
   resume sale
fend

sub muestraEnter( msg1$, msg2$ )
   string  msg1$, msg2$, t$, s$
   string ADATA$, D$, R$, x$
   integer*2 cod
   integer*1 f

  !call guardaDisplay
   TS.LINETYPE = 12  !DISPLAY BUFFERS AS PASSED (TERMINAL MONITOR)
   s$ = string$( 20, " " )
   TS.TEMP1$ = left$( msg1$ + s$, 20 )
   TS.TEMP2$ = left$( msg2$ + s$, 20 )

   ! Limpia IOPROCE
   lockdev 32, purge

   t$ = ""
   f = -1
   while f
      call muestraOperador( TS.TEMP1$, TS.TEMP2$ )
      unlockdev 32, 99
      wait 32; 60000
      if event% = 32 then begin

         ! Lectura Teclado y Status
         TS.ER.RETURN = -1
         read #32; ADATA$, D$, D$, D$, D$, D$, D$, D$, D$, D$, D$
         if TS.ER.RETURN = -1 then begin
            x$ = mid$( ADATA$, 13, 1 )
            if x$ = "" then cod = 80 else cod = asc( x$ )
            if cod = 80 then begin
               f = 0
            endif
         endif else begin
            f = 0
         endif
      endif
   wend
   lockdev 32, purge
   !call restauraDisplay

end sub

!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
sub mostrarCheque
   integer*1 x
   x = 1
   while ( x )
      if x < 2  then begin
         call muestraOperador( "SAQUE CHEQUE DE LA", "IMPRESORA" )
         wait;8000
         x = x + 1
      endif else begin
         if java.init <> -1 Then begin
            call muestraEnter("B870 VERIFIQUE LA", "FIRMA DEL CHEQUE" )
         endif else begin
            jGuiSubState = 80536
            call javaEvent(terminalSubStateMsg)
            call muestraEnter("", "")
         endif
         exit sub
      endif
   wend
end sub

!------------------------------------------------------------------------------
! imprime un cheque
!------------------------------------------------------------------------------
sub imprimeCheque( monto$ )
   string monto$, letras$
   string L1$, L2$, L3$, L4$, L5$, LE$, L0$, LC$, \
          mes$, f$, LF, FLIP, DOBLE, ENFAT, EJECT, dato$
   integer*1 i, k
   integer*4 monto
!integer*4 estadoDI

   k = 60 !Linea mas larga en el cheque
   LF = chr$(10)
   FLIP = chr$(1BH)+chr$(35H)
   DOBLE = chr$(1Bh)+chr$(57H)
   ENFAT = chr$(1Bh)+chr$(47H)
   EJECT = chr$(1Bh) + chr$(6Dh)

   L3$ = beneficiario$
   LE$ = ciudad$

!call traceNUO( "imprimeCheque monto=" + monto$ + " a la orden=[" + L3$ + "] ciudad=[" + LE$ + "]" )

   !monto = int( val( monto$ ) / 100 )
   monto = val(left$( monto$, len(monto$) - 2 ))

   letras$ = crletras$( monto )
   letras$ = elider$( letras$ )
   L4$ = cortalin( letras$, 1, k )
   L5$ = cortalin( letras$, len(L4$) + 2 , k )

   ! Fecha
   f$ = "20" + date$
   !mid$( f$, 1, 2 ) + "/"  + mid$( f$, 3, 2 ) + "/" + mid$( f$, 4 )

   i = 9 * val( mid$( f$, 5, 2 ) ) - 8
   mes$ = mid$( "  ENERO  FEBRERO    MARZO    ABRIL     MAYO    JUNIO  " + \
      "  JULIO   AGOSTO  SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE", i, 9 )

   L0$ =  mid$( f$, 1, 4 ) + "/"  + mid$( f$, 5, 2 ) + "/" + mid$( f$, 7, 2 )

   monto = val( monto$ )
   L1$ = left$( formatea2Dec( monto ) + string$( 8, " " ), 8 )

   L2$ = right$( f$, 2 ) + " " + mes$ + " " + left$( f$, 4 )

   LC$ = L4$ + " DOLARES con " + right$( "00" + str$( monto ), 2 ) + " Ctvos"
!call traceNUO("L1$=["+L1$+"] L2$=["+L2$+"] L4$=["+L4$+"] L5$=["+L5$+"] LC$="+LC$+"]")
!estadoDI = GETLONG(35)
!call traceNUO( "imprimeCheque estadoDI=[" + hexanum$( estadoDI ) + "]" )

   i = -1
   while ( i )
      call TSHIEC04
      if not TS.DEV.FLAG then begin
         !i = i - 1
         call muestraBorrar( "B049 INSERTE DOCU-", "MENTO A FRANQUEAR" )
      endif else begin
         !Selecciona lanscape en el DI: cuando la impresora esta en modo Honor
         write #35; " "
         write #35; " "
         write #35; NUO.AlineaIzq$ + " "
         !Enfatizado
         write #35; ENFAT + chr$(1)
         !putlong 35, 80570001H
         putlong 35, 80440001H

         !Da vuelta el cheque para escritura
         !write #35; FLIP

         !Doble ancho
         !write #35; DOBLE + chr$(1)

         !Enfatizado
         write #35; ENFAT + chr$(1)

         !espaciado de linea 6 milimetros
         !write #35; chr$(1Bh) + chr$(33H) + chr$(30)

         !write #35; chr$(1Bh) + chr$(3CH) + chr$(0)

         write #35; LF
         write #35; LF
         write #35; " "
         write #35; LF
         write #35; LF
         write #35; NUO.AlineaIzq$ + LE$ + ", " + L0$
         write #35; NUO.AlineaIzq$ + " "
         write #35; LF
         write #35; LF
         write #35; LF
         !write #35; chr$(1Bh) + chr$(33H) + chr$(30)
         write #35; NUO.AlineaIzq$ + L5$
         write #35; NUO.AlineaIzq$ + LC$
         write #35; LF
         write #35; LF
         write #35; DOBLE + chr$(1)
         write #35; L3$
         !write #35; LF + L2$ + LF
         !write #35; LF
         write #35; "             "
         !write #35; NUO.LetraNormal$ + L1$
         write #35; L1$

         !Anula doble ancho
         write #35; DOBLE + chr$(0)

         !Anula enfatizado
         write #35; ENFAT + chr$(0)

         !Selecciona portrait en el DI:
         putlong 35, 80440000H

         !Expulsa cheque
         write #35; EJECT
         wait;1000
         i=0
      endif
   wend

   !sale:
   !call traceNUO( "imprimeCheque fin" + str$(i) )
   !exit sub

   !err.loc:
   !call traceNUO( "imprimeCheque ERROR " + err + hexanum$( errn ) + " sesion " + str$( errf% ) )
   !resume sale

end sub

!sub expulsa.cheque public
!   integer*1 f
!   string EJECT
!   EJECT = chr$(1Bh) + chr$(6Dh)
!
!   while( f )
!      call TSHIEC04
!      if TS.DEV.FLAG then begin
!         write #35;EJECT
!         wait;5000
!      endif else begin
!         f = 0
!      endif
!   wend
!end sub

!------------------------------------------------------------------------
! Captura datos para pagos con cheque
! retorna:
! 0 si no se ha identificado el cliente o no se ingresaron los datos
!-1 ingreso de datos correcto
!------------------------------------------------------------------------
function procesaCheque
   integer*1 procesaCheque
   string d$, doc$, che$
   integer*4 v

   procesaCheque = 0

   !no se ha identificado el cliente
   if datosCliente$(5) = "" then begin
      call muestraBorrar( "DEBE CAPTURAR DATO", "DEL CLIENTE  /Borrar" )
      goto salir
   endif

   jGuiSubState = 80501
   call javaEvent(terminalSubStateMsg)
   !solicita ingresar numero de documento
   d$ = ingresoDatos$( "INGRESE NUMERO" , "DE DOCUMENTO" )

   if java.init then begin
      jGuiSubState = 0
   endif

   if left$( d$, 1 ) = "A" then begin
      goto salir
   endif
   if left$( d$, 1 ) = "P" then begin
      doc$ = right$( d$, len(d$) - 2 )
      if doc$ = "" then begin
         goto salir
      endif
   endif

   !Solicita ingresar numero de cheque
   jGuiSubState = 80510
   call javaEvent(terminalSubStateMsg)
   d$ = ingresoDatos$( "INGRESE NUMERO" , "DE CHEQUE" )

   if java.init then begin
      jGuiSubState = 0
   endif

   if left$( d$, 1 ) = "A" then begin
      goto salir
   endif
   if left$( d$, 1 ) = "P" then begin
      che$ = right$( d$, len(d$) - 2 )
      if che$ = "" then begin
         goto salir
      endif
   endif

!call traceNUO( "cheque key=" + str$(TS.IO.KEYS(1)) + " DA3=" + TS.IO.DATA$(3) + " DA7=" + TS.IO.DATA$(7) + " BALDUE=" + str$( TS.BALDUE(0) ) )

   d$ = TS.IO.DATA$(3)
   if d$ = "" then d$ = "1"
   if ( d$ = "1" ) or ( d$ = "2" ) then begin
      TS.IO.DATA$(3) = d$
      TS.IO.DATA$(9) = doc$

      if TS.IO.KEYS(1) = 70 then begin
         v = consultaPago( TS.IO.DATA$(7), "", "2" + TS.IO.DATA$(3) )
      endif else begin
         v = descuentoPago( TS.IO.DATA$(7), "", "2" + TS.IO.DATA$(3) )
      endif
      if v > 0 then TS.IO.DATA$(7) = str$( val(TS.IO.DATA$(7)) - v )

      procesaCheque = -1

   endif

   salir:
   d$ = ""
   doc$ = ""
   che$ = ""
end function

function capturaAutorizacionCheque
   integer *1 capturaAutorizacionCheque, x
   string resAutorizacion

   capturaAutorizacioncheque = 0
   resAutorizacion = ""
!call traceNUO("Paso captura autorizacion:")
   x = 1
!call traceNUO("Paso1 captura autorizacion:")

   !Pide 2 veces autorizacion
   while (x)
      if x < 3  then begin
         resAutorizacion = ingresoDatos$( "INGRESE NUMERO" , "DE AUTORIZACION" )
!call traceNUO("Paso2 captura autorizacion:")
         if left$( resAutorizacion, 1 ) = "P" then begin
            resAutorizacion = right$( resAutorizacion, len(resAutorizacion) - 2 )
            if not esNumero( resAutorizacion ) then exit function
            if resAutorizacion <> "" then begin 
               x = x + 1
               if x = 3 then begin 
                  capturaAutorizacionCheque = -1
                  x = -1
                  exit function
               endif
            endif
         endif 
      endif
   wend
end function

!------------------------------------------------------------------------------
! carga parametros de cheques
! [CHEQUE]
! PROYECTO_ACTIVO              1
! PAGO_LIMITE                  40000
! INTENTOS                     3
! BENEFICIARIO                 MEGA SANTAMARIA S.A.
! CIUDAD                       QUITO
!------------------------------------------------------------------------------
sub MSMTCHEQ07 public
   integer*1 k
   integer*4 inicioBloque
   string    v$, r$

   fechaModificacion$ = " 16 May 2022"

   procesandoCheque = 0
   autorizacion.cheque = 0
   procesa.pago = 0
   estadocheque = 0

   call imprimeCJ( "***** MODULO CHEQUE ******************" )

   !cargar parametros del archivo PARGENER
   inicioBloque = abreParametros( "R::$ARGENER", "[CHEQUE]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE CHEQUE", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "CHEQUE NO CONFIGURADO" + fechaModificacion$ )
      call cierraParametros( 80 )
      exit sub
   endif

   !Proyecto activo 0 No, 1 Si
   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )

   if v$ <> "" then begin
      if esNumero( v$ ) then k = val( v$ ) else k = 0
      if ( k and 1 ) = 1 then estado  = -1
      if ( k and 2 ) = 2 then depurar = -1
      if ( k and 4 ) = 4 then dummy   = -1

      if estado then begin

         intentos = 3
         PagoLimite = 40000
         beneficiario$ = "MEGA SANTAMARIA S.A."
         ciudad$ = "QUITO"

         ! Limite maximo sin autorizacion de supervisor
         v$ = leeParametro$( "PAGO LIMITE", 80, inicioBloque )
         if esNumero( v$ ) then PagoLimite = val( v$ )

         ! Numero de intentos para reimprimir el cheque
         v$ = leeParametro$( "INTENTOS", 80, inicioBloque )
         if esNumero( v$ ) then intentos = val( v$ )

         ! Nombre beneficiario para imprimir en el llenado del cheque
         v$ = leeParametro$( "BENEFICIARIO", 80, inicioBloque )
         if v$ <> "" then beneficiario$ = v$

         ! Ciudad para imprimir en llenado del cheque
         v$ = leeParametro$( "CIUDAD", 80, inicioBloque )
         if v$ <> "" then ciudad$ = v$

      endif
   endif

   call cierraParametros( 80 )

   if depurar then begin
      call imprimeCJ( "PAGO LIMITE   " + right$( string$( 13, " " ) +  str$( PagoLimite ) , 13 ) )
      call imprimeCJ( "INTENTOS      " + right$( string$( 13, " " ) +  str$( intentos )   , 13 ) )
      call imprimeCJ( "BENEFICIARIO  " + right$( string$( 24, " " ) +  beneficiario$      , 24 ) )
      call imprimeCJ( "CIUDAD        " + right$( string$( 13, " " ) +  ciudad$            , 13 ) )
   endif

   if estado then r$ = "ON " else r$ = "OFF"
   call imprimeCJ( "ESTADO " + r$ + fechaModificacion$ )

end sub

!------------------------------------------------------------------------------
! Controla el medio de pago cheque
!------------------------------------------------------------------------------
sub MSMTCHEQ14 public
   integer*4 valorConsumo, v, t0, t1, t2
   string dato$,m$, datoingreso$

   !Es un pago con cheque
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 92 ) and TS.INTRX then begin

      ! Se obliga a totalizar si el estado de la transaccion es 0 - IN ITEM SALES
      autorizacion.cheque = 0
      procesa.pago = 0
      autCheq = 0
      datoingreso$ = ""

      if NOT TS.BAL.TAKEN then begin
         TS.GUIDANCE = 1020
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      if java.init = -1 then begin
         if NOT TS.BAL.TAKEN then begin
            TS.GUIDANCE = 1020
            TS.IO.MOTORKEY = 0
            exit sub
         endif
      endif

      if java.init = -1 then begin
         jGuiSubState = 80538
         call javaEvent(terminalSubStateMsg)
         datoingreso$ = ingresoDatos$( "INGRESE MONTO " , "" )
         if left$( datoingreso$, 1 ) = "A" then begin 
            call ignoraTeclas
            exit sub
         endif

         if left$( datoingreso$, 1 ) <> "P" then begin 
            call ignoraTeclas
            exit sub
         endif
         datoingreso$ = right$( datoingreso$, len( datoingreso$ ) - 2 )
         TS.IO.DATA$(7) = datoingreso$
         jGuiSubState = 0
      endif

      if TS.IO.DATA$(7) = ""  then begin
         TS.GUIDANCE = 1003 !B003 SECUENCIA DE TECLAS INCORRECTA
         TS.IO.MOTORKEY = 0
         exit sub
      endif

      call tarifasConDescuento( t0, t1, t2 )
      v = t0 + t1 + t2 - TS.TENDERED(0)
      valorConsumo = val( TS.IO.DATA$(7) )
!call traceNUO ("valorConsumo:= " + str$(valorConsumo) + " v:= " + str$(v))
      if TS.IO.KEYS(1) = 70 then begin
         anula = -1
      endif else begin
         anula = 0
         if valorConsumo > v then begin
            call muestraBorrar( "MONTO CHEQUE SUPERA", "VALOR FACTURA" )
            call ignoraTeclas
            exit sub
         endif
      endif

      ! Pide datos para pago con cheque
      estadocheque = procesaCheque

      if estadocheque and (valorConsumo > PagoLimite)  then begin
         autCheq = -1
      endif

      if not estadocheque then call ignoraTeclas
      if PRT4610.SS.ENABLE then exit sub
      if autCheq and (valorConsumo > PagoLimite) then begin
         autorizacion.cheque = -1
         procesa.pago = 0
      endif else begin
         autorizacion.cheque = 0
         procesa.pago = -1
         endif
      endif
end sub

!------------------------------------------------------------------------------
! Controla el error de la impresora
!------------------------------------------------------------------------------
sub MSMTCHEQ37 public

   if procesandoCheque then begin
      intentos = intentos - 1
      if intentos < 1 then begin
         TS.USER.RETURN = -1
         procesandoCheque = 0
         !call ignoraTeclas
      endif
   endif
end sub

sub MSMTCHEQ30 public

!call traceNUO ("MSMTCHEQ30:" + TS.IO.DATA$(7) ) 
   if estadocheque and not anula then begin  
!call traceNUO("autorizacion.cheque:= " + str$(autorizacion.cheque))   
      if procesa.pago then begin
         procesandoCheque = -1
         intentos = 3
         call imprimeCheque( TS.IO.DATA$(7) )
         call mostrarCheque
!call traceNUO("Paso x aqui-30")
         procesa.pago = 0
         autorizacion.cheque = 0
         autCheq = 0
         if TS.BALDUE(0) > 0 then begin
            if JAVA.INIT = -1 then begin
               jGuiSubState = 80539
               call javaEvent(terminalSubStateMsg)
            endif
         endif
      endif
   endif
end sub

sub MSMTCHEQ40 public

!call traceNUO ("MSMTCHEQ40:" + TS.IO.DATA$(7) )
   if autorizacion.cheque then begin
      procesa.pago = -1
!call traceNUO("Paso x Qaqui-40")
   endif

end sub

sub MSMTCHEQ02 public
!call traceNUO ("Paso 02")
   autorizacion.cheque = 0
   procesa.pago = 0
   estadocheque = 0

end sub

