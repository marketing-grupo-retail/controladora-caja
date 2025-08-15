!------------------------------------------------------------------------------
! Modulo: MSMTNCRE.BAS Modulo Notas de Credito
! Fecha : 06 de Noviembre de 2021
! Autor : Carlos Calderon
! Hisroty : 30-11-2021 CCY se incorpora a la funcionalidad de cambio de forma
!                      de pago, la opcion de otros medios de pagos (CHEQUE TARJETA)
!           09-12-2021 ccy se incorpora a la funcionalidad de cambio de forma
!                      de pago, la conexion con transbank para reversar el pago
!                      tarjeta pinpad.       
!------------------------------------------------------------------------------
%ENVIRON T

integer*1 NCinTrx,                                \ Indicador que refeleja si existen NC en la transaccion -1=si 0=no         
          NCoperation,                            \ 68 NC
          NCtipOperacion,                         \ Indicador --> 1=Cambio Forma de Pago 2=Como medio de pago
          NCanulaPago,                            \ Indicador --> -1 si se esta anulando una NC
          NCesPagoNc,                             \ Indicador --> -1 si se esta pagando con una NC
          NCmaxCanTrx,                            \ cantidad de NC de la transaccion en curso
          NCindiceAnula,                          \ Indice de matriz de NC a anular
          NCmedioPago,                            \ Pago y variedad de pago SMA para el pago Nota de Credito
          Dummy,                                  \
          estado,                                 \
          depurar                                 !

integer*2 NCteclaMotorCambMP                      !

integer*4 NCmontoConsumo,                         \ Monto NC ingresado
          NCmontoMPTef,                           \ Monto del medio de pago TEF
          pipeTimeout,                            \ Tiempo de espera time out
          NCdevPagos(2)                           ! Matriz para cambio de medios de pagos multiples

string    NCfolio$,                               \ numero folio NC LLL CCC NNNNNNNNN
          NCheader$,                              \ encabezado mensajeria
          NCfoliosIntrx$(2),                      \ Folios de NC en la transaccion
          NCpagosAutorizCambioMP$,                \ Medios de pagos Supermarket permitidos para cambio de MP en una NC
                                                  \
          NCrespuesta$,                           \ respuesta consulta
          NClineTrailer$,                         \
                                                  \
          NClogo1$,                               \                                        
          NCcedulaRucCliente$,                    \
          NCNombreCliente$,                       \
                                                  \
          lf$,                                    \
          fechaModificacion,                      \
          NCIndicComprobante$(2),                 \
          separador                               ! separador de campos mensajeria agente

%INCLUDE NUOTVARI.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE EAMTRANS.J86
%INCLUDE JAVAGUIV.J86

!Rutinas y funciones de librerias externas

%INCLUDE MSMTTARB.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE NUOTJAVA.J86
%INCLUDE MSMTFACT.J86
%INCLUDE JAVAGUIC.J86

!STATUS FILE UPDATING, UPDATE OPERATOR/TERMINAL TOTALS
!will attempt to write the summary log.
SUB TSSFEC01 EXTERNAL
END SUB

SUB TSHIEC04 EXTERNAL       ! get status of document insert station
END SUB

!TSHIEC01 : OPEN THE CASH DRAWER
SUB TSCSEC06 EXTERNAL       ! OPEN CASH DRAWER
END SUB

SUB TSHIEC02                ! GET STATUS OF CASH DRAWER
END SUB

! display guidance and wait for clear
SUB TSCSEC08 EXTERNAL
END SUB

!------------------------------------------------------------------
! graba log "R::Q:/" o "R::C:/DQ/" + "TRC." + TS.TERMINAL$
!------------------------------------------------------------------
sub logCNB( dato$ )
   string dato$
   if depurar then begin
      call traceNUO( dato$ )
   endif
end sub

!Funciones y subrutinas TEF del modulo MSMTTARB.BAS

!retorna el pago y variedad de pago TEF modulo MSMTTARB.BAS
function get.MP.Tef external
   integer*2 get.MP.Tef
end function

sub pideLineaCabecera( numL , dato$ ) external
    integer*1 numL
    string dato$
end sub    

function existePagosTEF
   integer*1 f, existePagosTEF, indic
   integer*4 MPTef                          ! Medio de pago SMA para TEF

   existePagosTEF = 0
   Indic          = 0

   MPTef = get.MP.Tef                               ! se obtiene el MP Tef del modulo MSMTTARB
   !call traceNUO( "existePagosTEF MPTef=[" + str$(MPTef) + "]" )

   for f = 1 to NCdevPagos(0,0)
      if MPTef = NCdevPagos(f,1) then begin

         reintenta:
         if not anulaPagoTEF then begin
            if indic = 0 then \
               exit function \
            else \
               goto reintenta
         endif else begin
            Indic = Indic + 1
         endif

      endif
   next f
   existePagosTEF = -1

end function

function existePagoNC
   integer*1 existePagoNC,                    \
             f

   existePagoNC = 0
   NCindiceAnula = 0

   for f = 1 to NCinTrx

!call traceNUO( "existePagoNC NCfoliosIntrx$(f,1)=["+NCfoliosIntrx$(f,1)+"]" )
!call traceNUO( "existePagoNC NCfolio$=["+NCfolio$+"]" )

      if ( NCfoliosIntrx$(f,1) = NCfolio$ ) then begin
         NCmontoConsumo = val(NCfoliosIntrx$(f,2))
         existePagoNC = -1
         NCindiceAnula = f
      endif

   next f

end function

! Valida que la trx este totalizada
function totalReady
   integer*1 totalReady

   totalReady = 0
   if TS.BAL.TAKEN then begin
      totalReady = -1
   endif

end function

! Detecta intento de pago con NC
! Ingreso de una NC como medio de pago con las secuencias
! tecla 95        --> para el pago con NC
! ANULA tecla 95  --> para la anulacion de pago

function esSecuenciaTeclaPagoNC
   integer*1 esSecuenciaTeclaPagoNC
   integer*4 t0, t1, t2
   string monto$

   esSecuenciaTeclaPagoNC = 0
   NCanulaPago = 0
   NCesPagoNc  = 0

   if ( TS.IO.MOTORKEY = 95  ) and \
      ( TS.IO.KEYS(7)  = 95  ) and \
      ( TS.IO.KEYS(3)  = 0   ) and \
      ( TS.IO.DATA$(3) = "" )  and \
      ( TS.IO.STATE    = 10  ) and \
      ( TS.INTRX ) then begin

!call traceNUO( "esSecuenciaTeclaPagoNC " )      

      if not totalReady then begin
         TS.IO.MOTORKEY = 0
         TS.GUIDANCE = 1020 ! Solicita total a la trx
         exit function
      endif

!call traceNUO( "esSecuenciaTeclaPagoNC-2" )     

      if TS.IO.KEYS(1) <> 70 then begin

      if java.Init = -1 Then begin 

         jGuiSubState = 80538
         call javaEvent(terminalSubStateMsg)

         monto$ = ingresoDatos$( "INGRESE MONTO  " , "" )
         if left$( monto$, 1 ) = "A" then begin
            call ignoraTeclas
            exit function
         endif
         if left$( monto$, 1 ) <> "P" then begin
            call ignoraTeclas
            exit function
         endif
         monto$ = right$( monto$, len( monto$ ) - 2 )
         TS.IO.DATA$(7) = monto$

      endif
      if ( TS.IO.DATA$(7) <> "" ) and esnumero(TS.IO.DATA$(7)) then NCmontoConsumo = val(TS.IO.DATA$(7)) \
      else begin
         call muestraBorrar( "ERROR DEBE INGRESAR","EL MONTO DE LA NC" )
         call ignoraTeclas
         exit function
      endif
   endif \
      else NCanulaPago = -1

      ! Control que impide recibir NC mas de la cantidad permitida
      if ( NCinTrx = NCmaxCanTrx ) and ( not NCanulaPago ) then begin
         call muestraBorrar( "NO SE PEMITEN MAS NC","EN LA TRX" )
         call ignoraTeclas
         exit function
      endif 

      call tarifasConDescuento( t0, t1, t2 )
!call traceNUO( "esSecuenciaTeclaPagoNC t0= [" + str$(t0) + "] t1= [" + str$(t1) + "] t2= [" + str$(t2) + "]" )
      if not NCanulaPago and ( NCmontoConsumo > (t0 + t1 + t2) ) then begin
         call muestraBorrar( "ERROR MONTO N.C","SUPERA VALOR FACTURA" )
         call ignoraTeclas
         exit function 
      endif

      NCtipOperacion = 2                        ! Indicador --> 1=Cambio Forma de Pago 2=Como medio de pago
      esSecuenciaTeclaPagoNC  = -1

!call traceNUO( "esSecuenciaTeclaPagoNC  NCanulaPago  = [" + str$(NCanulaPago) + "]" )
!call traceNUO( "esSecuenciaTeclaPagoNC  esSecuenciaTeclaPagoNC = [SI]" )

   endif

end function

! Detecta se presiono tecla 254 sin iniciar una trx, secuencia para cambiar NC por Efectivo en POS
function esCambioMedioPago
   integer*1 esCambioMedioPago, mp

   esCambioMedioPago = 0
   NCtipOperacion      = 0

   if ( TS.IO.MOTORKEY = NCteclaMotorCambMP  ) and \
      ( TS.IO.DATA$(2) = ""   ) and \
      ( TS.IO.KEYS(2)  = NCteclaMotorCambMP  ) and \
      ( TS.IO.STATE    = 10   ) and \
      ( not TS.INTRX ) then begin
      esCambioMedioPago = -1
      NCtipOperacion    = 1                    ! Indicador --> 1=Cambio Forma de Pago 2=Como medio de pago
   endif

end function

sub genera.trailer
   string m$
   m$ = date$
   NClineTrailer$ = right$(m$,2) + "/" + mid$(m$,3,2) + "/" + left$(m$,2)      + " "      ! fecha dd/mm/aa
   m$ = time$
   NClineTrailer$ = NClineTrailer$ + left$(m$,2) + ":" + mid$(m$,3,2)        + " "        ! hora hh:mm
   NClineTrailer$ = NClineTrailer$ + TS.STORE$                               + " "        ! tienda ####
   NClineTrailer$ = NClineTrailer$ + str$(val(TS.TERMINAL$))                 + " "        ! tienda ##
   NClineTrailer$ = NClineTrailer$ + right$("0000" + str$(SL.HD.TRANSNUM + 1),4) + " "    ! tienda ##
   NClineTrailer$ = NClineTrailer$ + str$(val(unpack$(TS.OPER$)))                         ! id cajero
end sub

sub generaEncabezadoNC

   NCheader$ = "P"                                 + separador + \ canal de comunicacion S=socket P=Pipe
                str$(NCoperation)                  + separador + \ requerimiento mensaje por pipe para MSM
                "0000"                             + separador + \ canal de respuesta
                TS.TERMINAL$                       + separador + \ terminal de origen
                "0"                                + separador + \ tipo de conexion 0=temporal 1=permanente
                "20" + date$ + time$                             ! fecha y hora de inicio de la operacion

   call genera.trailer

end sub

function generaCuerpoNC$
   string m$,                                               \
          generaCuerpoNC$                                   !

   generaCuerpoNC$ = ""

   m$ = NCheader$                          + separador +  \ header comun
        NCfolio$                           + separador +  \ Folio de la NC a consultar
        str$(NCmontoConsumo)               + separador +  \ Monto ingresado como valor de la NC
        str$(NCtipOperacion)                              ! 1=NC como forma de pago 2=NC cambio por Efectivo

   generaCuerpoNC$ = m$

end function

!Genera mensajeria consulta de NC
function generaMensajeNC
   string m$, generaMensajeNC

   call generaEncabezadoNC
   m$ = generaCuerpoNC$
   generaMensajeNC = m$

end function

function pideFolioNC
   integer*1 f, pideFolioNC
   string s$, m$

   pideFolioNC = 0
   NCfolio$     = ""
   f             = -1

!call traceNUO( "pideFolioNC  NCanulaPago   = [" + str$(NCanulaPago) )
!call traceNUO( "pideFolioNC  NCtipOperacion = [" + str$(NCtipOperacion) )
   
   if NCanulaPago then m$ = "PARA ANULAR" \
   else if NCtipOperacion = 1 then m$ = "POR CAMBIO DE PAGOS " \       !Indicador --> 1=Cambio Forma de Pago 2=Como medio de pago
   else m$ = "COMO MEDIO DE PAGO"

   while( f )
      if not (jGuiSubState = 80527) then begin
         jGuiSubState = 80501
      endif
      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("INGRESE NUMERO N.C.", m$ )
      if s$ = "A" then begin
         f = 0
         call ignoraTeclas
      endif else begin
         if len(s$) <> 17 then \
            call muestraBorrar( "FOLIO INVALIDO","LARGO 15 DIG" ) \
         else begin
            if left$(s$, 1) = "P" then begin
               m$ = right$(s$, 15 )
               if not esNumero( m$ ) then \
                  call muestraBorrar( "NRO. N.C. INVALIDO","REINTENTE" ) \
               else begin
                  NCfolio$ = m$
                  if NCtipOperacion = 2 then begin
                     if NCanulaPago then begin
                        if not existePagoNC then call muestraBorrar( "NRO NC NO EXISTE","EN TRANSACCION" ) \
                        else pideFolioNC = -1
                     endif \   
                     else begin
                        if existePagoNC then call muestraBorrar( "NRO NC YA EXISTE","EN TRANSACCION" ) \
                        else pideFolioNC = -1
                     endif
                  endif \
                  else pideFolioNC = -1
                  f = 0
               endif
            endif
         endif
      endif

      if jGuiSubState = 80501 then begin
         jGuiSubState = 80527
      endif else begin
         jGuiSubState = 0
      endif
   wend

end function

function pideMontoNC
   integer*1 f, pideMontoNC
   string s$, m$

   pideMontoNC    = 0
   NCmontoConsumo = 0
   f              = -1

   while( f )
      if not (jGuiSubState = 80542) then begin
         jGuiSubState = 80538
      endif
      call javaEvent(terminalSubStateMsg)
      s$ = ingresoDatos$("INGRESE MONTO N.C.  ", "")
      if s$ = "A" then begin
         f = 0
         call ignoraTeclas
      endif else begin
         if left$(s$, 1) = "P" then begin
            m$ = right$(s$, len(s$)-2)
            if m$ = "" then \
               call muestraBorrar( "ERROR DEBE INGRESAR","UN MONTO VALIDO" ) \
            else if not esNumero( m$ ) then \
               call muestraBorrar( "MONTO INVALIDO","REINTENTE" ) \
            else if val(m$) = 0 then \
               call muestraBorrar( "MONTO NO PUEDE SER","CERO" ) \
            else begin
               NCmontoConsumo = val(m$)
               pideMontoNC = -1
               f = 0
            endif
         endif
      endif
      if jGuiSubState = 80538 then begin
         jGuiSubState = 80542
      endif else begin
         jGuiSubState = 0
      endif
   wend

end function

function enviaMsjNC( msgNC$ )
   integer*1 enviaMsjNC
   string msgNC$

   enviaMsjNC = 0

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   if not enviaMensaje( msgNC$ ) then exit function

   enviaMsjNC = -1

end function

function esperaRespuestaNC
   integer*1 esperaRespuestaNC

   esperaRespuestaNC = 0
   NCrespuesta$      = ""

   !Espera que lleguen datos
   if not esperaPipe( pipeTimeout, "Consulta N.C." ) then  \
      exit function

   esperaRespuestaNC = -1

end function

function recibeRespuestaNC
   integer*1 recibeRespuestaNC
   string m$

   NCrespuesta$ = ""

   m$ = recibeMensaje( "NC" )

   !Si hubo error de comunicacion
   if m$ = "" then begin
      call muestraBorrar( "ERROR EN RESPUESTA","" )
      exit function
   endif

   recibeRespuestaNC = -1
   NCrespuesta$ = m$

end function


function NCTraeIndicadores$( mp )
   integer*1 mp, x
   string NCTraeIndicadores$

   NCTraeIndicadores$ = "00"

   for x = 1 to val(NCIndicComprobante$( 0,0 ))
       if NCIndicComprobante$( x,1 ) = str$(mp) then begin
          NCTraeIndicadores$ = NCIndicComprobante$( x, 2 )
          exit function
       endif
   next x

end function

!------------------------------------------------------------------------------
! Codigos de errores que retorno de la funcionalidad, en caso de fallo
! SPLIT CRE:INT Err(1) --> Pagos y o variedad de los pagos fuera del rango 123456-123456
! SPLIT CRE:INT Err(2) --> MP no numericos o de largo distinto de dos digitos
! SPLIT CRE:INT Err(3) --> Monto del MP no numerico
! SPLIT CRE:INT Err(4) --> MP para cambio de MP, no autorizados
!------------------------------------------------------------------------------

function splitCambioPagosNC( m1$ )
   integer*1 splitCambioPagosNC,         \
             k, f, x, y, t, v, mp
   integer*4 mon.dev
   string m1$, m2$, m3$, campo$, msg$

   on error goto err.loc

!call traceNUO( "splitCambioPagosNC NCpagosAutorizCambioMP$=[" + NCpagosAutorizCambioMP$ + "]" )
  
   !  12345678901234567890
   ! "11 180 21 100 45 120"
   ! "11 1710 21 2000"

   dim NCdevPagos(0,0)
   splitCambioPagosNC = 0
   
   m2$ = ""   ! Pagos  de 2 por cada uno
   m3$ = ""   ! Montos de 7 por cada uno
   
   f = -1
   x = 1
   y = 0
   mon.dev = 0
 
   if m1$ = "" then begin
      msg$ = "SPLIT NC Err(1)"
      call muestraBorrar("ERROR SPLIT NC",msg$)
      exit function
   endif

   m1$ = m1$ + " "

!call traceNUO( "splitCambioPagosNC m1$=[" + m1$ + "]" )

   while( f )
     k = match(" ",m1$,x)
     if k <> 0 then begin
        campo$ = mid$(m1$,x,k-x)
        if ( y = 0 ) then begin
           !call traceNUO( "splitCambioPagosNC campo$ MP=[" + campo$ + "]" )
           if ( len(campo$) = 2 ) and esnumero( campo$ ) then begin
              t = val(left$(campo$,1))
              v = val(right$(campo$,1))
              if ( match(str$(t),"123456",1) = 0 ) or ( match(str$(v),"123456",1) = 0 ) then begin
                msg$ = "SPLIT NC Err(2)"
                call muestraBorrar("ERROR SPLIT NC",msg$)
                exit function
              endif
              if match(campo$ + ";", NCpagosAutorizCambioMP$,1) = 0 then begin
                 !call traceNUO( "splitCambioPagosNC medio pago no autorizado para cambio de pago" + "] campo=[" + campo$ + "]" )
                 msg$ = "SPLIT NC Err(3)"
                 call muestraBorrar("MEDIO PAGO NO AUTORI","PARA CAMBIO MP[" + campo$ + "]")
                 !call TSCSEC08
                 exit function
              endif
              m2$ = m2$ + campo$
              y   = 1
           endif \
           else begin
             msg$ = "SPLIT NC Err(4)"
             call muestraBorrar("ERROR SPLIT NC",msg$)
             exit function
           endif
        endif \
        else begin
           !call traceNUO( "splitCambioPagosNC campo$ MONTO=[" + campo$ + "]" )
           if not esnumero( campo$ ) then begin
              msg$ = "SPLIT NC Err(5)"
              call muestraBorrar("ERROR SPLIT NC",msg$)
              exit function
           endif
           m3$ = m3$ + left$(campo$ + string$(7," "),7)
           mon.dev = mon.dev + val(campo$)
           y   = 0
           if k = len(m1$) then begin
              !call traceNUO( "splitCambioPagosNC SALI BIEN" )
              f = 0
           endif
        endif 
        x = k + 1
     endif     
   wend 

   !PAGO_AUTORIZ_CAMBIO_MP       11;21;22;45;13;14;15
   !IMPRESION_COMPROBANTE        11;01;01;01;11;11;11

   !11 3179
   !13 74
   !14 4 
   !15 43 
   !22 1000

   !m2$ = "4513141522"           ! no se imprime cheque 22
   !m2$ = "4513141521"           ! no se imprime cheque 21 ni el 14

   dim NCdevPagos(len(m2$)/2,4)
   NCdevPagos(0,0) = len(m2$)/2                           ! 
   NCdevPagos(0,1) = mon.dev                                        !

   for f = 1 to NCdevPagos(0,0)
      mp = val(mid$(m2$,f*2-1,2))
      NCdevPagos(f,1) = mp                                         ! Pago y variedad para cambio de medio de pago ej:11 Efectivo 21Cheque
      NCdevPagos(f,2) = val(elider$(mid$(m3$,f*7-6,7)))            ! Monto del pago para cambio de medio de pago
      a$ = NCTraeIndicadores$( mp )
      NCdevPagos(f,3) = val(left$(a$,1))                           ! Indicador s1 se imprime comprobante por si solo
      NCdevPagos(f,4) = val(right$(a$,1))                          ! Indicador si se imprime solo si hay efectivo entre los medios de pagos
   next f

   splitCambioPagosNC = -1

sale:
   exit function

err.loc:
   if errf% = 39 then resume
   !write #34;"Error " + err + " " + hexanum$(errn)
   resume sale

end function

function analizaRespuestaNC
   integer*1 analizaRespuestaNC,                     \
             NC.status,                               \
             NC.indicaConsumo                         !

   string    m1$,m2$

   analizaRespuestaNC = 0

   ! Ejemplo respuesta consumo autorizado, simple
   !
   ! 1        2    3      4     5   6              7   8   9
   ![00188P'|'68'|'0000'|'100'|'0'|'202111301533'|'0'|'1'|'OK]

   ! Ejemplo respuesta consumo autorizado, para cambio de MP con multiples MP
   !
   ! 1        2    3      4     5   6              7   8   9                      10
   !["00078P'|'68'|'0000'|'100'|'0'|'202112061533'|'0'|'1'|'OK'|'11 180 21 100 45 120"]

   ! Ejemplo respuesta consumo no autorizado
   !
   ! 1        2    3      4     5   6              7   8   9
   ![00078P'|'68'|'0000'|'100'|'0'|'202111301533'|'0'|'2'|'NOTA DE CREDITO NO ENCONTRADA]

   m1$ = buscaCampo( NCrespuesta$, separador,  7 )

   if not esnumero( m1$ ) or ( len(m1$) <> 1 ) or (match(m1$,"01",1) = 0 ) then begin
      call muestraBorrar("ERROR RESPUESTA","INCONSISTENTE(" + m1$ + ")" )
      call logCNB("Funcion analizaRespuestaNC ERROR Campo 7 Codigo respuesta comunicacion BD=[" + m1$ + "]" )
      exit function
   endif
   NC.status = val(m1$)                                       ! Indicador 0=Exitoso 1=Error de comunicacion

   if NC.status <> 0 then begin
      call muestraBorrar("ERROR D COMUNICACION","")
      call logCNB("Funcion analizaRespuestaNC ERROR Campo 7 [" + str$(NC.status) + "]" )
      exit function
   endif

   m1$ = buscaCampo( NCrespuesta$, separador,  8 )

   if not esnumero( m1$ ) or ( len(m1$) <> 1 ) or (match(m1$,"12",1) = 0 ) then begin
      call muestraBorrar("ERROR RESPUESTA","INCONSISTENTE(" + m1$ + ")" )
      call logCNB("Funcion analizaRespuestaNC ERROR Campo 8 Indicador de Consumo=[" + m1$ + "]" )
      exit function
   endif
   NC.indicaConsumo = val(m1$)                             ! Indicador Consumo autorizado = 1 Consumo NO autorizado = 2

   if NC.indicaConsumo = 2 then begin
      m1$ = buscaCampo( NCrespuesta$, separador,  9 ) + string$(20," ")
      m2$ = mid$(m1$,21,20)
      m1$ = left$(m1$,20)
      call muestraBorrar(m1$, m2$)
      call logCNB("Funcion analizaRespuestaNC WARNING " + m1$ + " " + m2$ )
      exit function
   endif \
   else begin
      if NCtipOperacion = 1 then begin                    !Indicador --> 1=Cambio Forma de Pago 2=Como medio de pago            

         NCcedulaRucCliente$ = ""
         NCNombreCliente$    = ""

         m1$ = buscaCampo( NCrespuesta$, separador, 10 )
         NCcedulaRucCliente$ = buscaCampo( NCrespuesta$, separador, 11 )
         NCNombreCliente$    = buscaCampo( NCrespuesta$, separador, 12 )
         if not splitCambioPagosNC( m1$ ) then \
            exit function
      endif 
   endif
   
   analizaRespuestaNC = -1

end function

sub inyectaPagoNC

   if NCanulaPago then TS.IO.KEYS(1) = 70
   NCesPagoNc = -1
   
   TS.IO.MOTORKEY = 95
   TS.IO.KEYS(3)  = 78
   TS.IO.DATA$(3) = str$(1)
   TS.IO.KEYS(7)  = 95
   TS.IO.DATA$(7) = str$(NCmontoConsumo)
   TS.IO.DATA$(9) = NCfolio$

end sub

!--------------------------------------------------------------------
!Genera y Graba una transaccion de tender cashing cambio de medio de pago
!se incrementa el valor de la variable SL.HD.TRANSNUM
!--------------------------------------------------------------------

sub grabaCambioMedioPago
   integer*1 numstr, f
   
   integer*2 MP.Tef              ! medio de pago SMA TEF
   
   string     mat1$(1),           \
              s3$                 !

   on error goto err.grabaCambioMedioPago

   SL.HD.TRANSNUM = SL.HD.TRANSNUM + 1
   numstr = NCdevPagos(0,0) + 1
   dim mat1$(numstr)

   mat1$(1) = chr$(00H)       + ":"   + TS.TERMINALP$                 + ":"   + \
              pack$( right$( "0000"   + str$( SL.HD.TRANSNUM ), 4 ) ) + ":"   + \
              pack$( date$    + left$( time$, 4 ) )                   + ":"   + \
              pack$("02")     + ":"   + pack$( str$( numstr ) )       + ":"   + \
              SL.HD.OPERATOR$ + ":"   + SL.HD.PASSWORD$               + ":::" + \
              pack$("108")    + ":::" + pack$("237")                  + ":"   + pack$("32")

   for f=1 to NCdevPagos(0,0)

      mat1$(1) = mat1$(1)                                                 + \
         chr$( 22H ) + chr$( 2CH ) + chr$( 22H )                      + \
         chr$( 6H ) + ":" + pack$(str$(NCdevPagos(f,1))) + ":" + pack$(str$(NCdevPagos(f,2))) + "::" + pack$(NCfolio$) + ":" + pack$("00")   !

   next f

   mat1$(1) = mat1$(1)                                                     + \
      chr$( 22H ) + chr$( 2CH ) + chr$( 22H )                      + \
      chr$( 5H ) + ":" + pack$(str$(NCmedioPago)) + ":" + pack$( str$(NCdevPagos(0,1)) ) + "::" + pack$(NCfolio$) + ":" + pack$("51")   !

   write matrix #27; mat1$(1), 1

   TS.TEMP1I2 = 5                        ! end of tnx
   CALL TSSFEC01                         ! update status

   fin.grabaCambioMedioPago:
   exit sub

   err.grabaCambioMedioPago:
   if errf% = 39 then resume
   if errf% <> 27 then resume
   call traceNUO( "grabaCambioMedioPago: ERROR: " + err + " " + hexanum$(errn) )
   resume fin.grabaCambioMedioPago

end sub

function get.desc.pago$( pagoSMA )
   integer*4 pagoSMA
   string get.desc.pago$, texto$
   
                        texto$ = "PAGO NO DEFINID"
   if pagoSMA = 11 then texto$ = "EFECTIVO       "
   if pagoSMA = 13 then texto$ = "RETENCION FUENT"
   if pagoSMA = 14 then texto$ = "RETENCION IVA  "
   if pagoSMA = 15 then texto$ = "RETENC.IVA 100%"
   if pagoSMA = 21 then texto$ = "CHEQUE SI      "
   if pagoSMA = 22 then texto$ = "CHEQ. PERSONAL "
   if pagoSMA = 31 then texto$ = "TARJETA DEBITO "
   if pagoSMA = 42 then texto$ = "TARJ.DEB.MANUAL"
   if pagoSMA = 43 then texto$ = "TARJ.CRE.MANUAL"
   if pagoSMA = 45 then texto$ = "TARJETA CREDITO"
   if pagoSMA = 53 then texto$ = "CRD.INST.ONLINE"
   if pagoSMA = 62 then texto$ = "DEVOLUCION IVA "
   if pagoSMA = 63 then texto$ = "TARJETA REGALO "
   if pagoSMA = 64 then texto$ = "TARJETA PREMIO "
   if pagoSMA = 65 then texto$ = "CANJE          "
   if pagoSMA = 66 then texto$ = "DEVOLUCIONES   "

   get.desc.pago$ = TEXTO$

end function

sub openCloseGabeta
   integer*1 f
   string m$

   m$ = formatea2Dec( NCmontoConsumo )
   call TSCSEC06                                                     ! Abre la gabeta
   call muestraBorrar("ENTREGUE AL CLIENTE","USD$ " + m$ )
   f = -1
   while( f )
      call TSHIEC02                                                   ! GET STATUS OF CASH DRAWER
      if TS.DEV.FLAG then begin
         call muestraBorrar( "CIERRE LA GABETA", "BORRAR CONTINUA" )
      endif else f = 0
   wend

end sub

function existePagoEfectivo
   integer*1 existePagoEfectivo,x

   existePagoEfectivo = 0
   for x = 1 to NCdevPagos(0,0)
      if NCdevPagos(x,1) = 11 then existePagoEfectivo = -1
   next x

end function

function existePagoCheque
   integer*1 existePagoCheque,x

   existePagoCheque = 0
   for x = 1 to NCdevPagos(0,0)
      if NCdevPagos(x,1) = 21 or \
         NCdevPagos(x,1) = 22 then existePagoCheque = -1
   next x

end function

function exitenPagosPorSiSolo
   integer*1 exitenPagosPorSiSolo, f

   exitenPagosPorSiSolo = 0

   for f = 1 to NCdevPagos(0,0)
      if NCdevPagos(f,3) = 1 then begin
         exitenPagosPorSiSolo = -1
         exit function
      endif
   next f

end function

sub imprimeComprobante
   integer*1 x, y
   string dato$

   NUOcmdImpresion$ = NUO.Centrado$
   call imprime34( NClogo1$ )
   NUOcmdImpresion$ = NUO.LetraJunta$ + NUO.NegritaA$
   call pideLineaCabecera( 1 , dato$ )
   call imprimeCJ( dato$ )
   NUOcmdImpresion$ = NUO.NegritaD$
   call pideLineaCabecera( 2 , dato$ )
   call imprimeCJ( dato$ )
   NUOcmdImpresion$ = NUO.NegritaA$
   call imprimeCJ( "   CONSTANCIA DEVOLUCION DE DINERO    ")
   NUOcmdImpresion$ = NUO.NegritaD$
   dato$ = left$("CED./RUC    : " + NCcedulaRucCliente$ + string$(38," "),38)
   call imprimeCJ( dato$ )
   dato$ = left$("CLIENTE     : " + NCNombreCliente$ + string$(38," "),38)
   call imprimeCJ( dato$ )
   dato$ = "NUMERO N.CREDITO    : " + NCfolio$  + " "
   call imprimeCJ( dato$ )
   dato$ = formatea2Dec( NCmontoConsumo ) + " USD"
   dato$ = "VALOR N.CREDITO     :" + dato$ + string$(38-(22 + len(dato$))," ")
   call imprimeCJ( dato$ )
   call imprimeCJ( "VALORES DEVUELTOS                     ")

   for x = 1 to NCdevPagos(0,0)
      if ( NCdevPagos( x,3 ) = 1 ) or \
         ( NCdevPagos( x,4 ) = 1 ) and ( existePagoEfectivo or existePagoCheque ) then begin
         dato$ = left$(" EN " + get.desc.pago$( NCdevPagos(x,1 )) + right$("        " + formatea2Dec(NCdevPagos(x,2)),8) + " USD " + string$(38," "),38)
         call imprimeCJ( dato$ )
      endif
   next x

   call imprimeCJ( "                                      ")
   call imprimeCJ( "                                      ")
   call imprimeCJ( "FIRMA    : _ _ _ _ _ _ _ _ _ _ _ _ _ _")
   call imprimeCJ( "TELEFONO : _ _ _ _ _ _ _ _ _ _ _ _ _ _")
   call imprimeCJ( string$(38," ") )
   dato$ = left$(" " + NClineTrailer$ + string$(38," "),38)
   call imprimeCJ( dato$ )
   call imprimeCJ( string$(38," ") )
   call imprimeCJ( string$(38," ") )
   call imprimeCJ( NUO.CortaPapel$ )

   if JAVA.INIT = -1 then begin
      jGuiSubState = 80600
      call javaEvent(terminalSubStateMsg)
   endif

end sub

sub MSMTNCRE02 public
   NCinTrx = 0
end sub

sub NCparceaparametros(  IndicPar$ )                                                   ! par$ = "11;01;01;01;11;11;11;"
   integer*1 x
   string IndicPar$

   dim NCIndicComprobante$( len( IndicPar$ )/3,2 )
   NCIndicComprobante$( 0,0 ) = str$( len( IndicPar$ )/3)
   for x = 1 to len( IndicPar$ )/3
      NCIndicComprobante$( x,1) = mid$(NCpagosAutorizCambioMP$,x*3-2,2)          ! NCpagosAutorizCambioMP$ = "11;21;22;45;13;14;15;"
      NCIndicComprobante$( x,2) = mid$(IndicPar$,x*3-2,2)                        ! 
   next x

end sub

sub MSMTNCRE07 public
   integer*1 k
   integer*4 inicioBloque
   string    v$, NCinicadoresPagos$

   fechaModificacion  = " 26 Abr 2022"
   lf$                = chr$(10)
   NCoperation        = 68
   separador          = "'|'"
   estado             = 0
   depurar            = 0
   dummy              = 0
   pipeTimeout        = 5000                     ! valor por defecto
   NCmedioPago        = 51                       ! Pago y variedad de pago para el pago Nota de Credito
   NCmaxCanTrx        = 5
   NCteclaMotorCambMP = 0
   NCinTrx            = 0
   NCpagosAutorizCambioMP$ = ""                  ! Medios de pagos SMA permitido para cambio de MP EJ:11;21;54
   NClogo1$           = chr$(1DH) + chr$(2FH) + chr$(00H) + chr$(01H)  ! Logo 1
   
   call imprimeCJ( "***** MODULO NOTA DE CREDITO *********" )

   inicioBloque = abreParametros( "R::$ARGENER", "[NOTA DE CREDITO]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE NC", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO" + fechaModificacion )
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

      !tiempo de espera respuesta
      v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
      if v$ <> "" then begin
         if esNumero( v$ ) then pipeTimeout = val( v$ )
      endif

      !Maxima cantidad de NC permitidas en una TRX como MP
      v$ = leeParametro$( "MAX_NC_TRX", 80, inicioBloque )
      if v$ <> "" then begin
         if esNumero( v$ ) then NCmaxCanTrx = val( v$ )
      endif
      dim NCfoliosIntrx$(NCmaxCanTrx,2)

      !tecla motora para cambio de forma de pago
      v$ = leeParametro$( "TECLA_MOTORA_CAMB_MP", 80, inicioBloque )
      if v$ <> "" then begin
         if esNumero( v$ ) then NCteclaMotorCambMP = val( v$ )
      endif

      !Pago supermarket para el pago Nota de Credito
      v$ = leeParametro$( "MEDIO_PAGO_NC", 80, inicioBloque )
      if v$ <> "" then begin
         if esNumero( v$ ) then NCmedioPago = val( v$ )
      endif

      !Medios de pagos permitidos para cambio de MP
      v$ = leeParametro$( "PAGO_AUTORIZ_CAMBIO_MP", 80, inicioBloque )
      if v$ <> "" then \
         NCpagosAutorizCambioMP$ = v$ + ";"

      !Condiciones parea imprimir comprobante por cambio de MP
      v$ = leeParametro$( "IMPRESION_COMPROBANTE", 80, inicioBloque )
      if v$ <> "" then begin
         NCinicadoresPagos$ = v$ + ";"
         call NCparceaParametros( NCinicadoresPagos$ )
      endif
      call cierraParametros( 80 )

      if estado then v$ = "ON " else v$ = "OFF"
      call imprimeCJ( "ESTADO " + v$ + fechaModificacion )
      if depurar then begin
         call imprimeCJ( "TIEMPO DE ESPERA  " + right$( string$( 10, " " ) + str$( pipeTimeout ), 20 ) )
         call imprimeCJ( "MAX. N.C. EN TRX  " + right$( string$( 10, " " ) + str$( NCmaxCanTrx ), 20 ) )
         call imprimeCJ( "TECLA_CAMB_MP     " + right$( string$( 10, " " ) + str$( NCteclaMotorCambMP ), 20) )
         call imprimeCJ( "MEDIO DE PAGO NC  " + right$( string$( 10, " " ) + str$( NCmedioPago ), 20 ) )
         call imprimeCJ( "MP CAMBIO MP      " + right$( string$( 10, " " ) + NCpagosAutorizCambioMP$, 20 ) )
         call imprimeCJ( "IMPRME_COMPROBANT " + right$( string$( 10, " " ) + NCinicadoresPagos$, 20 ) )
         if dummy then call imprimeCJ( "ACTIVADA DUMMY" )
      endif
   endif

end sub

sub MSMTNCRE14 public
   string msg$

   !Impedir que ingrese por teclado la forma de pago devoluciones (6 / monto BONO) 
   if ( TS.IO.STATE = 10 ) and ( TS.IO.MOTORKEY = 96 ) and \
      ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "6" ) and \
      ( TS.IO.KEYS(7)  = 96 ) then begin
      TS.GUIDANCE = 1034 !COMPRUEBE VARIEDAD FORMA PAGO
      TS.IO.MOTORKEY = 0
      exit sub
   endif

   if not estado then exit sub
   if esCambioMedioPago          then begin
      if requiereCambioCaja      then exit sub
      if not pideFolioNC         then goto sale
      if not pideMontoNC         then goto sale
      msg$ = generaMensajeNC
      if not enviaMsjNC(msg$)    then goto sale
      if not esperaRespuestaNC   then goto sale
      if not recibeRespuestaNC   then goto sale
      if not analizaRespuestaNC  then goto sale

      if not existePagosTEF then goto sale
      if exitenPagosPorSiSolo then call imprimeComprobante
      call grabaCambioMedioPago
      call openCloseGabeta
      call ignoraTeclas
   endif else begin
      if not esSecuenciaTeclaPagoNC then exit sub
      if not NCanulaPago           then begin
         if not totalReady         then goto sale
         if not pideFolioNC        then goto sale
         msg$ = generaMensajeNC
         if not enviaMsjNC(msg$)   then goto sale
         if not esperaRespuestaNC  then goto sale
         if not recibeRespuestaNC  then goto sale
         if not analizaRespuestaNC then goto sale
      endif \
      else begin
         if not pideFolioNC then goto sale
         call muestraOperador( "ANULANDO NUMERO NC", NCfolio$ )
         wait;2000
      endif
      NCesPagoNc = -1
      call inyectaPagoNC
      exit sub
   endif

   sale:
   call ignoraTeclas
end sub

sub MSMTNCRE22 public
   integer*1 x
   string f$
   if not estado then exit sub

   if ( TS.LINETYPE = 9 ) and ( TS.LINEDATA = 3 ) then begin
      if JAVA.INIT = -1 AND TS.IO.MOTORKEY = 81 and ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "1" ) then begin
         TS.IO.MOTORKEY = 95
      endif
      if ( TS.IO.MOTORKEY = 95 ) and ( TS.IO.KEYS(3) = 78 ) and ( TS.IO.DATA$(3) = "1" ) then begin
         x = match("Valor Compra       :",TS.PRTBUF$,1)
         if x = 0 then x = match("Factura Nro        :",TS.PRTBUF$,1)
         if x <> 0 then begin
            f$ = "          Factura Nro        :" + right$(TS.STORE$,3) + "-" + TS.TERMINAL$ + "-" + right$("00000000" + str$(MSMTFACTnumeroFactura + 1) ,8) + "   "
            TS.PRTBUF$ = f$
         endif
      endif
   endif
   
end sub

SUB MSMTNCRE30 PUBLIC
   integer*1 x
   string aux$, net$, iva$, exc$
   real   r

   if not estado then exit sub
   
   if NCesPagoNc then begin
      NCesPagoNc = 0
      if NCanulaPago then NCfoliosIntrx$(NCindiceAnula,1) = "" \
      else begin
         NCinTrx = NCinTrx + 1
         NCfoliosIntrx$(0,0) = str$(NCinTrx)
         NCfoliosIntrx$(NCinTrx,1) = NCfolio$
         NCfoliosIntrx$(NCinTrx,2) = str$(NCmontoConsumo)
      endif
     
   endif

END SUB

!ACTUALIZA EL TOTAL DESPUES DE LA INSERCION DE UN DOCUMENT
SUB MSMTNCRE62 PUBLIC
   if TS.BALDUE(0) > 0 then begin
      if JAVA.INIT = -1 then begin
         jGuiSubState = 80539
         call javaEvent(terminalSubStateMsg) 
      endif
   endif
END SUB

SUB MSMTNCRE53 PUBLIC

   if not estado then exit sub

   ! Detectar cadena 05 y 06 pago y anulacion de pago de NC para eliminarlos
   if not EsCadenaPago( SL.STR.ENTRY$ )    then exit sub
   if NUOuserData$(2) <> str$(NCmedioPago) then exit sub
   TS.TEMP1I2 = 0

END SUB