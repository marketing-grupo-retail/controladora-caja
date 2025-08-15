!-----------------------------------------------------------------------------
! Modulo: NUOTELEC.BAS
! Fecha : 11 de diciembre de 2020
! Autor : Alejandro Farre P.
! Modulo para informacion electoral
!
! Fecha      : 17 de junio de 2021
! Autor      : Alexander Baidal M.
! Cambio     : Se integra funcionalidad a nueva aplicacion Base NUO
!------------------------------------------------------------------------------
%ENVIRON T

!variables del modulo
integer*1 estado,            \indicador estado de funcionalidad 0 -> desactivado
          depurar            !indicador para depurar el programa

string    lf$,               \salto de linea chr$(10)
          bodyFile$,         \archivo con cuerpo del mensaje de respuesta
          separador          !separador de campos agente "'|'"

!variables para las tramas de comunicación
string    elec.estado,         \estado valores: 1 cuando existe informacion, 0 en otro caso
          elec.cedula,         \cedula de cliente
          elec.nombre,         \nombre del cliente
          elec.provincia,      \provincia de sufragio
          elec.canton,         \canton de sufragio
          elec.parroquia,      \parroquia de sufragio
          elec.zona,           \zona de sufragio
          elec.recinto,        \recinto de sufragio
          elec.junta,          \junta de sufragio
          elec.sexo,           \junta masculino femenino de sufragio
          elec.miembroJrv,     \miembro junta receptora de voto
          elec.recintoJrv,     \recinto junta receptora de voto
          elec.funcionJrv      !funcion junta receptora de voto

integer*4 PipeTimeout    !Tiempo de espera

!------------------------------------------------------------------
! Rutinas de otros modulos
!------------------------------------------------------------------
%INCLUDE EAMTSWKG.J86
%INCLUDE NUOTVARI.J86
%INCLUDE NUOTRUTI.J86

!------------------------------------------------------------------
! graba log "R::Q:/" o "R::C:/DQ/" + "TRC." + TS.TERMINAL$
!------------------------------------------------------------------
sub logELEC( dato$ )
   string dato$
   if depurar then begin
      call traceNuo( dato$ )
   endif
end sub

!-------------------------------------------------------------------------------
! Imprime en lineas de 42 caracteres
!-------------------------------------------------------------------------------
sub imprime42( b$ )
   string b$, s$, e$
   integer*2 i, n, k

   k = 42
   e$ = string$( k, " " )
   n = len(b$) + 1
   i = 1
   while i < n
      s$ = left$( mid$( b$, i, k ) + e$, k )
      write #34; s$ + lf$
      i = i + k
   wend
end sub

!-----------------------------------------------------------------------------
! Lee archivo que contiene el cuerpo del mensaje
!-----------------------------------------------------------------------------
function leeBody
   string leeBody, a$, b$, f$
   !integer*4 t

   leeBody = ""
   close 47
   a$ = bodyFile$
   b$ = ""

   TS.ER.RETURN = -1
   open a$ as 47
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "No se puede abrir", "Archivo de respuesta" )
      exit function
   endif

   !informacion del archivo debe contener caracter de fin de linea

   !para evitar errores cuando vienen comas usar read # form
   !t = size( a$ )

   read #47; b$

   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "No se puede leer", "Archivo de respuesta" )
      delete 47
      exit function
   endif

   if len( b$ ) = 0 then begin
      call muestraBorrar( "Archivo de respuesta", "sin informacion.." )
      delete 47
      exit function
   endif

   leeBody = b$
   delete 47

end function

!------------------------------------------------------------------------
! recibe un mensaje del PIPE
!------------------------------------------------------------------------
function recibeRespuesta
   string recibeRespuesta, a$, f$

   recibeRespuesta = ""
   a$ = recibeMensaje("servicio electoral..") 

      if buscaCampo(a$, separador, 7) = "1" then begin
      !call logELEC( "recibeRespuesta: [" + a$ + "] Error de comunicacion" )
         call restauraVisor(1)
      call muestraBorrar( "Error de" , "comunicacion" )
         exit function
      endif

      f$ = leeBody
      a$ = a$ + separador + f$

   !call logELEC( "recibeRespuesta: [" + a$ + "]" )

      if f$ = "" then begin
         call restauraVisor(1)
         exit function
      endif

   recibeRespuesta = a$

   call restauraVisor(1)

end function

!------------------------------------------------------------------------
!Inicia variables
!------------------------------------------------------------------------
sub iniciaVariables
   elec.estado       = "0"
   elec.cedula       = ""
   elec.nombre       = ""
   elec.provincia    = ""
   elec.canton       = ""
   elec.parroquia    = ""
   elec.zona         = ""
   elec.recinto      = ""
   elec.junta        = ""
   elec.sexo         = ""
   elec.miembroJrv   = ""
   elec.recintoJrv   = ""
   elec.funcionJrv   = ""
end sub

!------------------------------------------------------------------------
! Genera cuerpo del mensaje
!------------------------------------------------------------------------
function generaMensaje
   string generaMensaje

   generaMensaje = "P" + separador +     \ canal de comunicacion S=socket P=Pipe
      "64" + separador +                 \ requerimiento mensaje por pipe para MSM
      "0000" + separador +               \ canal de respuesta
      TS.TERMINAL$ + separador +         \ terminal de origen
      "0" + separador +                  \ tipo de conexion 0=temporal 1=permanente
      "20" + date$ + time$ + separador + \ fecha y hora de inicio de la operacion
      elec.cedula

end function

!------------------------------------------------------------------------
!Asigna valor de la respuesta a las variables elec.xxx
!------------------------------------------------------------------------
sub leeRespuesta( m$ )
   string    m$

   elec.estado = buscaCampo( m$, separador, 8 )

   if elec.estado = "0" then exit sub

   elec.nombre       = buscaCampo( m$, separador, 10 )
   elec.provincia    = buscaCampo( m$, separador, 11 )
   elec.canton       = buscaCampo( m$, separador, 12 )
   elec.parroquia    = buscaCampo( m$, separador, 13 )
   elec.zona         = buscaCampo( m$, separador, 14 )
   elec.recinto      = buscaCampo( m$, separador, 15 )
   elec.junta        = buscaCampo( m$, separador, 16 )
   elec.sexo         = buscaCampo( m$, separador, 17 )
   elec.miembroJrv   = buscaCampo( m$, separador, 18 )
   elec.recintoJrv   = buscaCampo( m$, separador, 19 )
   elec.funcionJrv   = buscaCampo( m$, separador, 20 )

end sub

!------------------------------------------------------------------------
!Lee parametros del archivo ASCNTRL
!incorporar a USER 7
!------------------------------------------------------------------------
sub leeParametros
   string    r
   integer*1 k
   string    n$, v$
   integer*4 inicioBloque

   separador = "'|'"
   lf$ = chr$(10)
   PipeTimeout = 15000
   estado = 0
   depurar = 0
   bodyFile$ = "R::C:/CNB/ELEC" + TS.TERMINAL$ + ".DAT"

   call iniciaVariables

   call imprimeCJ("**** MODULO INFORMACION ELECTORAL ****")
   inicioBloque = abreParametros( "R::$ARGENER", "[INFORMACION ELECTORAL]", 80 )
   if inicioBloque = -1 then begin
      call muestraBorrar( "NO EXISTE ELECTORAL", "EN ARCHIVO PARGENER" )
      call imprimeCJ( "NO CONFIGURADO  3 Dic 2022" )
      call cierraParametros( 80 )
      exit sub
   endif

   v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
   if v$ <> "" then begin

      !Proyecto activo 0 No, 1 Si
      v$ = leeParametro$( "PROYECTO_ACTIVO", 80, inicioBloque )
      if v$ <> "" then begin
         if esNumero( v$ ) then k = val( v$ ) else k = 0
         if ( k and 1 ) = 1 then estado = -1
         if ( k and 2 ) = 2 then depurar = -1

         !tiempo de espera respuesta servicio electoral
         v$ = leeParametro$( "TIEMPO_ESPERA", 80, inicioBloque )
         if esNumero( v$ ) then PipeTimeout = val( v$ )

      endif
   endif

   call cierraParametros( 80 )

   if estado then r = "ON " else r = "OFF"
   call imprimeCJ( "ESTADO " + r + "  16 Dic 2020" )
   if estado then begin
      call imprimeCJ( "TIEMPO DE ESPERA" + right$( string$( 10, " " ) + str$( pipeTimeout ), 10 ) )
      if depurar then call imprimeCJ("ACTIVADA DEPURACION")
   endif

end sub

!-----------------------------------------------------------------------------
! Elimina archivo de respuesta
!-----------------------------------------------------------------------------
sub deleteBody
   close 47
   TS.ER.RETURN = -1
   open bodyFile$ as 47
   if TS.ER.RETURN <> -1 then exit sub
   delete 47
end sub

!------------------------------------------------------------------------
! Consulta Informacion electoral
!------------------------------------------------------------------------
sub NUOTELEC.consulta( cedula$ ) public
   string cedula$, m$, s$

   if not estado then exit sub

   !Inicia variables de consultar informacion electoral
   call iniciaVariables

   !No genera informacion electoral para consumidor final ni empresas
   if len( cedula$ ) <> 10 or cedula$ = "9999999999" then exit sub

   call deleteBody

   elec.cedula = cedula$
   m$ = generaMensaje

   !para evitar que existan mensajes antiguos
   call limpiaPipe

   !envia mensaje
   if not enviaMensaje(m$) then exit sub

   !Espera que lleguen datos
   if not esperaPipe( PipeTimeout, "Cons Electoral" ) then begin
      !call muestraBorrar( "No se recibe" , "Cons Electoral" )
      exit sub
   endif

   !lee respuesta
   m$ = recibeRespuesta

   !Si hubo error de comunicacion
   if m$ = "" then exit sub

   call leeRespuesta( m$ )

end sub

!--------------------------------------------------------------------------
! Imprime informacion electoral
!--------------------------------------------------------------------------
sub NUOTELEC.imprime public
   string s$, b$, n$
   integer*2  k

   if not estado then exit sub

   if elec.estado = "0" then exit sub

   s$ = string$( 9, "-" )
   write #34; lf$
   call imprime42( s$ + " INFORMACION  ELECTORAL " + s$ )

   !activa des-activa negrita
   b$ = chr$(1Bh) + chr$(47H) + chr$(1)
   n$ = chr$(1Bh) + chr$(47H) + chr$(0)
   s$ = string$( 8, " " )
   k = 26 - len( elec.cedula )
   write #34; lf$ + b$ + "Identificacion: " + n$ + elec.cedula + string$( k, " " )
   write #34; lf$ + b$ + "Nombre:" + n$ + string$( 35, " " ) + lf$
   call imprime42( elec.nombre )
   write #34; lf$ + b$ + "Provincia:" + n$ + string$( 32, " " ) + lf$
   call imprime42( elec.provincia )
   write #34; b$ + "Canton:" + n$ + string$( 35, " " ) + lf$
   call imprime42( elec.canton )
   write #34; b$ + "Parroquia:" + n$ + string$( 32, " " ) + lf$
   call imprime42( elec.parroquia )  
   !write #34; b$ + "Fecha nacimiento:" + n$ + string$( 25, " " ) + lf$
   write #34; b$ + "Zona:" + n$ + string$(37, " ") + lf$
   call imprime42( elec.zona )
   write #34; b$ + "Recinto:" + n$ + string$( 34, " " ) + lf$
   call imprime42( elec.recinto )
   k = 14 - len( elec.sexo )
   write #34; lf$ + b$ + "Junta:" + n$ + right$( s$ + elec.junta, 8 ) + s$ + \
                    b$ + "Sexo: " + n$ + elec.sexo + string$( k, " " ) + lf$
   k = 24 - len( elec.miembroJrv )
   write #34; lf$ + b$ + "Miembro de Junta: " + elec.miembroJrv + n$ + string$( k, " " ) + lf$

!   s$ = left$( elec.recintoJrv, 28 )
!   k = 28 - len( s$ )
!   write #34; lf$ + b$ + "PRIMERA DOSIS:" + s$ + n$ + string$( k, " " ) + lf$

!   s$ = left$( elider$( elec.funcionJrv ), 42 )
!   if len( s$ ) = 0 then s$ = "SEGUNDA DOSIS:PENDIENTE ASIGNACION"
!   k = 42 - len( s$ )
!   write #34; b$ + s$ + n$ + string$( k, " " ) + lf$

   if elec.miembroJrv = "SI" then begin
      call imprime42( elec.recintoJrv )
      write #34; lf$
      call imprime42( elec.funcionJrv )
   endif

   s$ = string$( 42, "-" )
   call imprime42( s$ )

   call iniciaVariables

end sub

!------------------------------------------------------
! Rutina debe llamarse en user 07
!------------------------------------------------------
sub NUOTELEC07 public
    call leeParametros
end sub