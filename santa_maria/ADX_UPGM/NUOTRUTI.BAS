!------------------------------------------------------------------------------
!Rutinas de uso general
!Fecha: 23 de marzo de 2023
!Autor: Alejandro Farre P.
!------------------------------------------------------------------------------
%ENVIRON T

integer*1 existeArchivoTrace,    \ indicador exite archivo tracetrace
          teclaAnular,           \ se usa la tecla ANULAR para salir del ingreso de texto
          llavePuesta            ! indicador llave esta puesta
integer*2 variableImpresionE(1)  ! para almacenar las variables de impresion enteras
string    variableImpresion$(1), \ para almacenar las variables de impresion cadenas
          mapParametro$(2),      \ para cargar todos los parametros de un grupo
          codigosTeclado$,       \ lista de codigos del teclado del archivo ALFANUM.DAT
          archivoTraceNUO$       ! Nombre completo del archivo trace

! Para la comunicacion con el agente
string                 \
   PipeId$,            \
   PipeDest$

integer*2              \
   PipeSess

integer*4              \
   PipeWrite

! Codigo de error para un pi:
string \
   codErrorPipe

!Para guardar variables del diplay
integer*2 variablesDisplay(1)

!Para guardar variables del IO
string    varDATA$(1), varPrevDATA$(1)
integer*2 varKEYS(1), varPrevKEYS(1), \
          varMOTORKEY,                \ LAST MOTOR KEY USED
          varSTATE,                   \ CURRENT STATE OF IO PROCESSOR
          varNEXTSTATE,               \ NEXT STATE REQD - SET BY INDIVIDUAL APPL
          varPrevMOTORKEY,            \ PREV MOTOR KEY USED
          varDEVICE                   ! ORIGINATING DEVICE

Integer*2 Global GrtraceOn%            ! Grabacion trace de rastreo aplicacion

!variables globales de uso general
%INCLUDE NUOTVARI.J86

!Variables globales del supermarket
%INCLUDE EAMSOPTS.J86
%INCLUDE EAMTSWKG.J86
%INCLUDE JAVAGUIV.J86
%INCLUDE EAMTOPTS.J86
!%INCLUDE EAMP4VAJ.J86 ! TIPO DE IMPRESORA
%INCLUDE EAMTRANS.J86
%INCLUDE EAMTERMS.J86
!%INCLUDE QXLGVART.J86

INTEGER*2 GLOBAL       \
   SAVE.SL.END,        \ save last index
   SL.END              ! last index to be used in summary log

INTEGER*2 GLOBAL               \
                               \
          OA.INDICAT0,         \ BIT 0 - CHECKOUT TRANSACTION ALLOWED
                               \ BIT 1 - TENDER CASHING ALLOWED
                               \ BIT 2 - TENDER EXCHANGE ALLOWED
                               \ BIT 3 - LOAN TRANSACTION ALLOWED
                               \ BIT 4 - PICKUP TRANSACTION ALLOWED
                               \ BIT 5 - TENDER LISTING ALLOWED
                               \ BIT 6 - PRICE VERIFICATION ALLOWED
                               \ BIT 7 - OPERATOR TRAINING ALLOWED
                               \ BIT 8 - TERMINAL TRANSFER ALLOWED
                               \ BIT 9 - TERMINAL MONITER ALLOWED
                               \ BIT10 - TENDER COUNT ALLOWED
                               \ BIT11 - 13 RESERVED
                               \ BIT14 - USER NON-SALES 1 ALLOWED
                               \ BIT15 - USER NON-SALES 2 ALLOWED
                               \
          OA.INDICAT1          ! BIT 0 - NO-SALE OPEN CASH DRAWER ALLOWED
                               \ BIT 1 - NO-SALE TENDER REMOVAL ALLOWED
                               \ BIT 2 - NO-SALE TILL EXCHANGE ALLOWED
                               \ BIT 3 - NO SALE TENDER VERIFY ALLOWED
                               \ BIT 4 - NO-SALE TILL REPORT ALLOWED
                               \ BIT 5 - REFUNDS ALLOWED
                               \ BIT 6 - DISCOUNTS ALLOWED
                               \ BIT 7 - MISC. ITEM PAYOUTS ALLOWED
                               \ BIT 8 - IMMEDIATE PRICE CHANGES ALLOWED
                               \ BIT 9 - DELAYED PRICE CHANGES ALLOWED
                               \ BIT 10-13 RESERVED
                               \ BIT14 - USER FUNCTION 1 ALLOWED
                               ! BIT15 - USER FUNCTION 2 ALLOWED

INTEGER*4 GLOBAL SL.LEN.TOTAL      ! total length of the tlog string

%INCLUDE JAVAGUIC.J86
! system services routines
%INCLUDE EAMADXRT.J86

! ASSEMBLER SUBROUTNE
%INCLUDE EAMASMCT.J86

%INCLUDE JAVA_EXT.J86

!------------------------------------------------------------------------------
! listing terminal RAM disk files (X: Y: C:)
!------------------------------------------------------------------------------
SUB ADXDIR( RETC, DISKID, OUTINFO, FILEINDX ) EXTERNAL
   INTEGER*4 RETC
   STRING DISKID, OUTINFO
   INTEGER*2 FILEINDX
END SUB

!para grabar en el journal electronico
FUNCTION QXL.TSUPEC21(PRTLINE) EXTERNAL
   STRING QXL.TSUPEC21, PRTLINE
FEND

!Lee TSL
FUNCTION READ.SL.STR$(INDEX) EXTERNAL
STRING READ.SL.STR$
INTEGER*2 INDEX
END FUNCTION

!To print data on one or more of the terminal printers.
SUB TSPREC01 EXTERNAL
END SUB

!STATUS FILE UPDATING, UPDATE OPERATOR/TERMINAL TOTALS
!will attempt to write the summary log.
SUB TSSFEC01 EXTERNAL
END SUB

! TSDSEC01 display routine
! Purpose : To display data on the terminal display
!      TS.LINETYPE controls the type of display required. This
!      variable must be set. The value it is set to determines
!      which other variables need specifying. The linetype
!      specific documentation which follows lists all these
!      other variables. These other variables MUST be set for
!      the display to function correctly.
!
!      Values of this parameter have the following meaning:
!
!        0:  invalid
!        1:  item entry
!        2:  tender  (chg/bal, f.s. chg, fee, loan/pkp)
!        3:  balance due
!        4:  f.s. balance due (total loan/pkp)
!        5:  void transaction
!        6:  sales prompt
!        7:  non-sale prompt
!        8:  error guidance (with item code,etc)
!        9:  post error guidance prompt
!       10:  overrideable error guidance
!       11:  item verification (price change/verify)
!       12:  display buffers as passed (e.g.terminal monitor)
!       13:  display feature descriptor IR76369
!     > 13:  invalid
!
!      Assumptions
!
!      - types of display will only be specified for functions
!        that are permitted/used in the system (e.g. a system not
!        using food stamps will not attempt to display type 4)
!
!      The following variables are reset on exit from this routine
!      and need to be saved if this routine is called from a
!      user exit:
!          TS.LINETYPE        2 byte integer
!          TS.LINEDATA        2 byte integer
!          TS.LINEDATA2       2 byte integer
!          TS.LINEDATA3       2 byte integer
!          TS.DS.NOSAVE       2 byte integer
!          TS.XXMOD           2 byte integer
!          TS.YYMOD           2 byte integer
!
SUB TSDSEC01 EXTERNAL
END SUB

! display guidance and wait for clear
SUB TSCSEC08 EXTERNAL
END SUB

!******************* TSCSECRK *********************************
! This routine reads the I/O processor input and puts the data
! into the arrays.
!
! The following fields are set up:
!
! TS.IO.KEYS
! TS.IO.DATA
!    These fields contain the data that were entered from the
!    keyboard and/or scanners.
!
! TS.GUIDANCE
!    This field will be set to the appropriate error number if
!    an error occurred.  Otherwise it will be set to zero.
!
! TS.IO.MOTORKEY
! TS.IO.DEVICE
!    These fields are set to the appropriate values returned
!    by the input analyser.
!
!    Note that if TS.IO.MOTORKEY returned as zero then mainline
!    will ignore the entry.
!
!******************** B B D L *********************************
!
! Read the I/O processor
! CALL TSUPEC14 user exit after read
!
!**************************************************************
SUB TSCSECRK EXTERNAL
END SUB

!------------------------------------------------------------------------------
! routine to que the OS's timedate table Choices:
! 1 - millisec's since midnight
! 2 - minutes from UCT (timezone)
! 3 - day of week 0-Sunday 6-Saturday
!------------------------------------------------------------------------------
function timedate(choice) external
   integer*4   timedate
   integer*2   choice
end function

!------------------------------------------------------------------------------
!Crea un PIPE para recibir datos por medio del Pipe Routing Services
!Pipe.sess  Un numero de sesion
!Pipe.size  El numero de byte que puede alojar el PIPE
!Pipe.id$   Letra del PIPE a crear
!Retorna:
!0 Good completion
!-1000 Pipe no valido
!-1001 Pipe ya existe
!------------------------------------------------------------------------------
function prstcrt( Pipe.sess, Pipe.size, Pipe.id$ ) external
   integer*2 prstcrt
   integer*2 Pipe.sess, Pipe.size
   string Pipe.id$
end function

!------------------------------------------------------------------------------
!Inicializa el servicio de escritura de Pipe Routing Services
!retorna un entero de 4 bytes que identifica el PRS
!------------------------------------------------------------------------------
function prstint external
   integer*4 prstint
end function

!------------------------------------------------------------------------------
!Escribir por medio de PRS a un controlador u otro terminal
!Pipe.write Contiene el numero retornado por la inicializacion PRS
!Pipe.dest$ Contiene el PIPE de destino de la forma aaaw.
!           si escribe a otro terminal aaa contiene el numero de terminal
!           si escribe a un controlador aaa contiene 0xy
!           xy id del nodo CC a ZZ, AA para el master, BB para el
!           controlador al que esta conectada la caja
!msg$  Contiene los datos a enviar. El largo maximo que el terminal
!      puede escribir a otro terminal es 120 bytes y a un controlador
!      es 512 bytes.
!retorna
!0 Good completion.
!-1 pipe esta lleno o no hay espacio para almacenar los datos escritos.
!-1010 PIPE de destino no valido.
!-1011 PIPE de destino no encontrado.
!-1012 Error al escribir al PIPE de destino.
!-1013 Largo de datos mayor al maximo permitido.
!-1014 Buffer no es valido.
!------------------------------------------------------------------------------
function prstwrc( Pipe.write, Pipe.dest$, msg$ ) external
   integer*4 prstwrc
   integer*4 Pipe.write
   string Pipe.dest$, msg$
end function

!------------------------------------------------------------------------------
!Convierte un numero de 4 bytes a caracteres hexadecimales
!------------------------------------------------------------------------------
! Retorna una cadena de 8 caracteres
function hexanum$( numero ) public
   string    hexanum$
   integer*4 numero

   string s$
   integer*2 i, k

   s$ = ""
   for i = 0 to 7
      k = shift(numero, i*4) and 000FH
      if k < 10 then \
         k = k + 48  \
      else           \
         k = k + 55
      s$ = chr$(k) + s$
   next i
   hexanum$ = s$
fend

!------------------------------------------------------------------------------
! Convierte una cadena de caracteres hexadecimales a un entero de 4 bytes
! Retorna un entero de 4 bytes.
!------------------------------------------------------------------------------
function numhexa( numhex$ ) public
   STRING    cadena$
   INTEGER*4 numhexa
   INTEGER*1 i, k, n
   INTEGER*4 v

   n = len( cadena$ )
   if n > 8 then n = 8
   v = 0
   for i = 1 to n
      k = val( mid$( cadena$, i, 1 ) )
      if k > 64 and k < 71 then k = k + 9
      v = 16 * v + ( k and 0FH )
   next i
   numhexa = v
fend

!------------------------------------------------------------------------------
!retorna una linea en caracteres hex
!------------------------------------------------------------------------------
function lineahex$( cadena$ ) public
   string lineahex$, cadena$, h$, t$
   integer*2 i, n
   integer*2 c, d

   h$ = ""
   t$ = ""
   n = len( cadena$ ) + 1
   i = 1
   while i < n
      c = asc( mid$( cadena$, i, 1 ) )
      if c < 32 or c > 127 then t$ = t$ + "." else t$ = t$ + chr$( c )
      d = shift( c, 4 )
      if d > 9 then d = d + 55 else d = d + 48
      h$ = h$ + chr$( d )
      d = ( c and 0FH )
      if d > 9 then d = d + 55 else d = d + 48
      h$ = h$ + chr$( d ) + " "
      i = i + 1
   wend
   lineahex$ = h$ + " [" + t$ + "]"
fend

!------------------------------------------------------------------------------
!retorna una cadena en caracteres hex
!------------------------------------------------------------------------------
function cadenahex$( cadena$ ) public
   string cadenahex$, cadena$, s$, h$, t$
   integer*2 i, n
   integer*2 c, d

   s$ = ""
   h$ = ""
   t$ = ""
   n = len( cadena$ ) + 1
   i = 1
   while i < n
      c = asc( mid$( cadena$, i, 1 ) )
      if c < 32 or c > 127 then t$ = t$ + "." else t$ = t$ + chr$( c )
      d = shift( c, 4 )
      if d > 9 then d = d + 55 else d = d + 48
      h$ = h$ + chr$( d )
      d = ( c and 0FH )
      if d > 9 then d = d + 55 else d = d + 48
      h$ = h$ + chr$( d ) + " "
      if mod( i, 16 ) = 0 then begin
         s$ = s$ + "   " + h$ + " " + t$ + crlf$
         h$ = ""
         t$ = ""
      endif
      i = i + 1
   wend
   n = len( t$ )
   if n > 0 then s$ = s$ + "   " + h$ + string$( 49 - 3 * n, " " ) + t$ + crlf$
   cadenahex$ = s$
fend

!------------------------------------------------------------------------------
! Funcion que retorna que se requiere cambio de caja
!------------------------------------------------------------------------------
function requiereCambioCaja public
   integer*1 requiereCambioCaja

   requiereCambioCaja = 0

   if ( TE.TR.STATUS and 1000H ) = 1000H then begin
      TS.GUIDANCE = 1085   !REQUIERE CAMBIO DE CAJA
      TS.IO.MOTORKEY = 0
      requiereCambioCaja = -1
   endif

end function

!------------------------------------------------------------------------------
! Corta el mensaje en dos lineas para el display
! msg1$ primeros 20 caracteres
! msg2$ segundos 20 caracteres
!------------------------------------------------------------------------------
sub separaMensajes( msg$, msg1$, msg2$ ) public
  string msg$, msg1$, msg2$

  msg1$ = left$( msg$ + string$( 20, " "), 20 )
  msg2$ = mid$( msg$ + string$( 20, " "), 21, 20 )

end sub

!------------------------------------------------------------------------------
!Eliminar los espacios que estan a la derecha de la cadena
!------------------------------------------------------------------------------
function elider$( v$ ) public
   string    elider$, v$
   integer*2 k

   elider$ = ""
   k = len(v$)
   while k > 0
      if mid$(v$, k, 1) = " " then begin
         k = k - 1
      endif else begin
         elider$ = left$( v$, k )
         k = 0
      endif
   wend

fend

!------------------------------------------------------------------------------
!Elimina los espacios que estan a la izquierda de la cadena
!------------------------------------------------------------------------------
function eliizq$( v$ ) public
   string    eliizq$, v$
   integer*2 k, i

   eliizq$ = ""
   k = len(v$)
   i = 1
   while i <= k
      if mid$( v$, i, 1 ) = " " then begin
         i = i + 1
      endif else begin
         eliizq$ = right$( v$, k - i + 1 )
         i = k + 1
      endif
   wend

fend

!------------------------------------------------------------------------------
! Elimina espacios a la izquierda y derecha de la cadena
!------------------------------------------------------------------------------
function trim$( v$ ) public
   string trim$, v$, s$

   s$ = eliizq$( v$ )
   trim$ = elider$( s$ )

end function

!------------------------------------------------------------------------------
! Valida que todos los caracteres sean numeros
!retorna 0 si hay caracteres no numericos o es una cadena vacia
!       -1 si todos son numeros
!------------------------------------------------------------------------------
function esNumero( num$ ) public
   integer*1 esNumero
   string    num$
   integer*2 i, k, c

   esNumero = 0
   k = len( num$ )
   i = 0
   while i < k
      i = i + 1
      c = asc( mid$( num$, i, 1 ) )
      if ( c < 48 ) or ( c > 57 ) then exit function
   wend
   if i > 0 then esNumero = -1
end function

!------------------------------------------------------------------------------
! EMULA LA PRESION DE LA TECLA BORRAR.
! RESTRICCION: ESTA FUNCION SOLO PUEDE SER LLAMADA DENTRO DE LA USER 14.
!------------------------------------------------------------------------------
sub ignoraTeclas public

   !limpia ioproce
   !lockdev 32, purge

   !limpia variables IO
   DIM TS.IO.DATA$(10)
   DIM TS.IO.KEYS(10)
   !DIM TS.IO.PREV.DATA$(10)
   !DIM TS.IO.PREV.KEYS(10)

   !Estado main
   TS.IO.NEXTSTATE = 10

   !tecla borrar
   TS.IO.MOTORKEY = 73

end sub

!------------------------------------------------------------------------------
!Respalda variables de impresion
!------------------------------------------------------------------------------
sub respaldaImpresion public
   DIM variableImpresionE(12)
   DIM variableImpresion$(5)
   variableImpresionE(1)  = TS.LINETYPE
   variableImpresionE(2)  = TS.LINEDATA
   variableImpresionE(3)  = TS.LINEDATA2
   variableImpresionE(4)  = TS.LINEDATA3
   variableImpresionE(5)  = TS.PRT.PARM
   variableImpresionE(6)  = TS.PRT.OPT
   variableImpresionE(7)  = TS.PRT.SJDI
   variableImpresionE(8)  = TS.SAVPRT.OPT
   variableImpresionE(9)  = TS.XXMOD
   variableImpresionE(10) = TS.YYMOD
   variableImpresionE(11) = TS.ZMOD
   variableImpresionE(12) = TS.PRINTPRM
   variableImpresion$(1)  = TS.PRTBUF$
   variableImpresion$(2)  = TS.PRDATA$
   variableImpresion$(3)  = TS.FORMCR$
   variableImpresion$(4)  = TS.SJDATA$
   variableImpresion$(5)  = TS.SAVPRT$
end sub

!------------------------------------------------------------------------------
!restaura variables de impresion
!------------------------------------------------------------------------------
sub restauraImpresion public
   TS.LINETYPE   = variableImpresionE(1)
   TS.LINEDATA   = variableImpresionE(2)
   TS.LINEDATA2  = variableImpresionE(3)
   TS.LINEDATA3  = variableImpresionE(4)
   TS.PRT.PARM   = variableImpresionE(5)
   TS.PRT.OPT    = variableImpresionE(6)
   TS.PRT.SJDI   = variableImpresionE(7)
   TS.SAVPRT.OPT = variableImpresionE(8)
   TS.XXMOD      = variableImpresionE(9)
   TS.YYMOD      = variableImpresionE(10)
   TS.ZMOD       = variableImpresionE(11)
   TS.PRINTPRM   = variableImpresionE(12)
   TS.PRTBUF$    = variableImpresion$(1)
   TS.PRDATA$    = variableImpresion$(2)
   TS.FORMCR$    = variableImpresion$(3)
   TS.SJDATA$    = variableImpresion$(4)
   TS.SAVPRT$    = variableImpresion$(5)
   DIM variableImpresionE(12)
   DIM variableImpresion$(5)
end sub

!------------------------------------------------------------------------------
!define nombre del archivo de trace y lo crea si el indicador es -1
!------------------------------------------------------------------------------
sub defineArchivoTrace( nombre$, crear ) public
   string    nombre$
   integer*1 crear

   on error goto orroroso

   archivoTraceNUO$ = nombre$
   existeArchivoTrace = -1

   if crear then begin
      create archivoTraceNUO$ as 103
   endif else begin
      open archivoTraceNUO$ as 103 append
      if not existeArchivoTrace then begin
         existeArchivoTrace = -1
         create archivoTraceNUO$ as 103
      endif
   endif
   wait; 500
   close 103

   salir:
   exit sub

   orroroso:
   if err = "OE" and errn = 80204010H and errf% = 103 then begin
      existeArchivoTrace = 0
      resume
   endif
   if errf% = 39 then resume
   resume salir

end sub

!------------------------------------------------------------------------------
!graba un mensaje en archivo de trace
!------------------------------------------------------------------------------
sub traceNUO( s$ ) public
   string s$, f$, xd$

   on error goto orroroso

   if not existeArchivoTrace then exit Sub
   
   If GrtraceOn% <> 1 Then Exit Sub 				! TRACE NO ACTIVO
   
   xd$ = "["+date$+"-"+time$+"]:"
   s$ = xd$ + s$
   open archivoTraceNUO$ as 103 append
   f$ = "C" + str$( len( s$ ) + 2 )
   write form f$; #103; s$ + CRLF$
   close 103

   salir:
   exit sub

   crea:
   if TS.STANDALONE then begin
      call defineArchivoTrace("C:/TRACE" + TS.TERMINAL$ + ".TXT", -1)
   endif else begin
      call defineArchivoTrace(archivoTraceNUO$, -1)
   endif
   open archivoTraceNUO$ as 103 append
   f$ = "C" + str$( len( s$ ) + 2 )
   write form f$; #103; s$ + CRLF$
   close 103
   goto salir

   orroroso:
   if err = "OE" and errn = 80204010H and errf% = 103 then resume crea
   if errf% = 39 then resume
   resume salir
end sub

!------------------------------------------------------------------------------
! Limpia el buffer de impresion
!------------------------------------------------------------------------------
sub limpiaImpresion public

   !Imprime el mensaje en Journal
   call QXL.TSUPEC21( TS.PRTBUF$ )

   !limpia buffer de impresion
   TS.SAVPRT$ = ""
   TS.PRTBUF$ = ""

   ! para no saltar lineas
   TS.FORMCR$ = "C38 A0"
   TS.PRINTPRM = 0

end sub

!------------------------------------------------------------------------------
! Justifica texto para el ancho de impresion
!------------------------------------------------------------------------------
function justificarTexto$( texto$ ) public
   string justificarTexto$, texto$
   justificarTexto$ = left$( texto$ + string$( 38, " " ), 38 )
end function

!------------------------------------------------------------------------------
! Imprime en la estacion de recibo del cliente
!------------------------------------------------------------------------------
! esta rutina desactiva temporalmente la user exit 20 para utilizar la rutina
! TSPREC01
sub imprimeCR( texto$ ) public
   string texto$

   TO.USEREXIT(20) = 0
   call respaldaImpresion
   TS.SAVPRT$ = texto$
   TS.SAVPRT.OPT = 4100H
   TS.LINETYPE   = 29
   call TSPREC01
   call restauraImpresion
   TO.USEREXIT(20) = -1

end sub

!------------------------------------------------------------------------------
! Imprime en la estacion de journal electronico
!------------------------------------------------------------------------------
! esta rutina desactiva temporalmente la user exit 20 para utilizar la rutina
! TSPREC01
sub imprimeSJ( texto$ ) public
   string texto$

   TO.USEREXIT(20) = 0
   call respaldaImpresion
   TS.SAVPRT$ = texto$
   TS.SAVPRT.OPT = 2100H
   TS.LINETYPE   = 29
   call TSPREC01
   call restauraImpresion
   TO.USEREXIT(20) = -1

end sub

!------------------------------------------------------------------------------
! Imprime en la estacion de recibo del cliente
! el texto se justifica en 38 caracteres, se le anteponen los comandos de
! impresion y agrega un retono de carro al final
! si no hay texto solo se imprimen los comando de impresion
! siempre se limpia la variable NUOcmdImpresion$
!------------------------------------------------------------------------------
sub imprime34( texto$ ) public
   string texto$

   if len( texto$ ) > 0 then begin
      write #34; NUOcmdImpresion$ + justificarTexto$( texto$ ) + chr$(0AH)
   endif else begin
      if len( NUOcmdImpresion$ ) > 0 then write #34; NUOcmdImpresion$
   endif
   NUOcmdImpresion$ = ""
!   if (TS.TS11WERR$ <> "") AND (TS.ERRF = 34) then begin
!      call traceNUO("imprime34 TS11WERR$=" + TS.TS11WERR$)
!      call QXL.TSUPEC21("TS11WERR$=" + TS.TS11WERR$)
!      wait; 100
!   endif
end sub

!------------------------------------------------------------------------------
! Imprime en la estacion de recibo del cliente y graba al journal
! texto$ -> texto a imprimir
!------------------------------------------------------------------------------
sub imprimeCJ( texto$ ) public
   string texto$, s$

   call QXL.TSUPEC21( texto$ )
   call imprime34( texto$ )

   !envia mensaje a la GUI
   s$ = TS.PRTBUF$
   TS.PRTBUF$ = texto$
   call javaEvent(updateReceiptMsg)
   TS.PRTBUF$ = s$

end sub

!------------------------------------------------------------------------------
! Imprime un buffer binario mayor de 240 bytes en el CR
! se debe usar cuando el buffer tiene un comando de mas de 240 bytes
! como codigos PDF417, graficos, etc.
!------------------------------------------------------------------------------
sub imprimeMuchos( s$ ) public
   string    s$, p$, c$
   integer*2 i, n, b

   !comando continuacion
   c$ = CHR$(1BH) + CHR$(2EH)
   b = 238
   n = len(s$)
   i = 1
   p$ = ""
   while i <= n
      p$ = p$ + mid$(s$, i, b)
      wait; 200
      write #34; p$
      p$ = c$
      i = i + b
   wend

end sub

!------------------------------------------------------------------------------
!graba mensaje de error del grupo N
!------------------------------------------------------------------------------
sub grabaError( mnum, severity, unique$ ) public
   integer*1 severity
   integer*2 mnum
   string unique$
   call adxerror( val( TS.TERMINAL$ ), 78, mnum, severity, 1, unique$ )
End sub

!------------------------------------------------------------------------------
!graba las variables del IO en el archivo TRACE de la caja
!------------------------------------------------------------------------------
sub grabaVariablesIO( s$ ) public
   integer*1 k
   string s$

   call traceNUO( s$ + "TS.IO.STATE=[" + str$(TS.IO.STATE) + "] TS.IO.MOTORKEY=[" + str$(TS.IO.MOTORKEY) + "] TS.IO.DEVICE =[" + str$(TS.IO.DEVICE)  + "]" )
   for k = 0 to 10
      if (TS.IO.KEYS(k) <> 0) or (TS.IO.DATA$(k) <> "") then \
         call traceNUO( "TS.IO.KEYS(" + str$(k) + ")=[" + str$(TS.IO.KEYS(k)) + \
            "] TS.IO.DATA$(" + str$(k) + ")=[" + TS.IO.DATA$(k) + "]" )
      if (TS.IO.PREV.KEYS(k) <> 0) or (TS.IO.PREV.DATA$(k) <> "") then \
         call traceNUO( "TS.IO.PREV.KEYS(" + str$(k) + ")=[" + str$(TS.IO.PREV.KEYS(k)) + \
            "] TS.IO.PREV.DATA$(" + str$(k) + ")=[" + TS.IO.PREV.DATA$(k) + "]" )
   next k

   !----- Otras Variables relacionadas al IOPROCE ----
   !call traceNUO( s$ + "TS.PROCEDURE=[" + str$(TS.PROCEDURE) + "]")
   !call traceNUO( s$ + "OA.INDICAT0 =[" + str$(OA.INDICAT0)  + "]")
   !call traceNUO( s$ + "OA.INDICAT1 =[" + str$(OA.INDICAT1)  + "]")
   !call traceNUO( s$ + "TS.DISP1$   =[" + TS.DISP1$ + "] TS.DISP2$=[" + TS.DISP2$ + "]" )
   !call traceNUO( s$ + "TS2.DISP1$  =[" + TS2.DISP1$ + "] TS2.DISP2$=[" + TS2.DISP2$ + "]" )

end sub

function esVentaArticulo public
   integer*1 esVentaArticulo

   esVentaArticulo = 0 

   if TS.IO.STATE = 10 then begin
      if (TS.IO.MOTORKEY = 80) and \
         (TS.IO.KEYS(2)  = 80) and \
         (TS.IO.KEYS(6)  = 0)  and \
         (TS.IO.DATA$(2) <> "") then begin
         esVentaArticulo = -1
      endif
   endif
end function

!Existen dos funciones para lee el maestro de articulos
!en vez de hacer un read #... usar una de las dos
 !****************************************************************************!
 !* IRRFEC.READ01:   Read the item record with key KEY$ into a single string *!
 !*                  DATA$.  No local error handling in the subroutine.      *!
 !****************************************************************************!
 !SUB IRRFEC.READ01 (RECKEY$, SESS.NO, DATA$, LOCK.IT) EXTERNAL
 !  STRING    RECKEY$    !* KEY of the record to be read.                     *!
 !  INTEGER*2 SESS.NO    !* SESSION number to be used.                        *!
 !  STRING    DATA$      !* String to hold the data read from file.           *!
 !  INTEGER*2 LOCK.IT    !* AUTOLOCK the record.                              *!
 !END SUB

 !****************************************************************************!
 !* IRRFEC.READ02:   Read the item record with key IR.ITEMCODE$ into the     *!
 !*                  IR.???? variables.                                      *!
 !****************************************************************************!
 !SUB IRRFEC.READ02 (SESS.NO, LOCK.IT) EXTERNAL
 !  INTEGER*2 SESS.NO    !* SESSION number to be used.                        *!
 !  INTEGER*2 LOCK.IT    !* Lock the record ?                                 *!
 !END SUB
!para que se usa esta funcion
function esArticuloPesable( codItem$ ) public
   integer*1 esArticuloPesable, indic
   string codItem$, dato$,registro$
   
   on error goto errLoc
   
   esArticuloPesable = 0
   
   dato$ = pack$(right$("000000000000" + codItem$, 12))
   
   read form "C6 I1 C70";#4 key dato$; dato$,indic,registro$
   if ( indic and 40H ) then esArticuloPesable = -1

   sale:
   exit function

   errLoc:
   if errf% = 39 then resume
   call traceNUO("ERROR EN esArticuloPesable " + + err + hexanum$( errn )  )   
   resume sale
end function

!------------------------------------------------------------------------------
!respalda las variables del IO
!------------------------------------------------------------------------------
sub respaldaVariablesIO public
   integer*1 i

   dim varDATA$(10)
   dim varPrevDATA$(10)
   dim varKEYS(10)
   dim varPrevKEYS(10)

   for i = 0 to 10
      varDATA$(i) = TS.IO.DATA$(i)
      varPrevDATA$(i) = TS.IO.PREV.DATA$(i)
      varKEYS(i) = TS.IO.KEYS(i)
      varPrevKEYS(i) = TS.IO.PREV.KEYS(i)
   next i
   varMOTORKEY = TS.IO.MOTORKEY
   varPrevMOTORKEY = TS.IO.MOTORKEY
   varSTATE = TS.IO.STATE
   varNEXTSTATE = TS.IO.NEXTSTATE
   varDEVICE = TS.IO.DEVICE

end sub

!------------------------------------------------------------------------------
!restaura las variables del IO
!------------------------------------------------------------------------------
sub restauraVariablesIO public
   integer*2 i

   for i = 1 to 10
      TS.IO.DATA$(i) = varDATA$(i)
      TS.IO.PREV.DATA$(i) = varPrevDATA$(i)
      TS.IO.KEYS(i) = varKEYS(i)
      TS.IO.PREV.KEYS(i) = varPrevKEYS(i)
   next i
   TS.IO.MOTORKEY = varMOTORKEY
   TS.IO.MOTORKEY = varPrevMOTORKEY
   TS.IO.STATE = varSTATE
   TS.IO.NEXTSTATE = varNEXTSTATE
   TS.IO.DEVICE = varDEVICE

end sub

!------------------------------------------------------------------------------
! formatea una cadena insertando separadores de miles
! separador de miles ","
!------------------------------------------------------------------------------
function formateaMiles( numero$ ) public
   string formateaMiles, numero$

   string    v$
   integer*1 i, k, n

   n = len( numero$ )
   k = ( n - 1 ) / 3 + 1
   v$ = numero$

   i = 1
   while i < k
      v$ = left$( v$, n - 3 * i ) + "," + right$( v$, 4 * i - 1)
      i = i + 1
   wend
   formateaMiles = v$

end function

!------------------------------------------------------------------------------
! formatea un numero entero considerando 2 decimales
! separador decimal "."
! separador miles   ","
! ejemplos 3 -> 0.03
! 123456789 -> 1,234,567.89
!------------------------------------------------------------------------------
function formatea2Dec( numero ) public
   string    formatea2Dec
   integer*4 numero

   string    v$, pd$, pe$, sg$
   integer*1 k

   sg$ = ""
   if numero < 0 then sg$ = "-"
   v$ = str$( abs( numero ) )
   k = len( v$ )
   if k > 2 then begin
      pd$ = right$( v$, 2 )
      pe$ = left$( v$, k - 2 )
   endif else begin
      pd$ = right$( "00" + v$, 2 )
      pe$ = "0"
   endif

   formatea2Dec = sg$ + formateaMiles( pe$ ) + "." + pd$
end function

!------------------------------------------------------------------------------
! formatea un numero entero considerando 3 decimales
! separador decimal "."
! separador miles   ","
! ejemplos 3 -> 0.003
! 123456789 -> 123,456.789
!------------------------------------------------------------------------------
function formatea3Dec( numero ) public
   string    formatea3Dec
   integer*4 numero

   string    v$, pd$, pe$, sg$
   integer*1 k

   sg$ = ""
   if numero < 0 then sg$ = "-"
   v$ = str$( abs( numero ) )
   k = len( v$ )
   if k > 3 then begin
      pd$ = right$( v$, 3 )
      pe$ = left$( v$, k - 3 )
   endif else begin
      pd$ = right$( "000" + v$, 3 )
      pe$ = "0"
   endif

   formatea3Dec = sg$ + formateaMiles( pe$ ) + "." + pd$
end function

!------------------------------------------------------------------------------
! formatea un numero entero considerando 4 decimales
! separador decimal "."
! separador miles   ","
! ejemplos 123 -> 0.0123
! 123456789 -> 12,345.6789
!------------------------------------------------------------------------------
function formatea4Dec( numero ) public
   string    formatea4Dec
   integer*4 numero

   string    v$, pd$, pe$, sg$
   integer*1 k

   sg$ = ""
   if numero < 0 then sg$ = "-"
   v$ = str$( abs( numero ) )
   k = len( v$ )
   if k > 4 then begin
      pd$ = right$( v$, 4 )
      pe$ = left$( v$, k - 4 )
   endif else begin
      pd$ = right$( "0000" + v$, 4 )
      pe$ = "0"
   endif

   formatea4Dec = sg$ + formateaMiles( pe$ ) + "." + pd$
end function

!------------------------------------------------------------------------------
! formatea un entero considerando 2 decimales, sin separador de miles
! separador decimal "."
! ejemplos 3 -> 0.03  123456789 -> 1234567.89
!------------------------------------------------------------------------------
function formateaMonto( numero ) public
   string    formateaMonto
   integer*4 numero

   string    v$, pd$, pe$, sg$
   integer*1 k

   sg$ = ""
   if numero < 0 then sg$ = "-"
   v$ = str$( abs( numero ) )
   k = len( v$ )
   if k > 2 then begin
      pd$ = right$( v$, 2 )
      pe$ = left$( v$, k - 2 )
   endif else begin
      pd$ = right$( "00" + v$, 2 )
      pe$ = "0"
   endif

   formateaMonto = sg$ + pe$ + "." + pd$
end function

!------------------------------------------------------------------------------
! restaura el texto de los visores
! visor: 1 Visor del Operador
!        2 Visor del Cliente
!        3 Ambos
!------------------------------------------------------------------------------
sub restauraVisor( visor ) public
   integer*1 visor

   locate #visor; 1, 1, OFF

   if (visor and 1) = 1 then begin
      write form "2C20"; #30; TS.DISP1$, TS.DISP2$
   endif

   if (visor and 2) = 2 then begin
      write form "2C20"; #65; TS2.DISP1$, TS2.DISP2$
   endif

end sub

!------------------------------------------------------------------------------
! respalda variables del proceso que despliega mensajes en visor
!------------------------------------------------------------------------------
sub guardaDisplay public

   dim variablesDisplay(7)

   variablesDisplay(1) = TS.LINETYPE
   variablesDisplay(2) = TS.LINEDATA
   variablesDisplay(3) = TS.LINEDATA2
   variablesDisplay(4) = TS.LINEDATA3
   variablesDisplay(5) = TS.XXMOD
   variablesDisplay(6) = TS.YYMOD
   variablesDisplay(7) = TS.DS.NOSAVE

end sub

!------------------------------------------------------------------------------
! restaura variables del proceso que despliega mensajes en visor
!------------------------------------------------------------------------------
sub restauraDisplay public

   TS.LINETYPE  = variablesDisplay(1)
   TS.LINEDATA  = variablesDisplay(2)
   TS.LINEDATA2 = variablesDisplay(3)
   TS.LINEDATA3 = variablesDisplay(4)
   TS.XXMOD     = variablesDisplay(5)
   TS.YYMOD     = variablesDisplay(6)
   TS.DS.NOSAVE = variablesDisplay(7)

end sub

!------------------------------------------------------------------------------
! muestra texto en un visor
! visor: 1 Visor del Operador
!        2 Visor del Cliente
!        3 Ambos
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
!------------------------------------------------------------------------------
sub muestraVisor(visor, msg1$, msg2$) public
   integer*1 visor
   string    msg1$, msg2$

   string s$, m1$, m2$

   s$ = string$( 20, " " )
   m1$ = left$( msg1$ + s$, 20 )
   m2$ = left$( msg2$ + s$, 20 )

   if (visor and 1) = 1 then begin
      !locate #30; 1, 1, OFF
      write form "2C20"; #30; m1$, m2$
   endif

   if (visor and 2) = 2 then begin
      !locate #65; 1, 1, OFF
      write form "2C20"; #65; m1$, m2$
   endif

end sub

!------------------------------------------------------------------------------
! muestra texto en el visor del operador y espera por tecla borrar
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
!------------------------------------------------------------------------------
sub muestraBorrar( msg1$, msg2$ ) public
   string msg1$, msg2$, s$

   call guardaDisplay
   TS.LINETYPE = 12  !DISPLAY BUFFERS AS PASSED (TERMINAL MONITOR)
   s$ = string$( 20, " " )
   TS.TEMP1$ = left$( msg1$ + s$, 20 )
   TS.TEMP2$ = left$( msg2$ + s$, 20 )
   call TSCSEC08
   call restauraDisplay

end sub

!------------------------------------------------------------------------------
! muestra texto en el visor del operador
! para volver al mensaje anterior usar restauraDisplay
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
!------------------------------------------------------------------------------
sub muestraOperador( msg1$, msg2$ ) public
   string msg1$, msg2$, s$

   call guardaDisplay
   TS.LINETYPE = 12  !DISPLAY BUFFERS AS PASSED (TERMINAL MONITOR)
   s$ = string$( 20, " " )
   TS.TEMP1$ = left$( msg1$ + s$, 20 )
   TS.TEMP2$ = left$( msg2$ + s$, 20 )
   call TSDSEC01

end sub

!------------------------------------------------------------------------------
! lector de banda magnetica
! 0 dato directo de la lectura
! 1 hace unpack pack
!------------------------------------------------------------------------------
function leeMSR$( msr ) public
   string leeMSR$, D$
   integer*1 msr

   on error goto orror

   leeMSR$ = ""
   unlockdev 41
   unlockdev 32
   wait 32, 41; 60000
   if event% = 41 then begin
      read #41; line D$
      if ( msr = 1 ) then begin
         leeMSR$ = unpack$( pack$( D$ ) )
      endif else begin
         leeMSR$ = D$
      endif
   endif

   sale:
   exit function

   orror:
   if errf% = 39 then resume
   if err="FU" and errf% = 41 then begin
      open "MSR:" as 41
      wait; 200
      resume retry
   endif

   call traceNUO( "leeMSR$ ERROR " + err + hexanum$(errn) )
   resume sale

end function

!------------------------------------------------------------------------------
! Permite habilitar o des-habilitar el uso de la tecla ANULAR
! f = -1 habilitado, la tecla BORRAR solo limpia lo ingresado
! f =  0 la tecla BORRAR aborta el ingreso
!------------------------------------------------------------------------------
sub usoTeclaAnular( f ) public
   integer*1 f
   teclaAnular = f
end sub

!------------------------------------------------------------------------------
! Recibe datos desde teclado o escaner
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
! retorna una cadena con
!    "A" si se presiono BORRAR
!    "P" y "G" o " " si la llave esta girada o no y a
!    continuacion los datos ingresados
!------------------------------------------------------------------------------
function ingresoDatos$( msg1$, msg2$ ) public
   string ingresoDatos$, msg1$, msg2$, t$
   string D$(1), R$, x$
   integer*1 f
   integer*2 cod

   ! Limpia IOPROCE
   lockdev 32, purge

   t$ = ""
   f = -1
   while f

      ! muestra mensaje en el visor de operador
      call muestraOperador( msg1$, msg2$ )

      ! Desbloquea teclado y pasa al Estado ENTRAR
      unlockdev 32, 99
      wait 32; 60000

      if event% = 32 then begin

         ! Lectura Teclado y Status
         dim D$(10)
         TS.ER.RETURN = -1
         read #32; D$(0), D$(1), D$(2), D$(3), D$(4), D$(5), D$(6), D$(7), D$(8), D$(9), D$(10)

         if TS.ER.RETURN <> -1 then begin
            ingresoDatos$ = "A"
            f = 0
            goto continua
         endif

         ! Llave puesta
         r$ = " "
         if mid$( D$(0), 11, 1 )  = "1" then R$ = "G"

         x$ = mid$( D$(0), 13, 1 )
         if x$ = "" then cod = 0 else cod = asc( x$ )

         ! tecla BORRAR
         if cod = 73 then begin

            if teclaAnular then begin

               ! borra los caracteres ingresados
               lockdev 32, purge
               t$ = ""
               if (JAVA.INIT = -1) then begin
                  jGuiSubState = 80535
                  call javaEvent(terminalSubStateMsg)
               endif

            endif else begin

               ! aborta el ingreso
               ingresoDatos$ = "A"
               f = 0

            endif
            goto continua

         endif

         ! tecla ANULAR
         if cod = 70 then begin

            !  aborta el ingreso
            ingresoDatos$ = "A"
            f = 0
            goto continua

         endif

         ! tecla ENTER
         if cod = 80 then begin
            if len( D$(10) ) > 0 then D$(10) = right$( D$(10), len( D$(10) ) - 1 )
            ingresoDatos$ = "P" + R$ + D$(10)
            f = 0
            goto continua
         endif
         t$ = t$ + right$( D$(10), 1 )
         msg2$ = right$( t$, 20 )

      endif

      continua:

   wend

   lockdev 32, purge

end function

!------------------------------------------------------------------------------
! Recibe datos desde desde teclado en modo alfanumerico
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
! retorna una cadena donde el primer caracter indica
! "E" en caso de error, "P" dato ingresado (tecla PLU),
! "A" aborta el ingreso (tecla BORRAR o ANULAR)
! el segundo caracter "G" si la llave esta girada
! del tercer en adelante el valor ingresado
! si esta activo el indicador teclaAnular
! la tecla BORRAR limpia el texto ingresado y la tecla
! ANULAR aborta el ingreso
!------------------------------------------------------------------------------
function ingresoTexto$( msg1$, msg2$ ) public
   string ingresoTexto$, msg1$, msg2$
   string R$, t$, D$(1), x$
   integer*1 f
   integer*2 cod, k

   ! Limpia IOPROCE
   lockdev 32, purge

   t$ = ""
   f = -1
   while f
!call traceNUO("Texto$ msg1$=[" + msg1$ + "] msg2$=[" + msg2$ + "]")

      ! muestra mensaje en el visor de operador
      call muestraOperador( msg1$, msg2$ )
      !call muestraVisor(2, msg1$, msg2$)

      ! Desbloquea teclado y pasa al Estado ALFANUM
      unlockdev 32, 101
      wait 32; 60000

      if event% = 32 then begin

         ! Lectura Teclado y Status
         dim D$(10)
         TS.ER.RETURN = -1
         read #32; D$(0), D$(1), D$(2), D$(3), D$(4), D$(5), D$(6), D$(7), D$(8), D$(9), D$(10)
         if TS.ER.RETURN <> -1 then begin
            ingresoTexto$ = "A"
            f = 0
            goto continua
         endif

         ! Llave puesta
         R$ = " "
         if mid$( D$(0), 11, 1 )  = "1" then R$ = "G"

         x$ = mid$( D$(0), 13, 1 )
         if x$ = "" then cod = 0 else cod = asc( x$ )

!call traceNUO("Texto$ cod=" + str$(cod) + lineaHex$(D$(10)))

         ! tecla BORRAR
         if cod = 73 then begin

            if teclaAnular then begin

               ! borra los caracteres ingresados
               lockdev 32, purge
               t$ = ""
               if (JAVA.INIT = -1) then begin
                  jGuiSubState = 80535
                  call javaEvent(terminalSubStateMsg)
               endif
            endif else begin

               ! aborta el ingreso
               ingresoTexto$ = "A"
               f = 0

            endif
            goto continua

         endif

         ! tecla ANULAR
         if cod = 70 then begin

            !  aborta el ingreso
            ingresoTexto$ = "A"
            f = 0
            goto continua

         endif

         ! tecla ENTER
         if cod = 80 then begin
            if len( D$(10) ) > 1 then begin
               t$ = t$ + right$( D$(10), len( D$(10) ) - 1 )
            endif
            ingresoTexto$ = "P" + R$ + t$
            f = 0
            goto continua
         endif

         ! Traslada el cod a letra
         if cod = 222 then begin
            t$ = t$ + right$( D$(10), 1 )
         endif else begin
            k = matchb( chr$( cod ), codigosTeclado$, 1 )
            if k > 0 then begin
               if len( D$(10) ) > 1 then begin
                  t$ = t$ + right$( D$(10), len( D$(10) ) - 1 )
               endif
               t$ = t$ + chr$( k + 31 )
            endif
         endif

      endif

      continua:
      msg2$ = right$( t$, 20 )

   wend

   lockdev 32, purge

end function

!------------------------------------------------------------------------------
! Consulta binaria
! msg1$, msg2$ mensaje para linea 1 y linea 2 del visor
! retorna:
! -1 si se presiona la tecla PLU
!  0 si se presiona la tecla BORRAR
!------------------------------------------------------------------------------
function pregunta( msg1$, msg2$ ) public
   integer*1 pregunta
   string msg1$, msg2$
   string ADATA$, B$, x$
   integer*1 f
   integer*2 cod

   pregunta = 0

   ! Limpia IOPROCE
   lockdev 32, purge

   f = -1
   while f

      ! muestra mensaje en el visor de operador
      call muestraOperador( msg1$, msg2$ )

      ! Desbloquea teclado y pasa al Estado ENTRAR
      unlockdev 32, 99
      wait 32; 60000

      if event% = 32 then begin

         ! Lectura Teclado y Status
         TS.ER.RETURN = -1
         read #32; ADATA$, B$, B$, B$, B$, B$, B$, B$, B$, B$, B$
         if TS.ER.RETURN <> -1 then begin
            f = 0
            cod = 0
         endif else begin
            x$ = mid$( ADATA$, 13, 1 )
            if x$ = "" then cod = 0 else cod = asc( x$ )
         endif

         ! tecla BORRAR
         if cod = 73 then begin
            f = 0
         endif

         ! tecla ENTER
         if cod = 80 then begin
            pregunta = -1
            f = 0
         endif

      endif

   wend

   lockdev 32, purge

end function

!------------------------------------------------------------------------------
! ingresa numero de supervisor
!------------------------------------------------------------------------------
function leeTeclado$(m1$, m2$) public
   string leeTeclado$, m1$, m2$

   call muestraOperador( m1$, m2$ )

   !jGuiSubState = 80502
   !call javaEvent(terminalSubStateMsg)

   TO.USEREXIT(14) = 0
   UNLOCKDEV 32, 10, PRIORITY           ! set input state to main
   call TSCSECRK                        ! read the keyboard
   LOCKDEV 32                           ! lock it
   TO.USEREXIT(14) = -1

   if TS.IO.MOTORKEY = 80 then begin
      leeTeclado$ = "P" + mid$( TS.IO.HDR$, 11, 1 ) + TS.IO.DATA$(2)
      exit function
   endif

   if TS.IO.MOTORKEY = 89 then begin
      leeTeclado$ = "Y" + mid$( TS.IO.HDR$, 11, 1 ) + TS.IO.DATA$(2)
      exit function
   endif

   !Asigna respuesta tecla borrar (73)
   leeTeclado$ = "I" + mid$( TS.IO.HDR$, 11, 1 )

!call grabaVariablesIO( "leeTeclado$ " )

end function

!------------------------------------------------------------------------------
! solicita supervisor
! retorna -1 si fue autorizado
!          0 si no fue autorizado
!------------------------------------------------------------------------------
function supervisor public
   integer*1 supervisor, i
   string    numAut$

   dim TS.IO.DATA$(10)
   dim TS.IO.KEYS(10)

   llavePuesta = 0
   supervisor = 0

   ! Permite ingresar dos veces el numero de autorizacion
   i = 0
   ingresoClaveAutoriz:
   jGuiSubState = 80502
   call javaEvent(terminalSubStateMsg)
   UNLOCKDEV 32, 6, PRIORITY            ! set input state to enter/clear
   call TSCSECRK                        ! read the keyboard
   LOCKDEV 32                           ! lock it

   if TS.IO.KEYS(10) = 80 then begin      ! ALLOW FOR OVERRIDE HERE
      numAut$ = TS.IO.DATA$(10)
   endif
   llavePuesta = val( mid$( TS.IO.HDR$, 11, 1 ) )

   if numAut$ = "" then begin
      i = i + 1
      if i > 1 then exit function
      goto ingresoClaveAutoriz
   endif

   ! No hay llave puesta
   if llavePuesta <> 1 then begin
      call muestraBorrar( "SE NECESITA", "LLAVE DEL JEFE DEPTO" )
      if java.init then begin
         jGuiSubState = 0
      endif
      exit function
   endif

   ! Valida  autorizacion
   for i = 1 TO 20
      if TO.OVRIDNUM(i) = val( numAut$ ) then begin
         supervisor = -1
         if java.init then begin
            jGuiSubState = 0
         endif
         exit function
      endif
   next i

end function

!------------------------------------------------------------------------------
! ingresa numero de supervisor
!------------------------------------------------------------------------------
function capturaAutorizacion public
   string    capturaAutorizacion, m1$, m2$

   ! mensaje 406 de EAMSDESC.DAT "SE NECESITA         AUTORIZACION   "
   !call separaMensajes( TS.SDESC$(406), m1$, m2$ )
   call muestraOperador( "SE NECESITA", "AUTORIZACION" )

   jGuiSubState = 80502
   call javaEvent(terminalSubStateMsg)

   capturaAutorizacion = ""

   UNLOCKDEV 32, 6, PRIORITY            ! set input state to enter/clear
   call TSCSECRK                        ! read the keyboard
   LOCKDEV 32                           ! lock it

   if ( TS.IO.KEYS(10) = 79 ) and ( TS.IO.MOTORKEY = 80 ) then begin      ! ALLOW FOR OVERRIDE HERE
      if TS.IO.DATA$(10) <> "" then begin
         capturaAutorizacion = TS.IO.DATA$(10)
      endif
   endif
   llavePuesta = val( mid$( TS.IO.HDR$, 11, 1 ) )

   if java.init then begin
      jGuiSubState = 0
   endif

end function

!------------------------------------------------------------------------------
! solicita autorizacion de supervisor
! retorna -1 si fue autorizado
!          0 si no fue autorizado
!------------------------------------------------------------------------------
function entradaAutorizacion public
   integer*1 entradaAutorizacion, i
   string    numAut$, m1$, m2$

   dim TS.IO.DATA$(10)
   dim TS.IO.KEYS(10)

   llavePuesta = 0
   entradaAutorizacion = 0

   ! mensaje 32 de EAMSDESC.DAT "ENTRE AUTORIZACION   O PULSE BORRAR"
   call separaMensajes( TS.SDESC$(32), m1$, m2$ )
   call muestraBorrar( m1$, m2$ )

   ! Permite ingresar dos veces el numero de autorizacion
   i = 0
   ingresoClaveAutoiz:
   numAut$= capturaAutorizacion
   if numAut$ = "" then begin
      i = i + 1
      if i > 1 then exit function
      goto ingresoClaveAutoiz
   endif

   ! No hay llave puesta
   if llavePuesta <> 1 then begin
      ! mensaje 334 de EAMSDESC.DAT "SE NECESITA    LLAVE DEL JEFE DEPTO"
      !call separaMensajes( TS.SDESC$(334), m1$, m2$ )
      call muestraBorrar( "SE NECESITA", "LLAVE DEL JEFE DEPTO" )
      exit function
   endif

   ! Valida  autorizacion
   for i = 1 TO 20
      if TO.OVRIDNUM(i) = val( numAut$ ) then begin
         entradaAutorizacion = -1
         exit function
      endif
   next i

   ! mensaje 408 de EAMSDESC.DAT "COMPRUEBE LA        CONTRASEÑA     "
   !call separaMensajes( TS.SDESC$(408), m1$, m2$ )
   call muestraBorrar( "COMPRUEBE EL NUMERO", "DE AUTORIZACION" )
   
end function

!------------------------------------------------------------------------------
! Busca el primer espacio o TAB
! retorna la cantidad de caracteres antes del espacio o TAB, 0 si no hay
!------------------------------------------------------------------------------
function espacioTab( s$ ) public
   integer*2 espacioTab
   string    s$
   string    c$
   integer*2 i, k

   espacioTab = 0
   k = len( s$ )
   i = 0
   while i < k
      i = i + 1
      c$ = mid$( s$, i, 1 )
      if c$ = chr$(9) or c$ = chr$(32) then begin
         espacioTab = i - 1
         exit function
      endif
   wend
fend

!------------------------------------------------------------------------------
! Elimina espacios y TAB al inicio y al final
!------------------------------------------------------------------------------
function eliminaTab$( s$ ) public
   string    eliminaTab$, s$

   string    v$, c$
   integer*2 i, k

   v$ = ""
   k = len(s$)
   while k > 1
      c$ = mid$(s$, k, 1)
      if c$ = chr$(9) or c$ = chr$(32) then begin
         k = k - 1
      endif else begin
         v$ = left$(s$, k)
         k = 0
      endif
   wend

   k = len(v$) + 1
   i = 1
   while i < k
      c$ = mid$(v$, i, 1)
      if c$ = chr$(9) or c$ = chr$(32) then begin
         i = i + 1
      endif else begin
         v$ = right$(v$, k - i)
         i = k
      endif
   wend

   eliminaTab$ = v$

end function

!------------------------------------------------------------------------------
! Lee el valor de un parametro a partir de una posicion del archivo
! nombre$ nombre del parametro
! sesion  numero de sesion con que se abrio el archivo de parametros
! puntero posicion del registro donde inicia el grupo de parametros
! retorna el valor del parametro
!------------------------------------------------------------------------------
function leeParametro$( nombre$, sesion, puntero ) public
   string    leeParametro$, \ valor del parametro
             nombre$,       \ nombre del parametro
             r$, s$
   integer*1 sesion,        \ numero de sesion
             f
   integer*2 k
   integer*4 puntero        ! puntero al archivo de parametros

   on error goto errorParam

   leeParametro$ = ""
   s$ = nombre$
   point sesion; puntero
   f = -1
   while f
      read #sesion; line r$
      k = espacioTab( r$ )
      if k = 0 then begin
         f = 0
      endif else begin
         if left$( r$, k ) = s$ then begin
            s$ = right$( r$, len(r$) - k )
            leeParametro$ = eliminaTAB$( s$ )
            f = 0
         endif
      endif
   wend

   finParam:
   exit function

   errorParam:
   if errf% = 39 then resume
   if errf% = sesion and errn = 1CH then resume finParam
   if errf% = sesion then begin
      call traceNUO( "leeParametro$ ERROR " + err + hexanum$(errn) )
      resume finParam
   endif
   resume finParam
end function

!------------------------------------------------------------------------------
! Carga los parametros de un grupo
! sesion  numero de sesion con que se abrio el archivo de parametros
! puntero posicion del registro donde inicia el grupo de parametros
! retorna el numero total de parametros cargado
!------------------------------------------------------------------------------
function cargaParametros( sesion, puntero ) public
   integer*1 cargaParametros
   integer*1 sesion,        \ numero de sesion
             i, f
   integer*2 k
   integer*4 puntero        ! puntero al archivo de parametros
   string    r$, s$

   on error goto errorParam

   dim mapParametro$( 100, 1 )

   i = 0
   point sesion; puntero
   f = -1
   while f
      read #sesion; line r$
      k = espacioTab( r$ )
      if k = 0 then begin
         f = 0
      endif else begin
         i = i + 1
         if i > 99 then f = 0
         mapParametro$( i, 0 ) = left$( r$, k )
         s$ = right$( r$, len(r$) - k )
         mapParametro$( i, 1 ) = eliminaTAB$( s$ )
      endif
   wend

   finParam:
   cargaParametros = i
   exit function

   errorParam:
   if errf% = 39 then resume
   if errf% = sesion then begin
      call traceNUO( "cargaParametros ERROR " + err + hexanum$(errn) )
      resume finParam
   endif
   resume finParam
end function

!------------------------------------------------------------------------------
! Carga las lineas que hay a continuacion de un grupo
! sesion  numero de sesion con que se abrio el archivo de parametros
! puntero posicion del registro donde inicia el grupo de parametros
! retorna el numero total de parametros cargado
!------------------------------------------------------------------------------
function cargaLineas( sesion, puntero ) public
   integer*1 cargaLineas
   integer*1 sesion,        \ numero de sesion
             i, f
   integer*4 puntero        ! puntero al archivo de parametros
   string    r$

   on error goto errorLineas

   dim mapParametro$( 100, 1 )

   i = 0
   point sesion; puntero
   f = -1
   while f
      read #sesion; line r$
      r$ = trim$( r$ )
      if len( r$ ) > 2 then begin
         i = i + 1
         if i > 99 then f = 0
         mapParametro$( i, 1 ) = r$
      endif else begin
         f = 0
      endif
   wend

   finLineas:
   cargaLineas = i
   exit function

   errorLineas:
   if errf% = 39 then resume
   if errf% = sesion and errn = 1CH then resume finLineas
   if errf% = sesion then begin
      call traceNUO( "cargaLineas ERROR " + err + hexanum$(errn) )
      resume finLineas
   endif
   resume finLineas
end function

!------------------------------------------------------------------------------
! rescata un parametro del arreglo mapParametro$
! numero  numero del parametro a rescatar (1 a 100)
! nombre$ retorna el nombre del parametro
! valor$  retorna el valor del parametro
!------------------------------------------------------------------------------
sub rescataParametro( numero, nombre$, valor$ ) public
   integer*1 numero
   string    nombre$, valor$

   nombre$ = mapParametro$( numero, 0 )
   valor$ = mapParametro$( numero, 1 )

end sub

!------------------------------------------------------------------------------
! Abre un archivo de parametros y obtiene el puntero al primer
! elemento del grupo
! archivo$ nombre del archivo de parametros
! grupo$   nombre del grupo de parametros
! sesion   numero de sesion para abrir el archivo de parametros
! retorna  el puntero al primer registro del grupo de parametros
!          o -1 si hay un error
!------------------------------------------------------------------------------
function abreParametros( archivo$, grupo$, sesion ) public
   integer*4 abreParametros
   integer*2 sesion
   string    archivo$,   \ nombre del archivo de parametros
             grupo$,     \ nombre del grupo de parametros
             r$
   integer*1 f

   on error goto errorParam

   abreParametros = -1
   f = -1
   open archivo$ as sesion nowrite nodel
   if not f then begin
      close sesion
      open archivo$ as sesion nowrite nodel
      if not f then goto salir
   endif

   f = -1
   while f
      read #sesion; r$
      if r$ = grupo$ then begin
         abreParametros = PTRRTN + len( grupo$ ) + 2
         f = 0
      endif
   wend

   salir:
   exit function

   errorParam:
   if errf% = 39 then resume
   f = 0

   !sesion ocupada
   if err = "DF" and errn = 14h then resume

   !fin de archivo
   if err = "EF" and errn = 1Ch then resume

   call traceNUO( "abreParametros sesion " + str$( errf% ) + " ERROR " + err + hexanum$(errn) )

   resume salir

end function

!------------------------------------------------------------------------------
! Obtiene el puntero al primer elemento del grupo
! grupo$   nombre del grupo de parametros
! sesion   numero de sesion usada para abrir el archivo de parametros
! retorna  el puntero al primer registro del grupo de parametros
!          o -1 si hay un error
!------------------------------------------------------------------------------
function buscaParametros( grupo$, sesion ) public
   integer*4 buscaParametros
   integer*2 sesion
   string    grupo$,     \ nombre del grupo de parametros
             r$
   integer*1 f

   on error goto errorParam

   buscaParametros = -1
   point sesion; 0

   f = -1
   while f
      read #sesion; r$
      if r$ = grupo$ then begin
         buscaParametros = PTRRTN + len( grupo$ ) + 2
         f = 0
      endif
   wend

   salir:
   exit function

   errorParam:
   if errf% = 39 then resume
   call traceNUO( "buscaParametros sesion " + str$( errf% ) + " ERROR " + err + hexanum$(errn) )

   !fin de archivo
   if err = "EF" and errn = 1Ch then begin
      resume salir
   endif

   resume salir

end function

!------------------------------------------------------------------------------
! Cierra un archivo de parametros
! sesion   numero de sesion usada para abrir el archivo de parametros
!------------------------------------------------------------------------------
sub cierraParametros( sesion ) public
   integer*2 sesion

   on error goto errorParam

   close sesion

   salir:
   exit sub

   errorParam:
   if errf% = 39 then resume
   call traceNUO( "cierraParametros sesion " + str$( errf% ) + " ERROR " + err + hexanum$(errn) )
   resume salir

end sub

!------------------------------------------------------------------------------
! lee codigos del teclado
!------------------------------------------------------------------------------
sub leeCodigosTeclado
   string    r$

   !abre archivo de de codigos de teclas
   TS.ER.RETURN = -1
   open "R::ADX_UDT1:ALFANUM.DAT" as 38 nowrite nodel
   if TS.ER.RETURN <> -1 then begin
      call muestraBorrar( "NO EXISTE ARCHIVO", "ALFANUM.DAT" )
      exit sub
   endif

   codigosTeclado$ = ""
   read #38; line r$
   while TS.ER.RETURN = -1
      codigosTeclado$ = codigosTeclado$ + chr$( val( r$ ) )
      read #38; line r$
   wend

   close 38

end sub

!------------------------------------------------------------------------------
! Asigna valor a variables globales de uso general
! al iniciar una nueva transaccion
!------------------------------------------------------------------------------
sub NUOTRUTI01 public
   !PipeDest$ = "0BBZ"
end sub

!------------------------------------------------------------------------------
! Asigna valor a variables globales de uso general
! al iniciar el terminal
!------------------------------------------------------------------------------
sub NUOTRUTI07 public

   NUO.SaltaLineas$   = chr$(1BH) + chr$(64H)                ! Comando para saltar lineas, se le debe agregar el numero de lineas a saltar
   NUO.CortaPapel$    = chr$(0CH)                            ! Comando para cortar el papel
   NUO.AlineaIzq$     = chr$(1BH) + chr$(61H) + chr$(0)      ! Comando para alinear a la izquierda (valido solo al comienzo de la linea)
   NUO.AlineaDer$     = chr$(1BH) + chr$(61H) + chr$(2)      ! Comando para alinear a la derecha (valido solo al comienzo de la linea, no usar TAB)
   NUO.Centrado$      = chr$(1BH) + chr$(61H) + chr$(1)      ! Comando para centrar (valido solo al comienzo de la linea, no usar TAB)
   NUO.LetraGigante$  = chr$(1BH) + chr$(21H) + chr$(38) + \ ! Comando para letra muy grande (doble ancho, doble alto, negrita, font A, espaciado normal)
                        chr$(1BH) + chr$(20H) + chr$(3)
   NUO.LetraGrande$   = chr$(1BH) + chr$(21H) + chr$(19H) + \! Comando para letra grande (doble alto, negrita, font B, espaciado doble)
                        chr$(1BH) + chr$(20H) + chr$(6)
   NUO.LetraMediana$  = chr$(1BH) + chr$(21H) + chr$(1) + \  ! Comando para letra mediana (espaciado normal)
                        chr$(1BH) + chr$(20H) + chr$(3)
   NUO.LetraNormal$   = chr$(1BH) + chr$(21H) + chr$(0) + \  ! Comando para letra normal (espaciado normal)
                        chr$(1BH) + chr$(20H) + chr$(3)
   NUO.LetraJunta$    = chr$(1BH) + chr$(20H) + chr$(1)      ! Comando para letra junta
   NUO.InterLinea8$   = chr$(1BH) + chr$(31H)                ! Comando para inter-lineado a 1/8 de pulgada (debe ser el unico comando escrito)
   NUO.InterLinea6$   = chr$(1BH) + chr$(32H)                ! Comando para inter-lineado a 1/6 de pulgada (debe ser el unico comando escrito)
   NUO.InterLinea3$   = chr$(1BH) + chr$(33H) + chr$(24)     ! Comando para inter-lineado de 3 mm (debe ser el unico comando escrito)
   NUO.NegritaA$      = chr$(1BH) + chr$(47H) + chr$(1)      ! Comando para activar negrita
   NUO.NegritaD$      = chr$(1BH) + chr$(47H) + chr$(0)      ! Comando para desactivar negrita
   NUO.NegritaFontA$  = chr$(1BH) + chr$(21H) + chr$(8) + \  ! Comando para negrita en font A (espaciado normal)
                        chr$(1BH) + chr$(20H) + chr$(3)
   NUO.NegritaFontB$  = chr$(1BH) + chr$(21H) + chr$(8) + \  ! Comando para negrita en font B (espaciado 4 puntos)
                        chr$(1BH) + chr$(20H) + chr$(4)
   NUO.NegritaFontC$  = chr$(1BH) + chr$(21H) + chr$(8) + \  ! Comando para negrita en font C (espaciado reducido)
                        chr$(1BH) + chr$(20H) + chr$(1)
   CRLF$ = chr$(0DH) + chr$(0AH)

   teclaAnular = 0    ! se usa la tecla BORRAR para salir del ingreso de datos

   PipeSess      = 100
   PipeId$       = "F"
   PipeDest$     = "0BBZ"
   PipeWrite     = prstint
   TS.TEMP1I2    = prstcrt( PipeSess, 240, PipeId$ )
   if TS.TEMP1I2 <> 0 then begin
      if TS.TEMP1I2 = -1000 then \
         call imprimeCJ( "Invalid pipe ID [" + PipeId$ + "]" ) \
      else if TS.TEMP1I2 = -1001 then \
         call imprimeCJ( "Pipe [" + PipeId$ + "]" + " already exist" ) \
      else call imprimeCJ( "Error " + str$( TS.TEMP1I2 ) + " al crear pipe [" + PipeId$ + "]" ) \
   endif
   codErrorPipe = ""

   !recupera codigos de teclado del archivo ALFANUM.DAT
   call leeCodigosTeclado

   !imprime numero de terminal en grande y centrado
   !Porque el comando de centrado no funciona para la primera linea
   write #34; NUO.Centrado$ + NUO.LetraGrande$
   TS.TEMP1$ = "TERMINAL: " + TS.TERMINAL$
   call imprimeCJ( TS.TEMP1$ )
   write #34; NUO.LetraNormal$ + NUO.LetraJunta$ + NUO.InterLinea8$

   !define el nombre del archivo de trace y lo crea vacio si es antes de las 9 horas
   TS.TEMP1I1 = 0
   if val( mid$(time$, 1, 2) ) < 9 then TS.TEMP1I1 = -1
   call defineArchivoTrace( "R::C:/TRACE/TRACE" + TS.TERMINAL$ + ".TXT", TS.TEMP1I1 )
   if existeArchivoTrace then begin
      call imprimeCJ(archivoTraceNUO$)
   endif else begin
      TS.TEMP1$ = "ERROR al crear:"
      call imprimeCJ(TS.TEMP1$)
      call imprimeCJ(archivoTraceNUO$)
   endif

   !imprime numero de lineas maximo del arreglo TSL
   TS.TEMP1$ = "Control Tlog alerta "  + str$(GC.SL.WARN) + " lineas total " + str$(GC.SL.SIZE) + " lineas"
   call traceNUO( TS.TEMP1$ )
   TS.TEMP1$ = "Memoria total: "  + str$(FRE) + " bytes continua: " + str$(MFRE) + " bytes"
   call traceNUO( TS.TEMP1$ )
   if GC.SL.WARN < 200 then begin
      if GC.SL.SIZE > 50 then begin
         GC.SL.WARN = ( GC.SL.SIZE * 9 ) / 10
         TS.TEMP1$ = "Ajuste  Tlog alerta "  + str$(GC.SL.WARN) + " lineas total " + str$(GC.SL.SIZE) + " lineas"
         call traceNUO( TS.TEMP1$ )
      endif else begin
         call muestraBorrar( "MEMORIA INSUFICIENTE", "TOTAL " + str$( FRE ) )
         call muestraBorrar( "TOTAL    " + str$(FRE), "CONTINUA " + str$(MFRE) )
         stop
      endif
   endif

end sub

!------------------------------------------------------------------------------
! limpia el PRS
! Se utiliza para evitar que datos antiguos impidan interpretar la
! respuesta, se debe ejecutar antes de enviar una consulta
!------------------------------------------------------------------------------
sub limpiaPipe public
   string    s$
   integer*1 f

   on error goto eLimpia

   f = -1
   while f
      read form "C120"; #PipeSess; s$
   wend

   sLimpia:
   exit sub

   eLimpia:
   if errf% = PipeSess then begin
      f = 0
      resume
   endif
   if errf% = 39 then resume
   resume sLimpia

end sub

!------------------------------------------------------------------------------
! espera la llegada de un mensaje del PRS
!------------------------------------------------------------------------------
function esperaPipe( tiempo, m$ ) public
   integer*1 esperaPipe
   integer*4 tiempo, espera
   integer*2 evento
   string    m$, t$

   t$ = "Esperando respuesta"
   espera = tiempo / 1000
   if espera < 1 then espera = 1
   call muestraVisor( 1, t$, m$ + " " + str$( espera ) )

   esperaPipe = 0
   while espera > 0
      wait PipeSess; 1000
      evento = event%
      if evento = PipeSess then goto tengorespuesta
      if evento = 0 then begin
         espera = espera - 1
         call muestraVisor( 1, t$, m$ + " " + str$( espera ) )
      endif else begin
         call traceNUO( "EVENTO " + str$( evento ) + " en " + m$ + str$( espera ) + " seg " )
         call muestraVisor( 1, "EVENTO " + str$( evento ), m$ + " " + str$( espera ) )
         wait; 300
      endif
   wend

   call traceNUO( "No hay respuesta de " + m$ + " despues de " + str$( tiempo / 1000 ) + " seg" )
   call muestraVisor( 1, "No responde servicio", m$ )
   wait; 2000
   goto salir

   tengorespuesta:
   esperaPipe = -1

   salir:
   call restauraVisor( 1 )

end function

!------------------------------------------------------------------------------
! envia un mensaje al PRS
!------------------------------------------------------------------------------
function enviaMensaje( m$ ) public
   integer*1 enviaMensaje
   string    m$
   integer*4 ret

   enviaMensaje = 0
   if len( m$ ) = 0 then exit function
   m$ = right$( "00000" + str$( len( m$ ) ), 5 ) + m$

call traceNUO( "enviaMensaje pipeWrite=" + str$(pipeWrite) + " pipeDest=" + pipeDest$ + " [" + m$ + "]")

   ret = prstwrc( PipeWrite, PipeDest$, m$ )

call traceNUO( "enviaMensaje ret=" + hexanum$(ret) )

   if ret = 0 then begin
      enviaMensaje = -1
      exit function
   endif
   call traceNUO( "enviaMensaje:  Error al grabar PIPE retorno=" + str$( ret ) )
   call muestraOperador( "Error al grabar PIPE", str$( ret ) )
   !PipeDest$ = "0BBZ"

end function

!------------------------------------------------------------------------------
! recibe un mensaje del PRS
!------------------------------------------------------------------------------
function recibeMensaje( t$ ) public
   string recibeMensaje, t$, resp$, f$
   integer*2 x

   recibeMensaje = ""
   call muestraVisor( 1, "Leyendo respuesta", t$ )
   wait; 300
   read form "C5"; #PipeSess; resp$
   x = 0
   if esnumero(resp$) then x = val(resp$)
   if x < 1 or x > 512 then begin
      call traceNUO( "longitud de mensaje " + resp$ + " no es valido" )
      call restauraVisor(1)
      call muestraBorrar( "longitud de mensaje", "no es valido" )
      exit function
   endif

   f$ = "C" + str$( x )
   wait; 700
   read form f$; #PipeSess; resp$
   recibeMensaje = resp$

   call restauraVisor(1)

end function

!------------------------------------------------------------------------------
! Lee un pi: que fue escrito desde un programa java en el POS
!------------------------------------------------------------------------------
function leeComunicacionPI( sesionPipe )
   string leeComunicacionPI, \
          formato, \
          mensaje
   integer*2 sesionPipe
   integer*4 largo

   on error goto errores

   mensaje=""
   formato = "C5"

   wait; 300
   read form formato; #sesionPipe; mensaje

   largo = val( mensaje )
   formato = "C" + str$(largo)

   wait; 700
   read form formato; #sesionPipe; mensaje

   salida:
   leeComunicacionPI = mensaje
   close sesionPipe
   exit function

   errores:
   if (err = "CU" or err = "FU" or err = "NF") and (errf% = sesionPipe) then resume

   if (err = "EF") and (errf% = sesionPipe) then begin
      call traceNUO("leeComunicacionPI ERROR " + err + hexanum$(errn) + "]")
      resume salida
   endif
   call traceNUO("leeComunicacionPI ERROR " + err + hexanum$(errn) + "]")

   if errf% = 39 then resume
   resume salida

end function

!----------------------------------------------------------------------------------------------------------------
! Escribe sobre un pi: para que sea leido desde un programa java en el POS
!------------------------------------------------------------------------------
function enviaMensajePI( sesionPipe, mensaje$ ) public
   integer*1 enviaMensajePI
   integer*2 sesionPipe
   string    mensaje$

   on error goto errores

   enviaMensajePI = 0
   write #sesionPipe; mensaje$
   enviaMensajePI = -1

   salir:
   exit function

   errores:
   call traceNUO("enviaMensajePI ERROR " + err + hexanum$(errn) + " sesion " + str$(errf%))
   if (err = "FU") and (errf% = sesionPipe) then resume
   
   if errf% = 39 then resume
   resume salir

end function

!------------------------------------------------------------------------------
! Abre un pi: para comunicarse con un programa java en el POS
!------------------------------------------------------------------------------
Function abreComunicacionPI( sesionPipe, nombrePipe$, modoEscritura, bufferSize) Public
   Integer*2 sesionPipe
   Integer*1 abreComunicacionPI, \
             modoEscritura
   Integer*4 bufferSize
   String    nombrePipe$

!   On Error GoTo errOpenPi

   abreComunicacionPI = 0
   TS.ER.RETURN = -1
   Close sesionPipe
   If TS.ER.RETURN <> -1 Then Begin
   	  Call traceNUO("Err Close PI " + nombrePipe$ + " Err:" + err + hexanum$(errn) + " Ses:" + str$(sesionPipe))   
   EndIf
   If modoEscritura Then Begin 
   	  TS.ER.RETURN = -1
      Open nombrePipe$ As sesionPipe buffsize bufferSize Noread Nodel
      If TS.ER.RETURN <> -1 Then Begin
      	 Call traceNUO("Err Open PI " + nombrePipe$ + " Error " + err + "-"+ hexanum$(errn) + " Sesion:" + str$(sesionPipe))
      	 Exit Function 
      EndIf
   EndIf  Else Begin
   	  TS.ER.RETURN = -1   	
      Open nombrePipe$ As sesionPipe buffsize bufferSize
      If TS.ER.RETURN <> -1 Then Begin
      	 Call traceNUO("Err Open PI " + nombrePipe$ + " Err:" + err + hexanum$(errn) + " Ses:" + str$(sesionPipe))
      	 Exit Function 
      EndIf

   endif

   abreComunicacionPI = -1

   salir:
   Exit Function

   errOpenPi:
   
   call traceNUO("abreComunicacionPI " + nombrePipe$ + " ERROR " + err + hexanum$(errn) + " sesion " + str$(errf%))   
   
   if (err = "CU" or err = "FU") and (errf% = sesionPipe) then resume

   !Pipe no existe
   if err = "OE" and errn = 80104010H  and errf% = sesionPipe then begin
      call traceNUO( "abreComunicacionPI " + nombrePipe$ + " no existe, hora:" + time$ )
      resume salir
   endif

   !Pipe no liberado para apertura
   if err = "OE" and errn = 8010400CH and errf% = sesionPipe then begin
      call traceNUO( "abreComunicacionPI " + nombrePipe$ + " apertura bloqueada" )
      resume salir
   endif

   call traceNUO("abreComunicacionPI " + nombrePipe$ + " ERROR " + err + hexanum$(errn) + " sesion " + str$(errf%))
   if errf% = 39 then resume
   resume salir

end function

!------------------------------------------------------------------------------
! Recibe comunicacion de un pi:
! que fue escrito desde un programa java en el POS
!------------------------------------------------------------------------------
function recibeComunicacionPI( sesionPipe, nombrePipe$, bufferSize%, timeout ) public
   string recibeComunicacionPI,\
          nombrePipe$,\
          m$
   integer*2 sesionPipe
   integer*4 timeout, \
             segundo, \
             bufferSize%

   recibeComunicacionPI= ""
   m$= ""

   segundo = timeout / 1000
   if segundo < 1 then segundo = 1
   while segundo > 0
      call muestraVisor( 1, "Esperando respuesta", str$(segundo) + " segundos" )
      wait; 300
      if abreComunicacionPI( sesionPipe , nombrePipe$ , 0, bufferSize% ) then goto pipeDisponible
      segundo = segundo - 1
   wend

   call muestraVisor( 1, "No responde servicio", "de comunicacion" )
   wait; 5000
   goto salir

   pipeDisponible:
   while segundo > 0
      m$ = leeComunicacionPI( sesionPipe )
      if m$ <> "" then goto tengoRespuesta
      wait; 1000
      segundo = segundo - 1
   wend

   goto salir

   tengoRespuesta:
   recibeComunicacionPI = m$

   salir:
   call restauraVisor( 1 )

end function

!------------------------------------------------------------------------------
! crea un PIPE pi: para recibir mensajes desde un programa JAVA
! retorna -1 si el PIPE de escucha se ha creado
!          0 si no se pudo crear el PIPE de escucha
!------------------------------------------------------------------------------
function creaPIPE( sesionPipe, nombrePipe$, bufferSize) public
   integer*1 creaPIPE
   integer*2 sesionPipe
   integer*4 bufferSize
   string    nombrePipe$

   On Error GoTo creaError

   codErrorPipe = ""
   creaPIPE = 0
   Create nombrePipe$ As sesionPipe buffsize bufferSize
   creaPIPE = -1
   Call traceNUO("PIPE OK CREADO: "+nombrePipe$+" T:"+STR$(bufferSize))
   
   salir:
   Exit Function

   creaError:
   !Pipe no liberado para apertura
   Call traceNUO( "creaPIPE " + nombrePipe$ + " ERROR " + err + hexanum$(errn) + " sesion " + str$( errf% ))

   If errn = 8010400CH And errf% = sesionPipe Then Begin
      call traceNUO( "creaPIPE " + nombrePipe$ + " bloqueado" )
   EndIf 

   Call traceNUO( "creaPIPE " + nombrePipe$ + " ERROR " + err + hexanum$(errn) + " sesion " + str$( errf% ))
   codErrorPipe = err + hexanum$(errn)

   if errf% = 39 then resume
   resume salir

end function

!------------------------------------------------------------------------------
! Lee un pi: que fue escrito desde un programa java en el POS
! retorna el mensaje recibido o una cadena vacia
! si hubo un error se puede recuperar con la funcion errorPIPE
!------------------------------------------------------------------------------
function leePIPE( sesionPipe, pipeTimeOut ) public
   string    leePIPE, \
             f$, Rtam$, s$, p$
   integer*2 sesionPipe, gx%
   integer*4 pipeTimeOut, t, tw

   on error goto errorLee
   gx% = 0
   codErrorPipe = ""
   tw = 250 !tiempo necesario para que se escriban todos los bytes en el PIPE
   Rtam$ = ""
   t = pipeTimeOut / tw
   If t < 1 then t = 1
   Call tracenuo("Validando Promocion Local TO:"+Str$(pipeTimeOut)+" Rep:"+Str$(t))
   while t > 0
      If gx% > 4 Then gx% = 1
      If gx% = 1 Then p$ = "|"
      If gx% = 2 Then p$ = "/"
      If gx% = 3 Then p$ = "-"
      If gx% = 4 Then p$ = "\"
      gx% = gx% + 1
      Call muestraVisor( 1, "ESPERANDO RESPUESTA","ESPERE POR FAVOR "+p$)
      wait sesionPipe; tw
      if ( event% = sesionPipe ) then begin
         wait; tw
         read form "C5"; #sesionPipe; s$
         if esNumero( s$ ) then begin
            f$ = "C" + s$
            read form f$; #sesionPipe; Rtam$
            t = 0
         endif
      endif
      t = t - 1
   wend
   if t = 0 then begin
      codErrorPipe = "DO80B0000D"
      Rtam$ = ""
   endif

   salida:
   Call tracenuo("Termina Promocion Local Rep:"+Str$(t)+" Rta:"+Rtam$)
   leePIPE = Rtam$
   exit function

   errorLee:
   call traceNUO( "leePIPE ERROR " + err +" "+ hexanum$(errn) + " Ses:" + str$(errf%) )
   if err = "FU" and errf% = sesionPipe then begin
      call traceNUO( "leePIPE no esta abierto el PIPE")
   endif

   codErrorPipe = err + hexanum$(errn)

   if errf% = 39 then resume
   resume salida

end function

!------------------------------------------------------------------------------
! Retorna el codigo de error de la ultima operacion con un pi:
! "DO80B0000D" corresponde a un time out
!------------------------------------------------------------------------------
function errorPIPE public
   string errorPIPE
   errorPIPE = codErrorPipe
end function

!------------------------------------------------------------------------------
! Inicia una clase java
! Retorna -1 en caso de error
!------------------------------------------------------------------------------
function iniciaJava( nombre$ ) public
   integer*1 iniciaJava
   string nombre$

   integer*4 ret
   integer*2 p1

   iniciaJava = 0
   p1 = 0
   call adxserve(ret, 56, p1, nombre$)
   call traceNUO( "Inicia Clase " + nombre$ + " " + str$(p1) + " ret=" + hexanum$(ret) )
   if ret <> 0 then begin
      iniciaJava = -1
   endif

end function

!------------------------------------------------------------------------------
! Finaliza una clase java
! Retorna -1 en caso de error
!------------------------------------------------------------------------------
function finalizaJava( nombre$ ) public
   integer*1 finalizaJava
   string nombre$

   integer*4 ret
   integer*2 p1

   finalizaJava = 0
   p1 = 0
   call adxserve(ret, 57, p1, nombre$)
   call traceNUO( "Finaliza Clase " + nombre$ + " " + str$(p1) + " ret=" + hexanum$(ret) )
   if ret <> 0 then begin
      finalizaJava = -1
   endif

end function

!------------------------------------------------------------------------------
!Calculo check modulo 11
!------------------------------------------------------------------------------
function digitoModulo11$( dato$ ) public
   string    digitoModulo11$ ,dato$
   integer*1 f
   integer*2 i
   integer*4 largo, total

   largo = len( dato$ )
   f = 2
   total = 0
   for i = largo to 1 step -1
      total = total + f * val( mid$( dato$, i, 1) )
      f = f + 1
      if f = 8 then f = 2
   next i
   total = mod( total, 11 )
   if total > 1 then total = 11 - total
   digitoModulo11$ = str$( total )

end function

!------------------------------------------------------------------------------
! Devuelve el campo ubicado en la posicion 'campo' del string 'dato$'
! desempaquetando el campo
!------------------------------------------------------------------------------
function getCampoTSL$( dato$, campo ) public
   integer*2 i, k, campo
   string    dato$, getCampoTSL$, f$, t$

   t$ = dato$
   for i=1 to campo
      k = match( ":", t$, 1 )
      if(k > 0) then begin
         if k > 1 then \
            f$ = unpack$( left$( t$, k - 1 ) ) \
         else \
            f$ = ""
         if k < len(t$) then \
            t$ = right$( t$, len(t$) - k ) \
         else \
            t$ = ""
      endif else begin
         if len( t$ ) > 0 then begin
            f$ = unpack$(t$)
            t$ = ""
         endif else begin
            f$ = ""
         endif
      endif
   next i
   if len( f$ ) > 0 then begin
      if left$( f$, 1 ) = "?" then f$ = right$( f$, len( f$ ) - 1 )
      if left$( f$, 1 ) = "=" then f$ = "-" + right$( f$, len( f$ ) - 1 )
   endif
   getCampoTSL$ = f$
end function

!------------------------------------------------------------------------------
! Devuelve el campo ubicado en la posicion 'campo' del string 'dato$'
! sin desempaquetar el campo
!------------------------------------------------------------------------------
function getCampoASC$(dato$, campo) public
   integer*2 i, k, campo
   string    dato$, getCampoASC$, f$, t$

   t$ = dato$
   for i=1 to campo
      k = match(":", t$, 1)
      if(k > 0) then begin
         f$ = left$(t$,k-1)
         t$ = right$(t$, len(t$)-k)
      endif else begin
         f$ = t$
         t$ = ""
      endif
      getCampoASC$ = f$
   next i
end function

!------------------------------------------------------------------------------
! Devuelve el string ubicado en la posicion 'i' dentro de
! una transaccion en el TSL.
!------------------------------------------------------------------------------
function leeCadenaTSL$( p ) public
   string    leeCadenaTSL$
   integer*2 p

   leeCadenaTSL$ = ""
   if ( p > 0 ) and ( p <= SL.END ) then leeCadenaTSL$ = SL.STR$( p )

   end function

!------------------------------------------------------------------------------
! reemplaza el campo num en cadena$ con el contenido de campo$
! retorna la cadena modificada
!------------------------------------------------------------------------------
sub modificaCadenaTSL$( cadena$, campo$, num ) public
   string cadena$, campo$
   integer*1 num, n
   integer*2 i, k, r
   string  s$

   if num < 1 then exit sub
   r = len( cadena$ )
   n = num
   k = 0
   while n > 0
      i = k + 1
      k = match( ":", cadena$, i )
      if k < i then k = r + 1
      n = n - 1
   wend
   s$ = ""
   if i > 1 then s$ = left$( cadena$, i - 1 )
   cadena$  = s$ + campo$ + right$( cadena$, r - k + 1 )

end sub

!------------------------------------------------------------------------------
! escribe una cadena en el arreglo de transacciones
! INDEX posicion a escribir
! SOURCE$ cadena a escribir
!------------------------------------------------------------------------------
function escribeCadenaTSL( INDEX, SOURCE$ ) PUBLIC
   INTEGER*2 INDEX, n
   STRING SOURCE$

   n = len( SL.STR$( INDEX ) )
   if n > 0 then SL.LEN.TOTAL = SL.LEN.TOTAL - n - 3
   SL.STR$( INDEX ) = SOURCE$
   n = len( SOURCE$ )
   if n > 0 then SL.LEN.TOTAL = SL.LEN.TOTAL + n + 3

   SOURCE$ = ""
end function

!------------------------------------------------------------------------------
! agrega una cadena al arreglo de TSL
! no se debe llamar desde la user 68
!------------------------------------------------------------------------------
sub agregaCadenaTSL( cadena$ ) public
   string cadena$

   if SL.END < GC.SL.SIZE then begin
      SL.END = SL.END + 1
      call escribeCadenaTSL( SL.END, cadena$ )
      if SL.END > SAVE.SL.END then begin
         SAVE.SL.END = SL.END
      endif
   endif

end sub

!------------------------------------------------------------------------------
! agrega una cadena al arreglo de TSL
! solo para ser llamada desde la user 68
!------------------------------------------------------------------------------
sub agregaCadenaTSL68( cadena$ ) public
   string cadena$

   if SL.END < GC.SL.SIZE then begin
      call escribeCadenaTSL( SL.END, cadena$ )
      SL.END = SL.END + 1
      if SL.END > SAVE.SL.END then begin
         SAVE.SL.END = SL.END
      endif
   endif

end sub

!------------------------------------------------------------------------------
! evita agregar una cadena al arreglo de TSL en user 68
!------------------------------------------------------------------------------
sub evitaCadenaTSL( ELEMENT, ENTRY$ ) public
   integer*2 ELEMENT
   string ENTRY$

   ELEMENT = ELEMENT - 1
   SL.END = SL.END - 1
   ENTRY$  = SL.STR$( ELEMENT )

end sub

!------------------------------------------------------------------------------
! inserta una cadena en el arreglo de TSL
!------------------------------------------------------------------------------
sub insertaCadenaTSL( cadena$, posicion ) public
   string cadena$, s$, r$
   integer*2 posicion, i

   r$ = cadena$
   i = posicion
   while i <= SL.END
      s$ = SL.STR$(i)
      SL.STR$(i) = r$
      r$ = s$
      i = i + 1
   wend
   call agregaCadenaTSL( r$ )

end sub

!------------------------------------------------------------------------------
! elimina cadenas en el arreglo de TSL
!------------------------------------------------------------------------------
sub eliminaCadenaTSL( posicion, numero ) public
   integer*2 posicion, numero, i, k, n

   i = posicion
   k = posicion + numero
   while k <= SL.END

!call traceNUO( "mueve cadena [" + str$( i ) + "] <-- [" + str$(k) + "]" )
!call traceNUO( cadenaHex$( SL.STR$(i) ) )
!call traceNUO( cadenaHex$( SL.STR$(k) ) )

      call escribeCadenaTSL( i, SL.STR$(k) )

      i = i + 1
      k = k + 1
   wend

   while i < k

!call traceNUO( "limpia cadena [" + str$( i ) + "]" )
!call traceNUO( cadenaHex$( SL.STR$(i) ) )

      call escribeCadenaTSL( i, "" )
      i = i + 1
   wend
   if SL.END > 1 then SL.END = SL.END - numero
   if SL.END < ( posicion - 1 ) then SL.END = posicion - 1
end sub

!------------------------------------------------------------------------------
!retorna un numero de sesion disponible
!------------------------------------------------------------------------------
function sesionDisponible public
   integer*1 sesionDisponible

   string    nombre$
   integer*1 i, k, f
   integer*2 p1
   integer*4 ret
   string    p2

   on error goto err.disponible

   call adxserve(ret, 4, p1, p2)

   sesionDisponible = 0
   nombre$ = "R::C:/TEMP/SESS" + mid$(p2, 8, 3) + ".TXT"
   k = 0
   i = 1
   while i < 106
      f = -1
      create nombre$ as i nowrite nodel
      if f then k = i
      close i
      if k > 0 then begin
         sesionDisponible = k
         goto fin.disponible
      endif
      i = i + 1
   wend

   fin.disponible:
   if k > 0 then begin
      create nombre$ as k nowrite nodel
      delete k
   endif
   exit function

   err.disponible:
   if errf% = 39 then resume
   !Error DF00000014 Numero de sesion en uso
   if errn = 14H then begin
      f = 0
      resume
   endif

   !Error NF00000044 numero de sesion fuera de rango
   if errn = 44H then begin
      resume fin.disponible
   endif

   !Error ER0000001E numero de sesion no permitido (superior a 104)
   if errn = 1EH then begin
      resume fin.disponible
   endif

   call traceNUO( "sesionDisponible ERROR " + err + hexanum$(errn) + " sesion " + str$(errf%) )
   resume fin.disponible

end function

!------------------------------------------------------------------------------
! busca el campo n esimo en men$
! los campos estan separados por sep$
!------------------------------------------------------------------------------
function buscaCampo(men$, sep$, num) public
   string    buscaCampo, men$, sep$
   integer*2 num

   integer*2 f, i, k, h

   buscaCampo = ""
   h = len(sep$)
   men$ = men$ + sep$
   k = 1 - h
   f = 0
   while f < num
      i = k + h
      k = match(sep$, men$, i)
      if k = 0 then exit function
      f = f + 1
   wend
   buscaCampo = mid$(men$, i, k - i)

end function

!------------------------------------------------------------------------------
! busca valores entre 0 y 255 separados por ; en men$
! retorna una cadena con los caracteres correspondientes a los valores
!------------------------------------------------------------------------------
function buscaValores$( m$ ) public
   string    buscaValores$, m$, s$, r$, c$
   integer*2 i, k, n, v

   r$ = ""
   buscaValores$ = r$
   s$ = ";"
   n = len( m$ )
   if right$( m$, n ) <> s$ then begin
      m$ = m$ + s$
      n = n + 1
   endif
   i = 1
   while i < n
      k = matchb( s$, m$, i )
      if k > i then begin
         c$ = mid$( m$, i, k - i )
         if len( c$ ) < 4 then if esNumero( c$ ) then begin
            v = val( c$ )
            if v < 256 then r$ = r$ + chr$( v )
            i = k
         endif
      endif
      i = i + 1
   wend
   buscaValores$ = r$

end function

!------------------------------------------------------------------------------
!Retorna la cantidad de veces que se repite 'texto$' en 'cadena$'
!------------------------------------------------------------------------------
function cuentaCadenas( cadena$ , texto$ ) public
integer*2 cuentaCadenas,\
          nroVeces,     \
          i,            \
          k
string    cadena$,      \ Cadena de texto a evaluar
          texto$        ! Texto a buscar en 'cadena'

   cuentaCadenas = 0
   nroVeces = 0
   k = len( cadena$ )
   i = 1
   while i <= k
      if mid$( cadena$, i, len( texto$ ) ) = texto$ then nroVeces = nroVeces + 1
      i = i + 1
   wend
   cuentaCadenas = nroVeces
end function

!------------------------------------------------------------------------------
! Elimina todas las instancias de un caracter en la cadena
!------------------------------------------------------------------------------
function eliminaCar$( cadena$, caracter$ ) public
   string    eliminaCar$, cadena$, caracter$
   string    v$, c$
   integer*2 i, k

   v$ = ""
   k = len( cadena$ )
   i = 1
   while i <= k
      c$ = mid$( cadena$, i, 1 )
      if c$ <> caracter$ then v$ = v$ + c$
      i = i + 1
   wend

   eliminaCar$ = v$

end function

!------------------------------------------------------------------------------
! Elimina todas las instancias de un caracter a la izquierda de la cadena
!------------------------------------------------------------------------------
function eliminaCarIzq$( cadena$, caracter$ ) public
   string    eliminaCarIzq$, cadena$, caracter$
   string    v$, c$
   integer*2 i, k

   v$ = ""
   k = len( cadena$ )
   i = 1
   while i <= k
      c$ = mid$( cadena$, i, 1 )
      if c$ <> caracter$ then begin
         v$ = right$( cadena$, k - i + 1 )
         i = k
      endif
      i = i + 1
   wend

   eliminaCarIzq$ = v$

end function

!------------------------------------------------------------------------------
! Obtiene el valor de un numero con separador de miles y decimales de una cadena
! si la cadena contiene "1,024.56" retorna "102456"
! si la cadena contiene "0.23" retorna "23"
! si la cadena contiene "15.2" retorna "1520"
!------------------------------------------------------------------------------
function valorCadena$( cadena$ ) public
   string    valorCadena$, cadena$

   string    v$, c$
   integer*2 i

   v$ = ""
   i = 1
   while i <= len( cadena$ )
      c$ = mid$( cadena$, i, 1 )
      if c$ = "," then c$ = ""
      if c$ = "." then c$ = ","
      v$ = v$ + c$
      i = i + 1
   wend

   valorCadena$ = str$( 100.0 * val( v$ ) )

end function

!------------------------------------------------------------------------------
!Graba una transaccion de dotacion con cadenas de usuario
!num    numero de cadenas de usuario
!monto$ monto total de la dotacion
!mp$    medio de pago
!cad$   cadenas de usuario para agregar
!se incrementa el valor de la variable SL.HD.TRANSNUM
!------------------------------------------------------------------------------
sub grabaDotacion( num, monto$, mp$, cad$ ) public
   integer*1 num
   string    monto$, mp$, cad$
   integer*1 numstr
   integer*2 i
   string    mat1$(1), s1$, s2$

   on error goto err.grabaDotacion

   numstr = num + 1
   dim mat1$(numstr)
   SL.HD.TRANSNUM = SL.HD.TRANSNUM + 1

   mat1$(1) = chr$(00H) + ":" + TS.TERMINALP$ + ":" + \
      pack$( right$( "0000" + str$( SL.HD.TRANSNUM ), 4 ) ) + ":" + \
      pack$( date$ + left$( time$, 4 ) ) + ":" + \
      pack$("03") + ":" + pack$( str$( numstr ) ) + ":" + \
      SL.HD.OPERATOR$ + ":" + SL.HD.PASSWORD$ + ":::" + \
      pack$("14") + ":" + pack$("3") + "::" + \
      pack$("46") + ":" + pack$("0")

   if esNumero( monto$ ) then \
      s1$ = pack$( monto$ ) \
   else \
      s1$ = ""

   if esNumero( mp$ ) then \
      s2$ = pack$( mp$ ) \
   else \
      s2$ = pack$( "10" )

   mat1$(1) = mat1$(1) + \
      chr$( 22H ) + chr$( 2CH ) + chr$( 22H ) + \
      chr$( 13H ) + ":" + pack$( "1" ) + ":" + \
      s1$ + ":::" + \
      s2$ + ":" + s1$ + \
      ":" + chr$( 99H )

   mat1$(2) = cad$

   write matrix #27; mat1$(1), numstr

   ! guarda variable del terminal status EAMTERMS
   TE.TR.NUMLOANS = TE.TR.NUMLOANS + 1 ! Suma 1 dotacion
   TS.PLC.NUM     = TE.TR.NUMLOANS

   i = val( left$( mp$, 1 ) )
   TE.TR.AMT( i ) = TE.TR.AMT( i ) + val( monto$ )
   TE.TR.AMTLOANS = TE.TR.AMTLOANS + val( monto$ )

   TS.TEMP1I2 = 5                        ! end of tnx
   CALL TSSFEC01                         ! update status

   fin.grabaDotacion:
   exit sub

   err.grabaDotacion:
   if errf% = 39 then resume
   if errf% <> 27 then resume
   call traceNUO( "grabaDotacion ERROR " + err + hexanum$(errn) )
   resume fin.grabaDotacion

end sub

!------------------------------------------------------------------------------
!Graba una transaccion de retiro con cadenas de usuario
!num    numero de cadenas de usuario
!monto$ monto total de la dotacion
!mp$    medio de pago
!cad$   cadenas de usuario para agregar
!se incrementa el valor de la variable SL.HD.TRANSNUM
!------------------------------------------------------------------------------
sub grabaRetiro( num, monto$, mp$, cad$ ) public
   integer*1 num
   string    monto$, mp$, cad$
   integer*1 numstr
   string    mat1$(1), s1$, s2$
   integer*2 i

   on error goto err.grabaRetiro

   numstr = num + 1
   dim mat1$(numstr)
   SL.HD.TRANSNUM = SL.HD.TRANSNUM + 1

   mat1$( 1 ) = chr$( 00H ) + ":" + TS.TERMINALP$ + ":" + \
      pack$( right$( "0000" + str$( SL.HD.TRANSNUM ), 4 ) ) + ":" + \
      pack$( date$ + left$( time$, 4 ) ) + ":" + \
      pack$( "04" ) + ":" + pack$( str$( numstr ) ) + ":" + \
      SL.HD.OPERATOR$ + ":" + SL.HD.PASSWORD$ + ":::" + \
      pack$( "14" ) + ":" + pack$( "3" ) + "::" + \
      pack$( "46" ) + ":" + pack$( "0" )

   if esNumero( monto$ ) then \
      s1$ = pack$( monto$ ) \
   else \
      s1$ = ""

   if esNumero( mp$ ) then \
      s2$ = pack$( mp$ ) \
   else \
      s2$ = pack$( "10" )

   mat1$( 1 ) = mat1$( 1 ) + \
      chr$( 22H ) + chr$( 2CH ) + chr$( 22H ) + \
      chr$( 13H ) + ":::" + pack$( "1" ) + ":" + \
      s1$ + ":" + chr$( 99H ) + ":" + \
      s2$ + ":" + s1$

   mat1$(2) = cad$

   write matrix #27; mat1$(1), numstr

   ! guarda variable del terminal status EAMTERMS
   TE.TR.NUMPKUPS = TE.TR.NUMPKUPS + 1
   TS.PLC.NUM     = TE.TR.NUMPKUPS

   i = val( left$( mp$, 1 ) )
   TE.TR.AMT( i ) = TE.TR.AMT( i ) - val( monto$ )
   TE.TR.AMTPKUPS = TE.TR.AMTPKUPS + val( monto$ )

   TS.TEMP1I2 = 5                        ! end of tnx
   CALL TSSFEC01                         ! update status

   fin.grabaRetiro:
   exit sub

   err.grabaRetiro:
   if errf% = 39 then resume
   if errf% <> 27 then resume
   call traceNUO( "grabaRetiro ERROR " + err + hexanum$(errn) )
   resume fin.grabaRetiro

end sub

!------------------------------------------------------------------------------
! graba transaccion 99 al tsl
!num    numero de cadenas
!trx99$ contiene las cadenas separadas por "&|&"
!se incrementa el valor de la variable SL.HD.TRANSNUM
!------------------------------------------------------------------------------
sub grabatrx99(num, trx99$) public
   string trx99$,s$
   integer*2 num

   string mat.trx99$(1)
   integer*2 n, i, j

   on error goto err.loc

   dim mat.trx99$( num + 1 )
   SL.HD.TRANSNUM = SL.HD.TRANSNUM + 1

   n = 1
   s$ = unpack$(TS.OPER$)
   s$ = str$(val(s$))
   mat.trx99$(n) = chr$(99H) + ":" + \
      TS.TERMINALP$ + ":" + \
      pack$(right$("0000"+str$(SL.HD.TRANSNUM),4)) + ":" + \
      pack$(date$+left$(time$,4)) + ":" + \
      chr$(99H) + ":" + \
      pack$(str$(num)) + ":" + \
      pack$(s$)
   i = 1
   while i < len(trx99$)
      n = n + 1
      j = match("&|&", trx99$, i)
      if j = 0 then j = len( trx99$ ) + 1
      mat.trx99$(n) = mid$( trx99$, i, j - i )
      i = j + 3
   wend

   write matrix #27; mat.trx99$(1), n

   dim mat.trx99$(1)

   TS.TEMP1I2 = 5                        ! end of tnx
   CALL TSSFEC01                         ! update status

   sale:
   exit sub

   err.loc:
   if errf% = 39 then resume
   call traceNUO( "grabatrx99 ERROR " + err + hexanum$(errn) )
   resume sale

end sub

!------------------------------------------------------------------------------
!lee el nombre de un operador de cajas
!------------------------------------------------------------------------------
function leeOperador$( idOpe$ ) public
   string leeOperador$, idOpe$
   string a$, nom$

   nom$ = idOpe$
   a$ = pack$( right$( string$( 10, "0" ) + idOpe$, 10 ) )
   TS.ER.RETURN = -1
   open "R::EAMOPERA" keyed recl 72 as 38 nowrite nodel
   if TS.ER.RETURN = -1 then begin
      read form "C5, C67"; #38 key a$; a$, nom$
      leeOperador$ = mid$(nom$, 28, 20)
      close 38
   endif

end function

!------------------------------------------------------------------------------
!Calcula digito de chequeo EAN13
!ean$ contiene el numero sobre el que se calcula el digito verificador
!retorna el digito verificador calculado
!retorna -1 si ean$ contiene caracteres no numerico
!------------------------------------------------------------------------------
function digitoEAN( ean$ ) public
   integer*1 digitoEAN
   string ean$

   integer*1 i, n, v
   integer*2 s

   digitoEAN = -1
   if not esNumero( ean$ ) then exit function
   n = len( ean$ )
   if n <> 12 then exit function
   s = 0
   i = 1
   while (i <= n)
      v = val( mid$( ean$, i, 1 ) )
      if mod( i, 2 ) then begin
         s = s + v
      endif else begin
         s = s + 3 * v
      endif
      i = i + 1
   wend
   v = mod( s, 10 )
   if v <> 0 then v = 10 - v
   digitoEAN = v

end function

!------------------------------------------------------------------------------
!Calcula digito de chequeo modulo 10
!num$ contiene el numero sobre el que se calcula el digito verificador
!retorna el digito verificador calculado
!retorna -1 si num$ contiene caracteres no numerico
!------------------------------------------------------------------------------
function modulo10( num$ ) public
   integer*1 modulo10
   string    num$
   integer*1 i, n, v
   integer*2 s

   modulo10 = -1
   if not esNumero( num$ ) then exit function
   s = 0
   n = len( num$ )
   i = 1
   while i <= n
      v = val( mid$( num$, i, 1 ) )
      if mod( i, 2 ) then begin
         v = 2 * v
         if v > 9 then v = v - 9
      endif
      s = s + v
      i = i + 1
   wend
   v = mod( s, 10 )
   if v <> 0 then v = 10 - v
   modulo10 = v
end function

!------------------------------------------------------------------------------
!Verifica cedula
!num$ contiene el numero de cedula con el digito verificador al final
!retorna -1 el numero de cedula es correcto
!         0 el numero de cedula no es correcto
!------------------------------------------------------------------------------
function verificaCedula( num$ ) public
   integer*1 verificaCedula, d
   string    num$
   verificaCedula = 0
   d = len( num$ )
   if d <> 10 then Exit Function
   If Val(num$) <= 0 Then Exit Function      ! Cedula no valida Add GR 03Jul2025
   d = modulo10( left$( num$, d - 1 ) )
   if d = -1 then exit function
!call traceNUO( "CEDULA [" + num$ + "-" + str$(d) + "]" )
   if d <> val( right$( num$, 1 ) ) then exit function
   verificaCedula = -1
end function

!------------------------------------------------------------------------------
! Calcula clave de acceso SRI
!------------------------------------------------------------------------------
function claveAccesoSRI$( tipoDocumento, secuenciaDocumento, serie$, rucEstablecimiento$, tipoAmbiente$, secuenciaSRI$, tipoEmision$) public
   string \
      claveAccesoSRI$, \
      check$, \
      dato$, \
      tipoDocumentoSRI$, \
      serie$, \
      rucEstablecimiento$, \
      tipoAmbiente$, \
      secuenciaSRI$, \
      tipoEmision$

   integer*1 \
      tipoDocumento

   integer*4 secuenciaDocumento

   tipoDocumentoSRI$ = right$( string$( 2, "00" ) + str$( tipoDocumento ), 2 )
   dato$ = mid$(DATE$,5,2) + mid$(DATE$,3,2) + "20"+mid$(DATE$,1,2) + \ ! ddmmaaaa                  C8
      tipoDocumentoSRI$ + \                                             ! Tipo de comprobante       C2
      right$("0000000000000"+  rucEstablecimiento$ ,13) + \             ! Ruc del establecimiento   C13
      tipoAmbiente$ + \                                                 ! Tipo de Ambiente          C1
      serie$ + \                                                        ! Prefijo y terminal        C6
      right$("000000000"+ str$(secuenciaDocumento),9) + \               ! Secuencial                C9
      right$("00000000" + secuenciaSRI$, 8) + \                         ! Codigo Secuencia SRI      C8
      tipoEmision$                                                      ! Tipo de emision           C1

   ! Calculo digito chequeo Mod 11
   check$ = digitoModulo11$( dato$ )
   claveAccesoSRI$ = dato$ + check$
end function

Function EsCadenaPago( cad$ ) public
   integer*1 EsCadenaPago
   string cad$

   EsCadenaPago = 0
   f$ = getCampoTSL$(cad$,1)
   if match(f$ + "|","05|06|",1) = 0 then exit function
   dim NUOuserData$(6)
   NUOuserData$(1) = f$
   NUOuserData$(2) = getCampoTSL$(cad$,2)
   NUOuserData$(3) = getCampoTSL$(cad$,3)
   NUOuserData$(4) = getCampoTSL$(cad$,4)
   NUOuserData$(5) = getCampoTSL$(cad$,5)
   NUOuserData$(6) = getCampoTSL$(cad$,6)
   EsCadenaPago = -1

end function

!------------------------------------------------------------------------------
! lista el directorio de un disco RAM X:, Y: o C:
!------------------------------------------------------------------------------
sub directorioRAM( disco$ ) public
   string disco$
   string s$, t$
   integer*4 i

   s$ = disco$
   call traceNUO("CONTENIDO DISCO " + s$)
   i = 0
   call ADXDIR(i, s$, t$, 0)
   call traceNUO(t$)
   while i = 0
      call ADXDIR(i, s$, t$, 1)
      call traceNUO(t$)
   wend

end sub

!------------------------------------------------------------------------------
! Terminal online/offline 0 = offline 1 = online
!------------------------------------------------------------------------------
function estadd public
   integer*1 estadd
   string s$

   estadd = 1
   call adxserve( ts.temp4i4, 4, 0, s$ ) 
   call traceNUO("on/off " + mid$(s$, 11, 1))
   if mid$(s$, 11,1) = "0" then estadd = 0

fend

!------------------------------------------------------------------------------
! Imprime un codigo QR conteniendo la cadena s$
!    #select page mode \x1b\x4c
!    #select Scale of QR code.
!    Note 0 or 1 will work \x1d\x5f\x00
!    #left margin = 0 \x1b\x24\x00\x00
!    #Set Vertical position - down 180 = x00b4 \x1d\x24\x00\xb4
!    #setup size of page \x1b\x58\x00\x00\x00\x3c\x01\x90\x00\xcc
!    #QRCODE Left side \x1d\x4f\x04\x00\x1a data /x00
!    #set up position for QRcode Right side - Move horizontal position over 200 dots
!    \x1b\x24\x00\xc8
!    #QRCODE Right side \x1d\x4f\x04\x00\x1a data /x00
!    #Print Page \x1b\x0c
!    #select Standard Mode \x1b\x4f
!------------------------------------------------------------------------------
sub imprimeQR( s$ ) public
   string s$, q$

   !escala 0 -> 3 puntos por pixel, 5 es el valor por defecto
   !write #34; chr$(1DH) + chr$(5FH) + chr$(5)

   q$ = chr$(1DH) + chr$(4FH) + chr$(0) + chr$(1) + chr$(0) + \
        s$ + chr$(0)
   call imprimeMuchos(q$)
end sub
