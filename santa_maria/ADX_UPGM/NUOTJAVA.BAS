!-------------------------------------------------------------------------------
!Interfaz para java
!Fecha: 22 de febrero de 2022
!Autor: Alejandro Farré P.
!-------------------------------------------------------------------------------
%ENVIRON T

%INCLUDE EAMTSWKG.J86
%INCLUDE JAVAGUIV.J86

! system services routines
%INCLUDE EAMADXRT.J86
%INCLUDE JAVA_EXT.J86
%INCLUDE NUOTRUTI.J86
%INCLUDE JAVAGUIC.J86
%INCLUDE BASROUT.J86

!-------------------------------------------------------------------------------
! invoca un metodo de una clase java
! clase$  nombre de la clase
! metodo$ nombre del metodo
! param$  parametro a enviar
! retorna una cadena con el resultado
! si primer caracter es E hubo un error en la invocacion
!-------------------------------------------------------------------------------
function invocaMetodo$( clase$, metodo$, param$ ) public
   string    invocaMetodo$, clase$, metodo$, param$
   string    returnValue$, exception$, request$

   request$ = "C$"
   exception$ = ""
   invocaMetodo$ = ""

   ! Invoca metodo de la clase y le pasa parametros
   call JavaCall.Initialize.Request( clase$, metodo$, request$ )
   call traceNUO( "request$=[" + request$ + "]" )

   ! Agrega parametro
   call JavaCall.Addparameter.String( request$, param$ )
   call traceNUO( "request$=[" + request$ + "]" ) 

   !Ejecuta clase
   call JavaCall.Invokemethod.ReturnString( request$, returnValue$, exception$ )
   call traceNUO( "exception$=[" + exception$ + "]" )

   TS.PRTBUF$ = exception$
   call javaEvent(updateReceiptMsg)
   TS.PRTBUF$ = ""

   ! Si Hay Exception
   if len( exception$ ) > 0 then begin
      call traceNUO( "Error al invocar la clase " + clase$ + " metodo " + metodo$ + " " + exception$ )
      invocaMetodo$ = "E" + exception$
   endif else begin
      call traceNUO( "Resultado " + returnValue$ )
      invocaMetodo$ = "R" + returnValue$
   endif

end function

!------------------------------------------------------------------------------
!graba trace y muestra linea en la GUI
!------------------------------------------------------------------------------
sub muestraGUI( s$ )
   string s$
   call traceNUO( s$ )
   TS.PRTBUF$ = s$
   call javaEvent(updateReceiptMsg)
   TS.PRTBUF$ = ""
end sub

!--------------------------------------------------------------------------
!Eliminar los ceros que estan a la derecha de la cadena
!--------------------------------------------------------------------------
function eliminaCeros$( v$ )
   string    eliminaCeros$, v$
   integer*2 k

   eliminaCeros$ = ""
   k = len(v$)
   while k > 0
      if mid$(v$, k, 1) = chr$(0) then begin
         k = k - 1
      endif else begin
         eliminaCeros$ = left$( v$, k )
         k = 0
      endif
   wend
   
fend

!------------------------------------------------------------------------------
! lista archivos
!------------------------------------------------------------------------------
! Offs  Type                    Description     Notes
!  0    integer*4               key             use extract4 to access
!  4    string (18 bytes)       file name
! 22    integer*2               attributes      use extract2 to access
!                                               0x01 - Read Only
!                                               0x02 - Hidden
!                                               0x04 - System
!                                               0x08 - Volume Label
!                                               0x10 - Subdirectory
!                                               0x20 - Archive (Files)
!                                               0x40 - Security enabled
!                                               0x80 - Reserved
!                                               0x0000 - Local
!                                               0x4000 - Mirrored/Update
!                                               0xC000 - Mirrored/Close
!
!                                               0x6000 - Compound/Update
!                                               0xE000 - Compound/Close
! 24    integer*2               record size     use extract2 to access
! 26    integer*1               User ID of Owner
! 27    integer*1               Group ID of Owner
! 28    integer*2               File Security
! 30    string (6 bytes)        Reserved
! 36    integer*4               File Size       use extract4 to access
! 40    integer*2               Year            use extract2 to access
! 42    integer*1               Month
! 43    integer*1               Day
! 44    integer*1               Hour
! 45    integer*1               Minute
! 46    integer*1               Second
! 47    integer*1               Reserved
function listaArchivos( path$ ) public
   integer*1 listaArchivos
   string path$, n$, b$, s$, f$, t$
   integer*4 ret, num, tot, v
   integer*2 attributes

   listaArchivos = 0
   num = 0
   tot = 0
   n$ = path$ + "\*.*"
   b$ = string$( 48, chr$(0) )

   ret = srchdir( n$, b$ )
   if ret = -1 then begin
      call muestraGUI( "no existen la carpeta [" + path$ + "]" )
      listaArchivos = -1
      exit function
   endif
   if ret < 0 then begin
      call muestraGUI( "ERROR " + hexanum$(ret) + " al listar la carpeta [" + path$ + "]" )
      listaArchivos = -1
      exit function
   endif

   s$ = "CARPETA: " + path$
   call muestraGUI( s$ )

   ret = 1
   while ret
      ret = srchdir( n$, b$ )
      if ret < 0 then begin
         call muestraGUI( "ERROR " + hexanum$(ret) + " al listar la carpeta [" + path$ + "]" )
         listaArchivos = -1
         ret = 0
      endif else if ret > 0 then begin

         !Fecha
         f$ = right$( "00" + str$( cextract1( b$, 44 ) ), 2 ) + "-" + \
              right$( "00" + str$( cextract1( b$, 43 ) ), 2 ) + "-" + \
              right$( string$( 4, "0" ) + str$( cextract2( b$, 41 ) ), 4 ) + " " + \
              right$( "00" + str$( cextract1( b$, 45 ) ), 2 ) + ":" + \
              right$( "00" + str$( cextract1( b$, 46 ) ), 2 ) + " "

         !nombre
         s$ = eliminaCeros$( mid$( b$, 5, 18 ) )

         !atributos
         attributes = cextract2( b$, 23 )

         !tamaño
         v = cextract4( b$, 37 )

         !Es directorio
         if attributes = 10H then begin
            t$ = "<DIR>    "
         endif else begin
            t$ = right$( string$( 8, " " ) + str$( v ), 8 ) + " "
            tot = tot + v
            num = num + 1
         endif

         call muestraGUI( f$ + t$ + s$ )
      endif
   wend

   s$ = right$( string$( 10, " " ) + str$( num ), 10 ) + " Archivos " + \
        right$( string$( 10, " " ) + str$( tot ), 10 ) + " Bytes "
   call muestraGUI( s$ )

end function

!------------------------------------------------------------------------------
! ejecuta las instrucciones del archivo c:/TEMP/COMANDOS.TXT
!------------------------------------------------------------------------------
function instrucciones public
   integer*1 instrucciones
   string arc$, reg$, cl$, me$, pa$, co$, di$
   integer*2 k
   integer*4 v

   instrucciones = -1
   TS.ER.RETURN = -1
   arc$ = "R::C:/TEMP/COMANDOS.TXT"
   open arc$ as 79
   if TS.ER.RETURN <> -1 then begin
      call muestraGUI( "Error " + TS.TS11WERR$ + " al abrir " + arc$ )
      exit function
   endif

   if end #79 then salir
   while (1)
      read #79; reg$
      if len( reg$ ) > 0 then \
      if left$( reg$, 1 ) <> "#" then begin
         call muestraGUI( "Comando: [" + reg$ + "]" )
         k = match( " ", reg$, 1 )
         if k > 1 then begin
            co$ = left$( reg$, k - 1 )
            reg$ = right$( reg$, len( reg$ ) - k )
            if len( reg$ ) > 0 then begin
               if co$ = "DIRECTORIO" then call listaArchivos( reg$ )
               if co$ = "DISCO" then gosub disco
               if co$ = "PROGRAMA" then gosub programa
               if co$ = "INVOCA" then gosub invoca
               if co$ = "CLASE" then call iniciaJava( reg$ )
               if co$ = "CLASEFIN" then call finalizaJava( reg$ )
            endif
         endif
      endif
   wend

   salir:
   call muestraGUI( "Fin del archivo " + arc$ )
   close 79
   instrucciones = 0
   exit function

   invoca:
   k = match( " ", reg$, 1 )
   if k < 2 then return
   cl$ = left$( reg$, k - 1 )
   reg$ = right$( reg$, len( reg$ ) - k )
   k = match( " ", reg$, 1 )
   if k < 2 then return
   me$ = left$( reg$, k - 1 )
   pa$ = right$( reg$, len( reg$ ) - k )
   call muestraGUI( "INVOCA CLASE [" + cl$ + "] METODO [" + me$ + "] PARAMETROS [" + pa$ + "]" )
   reg$ = invocaMetodo$( cl$, me$, pa$ )
   call muestraGUI( reg$ )
   return

   disco:
   v = disksize( reg$ )
   if v < 0 then begin
      call muestraGUI( "Error " + hexanum$( v ) + " DISCO " + reg$ )
      return
   endif
   reg$ = "tamaño del disco " + reg$ + " " + str$(v) + " bytes"
   call muestraGUI( reg$ )
   return

   programa:
   v = osshell( reg$ )
   if v < 0 then begin
      call muestraGUI( "Error " + hexanum$( v ) + " en programa [" + reg$ + "]" )
      return
   endif
   reg$ = "programa " + reg$ + " ejecutado"
   call muestraGUI( reg$ )
   return

end function