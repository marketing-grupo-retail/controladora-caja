!------------------------------------------------------------------------------
! Modulo MSMTSEGU.BAS cambio de password cajero en POS
! Fecha: 16 de mayo de 2022
! Autor: Carlos calderon
!        Obtenido del modulo IVASTS28.011 de fuentes V6R5
!
! Observaciones: * Esta funcionalidad se base en que cuando un cajero se loguea
!                  en un POS y su password es 123456, el POS debe solicitar que
!                  realice un cambio de password.
!                * La nueva password no puede ser 123456, ya que esa condicion
!                  es la que gatilla cambio de password.
!                * Las password ya utilizadas se almacenan en un archivo para
!                  impedir su resiclaje.
!                * Se conservan la historia de las 8 ultimas password utilizadas
!                * Cantidad de digitos de la nueva password maximo 8 minimo 4 caracteres
!                * Se requiere de una archivo HICAMCLA --> Histori CAMbio CLAve
!                                             organizacion   keyed
!                                             distribucion   2 mirrow Perupdate
!                                             Largo key      5
!                                             Largo registro 40            
!                * Se requiere definir el nombre logico HICAMCLA en CC y DD
!                  ADXLXACN::ADX_UDT1:HICAMCLA.DAT
!------------------------------------------------------------------------------

%ENVIRON T

INTEGER*2 GLOBAL          \
          TS.ER.RETURN    ! INDICATES WHETHER APPLICATION WISHES TO
                          ! HANDLE ERROR LOCALLY

STRING GLOBAL             \
          TS.OPER$,       \ OPERATOR NUMBER (PACKED 5)
          TS.PSWD$        ! PASSWORD (PACKED 4)

%INCLUDE JAVAGUIV.J86

SUB TSHIECET EXTERNAL     ! Issue a tone warning
END SUB

%INCLUDE NUOTRUTI.J86
%INCLUDE JAVAGUIC.J86

function passwordUtilizada( newPassw$, allPassw$ )
   integer*1 passwordUtilizada,  \
             f%
   
   string newPassw$,             \
          allPassw$,             \
          dato$                  !

   passwordUtilizada = 0

   dato$ = unpack$(allPassw$)

!call traceNuo("TODAS PASS=[" + dato$ + "] Largo=[" + str$(len(dato$)) + "]")
!call traceNuo("NEW   PASS=[" + newPassw$ + "]")

   for f% = 1 to len(dato$)/8
      if newPassw$ = mid$( dato$, f%*8-7,8) then begin
         passwordUtilizada = f%
         !call traceNuo("PASSW YA UTILIZADA")
         exit function
      endif
   next f%
end function

sub MSMTSEGU28 public
   integer*1 K%,           \
             enablePass%,  \
             i%            !

   string IdPassw$,        \
          IdNombre$,       \
          Idresto1$,       \
          Idresto2$,       \
          operName$,       \
          dato$,           \
          pasFecha$,	   \
          msmClaves$,      \
          msmPass1$,       \
          msmAux1$,        \
          msmForm$

   TS.ER.RETURN = -1
   open "R::EAMOPERA" keyed recl 72 as 38 nowrite nodel
   if TS.ER.RETURN then begin
!call traceNuo("OK OPEN EAMOPERA")
      read form "T6 C4 C23 C20 C20"; #38 KEY TS.OPER$; IdPassw$,  \
                                                       Idresto1$, \
                                                       IdNombre$, \
                                                       Idresto2$  !
      if not TS.ER.RETURN then begin
         call muestraBorrar( "USUARIO NO EXISTE", "REINTENTE" )
         close 38
         exit sub
      endif

      operName$ = IdNombre$
      close 38
   endif \
   else exit sub

   K% = val(left$(time$,2))
   if K% < 24 then dato$ = " BUENAS NOCHES!!!   "
   if K% < 18 then dato$ = " BUENAS TARDES!!!   "
   if K% < 12 then dato$ = " BUENOS DIAS  !!!   "

   call TSHIECET                                 ! emite sonido beep
   call muestraVisor( 1, operName$, dato$ )
   wait; 3000

   enablePass% = 0
   if val(unpack$(IdPassw$)) = 123456 then enablePass% = 1

   if enablePass% then begin

      ingresoPass:

      jGuiSubState = 80700
      call javaEvent(terminalSubStateMsg)
      dato$ = ingresoDatos$( "INGRESE SU NUEVA", "PASSWORD" )
!call traceNuo("dato_1=[" + dato$ + "]" )
      if left$( dato$, 1 ) =  "A" then goto ingresoPass
      if left$( dato$, 1 ) <> "P" then goto ingresoPass
      dato$ = right$( dato$, len( dato$ ) - 2 )
!call traceNuo("dato_2=[" + dato$ + "]" )
      if dato$ = "123456"         then goto ingresoPass
      if (len(dato$) > 8) or (len(dato$) < 4) then begin
         call muestraBorrar( "PASSWORD NO VALIDA", "INTENTE CON OTRA" )
         goto ingresoPass
      endif

      TS.ER.RETURN = -1
      open "R::HICAMCLA" keyed recl 40 as 38
      if not TS.ER.RETURN then begin
         call muestraBorrar( "NO EXISTE ACHIVO", "HICAMCLA" )
         exit sub
      endif

!call traceNuo("SI EXISTE archivo HICAMCLA" )

      read Form "T6 C3 C32"; #38 key TS.OPER$; pasFecha$, msmClaves$
      if TS.ER.RETURN = -1 then begin

!call traceNuo("SI EXISTE HISTORIA OPERADOR [" + unpack$(TS.OPER$) + "]" )	  

         msmPass1$ = right$("00000000" + dato$, 8)
         i% = passwordUtilizada( msmPass1$, msmClaves$ )
         if i% > 0 then begin
            call muestraBorrar( "PASSWORD YA USADA", "INTENTE CON OTRA" )
            close 38
            goto ingresoPass
         endif \
         else begin
            msmClaves$ = pack$(left$(msmPass1$ + unpack$(msmClaves$), 64))
            pasFecha$ = pack$(date$)
            write form "C5 C3 C32"; #38; TS.OPER$,  \
                                         pasFecha$, \
                                         msmClaves$ !
         endif 
      endif \
      else begin
         msmClaves$ = pack$(left$(right$("00000000" + dato$, 8) + string$(56, "0"), 64))
         pasFecha$ = pack$(date$)
         write form "C5 C3 C32"; #38; TS.OPER$,   \
                                      pasFecha$,  \
                                      msmClaves$  !
      endif

      close 38
      TS.ER.RETURN = -1

      open "R::EAMOPERA" keyed recl 72 as 38                       !Read operator file
      if TS.ER.RETURN = -1 then begin
         msmPass1$ = pack$(right$("00000000" + dato$, 8))
         write Form "C5 C4 C23 C20 C20"; #38; TS.OPER$,   \
                                              msmPass1$,  \
                                              Idresto1$,  \
                                              IdNombre$,  \
                                              Idresto2$   !
         close 38
         enablePass% = 0
         TS.PSWD$ = msmPass1$

         !!msmAux1$ = IdPassw$  + ":" + IdNombre$ + ":" + msmPass1$
         !!open "R::EAMEXCPT" as 39 append
         !!msmForm$ = "C2 C2 C5 C1 C5 C1 C5" + " C" + str$(len(msmAux1$))
         !!write #39; "CC",                              \
         !!            pack$("0000"),                    \
         !!            pack$(date$ + mid$(time$, 1, 4)), \
         !!            pack$("30"),                      \
         !!               TS.OPER$,                      \
         !!               "2",                           \
         !!               TS.OPER$,                      \
         !!               msmAux1$                       !
         !!close 39
      endif
   endif
end sub
