!Decodificador de Transacciones Mega Santa Maria

String Global Numero.Tienda$,  \
              Boleta$(1),      \
              registro$,       \
              sepacad$,        \
              A$,              \
              B$,              \
              Trans.In$,       \
              Hora$,           \
              Filler$,         \
              Estado$,         \
              fecha$,          \
              Puntero$,        \
              Trans.Log$,      \
              Parm2$,          \
              Version$,        \
              Inicio.Trx$,     \
              Colon$,          \
              Num.Trans$,      \
              Term.Num$,       \
              Header.str$,     \
              Close.str$,      \
              Checkout.trans$, \
              Str.type99$,     \
              Str.type01$,     \
              Str.type02$,     \
              Str.type03$,     \
              Str.type04$,     \
              Str.type05$,     \
              Trans.type$,     \
              rh.nombre$,      \
              Clave$,          \
              precio.uni$,     \
              Archivo.Sal$,    \ !Nombre de archivo de salida
              Archivo.Aux$,    \ !Nombre de archivo de salida aux
              Archivo.Temp$,   \ !Nombre de archivo de datos temporal
              Arch.Cajeras$,   \
              Sal.Cajeras$,    \
              Aux.Cajeras$,    \
              MPago$,          \
              MontoTrx$,       \
              Operador$,       \
              Clave.Operador$, \
              NumTrx$,         \
              Pagos$(1),       \
              Aux$,            \
              Sigoff$,         \
              aString$,        \
              ch.pass$,        \
              rh.USER$,        \
              rh.PASWD$,       \
              rh.filler$,      \
              paso1$,          \
              paso2$,          \
              paso3$,          \
              archivo.cfg$,    \
              men$,            \
              sec.proforma$,   \
              nombre.archivo$, \
              linea$,          \
              X.LEC$, \
              X.KEY$,           \
              X.TIPO$,          \
              X.SERIAL$,        \
              X.RESOL$,         \
              X.FEC.EMI$,       \
              X.PREF$,         \
              X.AUT$,          \
              X.FEC$, \
              X.TERMINAL$

Integer*1 Global Existe,         \
                 Hay.Tr,         \
                 Loop,           \
                 Tipo.Documento, \
                 rh.nc.activa%,  \
                 sw,             \
                 pase,           \
                 signo%,         \
                 ya.ejecute,     \
                 ya.borre,       \
                 rh.proforma%

Integer*2 Global Tsl.Sess,    \
                 Term.Sess,   \
                 Ctr.Sess,    \
                 Pos.Sess,    \
                 Func,        \
                 Parm1,       \
                 Salida.Sess, \
                 Busca1,      \
                 Busca2,      \
                 Busca3,      \
                 Busca4,      \
                 Busca5,      \
                 Busca6,      \
                 Busca7,      \
                 Busca8,      \
                 Busca9,      \
                 Busca10,     \
                 Busca11,     \
                 Busca12,     \
                 Busca13,     \
                 Busca14,     \
                 Busca15,     \
                 Busca16,     \
                 Busca17,     \
                 Busca18,     \
                 Busca19,     \
                 Contador,    \
                 Caj.Sess,    \
                 auth.ret

Integer*4 global Cantidad,      \
                 retc,          \
                 ini,           \
                 pos.arc,       \
                 pos.cad,       \
                 Point.Restart, \
                 Ret.cod,       \
                 tiempo.espera, \
                 hora.manana%,  \
                 hora.tarde%,   \
                 numero.documento, \
                 Asc.Fis.NotaNum%, \
                 Asc.Fis.FacNum%, \
                 Asc.Fis.NotaCrd%, \
                 ue.for,      \                 
                 k%,          \
                 j%,          \
                 X.INI%,           \
                 X.FIN%

String    Global Gvtas$(2)
Integer*4 Global Gnro%, K, J

%include y2kfecha.c86
%include eamasmrt.j86

Function matchb( p1$, p2$, p3) external
Integer*2 matchb
String p1$
String p2$
Integer*2 p3
fend

Function BIGLEN(A$) EXTERNAL
  Integer*4 BIGLEN
  String A$
End Function

!-------------------------------------------------------------------------------
! Funci¢n para ejecutar un comando
!-------------------------------------------------------------------------------
Function osshell( cmd.line$) External
String     cmd.line$
Integer*4  osshell
End Function 

Sub ADXCOPYF (RETC,INFILE,OUTFILE,OPT0,OPT1) EXTERNAL
Integer*4 RETC
String INFILE,OUTFILE
Integer*2 OPT0,OPT1
End Sub

Function ADXSTART (NAME$,PARM$,MESS$) External
Integer*2 ADXSTART
String NAME$,PARM$,MESS$
End Function

Sub Adxserve( Ret, f, p1, p2$) External
Integer*4 ret
Integer*2 f, p1
String p2$
End Sub

Function ADXAUTH(FUNC, OPID$, OPPW$, OPID2$) EXTERNAL
String OPID$, OPPW$, OPID2$
Integer*2 ADXAUTH, FUNC
End Function

! Conversi¢n de datos a hexadecimal
Function ERRN.TO.HEX$(cod.err)
Integer*4 cod.err
String errn.to.hex$, errn$
Integer*2 i, retrn

errn$ = ""
For i = 0 to 7
   retrn = shift(cod.err, i*4) and 000FH
   If (retrn < 10) Then \
       retrn = retrn + 48 \
   Else \
       retrn = retrn + 55
   errn$ = chr$(retrn) + errn$
Next i
errn.to.hex$ = errn$
End Function

!-------------------------------------------------------------------------------
!Funci¢n para convertir numeros a hexadecimal
!-------------------------------------------------------------------------------
! La funci¢n convierte un entero de 4 bytes en una cadena hexadecimal.
! El resultado de la funci¢n es una cadena con 8 caracteres hexadecimales.

Function hex$(numdec)
String hex$, variab1$, variab3$
Integer*4 numdec
Integer*2 variab1, variab2

variab3$ = "0123456789ABCDEF"
variab1$ = ""
for variab1 = 0 to 28 step 4
   variab2  = shift(numdec, variab1) and 000FH
   variab1$ = mid$(variab3$, variab2 + 1, 1) + variab1$
next variab1
hex$ = variab1$
End Function 

! Rutina encargada de enviar mensajes Background
Sub Informar(Mensaje, tiempo)
String Mensaje
Integer*4 tiempo

Parm2$ = Mensaje
Func   = 26
Parm1  = len(Parm2$)
Call  adxserve(Ret.cod, Func, Parm1, Parm2$)
Print mensaje
!wait ; tiempo
End Sub

Sub NC.ACTIVA
Integer*1 sw
String ts.temp1$

on error goto errp

sw = 0
Open "ASCNTRL" AS 38 ReadOnly
rh.nc.activa% = 1
If sw = 0 Then Begin
   While sw = 0
      Read #38; TS.TEMP1$  ! Lectura registro
      If TS.TEMP1$ = "[RHCONF]" Then Begin
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         rh.nc.activa% = val(mid$(TS.TEMP1$,30,1))
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         Read #38; TS.TEMP1$
         rh.proforma% = val(mid$(TS.TEMP1$,30,1))
      EndIf
   Wend
EndIf
close 38

errp:
If err = "OE" and errf% = 38 Then Begin
   sw = 1
   resume
EndIf
If err = "EF" and errf% = 38 Then Begin
   sw = 1
   resume
EndIf
End Sub


! Grabaci¢n de archivo de control para posibles recuperaciones de decodificacion
!--------------------------------------------------------------------------
Function Write.control
String reg.control$

reg.control$ = right$("000000000" + str$(Point.restart), 8)
reg.control$ = reg.control$ + String$(30, "0")
write form " c38 c2 "; #Ctr.Sess, 1; reg.control$, chr$(13) + chr$(10)
FEND

! Lectura de registro control para decodificacion
!------------------------------------------------
Sub read.control
String reg.control$

read form "C40"; #Ctr.Sess, 1; reg.control$
Point.restart = val(left$(reg.control$, 8))
End Sub

! ------------------------------------------------------------------------------
! Retorna la siguiente cadena del registro, sin comillas.
! ------------------------------------------------------------------------------
Function reccaden
String reccaden
Integer*4 reccaden4i1

ini = ini + 1
reccaden4i1 = match(sepacad$, registro$, ini)
If reccaden4i1 < 0 Then \
   reccaden4i1 = reccaden4i1 and 0000FFFFh
If reccaden4i1 = 0 Then \
   reccaden4i1 = len (registro$)
reccaden = mid$(registro$, ini, reccaden4i1 - ini)
ini = reccaden4i1 + 2
fend

! ------------------------------------------------------------------------------
! Retorna el siguiente campo
! La cadena c1$ debe terminar en el caracter ":"
! El inicio de busqueda debe estar dentro de la cadena
! ------------------------------------------------------------------------------
Function sigcamp$(c1$, c1)
String sigcamp$, c1$, c2$
Integer*4 c1, c2, c3

c2  = matchb(":", c1$, c1)
c1$ = unpack$(mid$(c1$, c1, c2 - c1))
If c1$ = "" Then \
   sigcamp$ = "0" \
Else \
   sigcamp$ = c1$
end function

! ------------------------------------------------------------------------------
! Desempaqueta los campos desde la posicion c1
! ------------------------------------------------------------------------------
Function campvar$(c1$, c1)
String campvar$, c1$, c2$, c3$
Integer*4 c1, c2, c3

c1$ = c1$ + ":"
c3$ = ""
c2  = len(c1$)
While c1 <= c2
   c3  = matchb(":", c1$, c1)
   c2$ = unpack$(mid$(c1$, c1, c3 - c1))
   If c2$ = "" Then \
      c3$ = c3$ + " 0" \
   Else \
      c3$ = c3$ + " " + c2$
   c1 = c3 + 1
Wend
campvar$ = c3$
End Function

! ------------------------------------------------------------------------------
! Desempaqueta los campos desde la posicion c1
! ------------------------------------------------------------------------------
Function campvare$(c1$, c1)
String campvare$, c1$, c2$, c3$
Integer*4 c1, c2, c3

c1$ = c1$ + ":"
c3$ = ""
c2  = len(c1$)
While c1 <= c2
   c3  = matchb(":", c1$, c1)
   c2$ = unpack$(mid$(c1$, c1 + 1, c3 - c1 - 2))
   If c2$ = "" Then \
      c3$ = c3$ + " " \
   Else \
      c3$ = c3$ + " " + c2$
   c1 = c3 + 1
Wend 
campvare$ = c3$
End Function

Sub genera.archivo.mpagos
String datos$,      \
       cajeras$(1), \
       clave$,      \
       len$,        \
       aux$,        \
       reg$,        \
       total$
Integer*1 fin%,   \
          existe, \
          sw
Integer*2 i%, \
          j%, \
          k%, \
          l%
Integer*4 total%

Dim cajeras$(100)

On Error GoTo errp

fin% = 0
i%   = 2
Open "c:\adx_udt1\MPCAJERO.dat" direct recl 512 as 12 buffsize 512
Call  Informar("Procesando Archivo: " + "c:\adx_udt1\MPCAJERO.dat", 0)

While fin% = 0
   existe = 1
   read form "C512"; #12, i%; datos$


   datos$ = right$( datos$, 508 )! los primeros 4 bytes no se
   If existe  Then Begin
      j% = 1
      While j% <= 494
         If unpack$(mid$(datos$, j%, 8)) <> "0000000000000000" Then Begin
            clave$ = mid$(datos$, j%, 12)
            If l% = 0 Then Begin
               l% = l% + 1
               If mid$(datos$, j% + 12, 2) <> "00" Then \
                  Cajeras$(l%) = mid$(datos$, j%, 8) + "|" + \
                                 mid$(datos$, j% + 8, 4) + "|" + \
                                 mid$(datos$, j% + 12, 2) + "|" + \
                                 mid$(datos$, j% + 14, 8) \
               Else Begin
                  Cajeras$(l%) = mid$(datos$, j%, 8) + "|" + \
                                 mid$(datos$, j% + 8, 4) + "|" + \
                                 mid$(datos$, j% + 14, 10) + "|" + \
                                 mid$(datos$, j% + 24, 2)
                  fecha$ = mid$(datos$, j% + 14, 6)
               EndIf
            EndIf \
            Else Begin
               k% = 1
               sw = 0
               While k% <= l%
                  If mid$(cajeras$(k%), 1, 8) = mid$(datos$, j%, 8) And \
                     mid$(cajeras$(k%), 10, 4) = mid$(datos$, j% + 8, 4) Then Begin
                     sw = 1
                     If mid$(datos$, j% + 12, 2) <> "00" Then Begin
                        Cajeras$(k%) = Cajeras$(k%) + "|" + \
                                       mid$(datos$, j% + 12, 2) + "|" + \
                                       mid$(datos$, j% + 14, 8)
                     EndIf \
                     Else Begin
                        Cajeras$(k%) = mid$(datos$, j%, 8) + "|" + \
                                       mid$(datos$, j% + 8, 4) + "|" + \
                                       mid$(datos$, j% + 14, 10) + "|" + \
                                       mid$(datos$, j% + 24, 2) + "|" + \
                                       mid$(Cajeras$(k%), 15, Len(cajeras$(k%)) - 14)
                        fecha$ = mid$(datos$, j% + 14, 6)
                     EndIf
                     k% = k% + l%
                  EndIf
                  k% = k% + 1
               Wend
               If sw = 0 Then Begin
                  l% = l% + 1
                  If mid$(datos$, j% + 12, 2) <> "00" Then Begin
                     Cajeras$(l%) = mid$(datos$, j%, 8) + "|" + \
                                    mid$(datos$, j% + 8, 4) + "|" + \
                                    mid$(datos$, j% + 12, 2) + "|" + \
                                    mid$(datos$, j% + 14, 8)
                  EndIf \
                  Else Begin
                     Cajeras$(l%) = mid$(datos$, j%, 8) + "|" + \
                                    mid$(datos$, j% + 8, 4) + "|" + \
                                    mid$(datos$, j% + 14, 10) + "|" + \
                                    mid$(datos$, j% + 24, 2)
                     fecha$ = mid$(datos$, j% + 14, 6)
                  EndIf
               EndIf
            EndIf
         EndIf
         j% = j% + 26
      wend
   EndIf
   i% = i% + 1
Wend 
Close 12

i% = 1
If i% = l% Then cajeras$(i%) = cajeras$(i%) + "|"
While i% < l%

   j% = i% + 1
   While j% <= l%
      If mid$(Cajeras$(j%), 21, 10) < mid$(Cajeras$(i%), 21, 10) Then Begin
         aux$ = Cajeras$(j%)
         Cajeras$(j%) = Cajeras$(i%)
         Cajeras$(i%) = aux$
      EndIf
      j% = j% + 1
   Wend
   cajeras$(i%) = cajeras$(i%) + "|"
   i% = i% + 1
Wend
cajeras$(i%) = cajeras$(i%) + "|"

create "adx_udt1:TI" + fecha$ + "." + numero.tienda$ As 14
i% = 1
While i% <= l%
   If mid$(cajeras$(i%), 26, 2) = "0M" Or mid$(cajeras$(i%), 26, 2) = "0T" Then Begin
      total% = 0
      reg$ = "1" + "," + \
             mid$(cajeras$(i%), 15, 6) + "," + \
             mid$(cajeras$(i%), 21, 4) + "00" + "," + \
             Numero.tienda$ + "," + \
             mid$(cajeras$(i%), 27, 1) + "," + \
             mid$(cajeras$(i%), 10, 4) + "," + \
             str$(Val(mid$(cajeras$(i%), 1, 8))) + ","
      len$ = "C" + str$(Len(reg$) + 2)
      Write form len$; #14; reg$ + chr$(13) + chr$(10)
      If Len(cajeras$(i%)) > 28 Then Begin
         cajeras$(i%) = mid$(cajeras$(i%), 29, Len(cajeras$(i%)) - 28)
         While Len(cajeras$(i%)) > 0
            reg$ = "2" + "," + \
                   mid$(cajeras$(i%), 1, 2) + "," + \
                   str$(Val(mid$(cajeras$(i%), 4, 6))) + "." + str$(Val(mid$(cajeras$(i%), 10, 2))) + ","
            len$ = "C" + str$(Len(reg$) + 2)
            Write form len$; #14; reg$ + chr$(13) + chr$(10)
            total% = total% + Val(mid$(cajeras$(i%), 4, 8))
            cajeras$(i%) = mid$(cajeras$(i%), 13, Len(cajeras$(i%)) - 12)
         Wend
      EndIf \
      Else \
         total% = 0
      If total% > 0 Then Begin
         total$ = str$(total%)
         total$ = mid$(total$, 1, Len(total$) -2) + "." + right$(total$, 2)
      EndIf \
      Else \
         total$ = "0,00"
      reg$ = "3" + "," + \
             total$ + ","
      len$ = "C" + str$(Len(reg$) + 2)
      Write form len$; #14; reg$ + chr$(13) + chr$(10)
   EndIf
   i% = I% + 1
Wend
reg$ = "9" + "," + \
       "0" + ","
len$ = "C" + str$(Len(reg$) + 2)
Write form len$; #14; reg$ + chr$(13) + chr$(10)
Close 14
Exit Sub

errp:
If err = "EF" and errf% = 12 Then Begin
   existe = 0
   fin%   = 1
   resume
EndIf
Call  Informar("Genera Archivo: " + Err + " " + Str$(Errf%) + \
              " " + Errn.To.Hex$(Errn) + " " + Str$(Errl), 0)
End Sub

Sub Transmite.archivo
String buffer$, \
       lec$,    \
       finr$

FINR$ = CHR$(13) + CHR$(10)

Create "ADX_SDT1:PROFORMA.FTP" AS 15
buffer$ = "open oracle " + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "prompt" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "bin" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "user ventas ventas " + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "prompt" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "bin" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "cd vmega" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "send " + "C:\ADX_UDT1\PROFORMA\" + nombre.archivo$ +  finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida de ventas
Write Form Lec$; #15; buffer$           ! Grabacion del registro

buffer$ = "by" + finr$
LEC$    = "C" + Str$(Len(Buffer$))      ! Formatea la salida
Write Form Lec$; #15; buffer$           ! Grabacion del registro
Close 15
Call  OSSHELL("type adx_sdt1:proforma.ftp | ftp > c:\program\rta2.txt")
End Sub



Sub genera.nombre
String aux.dia$, aux.mes$
Integer*4 aux.anho

aux.dia$ = str$(val(mid$(date$,5,2))-1)
aux.mes$ = str$(val(mid$(date$,3,2)))
aux.anho = val("20" + mid$(date$,1,2))

If aux.dia$ = "0" Then Begin
   aux.mes$ = str$(val(aux.mes$)-1)
   If aux.mes$ = "1" or aux.mes$ = "3" or aux.mes$ = "5" or aux.mes$ = "7" or \
      aux.mes$ = "8" or aux.mes$ = "10" or aux.mes$ = "0" Then \
      aux.dia$ = "31" \
   Else If aux.mes$ = "4" or aux.mes$ = "6" or aux.mes$ = "9" or aux.mes$ = "11" Then \
      aux.dia$ = "30" \
   Else If aux.mes$ = "2" and mod(aux.anho,4)=0 Then \
      aux.dia$ = "29" \
   Else If aux.mes$ = "2" and mod(aux.anho,4)<>0 Then \
      aux.dia$ = "28"
   If aux.mes$ = "0" Then aux.mes$ = "12"
EndIf

If aux.dia$ = "10" Then aux.dia$ = "A" \
Else If aux.dia$ = "11" Then aux.dia$ = "B" \
Else If aux.dia$ = "12" Then aux.dia$ = "C" \
Else If aux.dia$ = "13" Then aux.dia$ = "D" \
Else If aux.dia$ = "14" Then aux.dia$ = "E" \
Else If aux.dia$ = "15" Then aux.dia$ = "F" \
Else If aux.dia$ = "16" Then aux.dia$ = "G" \
Else If aux.dia$ = "17" Then aux.dia$ = "H" \
Else If aux.dia$ = "18" Then aux.dia$ = "I" \
Else If aux.dia$ = "19" Then aux.dia$ = "J" \
Else If aux.dia$ = "20" Then aux.dia$ = "K" \
Else If aux.dia$ = "21" Then aux.dia$ = "L" \
Else If aux.dia$ = "22" Then aux.dia$ = "M" \
Else If aux.dia$ = "23" Then aux.dia$ = "N" \
Else If aux.dia$ = "24" Then aux.dia$ = "O" \
Else If aux.dia$ = "25" Then aux.dia$ = "P" \
Else If aux.dia$ = "26" Then aux.dia$ = "Q" \
Else If aux.dia$ = "27" Then aux.dia$ = "R" \
Else If aux.dia$ = "28" Then aux.dia$ = "S" \
Else If aux.dia$ = "29" Then aux.dia$ = "T" \
Else If aux.dia$ = "30" Then aux.dia$ = "U" \
Else If aux.dia$ = "31" Then aux.dia$ = "V"


If aux.mes$ = "10" Then aux.mes$ = "A" \
Else If aux.mes$ = "11" Then aux.mes$ = "B" \
Else If aux.mes$ = "12" Then aux.mes$ = "C"

rh.nombre$ = mid$(date$,1,2) + aux.mes$ + aux.dia$
End Sub

Sub Borra.archivos
String aux.dia$, aux.mes$
Integer*4 aux.anho

aux.dia$ = mid$(date$, 5, 2)
aux.mes$ = str$(val(mid$(date$, 3, 2)))
aux.anho = val("20" + mid$(date$, 1, 2))

If aux.mes$ = "1" Then aux.mes$ = "11" \
Else If aux.mes$ = "2" Then aux.mes$ = "12" \
Else If aux.mes$ = "3" Then aux.mes$ = "01" \
Else If aux.mes$ = "4" Then aux.mes$ = "02" \
Else If aux.mes$ = "5" Then aux.mes$ = "03" \
Else If aux.mes$ = "6" Then aux.mes$ = "04" \
Else If aux.mes$ = "7" Then aux.mes$ = "05" \
Else If aux.mes$ = "8" Then aux.mes$ = "06" \
Else If aux.mes$ = "9" Then aux.mes$ = "07" \
Else If aux.mes$ = "10" Then aux.mes$ = "08" \
Else If aux.mes$ = "11" Then aux.mes$ = "09" \
Else If aux.mes$ = "12" Then aux.mes$ = "10"

Call  Informar("Borrando Archivos del DDMM " + aux.dia$ + aux.mes$ , 0)
Call  OSSHELL("del c:\vt" + aux.dia$ + "\v" + aux.mes$ + aux.dia$ + "*.*")

End Sub

Sub GrData$(Greg$)
String Greg$
K  = MATCH(" ",Greg$,J)                                 ! SEARCH For  FIELD SEPERATOR
If (k-j) > 0 Then \
   A$ = MID$(Greg$,J,K-J) Else   \             ! UNPACK FIELD
	 A$ = ""
J  = K + 1                                              ! POINT TO BeginNING OF Next FIELD
End Sub 

Function New.Cabecera$(X00$, Xvta%)																					! Arma nueva cabecera registro
String X00$, Xvta$, New.Cabecera$, Xlin$
Integer*4 Xvta%
 Xlin$ = ""
 J = 4 
 Call  Grdata$(X00$)                                                  				! Caja
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! trx
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! fecha y hora 
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! tipo trx
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! nro strings
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! usuario
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! password
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! Gross +
 Xlin$ = Xlin$ + Str$(Xvta%) + " "
 Call  Grdata$(X00$)                                                  				! Gross - 
 Xlin$ = Xlin$ + "0" + " "
 Call  Grdata$(X00$)                                                  				! data 1
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! data 2
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! data 3
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! data 4
 Xlin$ = Xlin$ + A$ + " "
 Call  Grdata$(X00$)                                                  				! data 5
 Xlin$ = Xlin$ + A$ + " "
 Xlin$ = Left$(Xlin$+String$(101," "),101)
 New.Cabecera$ = "00 " + Xlin$
End Function 																																! Fir cabecera nueva

Sub Total.Itm.Vta(Itm$, Qty%, Pvt%, Dscto%,Opr$,Xprices$)
String Itm$, Opr$, Xprices$, x$
Integer*4 Qty%, Pvt%, Dscto%, Xi%, Xpos%
Integer*1 Fnd%, XSgn%
Fnd% = 0
For Xi% = 1 To Gnro%
    If Gvtas$(Xi%,0) = Itm$ Then \
    	 Fnd% = -1 : Xpos% = Xi%
Next Xi%
If Fnd% = 0 Then Begin																											! No existe
	 Gnro% = Gnro% + 1
	 Xsgn% = 1
	 If Left$(Opr$,1) = "8" Then Xsgn% = -1 
	 If Opr$ = "23" Then Xsgn% = -1 
	 Gvtas$(Gnro%,0) = Itm$																										! Almacena Item
	 Gvtas$(Gnro%,1) = Str$(Qty% * Xsgn%)																			! Almacena Qty
	 Gvtas$(Gnro%,2) = Str$(Pvt% * Xsgn%)																      ! Almacena Precio Total
	 Gvtas$(Gnro%,3) = Str$(Dscto%)																						! Almacena Dscto
	 Gvtas$(Gnro%,4) = Xprices$																								! Almacena PvP
EndIf Else Begin																														! Si Existe
	Xsgn% = 1
	If Left$(Opr$,1) = "8" Then Xsgn% = -1 
  If Opr$ = "23" Then Xsgn% = -1 
	 Gvtas$(Xpos%,1) = Str$( Val(Gvtas$(Xpos%,1)) + (Qty% * Xsgn%))						! Actualiza cantidades
	 Gvtas$(Xpos%,2) = Str$( Val(Gvtas$(Xpos%,2)) + (Pvt% * Xsgn%))			      ! Actualiza Totales
	 Gvtas$(Xpos%,3) = Str$( Val(Gvtas$(Xpos%,3)) + (Dscto%))		      	      ! Actualiza Totales
	 Gvtas$(Gnro%,4) = Xprices$																								! Almacena PvP
EndIf
End Sub 

!-- Arma trx acumulada para recuperacion GR-OVS 15Mzo2017
Sub ARMA.NEW.TRX
Integer*4 Gi%, Gq%, Pvt%, Dscto%, GVtaTot%, Gvtatrx%
String    Gb$, G00$, Xitm$, Xvta$, xData$(1), Xprc$, xOpr$, Xtipo$, Ng00$
   Gnro% = 0 
   Dim Gvtas$(1000,4)
   For Gi% = 1 To Contador																									! Recorre String almacenados
      Gb$ = Boleta$(Gi%)																										! Toma String 
      !Print "Str: "+Gb$
      If Left$(Gb$,2) = "00" Then Begin                                    \! Si es la cabecera
      	 G00$ = Gb$                                                         ! Salva cabecera de la trx
      	 Dim xData$(20)																											! Init vector datos
      EndIf
      If Left$(Gb$,2) = "01" Then Begin                                     ! Si es Item
      	 J = 4 
      	 Call  Grdata$(Gb$)                                                  ! Item
      	 XItm$ = Str$(Val(A$))
      	 Call  Grdata$(Gb$)                                                  ! Vta Total
      	 XVta$ = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 1
      	 Xdata$(1) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 2
      	 Xdata$(2) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 3
      	 Xdata$(3) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 4
      	 Xdata$(4) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 5 Tipo Movimiento
      	 Xdata$(5) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 1
      	 Xdata$(6) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 7 Precio venta publico
      	 Xdata$(7) = A$
      	 If Left$(Boleta$(Gi%+1),2) = "02" Then Gq% = 0 Else Gq% = 1        ! 
         Pvt% = Val(XVta$)
         Dscto% = 0 
         xOpr$ = Xdata$(5)
         Xprc$ = Xdata$(7)
         !Print "Graba:"+XItm$+" Sgn "+xOpr$ 
         Call  Total.Itm.Vta(XItm$, Gq%, Pvt%, Dscto%,xOpr$,Xprc$)
         
      EndIF																																	! Fin Item
      If Left$(Gb$,2) = "02" Then Begin                                     ! Si es Extension de Item
      	 J = 4
      	 Call  Grdata$(Gb$)                                                  ! Data 1
      	 Xdata$(1) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 2
      	 Xdata$(2) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 3
      	 Xdata$(3) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 4
      	 Xdata$(4) = A$
      	 Call  Grdata$(Gb$)                                                  ! Data 5 
      	 Xdata$(5) = A$
      	 Call  Grdata$(Gb$)                                                  ! Cantidad vendida
      	 Xdata$(6) = A$
      	 Gq% = Val(Xdata$(6))																								! Qty
      	 Pvt% = 0 
      	 Dscto% = 0
      	 !Call  Total.Itm.Vta(XItm$,  1, 0, 0,xOpr$,Xprc$)
         Call  Total.Itm.Vta(XItm$, Gq%, Pvt%, Dscto%,xOpr$,Xprc$)
      EndIF																																	! Fin Extension de Item
      If Left$(Gb$,2) = "99" Then Begin                                     ! Si es user data
      	 J = 4
      	 Call  Grdata$(Gb$)                                                  ! Numero del proyecto
         If A$ = "20110818" Then Begin                                      ! Dscto Aplicacion x mayor
         	  Call  Grdata$(Gb$)                                               ! Porcentaje descuento
         	  Call  Grdata$(Gb$)                                               ! Valor descuento
         	  Dscto% = Val(A$)
         	  Call  Grdata$(Gb$)                                               ! Ean Producto
         	  XItm$ = Str$(Val(A$))
         	  Call  Grdata$(Gb$)                                               ! Signo descuento
         	  If A$ <> "00" Then Dscto% = Dscto% * -1
         	  Gq% = 0 : Pvt% = 0 : Xprc$ = "" : Xopr$ = "00"
         	  Call  Total.Itm.Vta(XItm$, Gq%, Pvt%, Dscto%,xOpr$,Xprc$)
         EndIf
         If A$ = "97" Or A$ = "98" Then Begin                          			! Dscto Oracle
         	  Call  Grdata$(Gb$)                                               ! Tipo Promocion
         	  Xtipo$ = A$
         	  Call  Grdata$(Gb$)                                               ! ID promo Or
         	  Call  Grdata$(Gb$)                                               ! 
         	  Call  Grdata$(Gb$)                                               ! 
         	  Call  Grdata$(Gb$)                                               ! EAN/PLU
         	  Xitm$ = str$(Val(A$))
         	  Call  Grdata$(Gb$)                                               ! Cod Interno
         	  Call  Grdata$(Gb$)                                               ! PV IR
         	  Call  Grdata$(Gb$)                                               ! PV Or
         	  Call  Grdata$(Gb$)                                               ! Dscto 
         	  Dscto% = Val(A$)
         	  Call  Grdata$(Gb$)                                               ! Signo
         	  Xopr$ = A$
         	  Call  Grdata$(Gb$)                                               ! Cantidad Combos
         	  Gq% = Val(A$)
            If Xtipo$ = "01" Then Begin																			! Promocion Simple
         	     If Xopr$ <> "00" Then Dscto% = Dscto% * -1
         	     Gq% = 0 : Pvt% = 0 : Xprc$ = "" : Xopr$ = "00"
         	     Call  Total.Itm.Vta(XItm$, Gq%, Pvt%, Dscto%,xOpr$,Xprc$)
            EndIf																														! Fin Promo Simple
         	  If Xtipo$ = "02" Then Begin																			! Promocion Compleja
         	     If Xopr$ <> "00" Then Dscto% = Dscto% * -1
         	     Gq% = 0 : Pvt% = 0 : Xopr$ = "00"
         	     Call  Total.Itm.Vta(XItm$, Gq%, (Dscto%*-1), Dscto%,xOpr$,Xprc$)
         	  EndIf
         EndIf
      EndIf
   Next Gi%

   Contador = 0
   Dim Boleta$(8500)
   GI% = 0
   Gvtatrx% = 0
   For GI% = 1 To Gnro%
       Gvtatrx% =  Gvtatrx% + Val(Gvtas$(Gi%,2))														! Suma total de la trx resumida
   Next Gi%
   Contador = Contador + 1
   Ng00$ = New.Cabecera$(G00$,Gvtatrx%)																			! Arma Str de cabecera
   Boleta$(Contador) = NG00$																								! Cabecera de la trx

   For Gi% = 1 To Gnro%

       Contador = Contador + 1
       If Val(Gvtas$(Gi%,3)) <> 0 Then Begin																! Item con descuento
       	Xopr$ = "00"
       	If Val(Gvtas$(Gi%,3)) < 0 Then 																		 \! Si anulacion Dscto
       		 Xopr$ = "01" : Gvtas$(Gi%,3) = Str$(Val(Gvtas$(Gi%,3))*-1)       ! Invierte signo
       	   Boleta$(Contador) = "99" + " 20110818 00 " + Gvtas$(Gi%,3) +    \!
       	                       " " + Gvtas$(Gi%,0) + " " + Xopr$ + " 0"     !
       	   Contador = Contador + 1 
       EndIf																																! Fin String descuento
       If Val(Gvtas$(Gi%,1)) < 0 Then Xopr$ = "80" Else Xopr$ = "01"        ! Signo movimiento
       Boleta$(Contador) = "01" + " " + Gvtas$(Gi%,0) + " " +  			  		\	! Almacena Item
                           Gvtas$(Gi%,2) + " " +  					              \	! Valor Total 
                           "00 00 00 00" + " " +  			                  \	! Valores adicionales
                           Xopr$ + " " + Gvtas$(Gi%,1) + " " +            \ ! Signo del movimiento
                           Gvtas$(Gi%,4)                                    ! Precios de venta  
       Contador = Contador + 1
       Boleta$(Contador) = "02" + " 00 00 00 00 00 " +                    \ ! 
                           Gvtas$(Gi%,1) + " 00"   				                 	! Cantidad vendida
   Next Gi%

End Sub 


Function Genera.Ascii
String arctsl$,            \
       arcsal$,            \
       tiporeg$,           \
       tipocad$,           \
       cadena$,            \
       ascad$,             \
       crlf$,              \
       larcad$,            \
       Archivo.Resultado$, \
       codigo$
Integer*1 Genera.Ascii, \
          salto
Integer*4 sesssal, \
          i,       \
          Puntero, \
          k,       \
          j,       \
          a
Integer*4 ret,               \
          cantidad.original, \
          monto.original,    \
          monto

On Error Goto Err.Genera.Ascii

tiporeg$ = chr$(00H) + \ ! registro con encabezado
           chr$(20H) + \ ! registro de excepcion
           chr$(21H) + \ ! registro de cierre
           chr$(98H) + \ ! registro de operador en entrenamiento
           chr$(99H)     ! registro definido por el usuario

tipocad$ = chr$(00H) + \ ! cadena de cabecera
           chr$(01H) + \ ! cadena de articulo
           chr$(02H) + \ ! cadena de extension de articulo
           chr$(03H) + \ ! cadena de descuento
           chr$(04H) + \ ! cadena de invalidacion de descuento
           chr$(05H) + \ ! cadena de pago
           chr$(06H) + \ ! cadena de correccion de pago
           chr$(07H) + \ ! cadena de impuestos
           chr$(08H) + \ ! cadena de reembolso de impuestos
           chr$(09H) + \ ! cadena de cambio de estampillas de alimentacion
           chr$(10H) + \ ! cadena de autorizacion de supervisor
           chr$(11H) + \ ! cadena de entrada de dato
           chr$(12H) + \ ! cadena de cambio de precio
           chr$(13H) + \ ! cadena de cambio de gaveta
           chr$(14H) + \ ! cadena de cambio de articulo
           chr$(15H) + \ ! cadena de canje de estampillas
           chr$(16H) + \ ! cadena reservada para EFT
           chr$(20H) + \ ! cadena de excepcion
           chr$(21H) + \ ! cadena de cierre
           chr$(30H) + \ ! cadena reservada para EFT
           chr$(31H) + \ ! cadena reservada para EFT
           chr$(98H) + \ ! cadena de operador en entrenamiento
           chr$(99H)     ! cadena de usuario

crlf$   = chr$(13) + chr$(10)
sesssal = 18

!MkDir Directorio$
If pase = 0 Then Begin
   Call  osshell("md C:\VT" + mid$(fecha$, 5, 2) )
   pase = 1
EndIf

Archivo.Resultado$ = "Q:\VT" + mid$(fecha$, 5, 2) + "\V" + \
                     mid$(fecha$, 3, 4)                  + \ ! MMDD
                     right$("000" + Num.Trans$, 3)       + \ ! Numero de Transaccion
                     "."                                 + \ ! puntito
                     right$("000" + Term.Num$, 3)            ! Numero de Pos

Create posfile Archivo.Resultado$ direct 1 recl 102 as sesssal compound perupdate

Contador = 0
Dim Boleta$(1500)
Dim Pagos$(50)
sigoff$ = ""
k% = 0

i = match("\" + mid$(registro$, 2, 1), tiporeg$, 1) + 1
On i GoSub regisXX, regis00, regis20, regis21, regis99

!If rh.nc.activa%=1 Then Begin
   For ue.for = 1 To Contador
      salto    = 0
      cantidad = 0
      If Left$(Boleta$(ue.for), 2) = "00" Then Begin
         Busca1  = match(" ", Boleta$(ue.for), 1)
         Busca2  = match(" ", Boleta$(ue.for), Busca1 + 1)
         Busca3  = match(" ", Boleta$(ue.for), Busca2 + 1)
         Busca4  = match(" ", Boleta$(ue.for), Busca3 + 1)
         Busca5  = match(" ", Boleta$(ue.for), Busca4 + 1)
         Busca6  = match(" ", Boleta$(ue.for), Busca5 + 1)
         Busca7  = match(" ", Boleta$(ue.for), Busca6 + 1)
         Busca8  = match(" ", Boleta$(ue.for), Busca7 + 1)
         Busca9  = match(" ", Boleta$(ue.for), Busca8 + 1)
         Busca10 = match(" ", Boleta$(ue.for), Busca9 + 1)
         Busca11 = match(" ", Boleta$(ue.for), Busca10 + 1)
         Busca12 = match(" ", Boleta$(ue.for), Busca11 + 1)
         Busca13 = match(" ", Boleta$(ue.for), Busca12 + 1)
         Busca14 = match(" ", Boleta$(ue.for), Busca13 + 1)
         Busca15 = match(" ", Boleta$(ue.for), Busca14 + 1)
         Operador$ = right$("00000000" + mid$(Boleta$(ue.for), Busca6 + 1, Busca7 - Busca6 - 1), 8)
         Sigoff$   = mid$(Boleta$(ue.for), Busca4 + 1, Busca5 - Busca4 - 1)
         ch.pass$  = mid$(Boleta$(ue.for), Busca14 + 1, Busca15 - Busca14 - 1)
         If sigoff$ = "17" Then Begin
         	  If ch.pass$ = "64" Then Begin
         	     Open "EAMOPERA" KEYED RECL 72 AS 15 NODEL
         	     rh.USER$ = PACK$(RIGHT$("0000000000" + Operador$, 10))
               READ FORM "C5 C4 C63"; #15 KEY rh.USER$; rh.USER$,  \
                                                        rh.PASWD$, \
                                                        rh.filler$
               close 15
         	  	 auth.ret = ADXAUTH(8, str$(val(unpack$(rh.USER$))), str$(val(unpack$(rh.paswd$))), "")
         	  EndIf
            Sigoff$ = Left$(Boleta$(ue.for), 2) + "|" + mid$(Boleta$(ue.for), Busca3 + 1, Busca4 - Busca3 - 1)
         EndIf \
         Else \
            sigoff$ = ""
      EndIf \
      Else If Left$(Boleta$(ue.for), 2) = "05" Or \
         Left$(Boleta$(ue.for), 2) = "06" Or \
         Left$(Boleta$(ue.for), 2) = "09" Then Begin
         Busca1 = match(" ", Boleta$(ue.for), 1)
         Busca2 = match(" ", Boleta$(ue.for), Busca1 + 1)
         Busca3 = match(" ", Boleta$(ue.for), Busca2 + 1)
         MPago$ = mid$(Boleta$(ue.for), Busca1 + 1, Busca2 - Busca1 - 1)
         MontoTrx$ = mid$(Boleta$(ue.for), Busca2 + 1, Busca3 - Busca2 - 1)
       Pagos$(k% + 1) = Left$(Boleta$(ue.for), 2) + "|" + MPago$ + "|" + MontoTrx$
       k% = k% + 1
      EndIf \
      Else If Left$(Boleta$(ue.for), 2) = "01" Then Begin
         cantidad = cantidad + 1
         If Left$(Boleta$(ue.for + 1), 2) = "02" Then Begin
            Busca1 = match(" ", Boleta$(ue.for + 1), 1)
            Busca2 = match(" ", Boleta$(ue.for + 1), Busca1 + 1)
            Busca3 = match(" ", Boleta$(ue.for + 1), Busca2 + 1)
            Busca4 = match(" ", Boleta$(ue.for + 1), Busca3 + 1)
            Busca5 = match(" ", Boleta$(ue.for + 1), Busca4 + 1)
            Busca6 = match(" ", Boleta$(ue.for + 1), Busca5 + 1)
            Busca7 = match(" ", Boleta$(ue.for + 1), Busca6 + 1)

            cantidad    = cantidad + Val(mid$(Boleta$(ue.for + 1), Busca6 + 1, Busca7 - Busca6 - 1)) - 1
            precio.uni$ = mid$(Boleta$(ue.for + 1), Busca5 + 1, Busca6 - Busca5 - 1)
            salto = -1
         EndIf
         Boleta$(ue.for) = Left$(Boleta$(ue.for) + " " + str$(cantidad) + " " + precio.uni$ + String$(100, " "), 100)
      EndIf

      If salto Then \
         ue.for = ue.for + 1
   next ue.for
   ! GoSub Rebaja.Anulaciones -- Mod 8 mzo 2016
   ! GoSub Suma.Por.Mayor
   Call  ARMA.NEW.TRX
!-- Almacena trx en archivo 
   For ue.for = 1 To Contador
      ascad$  = Left$(Boleta$(ue.for) + String$(100, " "), 100)
      larcad$ = "C" + str$(len(ascad$)) + " C2"
      Write form larcad$; #sesssal; ascad$, crlf$
   Next ue.for
   Close sesssal
!EndIf
Genera.Ascii = -1
Exit Function

Rebaja.Anulaciones:
for ue.for = 1 to contador
   If (left$(Boleta$(ue.for), 2) = "01") Then Begin ! si es un articulo
      Puntero = ue.for
      GoSub Parsea.Datos
      
      !Print " Tipo :" + mid$(Boleta$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1)
      
      If (mid$(Boleta$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1) = "80") or \
         (mid$(Boleta$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1) = "81") Then Begin             ! Anulacion

         codigo$  = mid$(Boleta$(ue.for), Busca1 + 1, Busca2 - Busca1 - 1)
         cantidad = val(mid$(Boleta$(ue.for), Busca8 + 1, Busca9 - Busca8 - 1))
         monto    = val(mid$(Boleta$(ue.for), Busca2 + 1, Busca3 - Busca2 - 1))
         
!         Print "Cantidad Anulada "
!         Print "Codigo "+Codigo$
!         Print "Cantidad "+str$(cantidad)
!         Print "Monto "+str$(monto)
!         print "Qty Ori "+str$(cantidad.original)
!         print "Mto Ori "+str$(monto.original)
         
         for i = 1 to ue.for-1
            If (left$(Boleta$(i), 2) = "01") Then Begin                                        ! si es un articulo
               Puntero = i
               GoSub Parsea.Datos
               If val(codigo$) = val(mid$(Boleta$(i), Busca1 + 1, Busca2 - Busca1 - 1)) Then Begin
                  cantidad.original = val(mid$(Boleta$(i), Busca8 + 1, Busca9 - Busca8 - 1))
                  monto.original    = val(mid$(Boleta$(i), Busca2 + 1, Busca3 - Busca2 - 1))
                  If not(val(mid$(Boleta$(i), Busca5 + 1, Busca6 - Busca5 - 1)) and 4000H) Then Begin
                     If cantidad.original >= cantidad  Then Begin
                        cantidad.original = cantidad.original - cantidad
                        monto.original    = monto.original - monto
                        Boleta$(i) = left$(mid$(Boleta$(i), 1, busca2) +                     \
                                           str$(monto.original) +                            \
                                           mid$(Boleta$(i), busca3, busca8 - busca3 + 1) +   \
                                           str$(cantidad.original) + \
                                           mid$(Boleta$(i), busca9, len(boleta$(i)) - busca9 + 1) +   \
                                           String$(100, " "), 100)
                        i = ue.for
                     EndIf \
                     Else If cantidad.original < cantidad  Then Begin
                        cantidad = cantidad - cantidad.original
                        monto    = monto - monto.original
                        cantidad.original = 0
                        !monto.original    = monto.original - monto
                        Boleta$(i) = left$(mid$(Boleta$(i), 1, busca2) +                     \
                                           str$(monto.original) +                            \
                                           mid$(Boleta$(i), busca3, busca8 - busca3 + 1) +   \
                                           str$(cantidad.original) + \
                                           mid$(Boleta$(i), busca9, len(boleta$(i)) - busca9 + 1) +   \
                                           String$(100, " "), 100)
                     EndIf
                  EndIf \
                  Else Begin
                     If cantidad.original = cantidad  Then Begin
                        cantidad.original = cantidad.original - cantidad
                        monto.original    = monto.original - monto
                        Boleta$(i) = left$(mid$(Boleta$(i), 1, busca2) +                     \
                                           str$(monto.original) +                            \
                                           mid$(Boleta$(i), busca3, busca8 - busca3 + 1) +   \
                                           str$(cantidad.original) + \
                                           mid$(Boleta$(i), busca9, len(boleta$(i)) - busca9 + 1) +   \
                                           String$(100, " "), 100)
                        i = ue.for
                     EndIf
                  EndIf
               EndIf
            EndIf
         next i
      EndIf
   EndIf
next ue.for
Return

Suma.Por.Mayor:
For ue.for = 1 to contador - 1
   If (left$(Boleta$(ue.for), 2) = "01") Then Begin ! si es un articulo
      Puntero = ue.for
      GoSub Parsea.Datos
      If ((mid$(Boleta$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1) = "00") or \
          (mid$(Boleta$(ue.for), Busca7 + 1, Busca8 - Busca7 - 1) = "01")) Then Begin             ! Anulacion
         If not(val(mid$(Boleta$(ue.for), Busca5 + 1, Busca6 - Busca5 - 1)) and 4000H) Then Begin
            codigo$  = mid$(Boleta$(ue.for), Busca1 + 1, Busca2 - Busca1 - 1)
            cantidad = val(mid$(Boleta$(ue.for), Busca8 + 1, Busca9 - Busca8 - 1))
            monto    = val(mid$(Boleta$(ue.for), Busca2 + 1, Busca3 - Busca2 - 1))
            For i = ue.for + 1 to contador
               If (left$(Boleta$(i), 2) = "01") Then Begin                                        ! si es un articulo
                  Puntero = i
                  GoSub Parsea.Datos2
                  If (mid$(Boleta$(i), Busca17 + 1, Busca18 - Busca17 - 1) = "00") or \
                     (mid$(Boleta$(i), Busca17 + 1, Busca18 - Busca17 - 1) = "01") Then Begin             ! Anulacion
                     If val(codigo$) = val(mid$(Boleta$(i), Busca11 + 1, Busca12 - Busca11 - 1)) Then Begin
                        cantidad.original = val(mid$(Boleta$(i), Busca18 + 1, Busca19 - Busca18 - 1))
                        monto.original    = val(mid$(Boleta$(i), Busca12 + 1, Busca13 - Busca12 - 1))
                        cantidad = cantidad.original + cantidad
                        monto    = monto.original + monto
                        Boleta$(i) = left$(mid$(Boleta$(i), 1, busca12) +                     \
                                     str$(0) +                            \
                                     mid$(Boleta$(i), busca13, busca18 - busca13 + 1) +   \
                                     str$(0) + \
                                     mid$(Boleta$(i), busca19, len(boleta$(i)) - busca19 + 1) +   \
                                     String$(100, " "), 100)
                     EndIf
                  EndIf
               EndIf
            next i
            Boleta$(ue.for) = left$(mid$(Boleta$(ue.for), 1, busca2) +                     \
                                    str$(monto) +                            \
                                    mid$(Boleta$(ue.for), busca3, busca8 - busca3 + 1) +   \
                                    str$(cantidad) + \
                                    mid$(Boleta$(ue.for), busca9, len(boleta$(ue.for)) - busca9 + 1) +   \
                                    str$(cantidad) + \
                                    String$(100, " "), 100)
         EndIf
      EndIf
   EndIf
Next ue.for
Return

Parsea.Datos:
!Print "Puntero [" + Str$(Puntero) + "]"
!Print "Boleta [" + Boleta$(Puntero) + "]"

Busca1 = match(" ", Boleta$(Puntero), 1)
Busca2 = match(" ", Boleta$(Puntero), Busca1 + 1)
Busca3 = match(" ", Boleta$(Puntero), Busca2 + 1)
Busca4 = match(" ", Boleta$(Puntero), Busca3 + 1)
Busca5 = match(" ", Boleta$(Puntero), Busca4 + 1)
Busca6 = match(" ", Boleta$(Puntero), Busca5 + 1)
Busca7 = match(" ", Boleta$(Puntero), Busca6 + 1)
Busca8 = match(" ", Boleta$(Puntero), Busca7 + 1)
Busca9 = match(" ", Boleta$(Puntero), Busca8 + 1)
Return

Parsea.Datos2:
!Print "Puntero [" + Str$(Puntero) + "]"
!Print "Boleta [" + Boleta$(Puntero) + "]"

Busca11 = match(" ", Boleta$(Puntero), 1)
Busca12 = match(" ", Boleta$(Puntero), Busca11 + 1)
Busca13 = match(" ", Boleta$(Puntero), Busca12 + 1)
Busca14 = match(" ", Boleta$(Puntero), Busca13 + 1)
Busca15 = match(" ", Boleta$(Puntero), Busca14 + 1)
Busca16 = match(" ", Boleta$(Puntero), Busca15 + 1)
Busca17 = match(" ", Boleta$(Puntero), Busca16 + 1)
Busca18 = match(" ", Boleta$(Puntero), Busca17 + 1)
Busca19 = match(" ", Boleta$(Puntero), Busca18 + 1)
Return


! Registro desconocido
regisXX:
ascad$ = "ERRREG " +                                      \
         right$("00000" + hex$(pos.arc / 512), 5) + " " + \
         right$("000" + hex$(mod(pos.arc, 512)), 3) +     \
         " " + unpack$(mid$(registro$, 2, 1)) + crlf$
Return

! Registro con encabezado
regis00:
ini = 1
While ini < len(registro$)
   cadena$ = reccaden
   i = Match("\" + left$(cadena$, 1), tipocad$, 1) + 1
   On i GoSub cadenXX, caden00, caden01, caden02, caden03, caden04,          \
              caden05, caden06, caden07, caden08, caden09, caden10, caden11, \
              caden12, caden13, caden14, caden15, caden16, caden20, caden21, \
              caden30, caden31, caden98, caden99

   Contador = Contador + 1
   Boleta$(Contador) = ascad$
Wend

Return

! Registro de excepcion
regis20:
cadena$ = mid$(registro$, 2, len(registro$) - 1)
GoSub caden20
Contador = Contador + 1
Boleta$(Contador) = ascad$
Return 

! Registro de cierre
regis21:
cadena$ = mid$(registro$, 2, len(registro$) - 2)
GoSub caden21
Contador = Contador + 1
Boleta$(Contador) = ascad$
Return 

! Registro de operador en entrenamiento
regis98:
cadena$ = mid$(registro$, 2, len(registro$) - 2)
GoSub caden98
Contador = Contador + 1
Boleta$(Contador) = ascad$
Return 

! Registro de usuario
regis99:
cadena$ = mid$(registro$, 2, len(registro$) - 2)
GoSub caden99
Contador = Contador + 1
Boleta$(Contador) = ascad$
Return 

! Cadena desconocida
cadenXX:
pos.cad = pos.arc + ini - len(cadena$) - 4
ascad$ = "ERRCAD " +                                      \
         right$("00000" + hex$(pos.cad / 512), 5) + " " + \
         right$("000" + hex$(mod(pos.cad, 512)), 3) +     \
         " " + unpack$(mid$(cadena$, 1, 1))
Return 

! Cabecera
caden00:
ascad$ = unpack$(left$(cadena$, 1)) + " " +    \ ! tipo de cadena
         unpack$(mid$(cadena$, 3, 2)) + " " +  \ ! terminal
         unpack$(mid$(cadena$, 6, 2)) + " " +  \ ! numero de transaccion
         unpack$(mid$(cadena$, 9, 5)) + " " +  \ ! fecha y hora
         unpack$(mid$(cadena$, 15, 1)) +       \ ! tipo de transaccion
         campvar$(cadena$, 17) + " "
         
X.TERMINAL$ = unpack$(mid$(cadena$, 3, 2))
!Call  informar("X.TERMINAL$" + X.TERMINAL$, 0)
Return 

! Entrada de articulo
caden01:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Extension a la entrada de articulo
caden02:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Descuento
caden03:
ascad$ = unpack$(left$(cadena$, 1)) + " " +    \ tipo de cadena
         unpack$(mid$(cadena$, 3, 1)) +        \ numero del descuento
         campvar$(cadena$, 5)
Return

! Invalidacion de descuento
caden04:
ascad$ = unpack$(left$(cadena$, 1)) + " " +    \ tipo de cadena
         unpack$(mid$(cadena$, 3, 1)) +        \ numero del descuento
         campvar$(cadena$, 5)
Return

! Pago
caden05:
ascad$ = unpack$(left$(cadena$, 1)) + " " +    \ tipo de cadena
         unpack$(mid$(cadena$, 3, 1)) +        \ tipo de pago
         campvar$(cadena$, 5)
Return

! Correccion de pago
caden06:
ascad$ = unpack$(left$(cadena$, 1)) + " " +    \ tipo de cadena
         unpack$(mid$(cadena$, 3, 1)) +        \ tipo de pago
         campvar$(cadena$, 5)
Return

! impuesto
caden07:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! invalidacion de impuesto
caden08:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Cambio de estampillas de alimentacion
caden09:
ascad$ = unpack$(left$(cadena$, 1)) +    \ tipo de cadena
         campvar$(cadena$, 3) + " "
Return

! Autorizacion de supervisor
caden10:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Entrada de dato
caden11:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Cambio de precio
caden12:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Cambio de gaveta
caden13:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Cambio de articulo
caden14:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         right$(cadena$, len(cadena$) - 2)
Return

! Canje de estampillas
caden15:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Reservada para EFT
caden16:
ascad$ = unpack$(left$(cadena$, 1)) +          \ tipo de cadena
         campvar$(cadena$, 3)
Return

! Excepcion
caden20:
ascad$ = unpack$(left$(cadena$, 1))    + " " + \ ! tipo de cadena
         unpack$(mid$(cadena$, 3, 2))  + " " + \ ! numero de terminal
         unpack$(mid$(cadena$, 8, 2))  + " " + \ ! numero de transaccion
         unpack$(mid$(cadena$, 13, 5)) + " " + \ ! fecha hora
         unpack$(mid$(cadena$, 21, 1)) + " " + \ ! tipo de excepcion
         campvare$(cadena$, 24)
Return

! Cierre
caden21:
ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
         campvar$(cadena$, 3)
Return

! reservada para EFT
caden30:
ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
         campvar$(cadena$, 3)
Return

! reservada para EFT
caden31:
ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
         campvar$(cadena$, 3)
Return

! operador en entrenamiento
caden98:
ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
         campvar$(cadena$, 3)
Return

! Definida por el usuario
caden99:
J = 3
GoSub GETFIELD                                           ! Obtengo Primer  Campo del registro Tipo de String
If A$ = "20040903" Then Begin
   If len(cadena$) > 8 Then Begin
   GoSub GETFIELD1                                        ! Obtengo Segundo Campo del registro Numero de Documento
   Numero.Documento = val(A$)
!   Call  Informar("Numero.Documento " + str$(Numero.Documento), 0)
   GoSub GETFIELD                                        ! Obtengo Segundo Campo del registro Numero de Documento
   Tipo.Documento = Val(pack$(A$))
!   Call  Informar("Tipo.Documento " + str$(Tipo.Documento), 0)
   Open "EAMNMFIS" KEYED RECL 80 AS 12   ! Abre control numeacion fiscal
   X.KEY$ = RIGHT$("0000" + X.TERMINAL$, 4)
   X.KEY$ = PACK$(X.KEY$)
   X.LEC$ = "C2 2C15 C12 C3 C4 2I4 C6 C3 3I4"
   Read FORM X.LEC$; #12 KEY X.KEY$; X.KEY$,           \ !' Numero de la terminal
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
!   Call  Informar("Asc.Fis.FacNum% " + str$(Asc.Fis.FacNum%), 0)
   If Tipo.Documento = 2 and Asc.Fis.FacNum% <= Numero.Documento Then \
      Asc.Fis.FacNum% = Numero.Documento + 1
   If Tipo.Documento = 1 and Asc.Fis.NotaNum% <= Numero.Documento Then \
      Asc.Fis.NotaNum% = Numero.Documento + 1
   If Tipo.Documento = 3 and Asc.Fis.NotaCrd% <= Numero.Documento Then \
      Asc.Fis.NotaCrd% = Numero.Documento + 1
   Write FORM X.LEC$; #12; X.KEY$,           \ !' Numero de la terminal
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
   close 12
   EndIf
EndIf
If A$ = "20080812" Then Begin
   ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
            campvar$(cadena$, 3)
   Return
EndIf
If A$ = "20110818" Then Begin
   ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
            campvar$(cadena$, 3)
   Return
EndIf

If A$ = "97" Or A$ = "98" Then Begin
   ascad$ = unpack$(left$(cadena$, 1)) + \         ! tipo de cadena
            campvar$(cadena$, 3)
   Return
EndIf


If A$ = "20110831" and rh.proforma% = 1 Then Begin
   GoSub GETFIELD                                        ! Obtengo Segundo Campo del registro Numero de Documento
   sec.proforma$ = right$("000000000" + str$(val(mid$(A$, 3, 10))), 7)
   If mid$(A$, 1, 2) = "26" Then \
      nombre.archivo$ = "P" + sec.proforma$ + "." + numero.tienda$ \
   Else \
      nombre.archivo$ = "R" + sec.proforma$ + "." + numero.tienda$
   create "C:\ADX_UDT1\PROFORMA\" + nombre.archivo$ as 48
   linea$ = date$ + "," + sec.proforma$ + "," + chr$(13) + chr$(10)
   write form "C17"; #48;  linea$
   close 48
   Call  transmite.archivo
   Return
EndIf

ascad$ = unpack$(left$(cadena$, 1)) + " " + \            ! tipo de cadena
         unpack$(right$(cadena$, len(cadena$) - 2))


Return

!*********************************************************************
GETFIELD:
K  = MATCH(":",cadena$,J)                               ! SEARCH For  FIELD SEPERATOR
If (k-j) > 0 Then \
   A$ = UNPACK$(MID$(cadena$,J,K-J)) Else \             ! UNPACK FIELD
	 A$ = ""
J  = K + 1                                              ! POINT TO BeginNING OF Next FIELD
Return

GETFIELD1:
K  = MATCH(";",cadena$,J)                               ! SEARCH For  FIELD SEPERATOR
If (k-j) > 0 Then \
   A$ = UNPACK$(MID$(cadena$,J,K-J)) Else               ! UNPACK FIELD
 	 A$ = ""
J  = K + 1                                              ! POINT TO BeginNING OF Next FIELD
Return

!*********************************************************************

Err.Genera.Ascii:
If Err = "IH" Then Resume 
If err = "ME" And Errf% = 18 Then Begin
   ret = osshell("md C:\DATOS\" + Fecha$ + "\")
   wait;500
   Resume retry
EndIf

Call  Informar("Error No Esperado Genera: " + Err + " " + Str$(Errf%) + " " \
       + Errn.To.Hex$(Errn) + " " + Str$(Errl), 0)
Stop

Fend

!**************************************************************
!********************  PROGRAMA PRINCIPAL  ********************
!**************************************************************
On Error GoTo Err.Proceso

!'Version$ = "1.0.1" ! 20-10-2009 FVG Version Base Decodificador
!'Version$ = "1.0.2" ! 18-11-2009 IDB
!'Version$ = "1.0.3" ! 07-04-2010  CAA
!'Version$ = "1.0.4" ! 08-Mzo-2016  GR-OVS
!'Version$ = "1.1.0" ! 16-Mzo-2017  GR-OVS
!'Version$ = "1.1.1" ! 24-Abr-2017  GR-OVS
!'Version$ = "1.1.2" ! 10-May-2017  GR-OVS
!'Version$ = "1.1.3" ! 19-May-2017  GR-OVS

Version$ = "1.1.4" ! 05-Sep-2017  GR-OVS
Tsl.Sess      = 1
Term.Sess     = 2
Ctr.Sess      = 3
Pos.Sess      = 5
Salida.Sess   = 6
Caj.Sess      = 11
Tiempo.Espera = 6000

Open "ASCNTRL" AS 38 NOWRITE NODEL
If End #38 Then RH.UE.FINAL
While (1)                              ! Recorre archivo
   Read #38; aString$
   If aString$ = "[BOVEDA]" Then Begin
      Read #38; aString$
      hora.manana% = Val(Mid$(aString$, 30, 6))
      Read #38; aString$
      hora.tarde% = Val(Mid$(aString$, 30, 6))
      GoTo RH.UE.FINAL
   EndIf
Wend

RH.UE.FINAL:
Close 38

Header.str$     = pack$("00")                                    !' String header "00"
Close.str$      = pack$("21")                                    !' String Close "21"
Checkout.trans$ = "00"                                           !' Transacci¢n de Checkout
Str.type99$     = ",""" + pack$("99") + ":"                      !' String type matching "99"
Str.type01$     = ",""" + pack$("01") + ":"                      !' String type matching "01"
Str.type02$     = ",""" + pack$("02") + ":"                      !' String type matching "02"
Str.type03$     = ",""" + pack$("03") + ":"                      !' String type matching "03"
Str.type04$     = ",""" + pack$("04") + ":"                      !' String type matching "04"
Str.type05$     = ",""" + pack$("05") + ":"                      !' String type matching "05"
Colon$          = ":"
sepacad$        = chr$(34) + chr$(44) + chr$(34) + "?:"

start.bck:
pase = 0
ya.ejecute = 0
Call  Informar("Version : " + Version$, 0)

Archivo.Temp$ = "ADX_UDT1:RHPOINTA.DAT"
Arch.Cajeras$ = "ADX_UDT1:MPCAJERA.DAT"
Call  adxserve(0, 4, 0, Parm2$)
Numero.Tienda$ = right$("000" + mid$(Parm2$, 1, 4), 3)

Existe = 1
Open Archivo.Temp$ Keyed Recl 46 As Pos.Sess
If Not(Existe) Then Begin
   Call  Informar("Creando Archivo " + Archivo.Temp$ + "...", 500)
   Create Posfile Archivo.Temp$ Keyed 4,,,20000 Recl 46 As Pos.Sess compound perupdate
EndIf

Existe = 1
Open Arch.Cajeras$ Keyed Recl 26 As Caj.Sess
If Not(Existe) Then Begin
   Call  Informar("Creando Archivo " + Arch.Cajeras$ + "...", 500)
   Create Posfile Arch.Cajeras$ Keyed 14,,,4000 Recl 26 As Caj.Sess  compound perupdate
EndIf

Existe = 1
Open "ADX_UDT1:MPCAJERO.DAT" Keyed Recl 26 As 12
If Not(Existe) Then Begin
   Call  Informar("Creando Archivo " + "ADX_UDT1:MPCAJERO.DAT" + "...", 500)
   Create Posfile "ADX_UDT1:MPCAJERO.DAT" Keyed 14,,,4000 Recl 26 As 12 compound perupdate
EndIf
Close 12

!' Abro archivo de Status de Terminal
Open "EAMTERMS" Keyed Recl 72 as Term.Sess
Read Form "C2 C8 C62";#Term.Sess Key Pack$("9999"); A$, Trans.Log$, A$
Close Term.Sess

Call  Informar("Archivo TSL [" + Trans.Log$ + "]", 0)

!' Abro el archivo de entrada que corresponde al archivo de transacciones
!Open Trans.Log$ as Tsl.Sess Buffsize 1024 Unlocked Nowrite

Open "Q:/EAMTRANA.DAT" as Tsl.Sess Buffsize 1024 Unlocked Nowrite

!' Abro el archivo de control de transacciones procesadas. En el se mantiene
!' informaci¢n que permita asegurar que todas las transacciones de facturaci¢n
!' sean procesadas ante eventuales fallas.
Existe = 1
Open "adx_udt1:cntrpos.dat" Direct Recl 40 as Ctr.Sess
If Not(Existe) Then Begin
   Call  Informar("Creando Puntero....", 100)
   Create posfile "adx_udt1:cntrpos.dat" Direct 1 Recl 40 as Ctr.Sess compound perupdate
   Point.Restart = 0
   Call  Write.Control
EndIf

Call  nc.activa

!' Recupero valores de contadores y Offset de lectura
Call  read.control
!' Verifico si es una restauraci¢n
Point Tsl.Sess; Point.Restart

!' Se setea en TRUE la variable de control de ciclo
Loop = 1
While (Loop)
!   GoSub verifica.estado
   Hay.tr = -1
   !' Leo Una Transacci¢n
   Read #Tsl.Sess; Line Trans.In$
   Print "Longitud trama uno "+Len(Trans.In$) 
   Print "Longitud trama dos "+BigLen(Trans.In$) 
   
   !' Actualizo posici¢n offset de lectura
   Point.Restart = PTRRTN
   Call  Write.Control

   !' Si es un cierre
   If (Mid$(Trans.In$, 2, 1) = Close.Str$) Then Begin
      Parm2$ = "Transaccion de Cierre En Proceso " + Str$(Point.Restart)
      Call  Informar(Parm2$, 0)
      Loop = 0
      ya.borre = 0
      Archivo.Sal$ = "c:\adx_udt1\RHPOINTP.DAT"
      Sal.Cajeras$ = "c:\adx_udt1\MPCAJERO.DAT"
      Call  genera.nombre
      Archivo.Aux$ = "c:\adx_udt1\RHPO" + Rh.nombre$ + ".DAT"
      Aux.Cajeras$ = "c:\adx_udt1\MPCJ" + Rh.nombre$ + ".DAT"
      !' Copio archivo
      Call  ADXCOPYF (RETC, Archivo.Sal$, Archivo.Aux$, 0, 2)
      Call  ADXCOPYF (RETC, Archivo.Temp$, Archivo.Sal$, 0, 2)
      Call  ADXCOPYF (RETC, Sal.Cajeras$, Aux.Cajeras$, 0, 2)
      Call  ADXCOPYF (RETC, Arch.Cajeras$, Sal.Cajeras$, 0, 2)
      If retc = 0 Then Begin
         Delete Pos.Sess, Ctr.Sess, Caj.Sess
         Close Tsl.Sess
         Call  Informar("Se Creo el Archivo " + Archivo.Sal$, 3000)
         Call  adxstart("adx_upgm:rhcuadre.286", "T", "Informe Boveda Turno Tarde")
      EndIf \
      Else Begin
         Call  Informar("Error "+ str$(retc), 0)
      EndIf

   EndIf

   If Hay.tr Then Begin
      If (Mid$(Trans.In$, 2, 1) = Header.Str$) Then Begin
         !' Obtengo el numero de terminal, el numero de transacci¢n, la fecha
         !' y el tipo de transacci¢n
         Term.Num$   = unpack$(mid$(Trans.in$,  4, 2))
         Num.Trans$  = unpack$(mid$(Trans.in$,  7, 2))
         Fecha$      = unpack$(mid$(Trans.in$, 10, 3))
         Hora$       = unpack$(mid$(Trans.in$, 13, 2))

         Parm2$ = "Procesando = " +       \
                  str$(Point.restart) +   \
                  " " + Left$(Hora$, 2) + \
                  ":" + Right$(Hora$, 2)
         Call  Informar(Parm2$, 0)

         Clave$   = Pack$(Right$("00000000" + Term.Num$ + Num.Trans$, 8))
         Puntero$ = Pack$(Right$("00000000" + Str$(Point.Restart), 8))

         Tipo.Documento = 0
         Call  Informar("Grabando Puntero POS [" + Term.Num$ + "] Trx [" + Num.Trans$ + \
                       "] Puntero [" + Str$(Point.Restart) + "]", 0)
         registro$ = Trans.In$

         Call  Genera.Ascii

         Estado$ = Right$("   " + Str$(Tipo.Documento), 3)
         Filler$ = String$(35, "0")
         Write Form "C4 C4 C3 C35"; #Pos.Sess; Clave$,   \
                                               Puntero$, \
                                               Estado$,  \
                                               Filler$

         If k% > 0 Then Begin
            MontoTrx$ = ""
            For j%=1 To K%
               Clave.Operador$ = Operador$ + Term.Num$ + mid$(Pagos$(j%), 4, 2)
               sw = 0
               Read Form "C14 C8 C4"; #Caj.Sess key Clave.Operador$; Aux$,      \
                                                                     MontoTrx$, \
                                                                     NumTrx$
               Busca1 = match("|", Pagos$(j%), 1)
               Busca2 = match("|", Pagos$(j%), Busca1 + 1)
               signo% = -1
               If left$(Pagos$(j%), 2) = "05" Then \
                  signo% = 1
               If sw = 0 Then Begin
                  MontoTrx$ = str$(Val(MontoTrx$) + \
                                   Val(mid$(Pagos$(j%), Busca2 + 1, Len(Pagos$(j%)) - (Busca2)))*signo%)
                  MontoTrx$ = right$("00000000" + MontoTrx$, 8)
                  NumTrx$   = right$("0000" + str$(Val(NumTrx$) + 1*signo%), 4)
               EndIf \
               Else Begin
                  MontoTrx$ = str$(Val(mid$(Pagos$(j%), Busca2 + 1, Len(Pagos$(j%)) - (Busca2)))*signo%)
                  MontoTrx$ = right$("00000000" + MontoTrx$, 8)
                  NumTrx$   = right$("0000" + str$(1*signo%), 4)
               EndIf
               Write Form "C14 C8 C4"; #Caj.Sess; Clave.Operador$, \
                                                  MontoTrx$, \
                                                  NumTrx$
            Next j%
         EndIf \
         Else If sigoff$ <> "" Then Begin
            Clave.Operador$ = Operador$ + Term.Num$ + "00"
            MontoTrx$ = mid$(sigoff$, 4, 10)
            NumTrx$   = "0T"
            If Val(mid$(montoTrx$, 7, 4)) < 1500 Then NumTrx$ = "0M"
            Write Form "C14 C10 C2"; #Caj.Sess; Clave.Operador$, \
                                                MontoTrx$, \
                                                NumTrx$
         EndIf

      EndIf
   EndIf \
   Else Begin
      !' Indica la version de la aplicacion
      a$ = Y2K.FECHA$(Fecha$, 1)
      b$ = Hora$
      If a$ = " " Then \
         a$ = Y2K.FECHA$("", 1)
      If b$ = " " Then \
         b$ = Time$
      If Num.trans$ = " " Then \
         Num.trans$ = "0"

      Parm2$ = Str$(Point.Restart)     + \
               " " + Left$(b$, 2)      + \
               ":" + Mid$(b$, 3, 2)    + \
               " " + Term.Num$         + \
               " " + Num.Trans$        + \
               " " + Mid$(Time$, 1, 2) + \
               ":" + Mid$(Time$, 3, 2) + \
               " " + version$          + \
               " " + Mid$(a$, 7, 2)    + \
               "/" + Mid$(a$, 5, 2)    + \
               "/" + Mid$(a$, 1, 4)
      Call  Informar(Parm2$, 1000)

      If (Val(time$) > hora.manana%) And (Val(time$) < (hora.manana% + 1000)) And ya.ejecute = 0 Then Begin
         Call  adxstart("adx_upgm:rhcuadre.286", "M", "Informe Boveda Turno Mañana")
         ya.ejecute = 1
      EndIf

      If (Val(time$) > hora.tarde%) And (Val(time$) < (hora.tarde% + 1000)) And ya.ejecute = 0 Then Begin
         Call  adxstart("adx_upgm:rhcuadre.286", "T", "Informe Boveda Turno Tarde")
         ya.ejecute = 1
      EndIf

      If (Val(time$) > (hora.manana% + 10000)) and (Val(time$) < hora.tarde%) Then \
         ya.ejecute = 0
      If (Val(time$) > (hora.tarde% + 10000)) Then \
         ya.ejecute = 0
      If (Val(time$) < hora.manana%) Then \
         ya.ejecute = 0

      If val(time$) >= 233000 and ya.borre = 0 Then Begin
         Call  Borra.Archivos          
         ya.borre = 1
      EndIf 
       
   EndIf
Wend

Print "Regreso al comienzo"
GoSub start.bck


Verifica.Estado:
Existe = 1
Archivo.cfg$ = "c:\adx_udt1\rhadmfis.cfg"
Open Archivo.Cfg$ direct recl 82 As 48
If Not(Existe) Then Begin
   Call  Informar("Error No Existe CFG [" + Archivo.Cfg$ + "]", 0)
   Stop
EndIf
Existe = 1
Read Form "C80 C2"; #48, 2; paso2$, paso1$
If Not(Existe) Then Begin
   Call  Informar("Error al Leer CFG [" + Archivo.Cfg$ + "]", 0)
   Stop
EndIf
Read Form "C80 C2"; #48, 3; paso3$, paso1$
If Not(Existe) Then Begin
   Call  Informar("Error al Leer CFG [" + Archivo.Cfg$ + "]", 0)
   Stop
EndIf

While Len(paso2$) > 0
   If mid$(paso2$, 1, 2) <> "  " Then Begin
      estado$ = mid$(paso3$, val(mid$(paso2$, 1, 2)), 1)
      If estado$ = "0" Then Begin
         Call  adxserve(Ret.cod, 30, val(mid$(paso2$, 1, 2)), men$)
         If ret.cod = 0 Then Begin
            paso3$ = mid$(paso3$, 1, val(mid$(paso2$, 1, 2)) - 1) + "1" + \
                     mid$(paso3$, val(mid$(paso2$, 1, 2)) + 1, 80 - val(mid$(paso2$, 1, 2)))
            write Form "C80 C2"; #48, 3; paso3$, paso1$
         EndIf
       EndIf
       paso2$ = right$(paso2$, len(paso2$) - 3)
   EndIf \
   Else \
      paso2$ = ""
Wend 
Close 48
Return


Err.Proceso:
If Err = "IH" Then Resume 
If Err = "OE" And Errf% = Term.Sess Then Begin
   Call  Informar("Error Grave No Existe EAMTERMS", 0)
   Stop
EndIf \
Else If Err = "OE" And Errf% = Tsl.Sess Then Begin
   Call  Informar("Error Grave No Existe TSL: " + Trans.Log$, 0)
   Print Errn.To.Hex$(Errn) + " " + Str$(Errl)
   Stop
EndIf \
Else If Err = "OE" And Errf% = Ctr.Sess Then Begin
   Existe = 0
   Resume
EndIf \
Else If Err = "OE" And Errf% = Pos.Sess Then Begin
   Existe = 0
   Resume
EndIf \
Else If Err = "OE" And Errf% = Caj.Sess Then Begin
   Existe = 0
   Resume
EndIf \
Else If Err = "OE" And Errf% = 12 Then Begin
   Existe = 0
   Resume
EndIf \
Else If (Err = "EF" And Errf% = Caj.Sess) Then Begin
   sw = 1
   Resume
EndIf \
Else If (Err = "EF" And Errf% = Tsl.Sess) Then Begin
   hay.tr = 0
   wait; 1000
   Resume
EndIf

Call  Informar("Error No Esperado: " + Err + " " + Str$(Errf%) + \
              " " + Errn.To.Hex$(Errn) + " " + Str$(Errl), 0)
End
