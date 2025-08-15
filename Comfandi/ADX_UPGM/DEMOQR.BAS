!-------------------------------------
! Define variables and environment
!-------------------------------------
!

%ENVIRON T ! Environment is Terminal

STRING LF,CR,DI,FA,FB,FADH,FBDH,FADW,FBDW,FADHDW,FBDHDW
STRING MICR,FLIP,WORK$, CPI12, CPI15, CPI17, BARCODE, ENDBCODE
String UPCA,UPCE,JAN13,JAN8,CODE39,ITF,CODABAR,CODE128,CODE93, TS.TEMP1$
String QRCode$, Cufe$, DataQR$
Integer*2 PRT4610

!------------------------------------------------------------------------------
! Imprime un buffer binario mayor de 240 bytes en el CR
! se debe usar cuando el buffer tiene un comando de mas de 240 bytes
! como codigos PDF417, graficos, etc.
!------------------------------------------------------------------------------
Sub imprimeData( s$ ) Public
   String    s$, p$, c$
   Integer*4 Xn%, Xb%, XI%, Xj%

   !comando continuacion
   c$ = Chr$(1BH) + Chr$(2EH)
   Xb% = 238
   Xn% = len(s$)
   Xi% = 1
   p$ = ""
   While Xi% <= Xn%
      If Mid$(s$, Xi%, Xb%) <> "" Then \
         p$ = p$ + Mid$(s$, Xi%, Xb%)
      Wait; 200
      Write #34; p$
      p$ = c$
      Xi% = Xi% + Xb%
   Wend

End Sub 

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
Sub imprimeQR( Xdata$ ) Public
   String Xdata$, xq$

   !escala 0 -> 3 puntos por pixel, 5 es el valor por defecto
   !write #34; chr$(1DH) + chr$(5FH) + chr$(5)

   xq$ = Chr$(1DH) + Chr$(4FH) + Chr$(0) + Chr$(1) + Chr$(0) + \
         Xdata$ + Chr$(0)

   Call imprimeData(xq$)
   
End Sub

!--- Bloque Principal

Open "CR:" As 34
PRT4610 = 34

! LINEFEED
LF=CHR$(0DH)
! Select CR: station
CR=CHR$(1BH)+CHR$(63H)+CHR$(30H)+CHR$(02H)
! Select DI: station
DI=CHR$(1BH)+CHR$(63H)+CHR$(30H)+CHR$(04H)


cufe$ = "329cd35245a172dfe1f9f9703746c12634128708ab3bcc764a287026e0c60d1638fecd86e306afab072b65be08beea7b"

QRCode$ = "https://catalogo-vpfe.dian.gov.co/document/searchqr?documentkey=" + \
          Cufe$
Work$ = "NumFac=SETT000000123" + LF +            \!
        "FecFac=2024/05/09"  + LF+                         \!
        "HorFac=09:16:41"  + LF+                         \!
        "NitFac=8903032085"  + LF+                         \!
        "DocAdq=1144034221"  + LF+                         \!
        "ValFac=129870"  + LF+                         \!
        "ValIva=4375"  + LF+                         \!
        "ValOtroIm=397" + LF+                      \!
        "ValTolFac140000=" + LF                        !
          
DataQR$ = Work$ + QRCode$

Write #34 ; "PRUEBA IMPRESION QR " + LF
Call imprimeQR( DataQR$ )
Write #34 ; "FINAL PRUEBA IMPRESION QR " + LF
Close 34 

Stop 
