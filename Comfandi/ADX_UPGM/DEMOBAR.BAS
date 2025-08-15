!-------------------------------------
! Define variables and environment
!-------------------------------------
!

%ENVIRON T ! Environment is Terminal

STRING LF,CR,DI,FA,FB,FADH,FBDH,FADW,FBDW,FADHDW,FBDHDW
STRING MICR,FLIP,WORK$, CPI12, CPI15, CPI17, BARCODE, ENDBCODE
String UPCA,UPCE,JAN13,JAN8,CODE39,ITF,CODABAR,CODE128,CODE93, TS.TEMP1$


Open "CR:" As 34
PRT4610 = 34

! LINEFEED
LF=CHR$(0DH)
! Select CR: station
CR=CHR$(1BH)+CHR$(63H)+CHR$(30H)+CHR$(02H)
! Select DI: station
DI=CHR$(1BH)+CHR$(63H)+CHR$(30H)+CHR$(04H)

! Select font A
FA=CHR$(1BH)+CHR$(21H)+CHR$(00H)
! Select font B
FB=CHR$(1BH)+CHR$(21H)+CHR$(01H)
! Select font A - double high
FADH=CHR$(1BH)+CHR$(21H)+CHR$(10H)
! Select font B - double high
FBDH=CHR$(1BH)+CHR$(21H)+CHR$(11H)
! Select font A - double wide
FADW=CHR$(1BH)+CHR$(21H)+CHR$(20H)
! Select font B - double wide
FBDW=CHR$(1BH)+CHR$(21H)+CHR$(21H)
! Select font A - double high/double wide
FADHDW=CHR$(1BH)+CHR$(21H)+CHR$(30H)
! Select font B - double high/double wide
FBDHDW=CHR$(1BH)+CHR$(21H)+CHR$(31H)
! Select 12 CPI (characters per inch)
CPI12=CHR$(1BH)+CHR$(20H)+CHR$(05H)
! Select 15 CPI (characters per inch)
CPI15=CHR$(1BH)+CHR$(20H)+CHR$(03H)
! Select 17 CPI (characters per inch)
CPI17=CHR$(1BH)+CHR$(20H)+CHR$(02H)

!!Print various fonts on CR: station
!
! Select CR: and font A, add text, and end with linefeed
TS.TEMP1$ = "VALOR BONO :$9.999.999"

WORK$ = CR+FA+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font B (using the previously selected station),
! add text, and end with linefeed
WORK$ = FB+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font A double high (with previously selected station)
WORK$=FADH+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font B double high (with previously selected station)
WORK$=FBDH+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font A double wide (with previously selected station)
WORK$=FADW+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font B double wide (with previously selected station)
WORK$=FBDW+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font A double high/double wide (with previously selected station)
WORK$=FADHDW+TS.TEMP1$+LF
WRITE #PRT4610; WORK$
! Font A double high/double wide (with previously selected station)
WORK$=FBDHDW+TS.TEMP1$+LF
WRITE #PRT4610; WORK$

Stop 

!!Print various fonts on CR: station
!
! Select CR: and font A, add text, and end with linefeed
WORK$ = CR+FA+"This is Font A"+LF
WRITE #PRT4610; WORK$
!!set up variables
!
UPCA    = CHR$(00H)
UPCE    = CHR$(01H)
JAN13   = CHR$(02H)
JAN8    = CHR$(03H)
CODE39  = CHR$(04H)
ITF     = CHR$(05H)
CODABAR = CHR$(06H)
CODE128 = CHR$(07H)
CODE93  = CHR$(08H)
! Print Barcode command
BARCODE = CHR$(1DH)+CHR$(6BH)
ENDBCODE = CHR$(00H)
!!Print the supported barcodes
!
!WRITE #PRT4610; BARCODE+UPCA +"035846968269" +ENDBCODE
!WRITE #PRT4610; BARCODE+UPCE +"08753496" +ENDBCODE
!WRITE #PRT4610; BARCODE+JAN13 +"5012345678900" +ENDBCODE
!WRITE #PRT4610; BARCODE+JAN8 +"00017459" +ENDBCODE
!WRITE #PRT4610; BARCODE+CODE39 +"SC30-3461-2" +ENDBCODE
!WRITE #PRT4610; BARCODE+ITF +"12345678901234"+ENDBCODE
!WRITE #PRT4610; BARCODE+CODABAR+"C258542*" +ENDBCODE

Write #PRT4610; "EAN-128" + LF
Write #PRT4610; BARCODE+CODE128+"415770720991425380200105227093" +ENDBCODE

Write #PRT4610; "CODE 39" + LF
WRITE #PRT4610; BARCODE+CODE39 +"415770720991425380200105227093" +ENDBCODE

Write #PRT4610; "PROCESO TERMINADO" + LF

!WRITE #PRT4610; BARCODE+CODE93 +"SC30-3461-2" +ENDBCODE

STOP
