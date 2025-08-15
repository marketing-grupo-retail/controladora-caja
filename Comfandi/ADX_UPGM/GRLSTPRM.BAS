Integer*1 Global Terror%


Sub Lista.Promociones
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Aplicada%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, KEY$, A$, Mes$(1)
Integer*2 Key.Len, Rec.Len, Lpanta%, Mes1%, Mes2%
REC.NO = 1					             														 ! Initialize Counter
I% = 1						               														 ! Initialize Counter
Dim Mes$(13)
Mes$(01) = "Ene" : Mes$(02) = "Feb" : Mes$(03) = "Mzo"
Mes$(04) = "Abr" : Mes$(05) = "May" : Mes$(06) = "Jun"
Mes$(07) = "Jul" : Mes$(08) = "Ago" : Mes$(09) = "Sep"
Mes$(10) = "Oct" : Mes$(11) = "Nov" : Mes$(12) = "Dic"

Open "MKTP:PRMADMON.DAT" KEYED RECL  9   AS 34   						 ! Apertura Control de promociones
Open "MKTP:PRMADMON.DAT" DIRECT RECL 512 AS 33   						 ! Apertura control de promociones
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		                       ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
Terror% = -1                                                 ! Control de errores
Lpanta% = 2 : Clears
Print "PROMO   Fecha Inicial  Fecha Final    Estado"
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$= Unpack$(MID$(H$,X+0,2))                            ! Codigo de la promocion
    C02$= Unpack$(MID$(H$,X+2,3))                            ! Fecha de Inicio de la promocion
    C03$= Unpack$(MID$(H$,X+5,3))                            ! Fecha Final de la promocion
    C04$= Mid$(H$,X+8,1)                                     ! Estado de la promocion
    If C04$ = "0" Then C04$ = "Pendiente por Activar" Else C04$ = "Promocion Activa"
    Mes1% = Val(Mid$(C02$,3,2))
    Mes2% = Val(Mid$(C03$,3,2))
    Print C01$+" -- "+ \
          "20"+Left$(C02$,2)+"/"+Mes$(mes1%)+"/"+Right$(C02$,2) + \
          " -- " + \ 
          "20"+Left$(C03$,2)+"/"+Mes$(mes2%)+"/"+Right$(C03$,2) + \
          " -- "+ C04$
     X = X + REC.LEN                                          ! Index to next key
     KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
     If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
     Lpanta% = Lpanta% + 1 
     If Lpanta% > 23 Then Begin 
     	 Input "Presione Enter Continuar...";a$
     	 Clears
     	 Print "PROMO   Fecha Inicial  Fecha Final    Estado"
     	 Lpanta% = 2
     EndIf
  Wend
Next REC.NO
Input "Consulta Terminada, Presione Enter Continuar...";a$
End Sub 

On Error GoTo ErrAppl
Call Lista.Promociones
Close 33 : Close 34
Stop 

ErrAppl:
Terror% = 0
Print "Error Aplicacion "+Err
Stop 
