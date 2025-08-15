Integer*4 Global BLK.NUM, REC.LEN, KEY.LEN, TS.ERROR, REC.NO, R.S, R

On Error GoTo Paila
Clears
Print "Reactivacion Promociones Modulo Mercadeo Ver 1.0"
Open "MKTP:PRMADMON.DAT" KEYED RECL  9   AS 34   						 ! Apertura Control de promociones
Open "MKTP:PRMADMON.DAT" DIRECT RECL 512 AS 33   						 ! Apertura control de promociones
REC.NO = 1					             														 ! Initialize Counter
I% = 1						               														 ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		                       ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
TS.ERROR = -1                                                ! Control de errores
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
    MSG$ = "Activando  Promocion "+C01$+" STATUS "+C04$+" FI:"+C02$+" FF:"+C03$
    Print MSG$
    
     C01$ = PACK$(C01$)
     C02$ = PACK$(C02$)
     C03$ = PACK$(C03$)
     Write Form "C2 2C3 C1";#34; C01$, C02$, C03$, "0" 
     X = X + REC.LEN                                          ! Index to next key
     KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
     If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33 
Close 34
Print "Promociones Activadas"
Stop



Paila:

Print "Error Appl "+ ERR
Stop
