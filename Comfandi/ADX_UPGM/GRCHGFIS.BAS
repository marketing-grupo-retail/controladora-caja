! PROGRAMA GRCHGFIS.BAS
! CREACION NUEVA ESTRUCTURA FISCAL  TFNUMFIS         
!                                   
!---------------------------------------------------------------


%ENVIRON C						   																		 ! Ambiente de controlador
Integer*1 Global Terror%

%Include POSPVARI.BAS				  	                ! Rutinas Comunes
%Include EAMITEMR.J86				  	                ! Maestro Articulos
%Include ADX_UPGM:DMEXTR.J86    		            ! Inclucion Libreria Display Manager
%Include POSPRUTI.BAS				  	                ! Rutinas Comunes
%Include ADX_UPGM:BASROUT.J86

 Function GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
 End Function


Sub Actualiza.Fiscal
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%, UE.INDI1%, UE.TARIFA%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$, XCODENSA$, Prov$, IND1$, UE.BIN$, Tarifa$
String Rbuffer$, tag$, C07$, C08$, C09$
Integer*2 Key.Len, Rec.Len
Integer*4 U.Ini%, U.Fin%, U.Act%, U.Fail%

Ncli% = 0 
Terror% = -1
Open "TF:NUMFIS" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de promociones
REC.NO = 1					             ! Initialize Counter
I% = 1						             ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
U.Fail% = 0
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  LOCATE 4,1 : Print "PROCESANDO BLOQUE :"+STR$(REC.NO)+" DE "+STR$(BLK.NUM)
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$ = Unpack$(MID$(H$,X+ 0,2))                           ! Numero de terminal
    C02$ =        (MID$(H$,X+ 2,8))                           ! Serial Terminal
    C03$ = (MID$(H$,X+10,7))                           ! Numero de resolucion nuevo DIAN
    C04$ =        (MID$(H$,X+17,4))                           ! Prefijo facturacion
    C05$ = STR$(GETN4(H$,21))                           ! Numero inicio factura
    C06$ = STR$(GETN4(H$,25))                           ! Numero final factura
    C07$ = (MID$(H$,X+29,6))                           ! Numero resolucion anterior 
    C08$ = (MID$(H$,X+35,3))                           ! fecha autorizacion
    C09$ = STR$(GETN4(H$,38))                          ! Numero actual factura

    U.Act% = Val(C09$)
    U.Ini% = Val(C05$)
    U.Fin% = Val(C06$)

    C01$ = Pack$(RIGHT$("0000"+C01$,4))
    C09$ = Date$
    C09$ = Pack$(C09$)
    Terror% = -1
    Write Form "C2 C8 C7 C4 2I4 C6 2C3 I4"	;#76 ;   \! Grabacion de registros
        C01$,        \ Numero de terminal
        C02$,        \ Numero serial
        C03$,        \ Numero Autorizacion
        C04$,        \ Prefijo de facturacion
        U.INI%,      \ Rango inicial
        U.FIN%,      \ Rango Final
        C07$,        \ Numero Autorizacion
        C08$,        \ Fecha de autorizacion
        C09$,        \ Fecha de vencimiento
        U.ACT%       ! Numero actual de registro
    If Terror% = 0 Then Begin																		! Error grabación
    	  U.Fail% = U.Fail% + 1
    EndIf

    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 LOCATE 6,1 : Print "Total de registros fallidos :"+Str$(U.Fail%)
 Print "Procedimiento Migracion Finalizado"
End Sub 


!--- Bloque Principal

On Error GoTo ERR.FIS
Clears
Print "Grupo Retail Ltda - Migracion Numeracion Fiscal"
Print "Versión 17/Ene/2019" 
Terror% = -1
ARCH1$ = "TF:NUMNEW"
CREATE POSFILE ARCH1$ KEYED 2,,,5000 RECL 45 As 76 compound perupdate 		   ! Creacion nueva estructura
Call Actualiza.Fiscal
Close 76
Stop 

ERR.FIS:
Terror% = 0
  If ERR = "IH" Then Resume
  If ERR = "EF" Then Resume
  CALL TRADUCE.ERROR
  MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  LOCATE 24,1:PRINT MSG$                          ! Presenta Error pantalla
  Stop 
  
