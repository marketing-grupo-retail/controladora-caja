!**************************************************
!Empresa       : GRUPO RETAIL LTDA                *
!Programa      : PLACUSTA.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Junio 15 de 2.005                *
!Observaciones : Generacion de plano archivo de   *
!                EAMCUSTA                         *
!**************************************************

%ENVIRON C

STRING GLOBAL               \!
           FILEI1$,         \! Name of input data file 
           FILEI2$,         \! Name of input data file
           KEY$,            \! Key
           H$,              \! work area
           FSEP$,           \! Fields separator
           FINR$,           \! End of record characters
           PZERO$,          \! Packed zeros
           C01$,            \! Customer ID
           C02$,            \! Sentence Customer
           C03$,            \! Auxiliary variable 
           C04$,            \! Auxiliary variable 
           TMP.MSG$(1),     \! Vector Mensajes Aplicativo
           REG$,            \! Temporal Record
           LEC$,	          \! Definicion de registro
           BAN.PRG$         \! Temporal Variable
 
INTEGER *2 GLOBAL           \!
           KEY.LEN,         \! length of key
           REC.LEN,         \! length of record
           IOS1,            \! IO Session # 1
           IOS2,            \! IO Session # 2 
           R,               \! record count
           X,               \! record displacement
           R.S, Ctl%         ! Records on a given sector

INTEGER *4 GLOBAL           \!
           REC.NO,          \! # of current record 
           BLK.NUM,         \! Total # of rlocks
           MAX.R.SEC,       \! Max # of records on a sector
           V01%,            \! Initialize Value User Data
           V02%,            \! Initialize Value User Data
           V03%,            \! Initialize Value User Data
           V04%,            \! Initialize Value User Data
           V05%,            \! Initialize Value User Data
           V06%,            \! Initialize Value User Data
           V07%,            \! Initialize Value User Data
           V08%,            \! Initialize Value User Data
           ERRN%,           \! Error code program 
           RET.ERR%,        \! Control display manager
           I%,              \! Counter
           FJ%              \! Counter

STRING  GLOBAL  INITDM$, OPNDIS$, DISPD$,  POSF$,   NXTF$,     \ ! Var. display manager
                PUTF$,   CURS$,   CLSDIS$, PRM.ON$, PRM.OFF$,  \
                ON$,     OFF$,    TABS$,   TERM$,   INP$,      \
                ATTR$,   RET$,    FIN$        

INTEGER GLOBAL \
          ENTER.KEY, \         	! Definicion de la tecla Enter
          TAB.KEY,   \          ! Definicion de la tecla TAB
          ESC.KEY,   \        	! Definicion de la tecla ESCAPE
          F1.AYUDA,  \         	! Definicion de la tecla de funcion F1 
          F2.KEY  ,  \         	! Definicion de la tecla de funcion F2 
          F3.SALIR,  \        	! Definicion de la tecla de funcion F3
          F4.KEY  ,  \         	! Definicion de la tecla de funcion F4 
          F5.KEY  ,  \         	! Definicion de la tecla de funcion F5 
          F6.KEY  ,  \         	! Definicion de la tecla de funcion F6 
          F7.KEY  ,  \         	! Definicion de la tecla de funcion F7 
          F8.KEY  ,  \         	! Definicion de la tecla de funcion F8 
          F9.KEY  ,  \         	! Definicion de la tecla de funcion F9 
          FA.KEY  ,  \         	! Definicion de la tecla de funcion F10
          CALL.ORDER% ,\       	! Definicion orden llamado de pantalla
          J%,          \       	! Control de ciclos
          HELP%      \        	! Llamado Segunda Pantalla Display Manager


!------------------------------------------------------------------------------
!
!                             GETN1/GETN2/GETN4
!      Las siguientes rutinas extraen una/dos/cuatro bytes enteros de un
!      string. P2 es el offset dentro de la cadena de string
!
!------------------------------------------------------------------------------
 
  FUNCTION GETN1(P1$,P2) EXTERNAL                       
     INTEGER*1 GETN1
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN2(P1$,P2) EXTERNAL
     INTEGER*2 GETN2
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
  
  FUNCTION GETN4(P1$,P2) EXTERNAL
     INTEGER*4 GETN4
     STRING P1$
     INTEGER*2 P2
  END FUNCTION
!--------------------------------------------------------

!------------------------------------------------------------------------------
!
!                            PUTN1/PUTN2/PUTN4
!
!       These routines insert a one/two/four byte integer into a string.
!       P2 is the offset within the string and P3 is the source integer
!
!------------------------------------------------------------------------------
  
  FUNCTION PUTN1(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*1 P3
  END FUNCTION
  
  FUNCTION PUTN2(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2,P3
  END FUNCTION
  
  FUNCTION PUTN4(P1$,P2,P3) EXTERNAL
  STRING P1$
  INTEGER*2 P2
  INTEGER*4 P3
  END FUNCTION

!------------------------------------------------------------------------------

FUNCTION CIERRE1
String lista$(1)
CLEARS
PRINT "Inicio a Recorrer Archivo: "+FILEI2$
Print "Generando Interface "+FILEI1$
LOCATE 3,1: PRINT "Registro :"
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
READ FORM "T43 I4 I2 T55 I2 C456"; #IOS2,REC.NO;            \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		                       ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
BAN.PRG$ = "0"					             ! Program Control
FOR REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Dim LISTA$(8)
  READ FORM "T5 C508"; #IOS2, REC.NO;  H$                    ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = MID$(H$,X,KEY.LEN)                ! Extract First key
  WHILE KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1               
    LOCATE 3,18: PRINT R
    LISTA$(1) = UNPACK$(MID$(H$,X+0,7))                      ! Tipo variedad de pago
    lista$(1) = Right$(lista$(1),12)
    
!    LISTA$(2) = UNPACK$(MID$(H$,X+1,1))                      ! Tipo promo 1 o 2 
!    LISTA$(3) = UNPACK$(MID$(H$,X+2,3))                      ! fecha inicio
!    LISTA$(4) = UNPACK$(MID$(H$,X+5,3))                      ! fecha fin 
!    LISTA$(5) = UNPACK$(MID$(H$,X+8,2))                      ! ptg dscto
!    LISTA$(6) =        (MID$(H$,X+10,4))                     ! tipifica
!    LISTA$(7) = "PROMO "+UNPACK$(MID$(H$,X+14,2))                     ! cod promo

    
!    Write #IOS1;               \! 
!    LISTA$(1),LISTA$(2),LISTA$(3),LISTA$(4),LISTA$(5),LISTA$(6),LISTA$(7),FINR$

    Write form "C12 C2" ;#ios1; Lista$(1), finr$
    
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = MID$(H$,X,KEY.LEN)                                ! Pick up next key
    IF R.S = MAX.R.SEC THEN KEY$ = PZERO$                    ! If EOF() record or file
  WEND
NEXT REC.NO
END FUNCTION
!--- Fin del cierre de EAMFBACT

!---- Fin definicion de los procedimientos


!-------
! Bloque Principal
!-------

ON ERROR GOTO E2
FILEI2$ = "/LOGS/TFAFPAGO.COP"	                     ! Define input data file
IOS2 = 3					             ! Number of input session # 2
IOS1 = 4					             ! Number of input session # 1
FINR$ = CHR$(13)+CHR$(10)                                    ! End of record
Open FILEI2$ DIRECT RECL 512 AS IOS2                         ! Open EAMCUSTA
CREATE "ADX_UDT1:SALIDA.TXT" As IOS1                         ! Crea archivo Clientes matriculados en EAMCUSTA
Call CIERRE1
CLOSE IOS2
CLOSE IOS1
Stop

E2:  
 If Errf% = ios1 And ERR = "EF" Then Ctl% = 0 : Resume 


     LOCATE 18,1:PRINT "PLAPROMO.286 RUN TIME ERROR"         ! Routine Error
     LOCATE 19,1:PRINT "SESSION NUMBER => ";ERRF%    
     LOCATE 20,1:PRINT "ERROR CODE     => ";ERR      
     LOCATE 21,1:PRINT "ERRN           => ";ERRN      
     LOCATE 22,1:PRINT "RECORDS ON THE SECTOR => "; R.S
     LOCATE 23,1:PRINT "X                     => "; X
END