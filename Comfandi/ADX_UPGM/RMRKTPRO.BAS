!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : RMKTPROM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Agosto 21 de 2.002               *
!Observaciones : Carga promociones especiales para*
!                clientes y productos modulo de   *
!                mercadeo - ELIMINA -             *
!**************************************************

%ENVIRON C					! Ambiente Controlador

String    GLOBAL ERRFX$, MSG$, DATA$, ARCHIVO$, U.Msg$
INTEGER*1 GLOBAL BAN.PRG%, POS%,TS.ERROR

%INCLUDE ADX_UPGM:BASROUT.J86

Function TRADUCE.ERROR                                       !
INTEGER*4 HX%, S%, SX%, SUM%
String Z$
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    For S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIfT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        If SUM% > 9 Then SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    Next S%                                                  !
End Function

Sub PANTALLA
   CLEARS
   Locate 2, 4: Print CHR$(218)+String$(70,CHR$(196))+CHR$(191)	! TODO LO DE ARRIBA
   Locate 3, 4: Print CHR$(179)
   Locate 4, 4: Print CHR$(179)
   Locate 3,12: Print "**** RET. PROMOCIONES MODULO MERCADEO     Ver. 1.00 ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revisi¢n Software Agosto 11 de 2011 G.R.  ***"
   Locate 4, 7: Print CHR$(27)+"b7"
   Locate 4,75: Print CHR$(179)
   Locate 5, 4: Print CHR$(192)+String$(70,CHR$(196))+CHR$(217) ! LINEA DE ABAJO
   Locate 23,1: Print "Msg:"
End Sub

Sub BARRA
   POS% = POS% + 1
   If POS% = 1 Then Locate  12,45: Print "|"
   If POS% = 2 Then Locate  12,45: Print "/"
   If POS% = 3 Then Locate  12,45: Print "-"
   If POS% = 4 Then Locate  12,45: Print "\"
   If POS% = 4 Then POS% = 0
End Sub
!--- Fin barra de estado

Sub GENERA.PLANO.CARGA
String C01$, C02$, C03$, C04$, buffer$, LEC$, Finr$, H$, PZERO$, KEY$, REG$
Integer*4 REC.NO, I%, Max.R.SEC, BLK.NUM, R.S, X, R 
Integer*2 REC.LEN, KEY.LEN
Open "MKTP:MKPDEFPR.DAT" DIRECT RECL 512 AS 33   ! Apertura control de promociones
CREATE "MKTP:MKPTMP1.DAT" AS 81	
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
FINR$=CHR$(13)+CHR$(10)
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
    C01$= Unpack$(MID$(H$,X+0,7))                            ! Codigo Evento
    C02$= Unpack$(MID$(H$,X+7,2))                            ! Codigo del Mensaje
    C03$= Mid$(H$,X+9,10)                                    ! Valor de comparacion
    C04$= Mid$(H$,X+9,10)                                    ! Lista de clientes
    Print "LEIDO ",C01$
    If Left$(C01$,2) = "32" Then Begin 										 ! Mensaje no presencia depto
      Buffer$ = C01$           + \! Codigo del Depto
                C02$           + \! Mensaje a Generar
                Right$(C03$,6) + \! Valor de comparacion
                Right$(C04$,4) + \! Lista de Clientes    
                Finr$             ! Final de Registro
      Print BUFFER$          
      LEC$ = "C"+Str$(Len(Buffer$))                          ! Formatea la salida
      Write Form Lec$ ; #81 ;buffer$                         ! Grabacion del registro
    EndIf 
    
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33 
Close 81

End Sub 


Sub PROCESA
 String UE.LOG$, UE.COD$, M1$, M2$, M3$, M4$, M5$, M6$, UE.CUPON$, KEY$, M7$, M9$, MX$
 String C1$, C2$, C3$, C4$, C5$, C9$
 String X$, PROMOS$(1), X.A$
 Integer*1 IND.GRABA%, II%, X%, Y%, I%
 Integer*4 borrados%
 Open ARCHIVO$ AS 1
 If End #1 Then FIN.CARGA
 Locate 12,15: Print "Procesando Informacion ..."
 POS% = 0
 !--- Estructura archivo de carga
 !--- Tipo promocion, Codigo Itm/Clte, Msg, Vlr Comparacion, Lista Cliente
 !30 = Presencia de un producto 
 !31 = Presencia de un cliente
 !32 = No presencia de una sección 
 !33 = Compra mínima 
 !34 = Por no presencia de un PLU 
 !35 = Presencia de un departamento
 !36 = Presencia de una forma de pago
 
 While (1)
   Call BARRA
   Read #1;C1$, C2$, C3$, C4$, C5$, C9$
   C1$ = Right$("00"+C1$,2)                    ! Tipo de promocion
   C2$ = Right$("0000000000000000"+C2$,16)     ! Tipo de promocion
   C3$ = Right$("0000"+C3$,4)                  ! Mensaje Promocion
   C5$ = Right$("0000"+C5$,4)                  ! Ajusta dato lista cliente
   C5$ = Left$(C5$+"000000000000",10)          ! Cliente Objetivo
   M1$ = Pack$(C1$ + C2$)                      ! Arma llave del registro
   MX$ = Pack$(C3$)                            ! Empaqueta Mensaje
   Print "ANULA    "+UNPACK$(M1$)
   If C1$ <> "31" Then Begin 									 ! Si diferente a lista de clientes
      Delrec 33; M1$
   EndIf Else Begin
      BAN.PRG% = -1
      C4$ = Right$("0000"+C4$,4)               ! Promocion a retirar
      C2$ = ""
      READ FORM "C9 C2 C10 C45";#33 KEY M1$;  \! Lee registro
      M9$, M2$, M3$, M4$
      If BAN.PRG% = -1 Then Begin 						 ! Si existe el cliente
      	 M4$ = UNPACK$(M4$)                    ! LISTA DE PROMOCIONES ACTIVAS
         If C4$ <> "0000" Then Begin          \! Si promocion para un cliente especifico
          For I% = 1 To 88 STEP 4	 						 ! Recorre String
             C3$ = MID$(M4$,I%,4)							 ! Toma promocion
             If C3$ = C4$  Then BEGIN 		     ! Si encuentra la promocion
             	  C3$ = "0000"									 ! Asigna promocion
             	  C4$ = "0000"									 ! Init variable
             EndIf 														 !
             C2$ = C2$ + C3$                   ! ARMA LISTA DE PROMOCIONES
          Next I%
         EndIf Else Begin                      ! No hay retiro promocion
         	 C2$ = M4$													 ! Entrega lista intacta de promociones asignadas
         EndIf
         C4$ = Pack$(Left$(C2$+String$(90,"0"),90))   ! Cliente Objetivo
         If MX$ <> Pack$("0000") Then Mx$ = Pack$("0000")
         	
         Write Form "C9 C2 C10 C45";#33;      \! Almacena Informacion
             M1$,			                        \! Codigo del articulo / Cliente
             MX$,                             \! Mensaje Publicitario
             M3$,                             \! Valor Compra Minima
             C4$                               ! Lista de cliente a quien va dirigida la promocion
      EndIf   	
   EndIf  

 Wend
 FIN.CARGA:
    CLEARS
    Print "TOTAL BORRADOS "+STR$(BORRADOS%)
    Delete  1 
    Close 33 
    Call GENERA.PLANO.CARGA
End Sub

Sub TERMINAR
 Locate 23,1: Print String$(78," ")     	        ! si no existe lo crea. 
 Locate 23,1: Print "Msg: Fin Ejecucion Programa ..."	!
 Stop							! Fin del programa
End Sub
!--- Fin de terminar


!------
!--- Bloque Principal
!------

ON ERROR GOTO ERRORES
ARCHIVO$ = COMMAND$
CALL PANTALLA					! Pantalla principal
OPEN "MKTP:MKPDEFPR.DAT" KEYED RECL 66 AS 33 ! Apertura logos
CALL PROCESA					! Procesa Carga
CALL TERMINAR

!--- Definicion de las rutinas de error
ERRORES:
         If ERRF% = 4 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Locate 23,1: Print String$(78," ")             ! si no existe lo crea.
            Locate 23,1: Print "Msg: Error, Maestro de Promociones no Existe ..."
            STOP                                           ! retorna ejecucion
         EndIf                                             ! del programa
         If ERRF% = 4 AND ERR = "EF" Then Begin           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 33 AND ERR = "EF" Then Begin           \! Si encuentra fin de 
            BAN.PRG% = 0				   ! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 1 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Locate 23,1: Print String$(78," ")             ! si no existe lo crea.
            Locate 23,1: Print "Msg: Archivo de Carga No Existe ..."
            STOP                                           ! retorna ejecucion
         EndIf                                             ! del programa
         CALL TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 23,1:Print MSG$                            ! Presenta Error pantalla
