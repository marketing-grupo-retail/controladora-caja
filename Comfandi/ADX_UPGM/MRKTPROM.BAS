!************************************************** 
!Empresa       : Grupo Retail Ltda                *
!Programa      : MRKTPROM.BAS                     *
!Autor         : Oscar Valencia Sarmiento         *
!Lenguaje      : Basic 4690 IBM                   * 
!Fecha         : Agosto 21 de 2.002               *
!Observaciones : Carga promociones especiales para*
!                clientes y productos modulo de   *
!                mercadeo                         *
!**************************************************

%ENVIRON C					! Ambiente Controlador

String    GLOBAL ERRFX$, MSG$, DATA$, ARCHIVO$
INTEGER*1 GLOBAL BAN.PRG%, POS%,TS.ERROR, Terror%, Topen%, I%

%INCLUDE BASROUT.J86

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
   Locate 3,12: Print "**** ACT. PROMOCIONES MODULO MERCADEO     Ver. 1.00 ****"
   Locate 3,75: Print CHR$(179)
   Locate 4,10: Print CHR$(27)+"b3"
   Locate 4,12: Print "***  Ultima Revisi¢n Software Septiembre 12 2010 G.R. ***"
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
CREATE "MKTP:MKPTMP2.DAT" AS 82	
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
    C01$= Unpack$(MID$(H$,X+0,9))                            ! Codigo Evento
    C02$= Unpack$(MID$(H$,X+9,2))                            ! Codigo del Mensaje
    C03$= Mid$(H$,X+9,10)                                    ! Valor de comparacion
    C04$= Mid$(H$,X+19,45)                                   ! Lista de clientes
    Print "LEIDO ",C01$
    If Left$(C01$,2) = "32" Then Begin 										   ! Mensaje no presencia depto
      Buffer$ = C01$           + \! Codigo del Depto
                C02$           + \! Mensaje a Generar
                Right$(C03$,6) + \! Valor de comparacion
                C04$           + \! Lista de Clientes    
                Finr$             ! Final de Registro
      Print BUFFER$          
      LEC$ = "C"+Str$(Len(Buffer$))                          ! Formatea la salida
      Write Form Lec$ ; #81 ;buffer$                         ! Grabacion del registro

    EndIf 

    If Left$(C01$,2) = "34" Then Begin 										   ! Mensaje no presencia depto
      Buffer$ = C01$           + \! Codigo del Depto
                C02$           + \! Mensaje a Generar
                Right$(C03$,6) + \! Valor de comparacion
                C04$           + \! Lista de Clientes    
                Finr$             ! Final de Registro
      Print BUFFER$          
      LEC$ = "C"+Str$(Len(Buffer$))                          ! Formatea la salida
      Write Form Lec$ ; #82 ;buffer$                         ! Grabacion del registro

    EndIf 


    
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
Next REC.NO
Close 33 
Close 81
Close 82

End Sub 


Sub PROCESA
 String UE.LOG$, UE.COD$, M1$, M2$, M3$, M4$, M5$, M6$, UE.CUPON$, KEY$, M7$, M8$, M9$
 String C1$, C2$, C3$, C4$, C5$, C9$
 String X$, PROMOS$(1), X.A$
 Integer*1 IND.GRABA%, II%, X%, Y%
 TOpen% = 0
 Again.Procesa:
 Terror% = -1
 Open ARCHIVO$ As 1
 If Terror% = 0 Then Begin 
 	  Wait ; 1800
 	  TOpen% = TOpen% + 1 
 	  If TOpen% > 5 Then Stop 
 	  GoTo Again.Procesa
 Endif 
 
 If End #1 Then FIN.CARGA
 Locate 12,15: Print "Procesando Informacion ..."
 POS% = 0
 !--- Estructura archivo de carga
 !--- Tipo promocion, Codigo Itm/Clte, Msg, Vlr Comparacion, Lista Cliente, codigo promocion
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
   C2$ = Right$("0000000000000000"+C2$,16)     ! Plu - Cliente Promocion - Tipificacion
   C3$ = Right$("0000"+C3$,4)                  ! Mensaje Promocion
   M1$ = Pack$(C1$ + C2$)                      ! Arma llave del registro
   Print "ALMACENA "+UNPACK$(M1$)
   If C1$ <> "31" Then Begin 									 ! Si diferente a lista de clientes
      C4$ = Right$("000000000000"+C4$,10)      ! Valor Comparacion
      C5$ = Right$("0000"+C5$,4)               ! Ajusta dato lista cliente
      C5$ = Left$(C5$+String$(45,"0"),45)      ! Cliente Objetivo
      M2$ = Pack$(Right$("0000"+C3$,4))        ! Empaqueta Mensaje
      Write Form "C9 C2 C10 C45";#33;         \! Almacena Informacion
             M1$,			                        \! Codigo del articulo / Cliente
             M2$,                             \! Mensaje Publicitario
             C4$,                             \! Valor Compra Minima
             C5$                               ! Lista de cliente a quien va dirigida la promocion
             
   EndIf Else Begin																														! Carga de clientes
      BAN.PRG% = -1																														! Control de Error
      C4$ = Right$("0000"+C4$,4)																							! Ajusta lista promocion
      If C3$ = "00AA" Then C3$ = "0000" 																		  ! Si no cambia mensaje al cliente
      READ FORM "C9 C2 C10 C45";#33 KEY M1$;     		                         \! Lee registro
      M9$, M2$, M3$, M4$																											!
      If Ban.Prg% <> -1 Then Begin 																						! Cliente Nuevo
      	 If C3$ <> "0000" Then BEGIN                  												! si hay mensaje 
      	 	  M2$ = Pack$(Right$("0000"+C3$,4))         												! Empaqueta Mensaje
      	 EndIf Else M2$ = PACK$("0000")																				! Coloca mensaje en ceros
         M3$ = PACK$("00000000000000000000")          												! Relleno
         C4$ = Right$("0000"+C4$,4)                   												! Ajusta dato lista cliente
         C4$ = Pack$(Left$(C4$+String$(90,"0"),90))   												! Cliente Objetivo
      EndIf Else Begin 																												! Cliente Existente
      	 If C3$ <> "0000" Then BEGIN                  												! si hay mensaje 
      	 	  M2$ = Pack$(Right$("0000"+C3$,4))         												! Cambia mensaje al cliente
      	 EndIf
      	 M4$ = Unpack$(M4$)																										! Desempaqueta lista de promociones
      	 If C4$ <> "0000" Then Begin 																					! Si promocion para cliente especifico
      	 	If (Match(C4$,M4$,1) <= 0) Then Begin																! Promocion no matriculada
      	 		C2$ = ""																													! Inicializa lista promociones
            For I% = 1 To 88 STEP 4																						! Recorre String
             C3$ = MID$(M4$,I%,4)																							! Toma promocion
             If C3$ = "0000" Then BEGIN 																			! Si hay campo libre
             	  C3$ = C4$																											! Asigna promocion
             	  C4$ = "0000"																									! Init variable
             EndIf 																														!
             C2$ = C2$ + C3$                      														! ARMA LISTA DE PROMOCIONES
            Next I%
      	 		C4$ = Pack$(Left$(C2$+String$(90,"0"),90)) 												! Lista promociones para Cliente Objetivo
      	 	EndIf	Else Begin																										! Promocion ya existente
      	 		C4$ = Pack$(M4$)																									! Empaqueta lista Actual de promocion
      	 	EndIf																																! Fin matriculacion promocion en lista cliente
      	 EndIf
      EndIf
      Write Form "C9 C2 C10 C45";#33;         \! Almacena Informacion
             M1$,			                        \! Codigo del articulo / Cliente
             M2$,                             \! Mensaje Publicitario
             M3$,                             \! Valor Compra Minima
             C4$                               ! Lista de cliente a quien va dirigida la promocion
   EndIf 
                
   If UNPACK$(M2$) <> "0000" Then \
      Call Osshell("C:\GRMODUL\MSGCARGA.286 "+"C:\PROMODAT\MSGD"+UNPACK$(M2$)+".PRM") ! Carga mensaje
      
   !EndIf 

 Wend
 FIN.CARGA:
    Close 1 
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
            BAN.PRG% = 0				   													! ejecucion normal del
            RESUME
         EndIf
         If ERRF% = 1 AND                                 \! Validacion si existe 
            (ERR = "OE" OR ERR = "FU") Then Begin          ! el archivo de control
            Terror% = 0 
            Locate 23,1: Print String$(78," ")             ! si no existe lo crea.
            Locate 23,1: Print "Msg: Archivo de Carga No Existe ..."
            Resume                                         ! retorna ejecucion
         EndIf                                             ! del programa
         CALL TRADUCE.ERROR
         MSG$ = "Msg: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
         Locate 23,1:Print MSG$                            ! Presenta Error pantalla
