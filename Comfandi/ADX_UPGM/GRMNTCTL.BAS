! PROGRAMA GRMNTCTL.BAS
! LISTA EL MAESTRO DE ARTICULOS, LECTURA DIRECT FILE
! ELIMINACION PRODUCTOS MEDICAMENTOS
!---------------------------------------------------------------
! Mod 15Ene2019
! Se cambia la hora de ejecucion a las 2 am por solicitud
! de Comfandi.
!---------------------------------------------------------------


%ENVIRON C						   																		 ! Ambiente de controlador
Integer*1 Global U.Error%
Integer*2 Contador%
Integer*4 Total.Itm%, Xval%, Tot.Med%, PP
String Global UE.MSG$, ERRFX$, Finr$, XFecha$,Almacen$,Lista$, Archivo$, Men$, Tiva$(1)
Integer*1 Global Dptos(1)
String Global DptoIni$, Dptofin$, Detalla$, DATO.SO$

Sub ADXSERVE(RET,FUNC,PARM1,PARM2) External                  ! Funciones S.O.
   Integer*4 RET
   Integer*2 FUNC,PARM1
   String PARM2
End Sub

Function GETN1(P1$,P2) External
     Integer*1 GETN1
     String P1$
     Integer*2 P2
End Function 

Function TRADUCE.ERROR                                       !
Integer*4 HX%, S%, SX%, SUM%
String    Z$ 
    HX% = ERRN                                               ! Traduccion de los
    ERRFX$=""                                                ! codigos de error
    FOR S% = 28 TO 0 STEP -4                                 ! del sistema operativo
        SX% = SHIFT(HX%,S%)                                  !
        SUM% = SX% AND 000FH                                 !
        IF SUM% > 9 THEN SUM% = SUM% + 55                   \!
        ELSE SUM% = SUM% + 48                                !
        Z$ = CHR$(SUM%)                                      !
        ERRFX$ = ERRFX$ + Z$                                 !
    NEXT S%                                                  !
End Function

Sub Mensaje.Log
 If DATO.SO$ = "1" Then Call ADXSERVE(PP,26,1,MEN$) \
  Else Locate 15,1: Print MEN$
End Sub 


Sub RECORRER.ITEMR
Integer*4 REC.NO, I%, Blk.Num, MAX.R.SEC, X, R.S, R, Ncli%, X.Len%, XIR1%, XIR1A%, UE.INDI1%, UE.TARIFA%, UE.INDI2%
String H$, PZERO$, REG$, C01$, C02$, C03$, C04$, C05$, C06$, KEY$, X.Lec$, XCODENSA$, Prov$, IND1$, UE.BIN$, Tarifa$
String Rbuffer$, tag$
Integer*2 Key.Len, Rec.Len
Dim Dptos(5000)
Total.Itm% = 0
Ncli% = 0 
Contador%=0
Tot.Med% = 0
Open "EAMITEMR" DIRECT RECL 512 AS 33 buffsize 32640  	     ! Apertura control de promociones
REC.NO = 1					             ! Initialize Counter
I% = 1						               ! Initialize Counter
Read Form "T43 I4 I2 T55 I2 C456"; #33,REC.NO;              \! Read Firts Record
     BLK.NUM, REC.LEN, KEY.LEN, H$		             ! 
PZERO$ = PACK$(STRING$(2*KEY.LEN,"0"))                       ! Armed Key Control
MAX.R.SEC = 508/REC.LEN                                      ! Length record
U.Error% = -1                                                ! Control de errores
men$ = "TOTAL BLOQUES PROCESAR :"+STR$(BLK.NUM)
Call Mensaje.Log
wait ; 1200
For REC.NO = 2 TO BLK.NUM                                    ! Cicle to read all blocks  
  REG$=""
  Read Form "T5 C508"; #33, REC.NO;  H$                      ! H$ contains block 
  X = 1 : R.S = 0 : KEY$ = Mid$(H$,X,KEY.LEN)                ! Extract First key
  men$ = "PROCESANDO BLOQUE :"+STR$(REC.NO)
  Call mensaje.log
  While KEY$  NE  PZERO$                                     ! Inside sector loop 
    R.S = R.S + 1                                            ! Records On This Sector 
    R = R + 1                                                !
    C01$= Unpack$(MID$(H$,X+0,6))                            ! Codigo 
    ind1$ = MID$(H$,X+0,10)
    UE.INDI1% = GETN1(ind1$,7)                               ! INDICAT 1
    UE.INDI2% = GETN1(ind1$,8)                               ! INDICAT 2
    XIR1A% = Val(Unpack$(MID$(H$,X+8,1)))                    ! IR INDICAT1A
    !C04$= Unpack$(MID$(H$,X+10,2))                           ! Dpto
    C06$= Unpack$(MID$(H$,X+17,5))                           ! PRECIO
    C05$= (MID$(H$,X+24,18))                                 ! Descripcion
    C03$= Unpack$(MID$(H$,X+9,1))                            ! INDICAT2
    C04$= MID$(H$,X+46,31)                                   ! Estructura comercial
    
    If UE.INDI1% AND 80H Then                               \! Tax plan A
      UE.TARIFA% = 1                                        \!
    Else IF UE.INDI1% AND 40H Then                          \! Tax plan B
      UE.TARIFA% = 2                                        \! 
    Else If UE.INDI1% AND 20H Then                          \! Tax plan C
      UE.TARIFA% = 3                                        \!
    Else If UE.INDI1% AND 10H Then                          \! Tax plan D
      UE.TARIFA% = 4                                        \!
    Else UE.TARIFA% = 8                                      ! Excluido
    Total.Itm% = Total.Itm% + 1  														 ! Total de items
    
    TARIFA$ = STR$(UE.TARIFA%)
    men$ = C04$
    PROV$ = Str$(Val(C01$))
    If Ucase$(Detalla$) = "S" Then BEGIN  
     If Left$(C04$,4) = "1006" Then Begin
     	  Tot.Med% = Tot.Med% + 1
        Rbuffer$ = C01$
        locate 20,1 : Print Rbuffer$
        X.Len% = Len(Rbuffer$)						  					  								! Toma longitud del registro
        X.Lec$ = "C"+Str$(X.len%)+" C2"								  							  ! Arma estructura de grabacion
        Write form X.Lec$; #77 ; Rbuffer$, Finr$           							! Graba registro
     EndIf 
    EndIf  
    Proximo.Registro:
    X = X + REC.LEN                                          ! Index to next key
    KEY$ = Mid$(H$,X,KEY.LEN)                                ! Pick up next key
    If R.S = MAX.R.SEC Then KEY$ = PZERO$                    ! If EOF() record or file
  Wend
 Next REC.NO
 Close 33
 I% = 0
End Sub 

Sub LEER.CABECERA
 String PARM2$
 Integer*2 PARM1,RET
 Call ADXSERVE(RET,4,PARM1,PARM2$)                 ! 
 ALMACEN$ = RIGHT$("000"+STR$(VAL(MID$(PARM2$,1,4))),3)
End Sub 
!--- Fin de la funcion de lectura

Sub Reset.Maestro
String Lista$
Integer*4 Xi%, Xj%
  Xi% = 0 : Xj% = 0
  Open "EAMITEMR" KEYED RECL 77 As 4
  Open ARCHIVO$ As 77
  If End # 77 Then FIN.ACT.ITEMR																							! Si Error de apertura  
  While (1)
    Read #77; Lista$
    Xi% = Xi% + 1
    Men$ = "Procesa "+Str$(Xi%)+" de "+Str$(Tot.Med%)
    Call Mensaje.Log
    Lista$ = Pack$(Right$("000000000000"+Lista$,12))
    U.Error% = -1
    DelRec 4 ; Lista$
    If U.Error% = 0 Then Xj% = Xj% + 1
  Wend 
  FIN.ACT.ITEMR:
    Close 77 : Close 4
    Men$ = "Fin del Proceso, Fallidos "+Str$(Xj%)
    Call Mensaje.Log
End Sub 


Sub CALCULO.HORA
String Xh$, Xa$, Xb$
Integer*4 Xa%, Xb%
While Xb$ <> "X"
  Xb% = 100000          						! Calcula tiempo en tiempos de Relog 
  Call Adxserve(0,26,1,"Start Process At 2:00 am "+Left$(time$,2)+":"+mid$(time$,3,2))
  Wait ; Xb%
  If Left$(time$,2) = "02" Then Xb$ = "X"
Wend 
End Sub
!--- Fin calculo hora



!-- Bloque Principal
On Error GoTo Error.PRG				! Control de errores
CLEARS
DATO.SO$ = COMMAND$                                             ! Dato S.O

If Ucase$(DATO.SO$) = "VERSION" Then Begin
   Print "Grupo Retail Ltda - Eliminacion Medicamentos Maestro Productos "
   Print "Version 1.1 15-Ene-2019"
   Stop 
EndIf 
If DATO.SO$ = "BACKGRND" Then DATO.SO$ = "1"


	
Call Leer.Cabecera				! Toma numero de tienda del store system
FINR$=CHR$(13)+CHR$(10)
ARCHIVO$="ADX_UDT1:GRMNTCTL."+ALMACEN$        ! Nombre archivo de salida
CREATE ARCHIVO$ As 77
If DATO.SO$ <> "1" Then BEGIN
Print "Grupo Retail Ltda - Eliminacion Medicamentos Maestro Productos "
Print "Version 1.1 15-Ene-2019"
men$ = "Creando Estructura :"+archivo$
Call Mensaje.Log
ENDIF Else BEGIN
	Call CALCULO.HORA
ENDIF 
U.Error% = -1
Detalla$ = "S"
Call RECORRER.ITEMR				! Recorre maestro articulos
Close 77
wait ; 500
CLEARS
Print "Grupo Retail Ltda - Eliminacion Medicamentos Maestro Productos "
Print "Version 1.1 15-Ene-2019"
LOCATE 3,1 : Print "Procesando :"+archivo$
Men$ = "Total a eliminar :"+Str$(Tot.Med%)
Call mensaje.log
wait ; 1200
Call Reset.Maestro
Stop						! Para Ejecucion del programa

Error.Prg:
  U.Error% = 0
  If ERR = "IH" Then Resume
  If ERR = "EF" Then Resume
  CALL TRADUCE.ERROR
  UE.MSG$ = "Error: "+ERR+" Sesion: "+STR$(ERRF%)+"-"+ERRFX$
  Print UE.MSG$
  Stop 
